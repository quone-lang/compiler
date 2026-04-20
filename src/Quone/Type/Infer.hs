{-| Hindley-Milner inference for the v0.0.1 expression core.

Implements LANGUAGE.md sections 8.1 through 8.6 and 8.8:

* HM inference with generalisation at top-level and let bindings;
* type annotation checking against inferred types;
* monomorphic operator typing per section 8.8 (mixed-primitive
  operands rejected);
* pattern-matching typing per section 8.4, including record patterns
  in their short and long forms;
* record literal typing as closed records;
* dataframe verb typing is `[planned]` for stage 7.

The inference is total: any error becomes a 'Diagnostic' rather than
an exception. The result of running the inferer over a 'Program' is
either a 'Diagnostic' or a 'TypedProgram' carrying each top-level
binding's principal scheme.

-}
module Quone.Type.Infer
    ( -- * Top-level
      TypedProgram (..)
    , inferProgram
      -- * Per-expression entry (used by tests)
    , inferExprIn
    , runInfer
    , freshVar
    )
where

import Control.Monad (foldM, forM, unless, when, zipWithM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Diagnostic
    ( Category (..)
    , Diagnostic (..)
    , Severity (Error)
    )
import Quone.Position (SourceSpan, emptySpan)
import Quone.Type.Env
import Quone.Type.Types
import qualified Quone.Type.Verb as Verb
import qualified Prelude



-- ---------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------


-- | A type-checked program: the original AST plus the inferred scheme
-- for each top-level value binding.
data TypedProgram = TypedProgram
    { typedProgram :: Program
    , typedBindings :: Map.Map Text Scheme
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Infer types for every top-level binding. Returns the typed program
-- on success, or the first diagnostic on failure.
inferProgram :: Program -> Prelude.Either Diagnostic TypedProgram
inferProgram prog =
    let
        env = registerCustomTypes initialEnv (programDecls prog)
    in
    case runInfer (inferDecls env (programDecls prog)) of
        Prelude.Left d -> Prelude.Left d
        Prelude.Right (binds, _) ->
            Prelude.Right
                ( TypedProgram
                    { typedProgram = prog
                    , typedBindings = binds
                    }
                )



-- ---------------------------------------------------------------------
-- Inference monad
-- ---------------------------------------------------------------------


newtype Infer a = Infer
    { runInferState :: InferState -> InferResult a
    }


data InferResult a
    = IOk a InferState
    | IErr Diagnostic


data InferState = InferState
    { isNextVar :: Int
    , isSubst :: Subst
    -- | Type variables that appeared as a numeric operand somewhere
    -- and were not yet pinned to a concrete ground type. After a
    -- value declaration is fully inferred, 'defaultPendingNumerics'
    -- defaults any of these that are still free to 'Double',
    -- matching R's bare-numeric default. Cleared between decls.
    , isNumericVars :: Set.Set TyVar
    }


instance Prelude.Functor Infer where
    fmap f (Infer run) = Infer (\s -> case run s of
        IOk a s' -> IOk (f a) s'
        IErr d -> IErr d)


instance Prelude.Applicative Infer where
    pure x = Infer (\s -> IOk x s)
    Infer pf <*> Infer px = Infer (\s -> case pf s of
        IErr d -> IErr d
        IOk f s' -> case px s' of
            IErr d -> IErr d
            IOk x s'' -> IOk (f x) s'')


instance Prelude.Monad Infer where
    Infer run >>= k = Infer (\s -> case run s of
        IErr d -> IErr d
        IOk a s' -> runInferState (k a) s')


-- | Run an Infer action. Returns either a diagnostic or the value.
runInfer :: Infer a -> Prelude.Either Diagnostic a
runInfer (Infer run) =
    case run
        ( InferState
            { isNextVar = 1000
            , isSubst = emptySubst
            , isNumericVars = Set.empty
            }
        ) of
        IOk a _ -> Prelude.Right a
        IErr d -> Prelude.Left d


inferFail :: Diagnostic -> Infer a
inferFail d = Infer (\_ -> IErr d)


-- | Run a fresh inference action as a pure callback. Used by the verb
-- typer to evaluate row-scoped expressions without circular module
-- dependencies. Side-note: this discards the inferer's substitution
-- state, which is fine for predicate / mutator expressions whose
-- types must stand on their own.
runInferAsCallback :: Env -> Expr -> Prelude.Either Diagnostic Type
runInferAsCallback env e = runInfer (inferExprIn env e)


getSubst :: Infer Subst
getSubst = Infer (\s -> IOk (isSubst s) s)


putSubst :: Subst -> Infer ()
putSubst sub = Infer (\s -> IOk () (s {isSubst = sub}))


-- | Allocate a fresh type variable.
freshVar :: Text -> Infer Type
freshVar hint = Infer <| \s ->
    let
        nextId = isNextVar s
    in
    IOk
        (TyVarT (TyVar nextId hint))
        (s {isNextVar = nextId Prelude.+ 1})


-- | Record that an operand of an arithmetic / comparison operator
-- was a type variable. After a value declaration finishes inferring,
-- 'defaultPendingNumerics' rewrites any of these that are still
-- unresolved to 'Double'. Concrete (non-variable) operands are
-- ignored.
markNumeric :: Type -> Infer ()
markNumeric = \case
    TyVarT v -> Infer (\s ->
        IOk () (s {isNumericVars = Set.insert v (isNumericVars s)}))
    _ -> Prelude.pure ()


-- | Default every still-unresolved numeric type variable accumulated
-- during the current declaration to 'Double', then clear the set.
--
-- Per LANGUAGE.md section 8.8, R's bare numeric default is double;
-- a Quone author who wants integer arithmetic must either annotate
-- the binding or use the @L@ literal suffix. This rule runs at the
-- decl boundary so the defaulting decision is local: it cannot
-- affect inference for any binding other than the one currently
-- being checked.
defaultPendingNumerics :: Infer ()
defaultPendingNumerics = do
    sub <- getSubst
    pending <- Infer (\s -> IOk (isNumericVars s) s)
    Prelude.mapM_
        (\v ->
            case applySubst sub (TyVarT v) of
                TyVarT _ -> do
                    _ <- unifyAt emptySpan (TyVarT v) primDouble
                    Prelude.pure ()
                _ -> Prelude.pure ())
        (Set.toList pending)
    Infer (\s -> IOk () (s {isNumericVars = Set.empty}))



-- ---------------------------------------------------------------------
-- Custom-type registration
-- ---------------------------------------------------------------------


-- | Add user-declared custom types and their constructors to the
-- environment so the inferer can look them up. Run before inference
-- proper.
registerCustomTypes :: Env -> [Decl] -> Env
registerCustomTypes = List.foldl' step
  where
    step env decl = case decl of
        DType d ->
            let
                tyName = upperText (typeDeclName d)
                tparams =
                    Prelude.zipWith
                        (\i p -> TyVar (Prelude.negate (i Prelude.+ 1)) (lowerText p))
                        [0 ..]
                        (typeDeclParams d)
                resultTy =
                    List.foldl'
                        (\acc tv -> TyApp acc (TyVarT tv))
                        (TyCon tyName)
                        tparams

                env' = insertType tyName (Prelude.fromIntegral (Prelude.length tparams)) env

                addCon e v =
                    let
                        argTys =
                            Prelude.fmap
                                (typeAtomToType tparams)
                                (variantArgs v)

                        info =
                            ConstructorInfo
                                { ciTypeName = tyName
                                , ciTypeParams = tparams
                                , ciArgTypes = argTys
                                , ciResultType = resultTy
                                }

                        -- Constructor's value-level scheme:
                        -- forall tparams. arg1 -> ... -> argN -> result
                        ctorScheme =
                            Scheme
                                { schemeVars = tparams
                                , schemeBody =
                                    Prelude.foldr
                                        TyFun
                                        resultTy
                                        argTys
                                }
                    in
                    insertValue (upperText (variantName v)) ctorScheme
                        (insertConstructor (upperText (variantName v)) info e)
            in
            List.foldl' addCon env' (typeDeclVariants d)
        DTypeAlias d ->
            -- Aliases are expanded structurally during type lookup;
            -- registering arity is enough for v0.0.1.
            insertType
                (upperText (aliasDeclName d))
                (Prelude.fromIntegral (Prelude.length (aliasDeclParams d)))
                env
        _ -> env


-- | Translate a surface type-atom into the internal 'Type'.
--
-- @typeParams@ is the list of in-scope type variables for the current
-- declaration; lookups by name go there before falling back to a
-- globally fresh variable.
typeAtomToType :: [TyVar] -> TypeAtom -> Type
typeAtomToType params = \case
    TName n -> TyCon (upperText n)
    TVar n ->
        case List.find (\tv -> tyVarName tv Prelude.== lowerText n) params of
            Just tv -> TyVarT tv
            Nothing ->
                -- Fall back: treat as a fresh-style variable. The
                -- inferer will instantiate at use sites.
                TyVarT (TyVar 0 (lowerText n))
    TParen _ inner -> typeSigToType params inner
    TRecord r -> TyRecord (recordFields params r)
    TDataframe _ r -> TyDataframe (recordFields params r)


typeSigToType :: [TyVar] -> TypeSig -> Type
typeSigToType params = \case
    TFun _ a b -> TyFun (typeSigToType params a) (typeSigToType params b)
    TApp _ h xs ->
        List.foldl'
            (\acc x -> TyApp acc (typeAtomToType params x))
            (typeAtomToType params h)
            xs
    TAtom a -> typeAtomToType params a


recordFields :: [TyVar] -> RecordType -> Map.Map Text Type
recordFields params (RecordType {recordTypeFields = fs}) =
    Map.fromList
        (Prelude.fmap
            (\f ->
                ( lowerText (fieldTypeName f)
                , typeSigToType params (fieldTypeSig f)
                )
            )
            fs
        )



-- ---------------------------------------------------------------------
-- Top-level inference
-- ---------------------------------------------------------------------


inferDecls
    :: Env
    -> [Decl]
    -> Infer (Map.Map Text Scheme, Env)
inferDecls env decls = do
    -- Pre-allocate a fresh type variable for every top-level value so
    -- mutual references can resolve. v0.0.1 does not do strongly-
    -- connected-component analysis; mutual recursion at the top level
    -- gets the same fresh-variable treatment as a simple let.
    (env0, holes) <- preallocate env decls
    finalised <- foldM (typecheckOne holes) env0 decls
    let
        binds =
            Map.fromList
                ( Prelude.fmap
                    (\(name, _) ->
                        ( name
                        , Map.findWithDefault
                            (monoScheme (TyCon "Integer"))
                            name
                            (envValues finalised)
                        )
                    )
                    (Map.toList holes)
                )
    Prelude.pure (binds, finalised)


preallocate
    :: Env
    -> [Decl]
    -> Infer (Env, Map.Map Text Type)
preallocate env decls = do
    let
        valueDecls = [d | DValue d <- decls]
    placeholders <-
        Prelude.traverse
            (\d -> do
                tv <- freshVar (lowerText (valueDeclName d))
                Prelude.pure (lowerText (valueDeclName d), tv))
            valueDecls
    let
        env' =
            List.foldl'
                (\e (n, t) -> insertValue n (monoScheme t) e)
                env
                placeholders
    Prelude.pure (env', Map.fromList placeholders)


typecheckOne
    :: Map.Map Text Type
    -> Env
    -> Decl
    -> Infer Env
typecheckOne holes env decl = case decl of
    DValue v -> do
        let name = lowerText (valueDeclName v)
        bodyTy <- inferValueBody env v
        case Map.lookup name holes of
            Just placeholder -> do
                _ <- unifyAt (valueDeclSpan v) placeholder bodyTy
                Prelude.pure ()
            Nothing -> Prelude.pure ()
        -- Resolve any unconstrained numeric type variables that
        -- accumulated during this decl's binop inference. Must run
        -- BEFORE generalization so defaulted vars aren't quantified
        -- into the binding's scheme.
        defaultPendingNumerics
        sub <- getSubst
        let
            finalTy = applySubst sub bodyTy
            envFTV = freeTypeVarsEnvValues env
            qvars = Set.toList (Set.difference (freeTypeVars finalTy) envFTV)
            scheme = Scheme {schemeVars = qvars, schemeBody = finalTy}
        Prelude.pure (insertValue name scheme env)
    DImport (ForeignImport _ fname sig) ->
        -- Lower-case foreign imports (`import pkg.fn : Ty`) bind a
        -- value of the given type. The R `pkg::fn` qualification is
        -- emitted at code-gen time; here we just need the name in
        -- scope at the inferred type per LANGUAGE.md section 4.5.
        Prelude.pure
            ( insertValue
                (lowerText (foreignFn fname))
                (foreignScheme sig)
                env
            )
    _ ->
        -- Type / Alias / Quone-import declarations were already
        -- processed by 'registerCustomTypes' or are pure name-binding
        -- (handled by the resolver in 'Quone.Resolve.Names').
        Prelude.pure env


-- | Infer a value-binding's body. When the user supplied a top-level
-- annotation, we install it as the EXPECTED type before walking the
-- body: each parameter is bound to its position in the annotation,
-- and the body is checked against the annotation's return type.
--
-- This is what makes
--
-- @
-- normalize : Double -> Double -> Double
-- normalize max_score raw \<- raw / max_score
-- @
--
-- typecheck without forcing the user to add a literal `1.0` somewhere
-- to pin the operand types of `\/`.
inferValueBody :: Env -> ValueDecl -> Infer Type
inferValueBody env v =
    case valueDeclAnnotation v of
        Nothing ->
            inferLambda env (valueDeclParams v) (valueDeclBody v)
        Just sig -> do
            let
                annTy = typeSigToType [] sig
                params = valueDeclParams v
            (paramTys, returnTy) <- splitArrow (valueDeclSpan v) annTy params
            let
                env' =
                    Prelude.foldr
                        (\(n, t) e -> insertValue (lowerText n) (monoScheme t) e)
                        env
                        (Prelude.zip params paramTys)
            bodyTy <- inferExprIn env' (valueDeclBody v)
            _ <- unifyAt (valueDeclSpan v) returnTy bodyTy
            Prelude.pure annTy


-- | Strip @n@ argument positions off a function type, returning the
-- argument types and the residual return type. Errors with a
-- 'TypeMismatch' diagnostic if the annotation has too few arrows for
-- the parameter list.
splitArrow
    :: SourceSpan
    -> Type
    -> [LowerName]
    -> Infer ([Type], Type)
splitArrow sp annTy params = go annTy params []
  where
    go ret [] acc = Prelude.pure (Prelude.reverse acc, ret)
    go (TyFun a b) (_ : ps) acc = go b ps (a : acc)
    go other (_ : _) _ =
        inferFail
            ( typeMismatchDiag sp
                ( "type annotation has fewer arrows than the binding has parameters; got "
                    Prelude.<> showType other
                )
            )


-- | Build a top-level scheme for a foreign-import binding. Quantify
-- over every type variable mentioned in the signature so the binding
-- can be used at multiple instantiations.
foreignScheme :: TypeSig -> Scheme
foreignScheme sig =
    let
        body = typeSigToType [] sig
        vars = Set.toList (freeTypeVars body)
    in
    Scheme {schemeVars = vars, schemeBody = body}


inferLambda :: Env -> [LowerName] -> Expr -> Infer Type
inferLambda env params body = case params of
    [] -> inferExprIn env body
    (p : rest) -> do
        pty <- freshVar (lowerText p)
        let env' = insertValue (lowerText p) (monoScheme pty) env
        bodyTy <- inferLambda env' rest body
        Prelude.pure (TyFun pty bodyTy)



-- ---------------------------------------------------------------------
-- Expression inference
-- ---------------------------------------------------------------------


inferExprIn :: Env -> Expr -> Infer Type
inferExprIn env = \case
    ELit _ lit -> Prelude.pure (literalType lit)
    EVar n ->
        case lookupValue (lowerText n) env of
            Just sch -> instantiate sch
            Nothing ->
                inferFail
                    ( unboundDiag (lowerSpan n) (lowerText n) "value"
                    )
    ECon n ->
        case lookupConstructor (upperText n) env of
            Just info -> instantiateConstructor info
            Nothing ->
                inferFail
                    ( unboundDiag (upperSpan n) (upperText n) "constructor"
                    )
    ELambda _ ps body -> inferLambda env ps body
    ECase sp scrut arms -> do
        scrutTy <- inferExprIn env scrut
        result <- freshVar "case_result"
        Prelude.traverse
            (inferArm sp env scrutTy result)
            arms
        sub <- getSubst
        Prelude.pure (applySubst sub result)
    ELet _ binds body -> do
        env' <- foldM addBinding env binds
        inferExprIn env' body
    EApp sp f x -> do
        ft <- inferExprIn env f
        xt <- inferExprIn env x
        result <- freshVar "app_result"
        _ <- unifyAt sp ft (TyFun xt result)
        sub <- getSubst
        Prelude.pure (applySubst sub result)
    EBinOp sp op l r -> inferBinOp env sp op l r
    EUnary sp OpNeg e -> do
        et <- inferExprIn env e
        sub <- getSubst
        let resolved = applySubst sub et
        if isPrimNumeric resolved
            then Prelude.pure resolved
            else
                inferFail
                    ( typeMismatchDiag sp
                        ("unary minus expects Integer or Double; got " Prelude.<> showType resolved)
                    )
    EPipe sp lhs rhs -> do
        lt <- inferExprIn env lhs
        sub <- getSubst
        case (applySubst sub lt, rhs) of
            (TyDataframe schema, EVerb vsp verb args) ->
                -- Dataframe verb on a known schema: dispatch to the
                -- verb-specific typer (LANGUAGE.md section 8.7).
                case Verb.typeVerb runInferAsCallback env vsp verb schema args of
                    Prelude.Right newSchema ->
                        Prelude.pure (TyDataframe newSchema)
                    Prelude.Left d -> inferFail d
            _ -> do
                -- Generic pipe: xs |> f desugars to f xs at the type level.
                rt <- inferExprIn env rhs
                result <- freshVar "pipe_result"
                _ <- unifyAt sp rt (TyFun lt result)
                sub2 <- getSubst
                Prelude.pure (applySubst sub2 result)
    EField sp record fname -> do
        rty <- inferExprIn env record
        sub <- getSubst
        case applySubst sub rty of
            TyRecord fields ->
                case Map.lookup (lowerText fname) fields of
                    Just t -> Prelude.pure t
                    Nothing ->
                        inferFail
                            ( fieldDiag sp (lowerText fname) (TyRecord fields)
                            )
            other ->
                inferFail
                    ( typeMismatchDiag sp
                        ("field access requires a record; got " Prelude.<> showType other)
                    )
    ERecord _ binds -> do
        fields <-
            Prelude.traverse
                (\fb -> do
                    t <- inferExprIn env (fieldBindingValue fb)
                    Prelude.pure (lowerText (fieldBindingName fb), t))
                binds
        Prelude.pure (TyRecord (Map.fromList fields))
    ERecordUpdate sp target fbs -> do
        tt <- inferExprIn env target
        sub <- getSubst
        case applySubst sub tt of
            TyDataframe _ ->
                inferFail
                    ( typeMismatchDiag sp
                        "record update target is a dataframe; use `mutate` instead (LANGUAGE.md section 8.5)"
                    )
            TyRecord existing -> do
                fieldsM <-
                    Prelude.traverse
                        (\fb -> do
                            let nm = lowerText (fieldBindingName fb)
                            valTy <- inferExprIn env (fieldBindingValue fb)
                            case Map.lookup nm existing of
                                Just expected -> do
                                    _ <- unifyAt
                                        (fieldBindingSpan fb)
                                        expected
                                        valTy
                                    Prelude.pure ()
                                Nothing ->
                                    inferFail
                                        ( fieldDiag
                                            (fieldBindingSpan fb)
                                            nm
                                            (TyRecord existing)
                                        ))
                        fbs
                Prelude.pure (TyRecord existing)
            other ->
                inferFail
                    ( typeMismatchDiag sp
                        ("record update target must be a record; got " Prelude.<> showType other)
                    )
    EVector sp items -> do
        elementTy <- freshVar "vec_elem"
        Prelude.traverse
            (\it -> do
                t <- inferExprIn env it
                _ <- unifyAt sp elementTy t
                Prelude.pure ())
            items
        sub <- getSubst
        Prelude.pure (TyApp (TyCon "Vector") (applySubst sub elementTy))
    EDataframe _ binds -> do
        -- Each field's value should be a Vector of something.
        fields <-
            Prelude.traverse
                (\fb -> do
                    t <- inferExprIn env (fieldBindingValue fb)
                    Prelude.pure (lowerText (fieldBindingName fb), t))
                binds
        Prelude.pure (TyDataframe (Map.fromList fields))
    EVerb _ _ _ ->
        -- Verbs outside of a pipe context are typed as @forall a. a -> a@
        -- placeholders so usage like @filter pred xs@ still typechecks
        -- once the receiver schema is known. Programs that pipe a
        -- dataframe into the verb (the recommended style) get full
        -- schema-aware typing via the EPipe case above.
        do
            tv <- freshVar "verb_result"
            Prelude.pure tv



addBinding :: Env -> Binding -> Infer Env
addBinding env b = do
    bodyTy <- inferExprIn env (bindingBody b)
    sub <- getSubst
    let
        finalTy = applySubst sub bodyTy
        envFTV = freeTypeVarsEnvValues env
        qvars = Set.toList (Set.difference (freeTypeVars finalTy) envFTV)
        scheme = Scheme {schemeVars = qvars, schemeBody = finalTy}
    Prelude.pure (insertValue (lowerText (bindingName b)) scheme env)


literalType :: Literal -> Type
literalType = \case
    LInt _ -> primInteger
    LDouble _ -> primDouble
    LChar _ -> primCharacter



-- ---------------------------------------------------------------------
-- Operators (LANGUAGE.md section 8.8)
-- ---------------------------------------------------------------------


inferBinOp :: Env -> SourceSpan -> BinOp -> Expr -> Expr -> Infer Type
inferBinOp env sp op l r = do
    lt <- inferExprIn env l
    rt <- inferExprIn env r
    sub <- getSubst
    let
        lt' = applySubst sub lt
        rt' = applySubst sub rt
    case op of
        OpAdd -> sameNumeric sp lt' rt'
        OpSub -> sameNumeric sp lt' rt'
        OpMul -> sameNumeric sp lt' rt'
        OpDiv -> sameNumeric sp lt' rt'
        OpIntDiv -> bothInt sp lt' rt'
        OpMod -> bothInt sp lt' rt'
        OpExp -> bothDouble sp lt' rt'
        OpEq -> comparison sp lt' rt'
        OpNeq -> comparison sp lt' rt'
        OpGt -> comparison sp lt' rt'
        OpLt -> comparison sp lt' rt'
        OpGe -> comparison sp lt' rt'
        OpLe -> comparison sp lt' rt'


sameNumeric :: SourceSpan -> Type -> Type -> Infer Type
sameNumeric sp l r =
    case (l, r) of
        _ | l Prelude.== primInteger Prelude.&& r Prelude.== primInteger ->
            Prelude.pure primInteger
        _ | l Prelude.== primDouble Prelude.&& r Prelude.== primDouble ->
            Prelude.pure primDouble
        -- If one side is still an unconstrained type variable, force
        -- both sides to match the concrete side. This lets function
        -- bodies use operators on parameters without an annotation
        -- where the other side fixes the type.
        (TyVarT _, TyCon "Integer") -> do
            _ <- unifyAt sp l primInteger
            Prelude.pure primInteger
        (TyVarT _, TyCon "Double") -> do
            _ <- unifyAt sp l primDouble
            Prelude.pure primDouble
        (TyCon "Integer", TyVarT _) -> do
            _ <- unifyAt sp r primInteger
            Prelude.pure primInteger
        (TyCon "Double", TyVarT _) -> do
            _ <- unifyAt sp r primDouble
            Prelude.pure primDouble
        -- Both sides are still unconstrained type variables. Unify
        -- them so the result type follows the operands, register
        -- both as numeric-pending, and let 'defaultPendingNumerics'
        -- pick 'Double' at the decl boundary. This is what makes
        -- @add a b <- a + b@ infer @Double -> Double -> Double@
        -- without requiring an annotation.
        (TyVarT _, TyVarT _) -> do
            _ <- unifyAt sp l r
            markNumeric l
            markNumeric r
            Prelude.pure l
        _ ->
            inferFail
                ( typeMismatchDiag sp
                    ( "arithmetic operator expects Integer/Integer or Double/Double; got "
                        Prelude.<> showType l
                        Prelude.<> " and "
                        Prelude.<> showType r
                    )
                )


bothInt :: SourceSpan -> Type -> Type -> Infer Type
bothInt sp l r =
    case (l, r) of
        _ | l Prelude.== primInteger Prelude.&& r Prelude.== primInteger ->
            Prelude.pure primInteger
        -- The result type is fixed at Integer, so any unconstrained
        -- operand can be unified directly with Integer. No defaulting
        -- is needed since the operator itself pins both sides.
        (TyVarT _, _) | r Prelude.== primInteger -> do
            _ <- unifyAt sp l primInteger
            Prelude.pure primInteger
        (_, TyVarT _) | l Prelude.== primInteger -> do
            _ <- unifyAt sp r primInteger
            Prelude.pure primInteger
        (TyVarT _, TyVarT _) -> do
            _ <- unifyAt sp l primInteger
            _ <- unifyAt sp r primInteger
            Prelude.pure primInteger
        _ ->
            inferFail
                ( typeMismatchDiag sp
                    ( "// and % require both operands to be Integer; got "
                        Prelude.<> showType l
                        Prelude.<> " and "
                        Prelude.<> showType r
                    )
                )


bothDouble :: SourceSpan -> Type -> Type -> Infer Type
bothDouble sp l r =
    case (l, r) of
        _ | l Prelude.== primDouble Prelude.&& r Prelude.== primDouble ->
            Prelude.pure primDouble
        (TyVarT _, _) | r Prelude.== primDouble -> do
            _ <- unifyAt sp l primDouble
            Prelude.pure primDouble
        (_, TyVarT _) | l Prelude.== primDouble -> do
            _ <- unifyAt sp r primDouble
            Prelude.pure primDouble
        (TyVarT _, TyVarT _) -> do
            _ <- unifyAt sp l primDouble
            _ <- unifyAt sp r primDouble
            Prelude.pure primDouble
        _ ->
            inferFail
                ( typeMismatchDiag sp
                    ( "^ requires both operands to be Double; got "
                        Prelude.<> showType l
                        Prelude.<> " and "
                        Prelude.<> showType r
                    )
                )


comparison :: SourceSpan -> Type -> Type -> Infer Type
comparison sp l r =
    case (l, r) of
        _ | l Prelude.== r Prelude.&& isPrimComparable l ->
            Prelude.pure primLogical
        -- One side concrete and primitive-comparable: pin the other.
        (TyVarT _, _) | isPrimComparable r -> do
            _ <- unifyAt sp l r
            Prelude.pure primLogical
        (_, TyVarT _) | isPrimComparable l -> do
            _ <- unifyAt sp r l
            Prelude.pure primLogical
        -- Both unconstrained: unify them so the result is consistent,
        -- mark both as numeric-pending so the decl-boundary defaulting
        -- step pins them to Double if nothing else has done so.
        (TyVarT _, TyVarT _) -> do
            _ <- unifyAt sp l r
            markNumeric l
            markNumeric r
            Prelude.pure primLogical
        _ ->
            inferFail
                ( typeMismatchDiag sp
                    ( "comparison requires both operands to be the same primitive comparable type; got "
                        Prelude.<> showType l
                        Prelude.<> " and "
                        Prelude.<> showType r
                    )
                )



-- ---------------------------------------------------------------------
-- Pattern inference
-- ---------------------------------------------------------------------


inferArm
    :: SourceSpan
    -> Env
    -> Type     -- scrutinee type
    -> Type     -- result type to unify with the arm's body
    -> CaseArm
    -> Infer ()
inferArm _caseSpan env scrutTy resultTy arm = do
    bindings <- inferPattern env scrutTy (caseArmPattern arm)
    let
        env' =
            List.foldl'
                (\e (n, t) -> insertValue n (monoScheme t) e)
                env
                bindings
    bodyTy <- inferExprIn env' (caseArmBody arm)
    _ <- unifyAt (caseArmSpan arm) resultTy bodyTy
    Prelude.pure ()


-- | Type-check a pattern against the scrutinee type, returning the
-- new variable bindings the arm body sees.
inferPattern :: Env -> Type -> Pattern -> Infer [(Text, Type)]
inferPattern env scrutTy = \case
    PWildcard _ -> Prelude.pure []
    PVar n -> Prelude.pure [(lowerText n, scrutTy)]
    PLit sp lit -> do
        let lty = literalType lit
        _ <- unifyAt sp scrutTy lty
        Prelude.pure []
    PCon sp con args ->
        case lookupConstructor (upperText con) env of
            Nothing ->
                inferFail
                    ( Diagnostic
                        { diagSeverity = Error
                        , diagCategory = UnknownConstructor
                        , diagSpan = upperSpan con
                        , diagMessage =
                            "unknown constructor "
                                Prelude.<> T.pack (Prelude.show (upperText con))
                        , diagHint = Nothing
                        }
                    )
            Just info -> do
                -- Instantiate the constructor's type parameters fresh.
                renaming <-
                    Prelude.traverse
                        (\tv -> do
                            fresh <- freshVar (tyVarName tv)
                            Prelude.pure (tv, fresh))
                        (ciTypeParams info)
                let
                    sub = Map.fromList renaming
                    resultTy = applySubst sub (ciResultType info)
                    argTys = Prelude.fmap (applySubst sub) (ciArgTypes info)
                _ <- unifyAt sp scrutTy resultTy
                when (Prelude.length args Prelude./= Prelude.length argTys)
                    (inferFail
                        ( Diagnostic
                            { diagSeverity = Error
                            , diagCategory = UnknownConstructor
                            , diagSpan = sp
                            , diagMessage =
                                "constructor "
                                    Prelude.<> T.pack (Prelude.show (upperText con))
                                    Prelude.<> " expects "
                                    Prelude.<> T.pack (Prelude.show (Prelude.length argTys))
                                    Prelude.<> " arguments, got "
                                    Prelude.<> T.pack (Prelude.show (Prelude.length args))
                            , diagHint = Nothing
                            }
                        ))
                pairs <- zipWithM (inferPattern env) argTys args
                Prelude.pure (Prelude.concat pairs)
    PRecord sp fields -> do
        sub <- getSubst
        case applySubst sub scrutTy of
            TyRecord existing -> do
                results <-
                    Prelude.traverse
                        (\f -> case f of
                            RpfShort n ->
                                case Map.lookup (lowerText n) existing of
                                    Just t -> Prelude.pure [(lowerText n, t)]
                                    Nothing ->
                                        inferFail
                                            (fieldDiag (lowerSpan n) (lowerText n) (TyRecord existing))
                            RpfFull _ n inner ->
                                case Map.lookup (lowerText n) existing of
                                    Just t -> inferPattern env t inner
                                    Nothing ->
                                        inferFail
                                            (fieldDiag (lowerSpan n) (lowerText n) (TyRecord existing)))
                        fields
                Prelude.pure (Prelude.concat results)
            _ ->
                -- Build a record type from the pattern, then unify.
                do
                    sub' <- getSubst
                    inferred <-
                        Prelude.traverse
                            (\f -> case f of
                                RpfShort n -> do
                                    fv <- freshVar (lowerText n)
                                    Prelude.pure (lowerText n, fv, [(lowerText n, fv)])
                                RpfFull _ n inner -> do
                                    fv <- freshVar (lowerText n)
                                    binds <- inferPattern env fv inner
                                    Prelude.pure (lowerText n, fv, binds))
                            fields
                    let
                        recordTy =
                            TyRecord
                                ( Map.fromList
                                    (Prelude.fmap (\(n, t, _) -> (n, t)) inferred)
                                )
                        binds = Prelude.concatMap (\(_, _, bs) -> bs) inferred
                    _ <- unifyAt sp scrutTy recordTy
                    Prelude.pure binds



-- ---------------------------------------------------------------------
-- Unification
-- ---------------------------------------------------------------------


unifyAt :: SourceSpan -> Type -> Type -> Infer ()
unifyAt sp a b = do
    sub <- getSubst
    let
        a' = applySubst sub a
        b' = applySubst sub b
    case unify a' b' of
        Just newSub -> putSubst (newSub @@ sub)
        Nothing ->
            inferFail
                ( typeMismatchDiag sp
                    ("cannot unify " Prelude.<> showType a' Prelude.<> " with " Prelude.<> showType b')
                )


unify :: Type -> Type -> Maybe Subst
unify (TyVarT v) t = bindVar v t
unify t (TyVarT v) = bindVar v t
unify (TyCon a) (TyCon b)
    | a Prelude.== b = Just emptySubst
    | Prelude.otherwise = Nothing
unify (TyApp f1 x1) (TyApp f2 x2) = do
    s1 <- unify f1 f2
    s2 <- unify (applySubst s1 x1) (applySubst s1 x2)
    Just (s2 @@ s1)
unify (TyFun a1 b1) (TyFun a2 b2) = do
    s1 <- unify a1 a2
    s2 <- unify (applySubst s1 b1) (applySubst s1 b2)
    Just (s2 @@ s1)
unify (TyRecord fs1) (TyRecord fs2)
    | Map.keysSet fs1 Prelude.== Map.keysSet fs2 = unifyFields fs1 fs2
    | Prelude.otherwise = Nothing
unify (TyDataframe fs1) (TyDataframe fs2)
    | Map.keysSet fs1 Prelude.== Map.keysSet fs2 = unifyFields fs1 fs2
    | Prelude.otherwise = Nothing
unify _ _ = Nothing


unifyFields :: Map.Map Text Type -> Map.Map Text Type -> Maybe Subst
unifyFields a b =
    let
        pairs = Map.elems (Map.intersectionWith (,) a b)
    in
    Prelude.foldr step (Just emptySubst) pairs
  where
    step (l, r) (Just sub) = do
        more <- unify (applySubst sub l) (applySubst sub r)
        Just (more @@ sub)
    step _ Nothing = Nothing


bindVar :: TyVar -> Type -> Maybe Subst
bindVar v (TyVarT u)
    | v Prelude.== u = Just emptySubst
bindVar v t
    | Set.member v (freeTypeVars t) = Nothing  -- occurs check
    | Prelude.otherwise = Just (Map.singleton v t)



-- ---------------------------------------------------------------------
-- Generalisation and instantiation
-- ---------------------------------------------------------------------


-- | Free type variables of every value in the environment, for
-- generalisation.
freeTypeVarsEnvValues :: Env -> Set.Set TyVar
freeTypeVarsEnvValues env =
    List.foldl'
        (\acc s -> Set.union acc (freeTypeVarsScheme s))
        Set.empty
        (Map.elems (envValues env))


-- | Instantiate a scheme with fresh type variables.
instantiate :: Scheme -> Infer Type
instantiate sch = do
    pairs <-
        Prelude.traverse
            (\tv -> do
                fresh <- freshVar (tyVarName tv)
                Prelude.pure (tv, fresh))
            (schemeVars sch)
    let sub = Map.fromList pairs
    Prelude.pure (applySubst sub (schemeBody sch))


-- | Instantiate a constructor as a value: fresh-var its type
-- parameters, then build @arg1 -> ... -> argN -> result@.
instantiateConstructor :: ConstructorInfo -> Infer Type
instantiateConstructor info = do
    pairs <-
        Prelude.traverse
            (\tv -> do
                fresh <- freshVar (tyVarName tv)
                Prelude.pure (tv, fresh))
            (ciTypeParams info)
    let
        sub = Map.fromList pairs
        argTys = Prelude.fmap (applySubst sub) (ciArgTypes info)
        resultTy = applySubst sub (ciResultType info)
    Prelude.pure (Prelude.foldr TyFun resultTy argTys)



-- ---------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------


typeMismatchDiag :: SourceSpan -> Text -> Diagnostic
typeMismatchDiag sp msg =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = TypeMismatch
        , diagSpan = sp
        , diagMessage = msg
        , diagHint = Nothing
        }


unboundDiag :: SourceSpan -> Text -> Text -> Diagnostic
unboundDiag sp name kind =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = UnboundVariable
        , diagSpan = sp
        , diagMessage =
            "unbound " Prelude.<> kind Prelude.<> " " Prelude.<> T.pack (Prelude.show name)
        , diagHint = Nothing
        }


fieldDiag :: SourceSpan -> Text -> Type -> Diagnostic
fieldDiag sp fieldName ty =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = RecordField
        , diagSpan = sp
        , diagMessage =
            "field "
                Prelude.<> T.pack (Prelude.show fieldName)
                Prelude.<> " not present in "
                Prelude.<> showType ty
        , diagHint = Nothing
        }
