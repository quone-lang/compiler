{-| Hindley-Milner inference for the initial release expression core.

Implements LANGUAGE2.md sections 8.1 through 8.6 and 8.8:

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
    , inferProgramFrom
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
import qualified Quone.Ast.Validate as Validate
import Quone.Diagnostic
    ( Category (..)
    , Diagnostic (..)
    , Severity (Error)
    )
import qualified Quone.Parse.Desugar as Desugar
import Quone.Position (SourceSpan, emptySpan)
import qualified Quone.Prelude.Embed as Embed
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


-- | Infer types for every top-level binding. Returns the typed
-- program on success, or the first diagnostic on failure.
--
-- Seeds inference from the embedded prelude environment so that
-- prelude names and operator overloads are in scope automatically.
-- Callers that want to thread their own starting environment (e.g.
-- for incremental compilation) should prefer 'inferProgramFrom'.
inferProgram :: Program -> Prelude.Either Diagnostic TypedProgram
inferProgram prog =
    Prelude.fmap Prelude.fst (inferProgramFrom seedEnv prog)
  where
    -- Compute the prelude env once at module load (this binding is
    -- a CAF: GHC memoises it). A malformed prelude is a compiler-
    -- development bug; falling back to 'initialEnv' keeps callers
    -- that swallow diagnostics able to see at least primitive types.
    seedEnv = case loadPreludeEnv of
        Prelude.Right e -> e
        Prelude.Left _ -> initialEnv


-- | Load and type-check the embedded prelude, returning its final
-- environment. Pure (the source is embedded). Used by 'inferProgram'
-- to seed user-program inference and by 'Quone.Prelude.Load' for the
-- LSP / CLI compile paths.
loadPreludeEnv :: Prelude.Either Diagnostic Env
loadPreludeEnv = do
    rawProg <- Desugar.desugarFile Embed.preludeFilename Embed.preludeSource
    let prog = rawProg {programIsPrelude = Prelude.True}
    case Validate.validate prog of
        (d : _) -> Prelude.Left d
        [] -> do
            (_, finalEnv) <- inferProgramFrom initialEnv prog
            Prelude.pure finalEnv


-- | Infer types starting from a caller-supplied seed environment.
-- The seed already contains every prelude binding (loaded via
-- 'Quone.Prelude.Load.loadPrelude'); this function adds the user
-- program's custom types and value bindings on top of it.
--
-- Returns both the typed program and the final environment, so the
-- caller can pass that environment downstream (e.g. to LSP hover).
inferProgramFrom
    :: Env
    -> Program
    -> Prelude.Either Diagnostic (TypedProgram, Env)
inferProgramFrom seedEnv prog =
    let
        env = registerCustomTypes seedEnv (programDecls prog)
    in
    case runInfer (inferDecls env (programDecls prog)) of
        Prelude.Left d -> Prelude.Left d
        Prelude.Right (binds, finalEnv) ->
            Prelude.Right
                ( TypedProgram
                    { typedProgram = prog
                    , typedBindings = binds
                    }
                , finalEnv
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
        (TyVarT (mkTyVar nextId hint))
        (s {isNextVar = nextId Prelude.+ 1})


-- | Like 'freshVar' but with an Elm-style class constraint (M3.11).
-- Used when instantiating a binding whose scheme has a constrained
-- bound variable (e.g. @forall n: Number. n -> n -> n@): the
-- instantiated tyvar must inherit the constraint.
freshVarC :: TyVarConstraint -> Text -> Infer Type
freshVarC constraint hint = Infer <| \s ->
    let
        nextId = isNextVar s
    in
    IOk
        (TyVarT (mkTyVarC nextId hint constraint))
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
-- Per LANGUAGE2.md section 8.8, R's bare numeric default is double;
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
                        (\i p -> mkTyVar (Prelude.negate (i Prelude.+ 1)) (lowerText p))
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
            -- registering arity is enough for initial release.
            insertType
                (upperText (aliasDeclName d))
                (Prelude.fromIntegral (Prelude.length (aliasDeclParams d)))
                env
        DExtern (ExternType _ name params _) ->
            -- Primitive types declared by the prelude. Register name
            -- + arity so the type checker can validate signatures
            -- like @Vector Integer@.
            insertType
                (upperText name)
                (Prelude.fromIntegral (Prelude.length params))
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
                -- Defensive fallback. Callers SHOULD pre-collect every
                -- free type variable from the signature via
                -- 'collectSigTVars' so each name gets a unique
                -- 'tyVarId'; this branch only fires if a caller
                -- forgets, in which case all unrecognised names share
                -- id 0 and risk collision (M2.3).
                TyVarT (mkTyVar 0 (lowerText n))
    TParen _ inner -> typeSigToType params inner
    TRecord r -> TyRecord (recordFields params r)
    TDataframe _ r -> TyDataframe (ungroupedDf (recordFields params r))


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
    -- mutual references can resolve. After all decls are typed, a
    -- second pass re-generalises every value binding against the
    -- final substitution so mutually-recursive bindings can each
    -- become polymorphic (M3.6).
    (env0, holes) <- preallocate env decls
    finalised <- foldM (typecheckOne holes) env0 decls
    -- Second-pass generalisation (M3.6): each top-level value
    -- binding's type may have acquired more substitution refinements
    -- after the FIRST decl was typed (mutual references). Re-quantify
    -- each binding against the now-finalised substitution.
    regeneralised <- regeneraliseValueBindings env (Map.keys holes) finalised
    let
        binds =
            Map.fromList
                ( Prelude.fmap
                    (\name ->
                        ( name
                        , Map.findWithDefault
                            (monoScheme (TyCon "Integer"))
                            name
                            (envValues regeneralised)
                        )
                    )
                    (Map.keys holes)
                )
    Prelude.pure (binds, regeneralised)


-- | Re-run generalisation on every top-level value binding using the
-- current substitution. This is a no-op for non-mutually-recursive
-- bindings; for mutually-recursive ones it lets each binding become
-- polymorphic now that its references have been resolved (M3.6).
regeneraliseValueBindings
    :: Env       -- ^ original env (before this group of decls)
    -> [Text]    -- ^ names of the value decls in this group
    -> Env       -- ^ current env (with monomorphic schemes)
    -> Infer Env
regeneraliseValueBindings origEnv names finalEnv = do
    sub <- getSubst
    let
        envFTV = freeTypeVarsEnvValues origEnv
        upd e name =
            case Map.lookup name (envValues e) of
                Nothing -> e
                Just sch ->
                    let
                        finalTy = applySubst sub (schemeBody sch)
                        qvars = Set.toList
                            (Set.difference (freeTypeVars finalTy) envFTV)
                        scheme = Scheme {schemeVars = qvars, schemeBody = finalTy}
                    in
                    insertValue name scheme e
    Prelude.pure (List.foldl' upd finalEnv names)


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
            -- Classify the binding's body so verb-rhs checks can
            -- reject opaque user functions (LANGUAGE2.md sections 8.7
            -- and 9.2). Parameters are treated as elementwise (they
            -- are values, not callables) so a body like
            -- @weight / height@ classifies as 'Elementwise'.
            envWithParams =
                Prelude.foldr
                    (\p e -> insertClassification (lowerText p) Elementwise e)
                    env
                    (valueDeclParams v)
            -- Explicit classification on the binding (M3.10) wins
            -- over body-inferred classification. The default of
            -- `FCOpaque` falls through to body inference, preserving
            -- initial release behaviour for un-annotated bindings.
            cls = case valueDeclClassification v of
                FCElementwise -> Elementwise
                FCReducer -> Reducer
                FCOpaque -> Verb.classifyExpr envWithParams (valueDeclBody v)
            envOut = insertValue name scheme env
        Prelude.pure (insertClassification name cls envOut)
    DImport (ForeignImport _ classification fname sig) ->
        -- Lower-case foreign imports (`import pkg.fn : Ty`) bind a
        -- value of the given type. The R `pkg::fn` qualification is
        -- emitted at code-gen time; here we just need the name in
        -- scope at the inferred type per LANGUAGE2.md section 4.5.
        --
        -- An optional `as <newname>` rename (M3.8) binds the alias
        -- in the local scope; codegen still emits the original name.
        --
        -- The optional @elementwise@/@reducer@ modifier (LANGUAGE2.md
        -- section 4.5) is recorded in 'envClassifications' so the
        -- verb typer can reject opaque foreign callees in @mutate@ /
        -- @summarize@ right-hand sides.
        let
            name = lowerText (foreignBindName fname)
            envWithValue =
                insertValue name (foreignScheme sig) env
        in
        Prelude.pure
            ( insertClassification
                name
                (foreignClassificationToInfer classification)
                envWithValue
            )
    DExtern (ExternValue _ classification name sig _ _) ->
        -- Prelude-only @extern@ value declaration: bind the name to
        -- its declared scheme and record its classification. The body
        -- (R callable string) is consumed by codegen; the type checker
        -- only needs the signature.
        let
            nm = lowerText name
            envWithValue = insertValue nm (foreignScheme sig) env
        in
        Prelude.pure
            ( insertClassification
                nm
                (externClassificationToInfer classification)
                envWithValue
            )
    DExtern (ExternType _ name params _) ->
        -- Register a primitive type with no source-language
        -- constructors. Values come from literals (`1L`, `1.0`,
        -- `"x"`) and from `EVector` syntax (`[a, b, c]`); the type
        -- exists so type applications and signatures can refer to
        -- it.
        Prelude.pure
            ( insertType
                (upperText name)
                (Prelude.fromIntegral (Prelude.length params))
                env
            )
    DInfix d ->
        -- Append this overload's typing rule to the dispatch table.
        -- The rule itself is consumed by 'inferBinOpFromOverloads'
        -- below; the operator's R lowering string is consumed by the
        -- generator (Phase C).
        let
            sigParams =
                applyConstraints
                    (infixDeclConstraints d)
                    (collectSigTVars (infixDeclSig d))
            sigTy = typeSigToType sigParams (infixDeclSig d)
            (lhs, rhs, res) = splitArrowTriple sigTy
            oo =
                OperatorOverload
                    { ooLhs = lhs
                    , ooRhs = rhs
                    , ooResult = res
                    , ooFixity = infixDeclFixity d
                    , ooPrec = infixDeclPrec d
                    , ooR = infixDeclR d
                    }
        in
        Prelude.pure (insertOperatorOverload (infixDeclOp d) oo env)
    DPrefix d ->
        let
            sigParams =
                applyConstraints
                    (prefixDeclConstraints d)
                    (collectSigTVars (prefixDeclSig d))
            sigTy = typeSigToType sigParams (prefixDeclSig d)
            (operand, res) = splitArrowPair sigTy
            uo =
                UnaryOverload
                    { uoOperand = operand
                    , uoResult = res
                    , uoPrec = prefixDeclPrec d
                    , uoR = prefixDeclR d
                    }
        in
        Prelude.pure (insertUnaryOverload (prefixDeclOp d) uo env)
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
                sigParams = collectSigTVars sig
                annTy = typeSigToType sigParams sig
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
--
-- Pre-collects free type variable names from the signature and
-- assigns each a unique negative id (so they never collide with the
-- inferer's positive fresh-var counter, and so two TVars in the
-- same signature with different names get different ids — M2.3
-- soundness fix).
foreignScheme :: TypeSig -> Scheme
foreignScheme sig =
    let
        params = collectSigTVars sig
        body = typeSigToType params sig
        vars = Set.toList (freeTypeVars body)
    in
    Scheme {schemeVars = vars, schemeBody = body}


-- | Collect every free type-variable name that appears in a
-- 'TypeSig' (in source order, deduplicated) and assign each a
-- unique negative 'TyVar' id. Used by callers of 'typeSigToType'
-- whose signature can introduce its own type variables (foreign
-- imports, infix declarations, value annotations).
collectSigTVars :: TypeSig -> [TyVar]
collectSigTVars sig =
    Prelude.zipWith
        (\i name -> mkTyVar (Prelude.negate (i Prelude.+ 1)) name)
        [0 ..]
        (sigTVarNames sig)


-- | Apply the per-binder class constraints from a `forall n: Number,
-- ...` prefix to a list of fresh-allocated tyvars (M3.11). For each
-- binder name listed in 'constraints', if the corresponding tyvar is
-- present in 'tvars', its 'tyVarConstraint' is updated. Tyvars not
-- mentioned in the constraint list keep 'NoConstraint'. Constraint
-- names other than @Number@ are silently ignored in initial release; future
-- revisions could add 'Comparable' / 'Equatable'.
applyConstraints
    :: [(LowerName, Maybe UpperName)]
    -> [TyVar]
    -> [TyVar]
applyConstraints constraints =
    Prelude.fmap
        (\tv -> case List.lookup (tyVarName tv) namedConstraints of
            Just c -> tv {tyVarConstraint = c}
            Nothing -> tv)
  where
    namedConstraints =
        [ ( lowerText n
          , case mc of
                Just c -> classNameToConstraint (upperText c)
                Nothing -> NoConstraint
          )
        | (n, mc) <- constraints
        ]


classNameToConstraint :: Text -> TyVarConstraint
classNameToConstraint = \case
    "Number" -> NumberConstraint
    _ -> NoConstraint


sigTVarNames :: TypeSig -> [Text]
sigTVarNames = dedup Prelude.. go
  where
    go = \case
        TFun _ a b -> go a Prelude.++ go b
        TApp _ h xs -> goAtom h Prelude.++ Prelude.concatMap goAtom xs
        TAtom a -> goAtom a

    goAtom = \case
        TName _ -> []
        TVar n -> [lowerText n]
        TParen _ inner -> go inner
        TRecord (RecordType {recordTypeFields = fs}) ->
            Prelude.concatMap (\f -> go (fieldTypeSig f)) fs
        TDataframe _ (RecordType {recordTypeFields = fs}) ->
            Prelude.concatMap (\f -> go (fieldTypeSig f)) fs

    dedup = Prelude.foldr step []
    step x acc = if x `Prelude.elem` acc then acc else x : acc


-- | Translate the AST-level 'ForeignClassification' to the type-
-- environment 'Classification'. Absent modifier defaults to 'Opaque',
-- which @lookupClassification@ also returns for unknown names.
foreignClassificationToInfer :: ForeignClassification -> Classification
foreignClassificationToInfer = \case
    FCOpaque -> Opaque
    FCElementwise -> Elementwise
    FCReducer -> Reducer


-- | Translate the AST-level 'ExternClassification' (prelude @extern@
-- declarations) to the type-environment 'Classification'.
externClassificationToInfer :: ExternClassification -> Classification
externClassificationToInfer = \case
    ECOpaque -> Opaque
    ECElementwise -> Elementwise
    ECReducer -> Reducer


-- | Split a binary-operator signature @lhs -> rhs -> result@ into
-- its three component types. Used when registering @infix@ overloads.
-- A signature with the wrong shape produces an internal-error type
-- (the parser's grammar guarantees this can't happen for legal
-- prelude source).
splitArrowTriple :: Type -> (Type, Type, Type)
splitArrowTriple = \case
    TyFun a (TyFun b r) -> (a, b, r)
    other -> (other, other, other)


-- | Split a unary-operator signature @operand -> result@.
splitArrowPair :: Type -> (Type, Type)
splitArrowPair = \case
    TyFun a r -> (a, r)
    other -> (other, other)


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
        let resolvedScrut = applySubst sub scrutTy
        checkExhaustiveness sp env resolvedScrut arms
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
    EUnary sp op e -> do
        et <- inferExprIn env e
        sub <- getSubst
        let resolved = applySubst sub et
        inferUnaryFromOverloads env sp op resolved
    EPipe sp lhs rhs -> do
        lt <- inferExprIn env lhs
        sub <- getSubst
        case (applySubst sub lt, rhs) of
            (TyDataframe shape, EVerb vsp verb args) ->
                -- Dataframe verb on a known schema: dispatch to the
                -- verb-specific typer (LANGUAGE2.md section 8.7). The
                -- shape carries the schema plus any grouping keys
                -- introduced by a preceding `group_by` so that
                -- `summarize` / `ungroup` can consume them
                -- (KNOWN_FAILURES #4).
                case Verb.typeVerb runInferAsCallback env vsp verb shape args of
                    Prelude.Right newShape ->
                        Prelude.pure (TyDataframe newShape)
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
                        "record update target is a dataframe; use `mutate` instead (LANGUAGE2.md section 8.5)"
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
        -- Each column's value MUST be a `Vector α` for some α; the
        -- dataframe shape stores the per-column element type α (not
        -- the outer Vector). Without this check
        -- `dataframe { x = 1L }` would type as
        -- `dataframe { x : Integer }` and lower to broken R.
        fields <-
            Prelude.traverse
                (\fb -> do
                    elemTy <- freshVar "df_col"
                    t <- inferExprIn env (fieldBindingValue fb)
                    sub0 <- getSubst
                    let resolved = applySubst sub0 t
                    case unify resolved (TyApp (TyCon "Vector") elemTy) of
                        Just newSub -> do
                            putSubst (newSub @@ sub0)
                            sub <- getSubst
                            Prelude.pure
                                ( lowerText (fieldBindingName fb)
                                , applySubst sub elemTy
                                )
                        Nothing ->
                            inferFail
                                ( typeMismatchDiag
                                    (fieldBindingSpan fb)
                                    ( "dataframe column "
                                        Prelude.<> T.pack (Prelude.show (lowerText (fieldBindingName fb)))
                                        Prelude.<> " must be a Vector; got "
                                        Prelude.<> showType resolved
                                    )
                                ))
                binds
        Prelude.pure (TyDataframe (ungroupedDf (Map.fromList fields)))
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
-- Operators (LANGUAGE2.md section 8.8)
-- ---------------------------------------------------------------------


inferBinOp :: Env -> SourceSpan -> BinOp -> Expr -> Expr -> Infer Type
inferBinOp env sp op l r = do
    -- Static recycling check (M2.9): when both operands are vector
    -- literals with statically-known lengths, reject mismatched
    -- lengths up front so users see a friendly diagnostic instead
    -- of R's silent recycling.
    case (literalVectorLength l, literalVectorLength r) of
        (Just nl, Just nr) | nl /= nr ->
            inferFail
                ( typeMismatchDiag sp
                    ( "vector length mismatch: lhs has "
                        Prelude.<> T.pack (Prelude.show nl)
                        Prelude.<> " elements, rhs has "
                        Prelude.<> T.pack (Prelude.show nr)
                        Prelude.<> "; R would silently recycle, which is almost always a bug"
                    )
                )
        _ -> Prelude.pure ()
    lt <- inferExprIn env l
    rt <- inferExprIn env r
    sub <- getSubst
    let
        lt' = applySubst sub lt
        rt' = applySubst sub rt
    inferBinOpFromOverloads env sp op lt' rt'


-- | If @e@ is a literal vector (`[a, b, c]`), return its length.
-- Anything else returns 'Nothing' (we can't statically reason about
-- length).
literalVectorLength :: Expr -> Maybe Prelude.Int
literalVectorLength = \case
    EVector _ items -> Just (Prelude.length items)
    _ -> Nothing


-- | Dispatch a binary operator against the prelude-declared overload
-- table (LANGUAGE2.md section 8.8). Tries each declared overload in
-- order and picks the first whose lhs/rhs types unify with the
-- inferred operand types.
--
-- Defaulting (compiler-side, not prelude-defined): when both
-- operands are unconstrained type variables, the dispatch defers to
-- the @markNumeric@ pass so the decl-boundary defaulting step pins
-- them to @Double@. This preserves the initial release behaviour where
-- @add a b <- a + b@ infers @Double -> Double -> Double@.
inferBinOpFromOverloads
    :: Env
    -> SourceSpan
    -> BinOp
    -> Type
    -> Type
    -> Infer Type
inferBinOpFromOverloads env sp op lt rt =
    let
        overloads = lookupOperatorOverloads op env
    in
    if Prelude.null overloads
        then
            inferFail
                ( typeMismatchDiag sp
                    ( "no overloads for operator "
                        Prelude.<> T.pack (Prelude.show op)
                        Prelude.<> "; the prelude is missing a corresponding `infix` declaration"
                    )
                )
        else if isUnconstrainedTyVar lt Prelude.&& isUnconstrainedTyVar rt
            then defaultBothUnconstrained sp op lt rt overloads
            else do
                -- Instantiate each overload's tyvars fresh per call so
                -- two `+` uses don't accidentally share the same `n`
                -- binding via the global substitution (M3.11).
                fresh <- Prelude.traverse freshenOverload overloads
                case findMatchingOverload lt rt fresh of
                    Just (newSub, resTy) -> do
                        sub <- getSubst
                        putSubst (newSub @@ sub)
                        Prelude.pure (applySubst newSub resTy)
                    Nothing ->
                        inferFail
                            ( typeMismatchDiag sp
                                ( "no overload of "
                                    Prelude.<> T.pack (Prelude.show op)
                                    Prelude.<> " matches operands "
                                    Prelude.<> showType lt
                                    Prelude.<> " and "
                                    Prelude.<> showType rt
                                )
                            )


-- | Freshen an operator overload's free tyvars so each call site
-- gets its own unifiable copy. Without this, a `forall n: Number. n
-- -> n -> n` overload would persistently bind `n` to whatever the
-- first call instantiated it to (M3.11).
--
-- Concrete overloads (e.g. `Integer -> Integer -> Integer`) have no
-- free tyvars and pass through unchanged.
freshenOverload :: OperatorOverload -> Infer OperatorOverload
freshenOverload oo = do
    let
        free =
            Set.toList
                ( Set.unions
                    [ freeTypeVars (ooLhs oo)
                    , freeTypeVars (ooRhs oo)
                    , freeTypeVars (ooResult oo)
                    ]
                )
    pairs <-
        Prelude.traverse
            (\tv -> do
                fresh <- freshVarC (tyVarConstraint tv) (tyVarName tv)
                Prelude.pure (tv, fresh))
            free
    let sub = Map.fromList pairs
    Prelude.pure
        oo
            { ooLhs = applySubst sub (ooLhs oo)
            , ooRhs = applySubst sub (ooRhs oo)
            , ooResult = applySubst sub (ooResult oo)
            }


freshenUnary :: UnaryOverload -> Infer UnaryOverload
freshenUnary uo = do
    let
        free =
            Set.toList
                ( Set.union
                    (freeTypeVars (uoOperand uo))
                    (freeTypeVars (uoResult uo))
                )
    pairs <-
        Prelude.traverse
            (\tv -> do
                fresh <- freshVarC (tyVarConstraint tv) (tyVarName tv)
                Prelude.pure (tv, fresh))
            free
    let sub = Map.fromList pairs
    Prelude.pure
        uo
            { uoOperand = applySubst sub (uoOperand uo)
            , uoResult = applySubst sub (uoResult uo)
            }


-- | Find the first overload whose @ooLhs@ and @ooRhs@ unify with the
-- inferred operand types. Returns the merged substitution (so the
-- caller can commit it) and the overload's result type.
--
-- Operates purely on 'unify' (which returns 'Maybe Subst' without
-- side effects), so it can speculatively try each overload without
-- mutating the inferer's state.
findMatchingOverload
    :: Type
    -> Type
    -> [OperatorOverload]
    -> Maybe (Subst, Type)
findMatchingOverload lt rt = go
  where
    go [] = Nothing
    go (oo : rest) =
        case unify lt (ooLhs oo) of
            Nothing -> go rest
            Just s1 ->
                let
                    rt' = applySubst s1 rt
                    rhs' = applySubst s1 (ooRhs oo)
                in
                case unify rt' rhs' of
                    Nothing -> go rest
                    Just s2 ->
                        let merged = s2 @@ s1
                        in Just (merged, applySubst merged (ooResult oo))


-- | Both operands are unconstrained type variables; pick a
-- "defaulting" overload to commit to. We prefer the @Double@ overload
-- for arithmetic and comparison; if no @Double@-shaped overload
-- exists, fall back to the first declared scalar one.
--
-- After picking, mark the type vars as numeric so the decl-boundary
-- defaulting step pins them concretely (which keeps the
-- @add a b <- a + b@ test passing without an annotation).
defaultBothUnconstrained
    :: SourceSpan
    -> BinOp
    -> Type
    -> Type
    -> [OperatorOverload]
    -> Infer Type
defaultBothUnconstrained sp _op lt rt overloads =
    -- Prefer the overload whose lhs is `primDouble`. Fall back to
    -- the first scalar overload if the operator has no Double rule
    -- (none today, but defensive).
    case List.find (\oo -> ooLhs oo Prelude.== primDouble) overloads of
        Just oo -> commit oo
        Nothing -> case List.find (\oo -> Prelude.not (isVectorTy (ooLhs oo))) overloads of
            Just oo -> commit oo
            Nothing ->
                inferFail
                    ( typeMismatchDiag sp
                        "operator has no scalar default overload"
                    )
  where
    commit oo = do
        _ <- unifyAt sp lt (ooLhs oo)
        _ <- unifyAt sp rt (ooRhs oo)
        markNumeric lt
        markNumeric rt
        Prelude.pure (ooResult oo)


isUnconstrainedTyVar :: Type -> Prelude.Bool
isUnconstrainedTyVar = \case
    TyVarT _ -> Prelude.True
    _ -> Prelude.False


isVectorTy :: Type -> Prelude.Bool
isVectorTy = \case
    TyApp (TyCon "Vector") _ -> Prelude.True
    _ -> Prelude.False


-- | Dispatch a unary operator against the prelude-declared overload
-- table. Mirrors 'inferBinOpFromOverloads' but with one operand.
inferUnaryFromOverloads
    :: Env
    -> SourceSpan
    -> UnaryOp
    -> Type
    -> Infer Type
inferUnaryFromOverloads env sp op operand =
    let
        overloads = lookupUnaryOverloads op env
    in
    if Prelude.null overloads
        then
            inferFail
                ( typeMismatchDiag sp
                    ( "no overloads for unary operator "
                        Prelude.<> T.pack (Prelude.show op)
                    )
                )
        else if isUnconstrainedTyVar operand
            then defaultUnaryUnconstrained sp operand overloads
            else do
                fresh <- Prelude.traverse freshenUnary overloads
                case findMatchingUnary operand fresh of
                    Just (newSub, resTy) -> do
                        sub <- getSubst
                        putSubst (newSub @@ sub)
                        Prelude.pure (applySubst newSub resTy)
                    Nothing ->
                        inferFail
                            ( typeMismatchDiag sp
                                ( "no overload of unary "
                                    Prelude.<> T.pack (Prelude.show op)
                                    Prelude.<> " matches operand "
                                    Prelude.<> showType operand
                                )
                            )


findMatchingUnary
    :: Type
    -> [UnaryOverload]
    -> Maybe (Subst, Type)
findMatchingUnary operand = go
  where
    go [] = Nothing
    go (uo : rest) =
        case unify operand (uoOperand uo) of
            Nothing -> go rest
            Just s -> Just (s, applySubst s (uoResult uo))


defaultUnaryUnconstrained
    :: SourceSpan
    -> Type
    -> [UnaryOverload]
    -> Infer Type
defaultUnaryUnconstrained sp operand overloads =
    case List.find (\uo -> uoOperand uo Prelude.== primDouble) overloads of
        Just uo -> commit uo
        Nothing -> case List.find (\uo -> Prelude.not (isVectorTy (uoOperand uo))) overloads of
            Just uo -> commit uo
            Nothing ->
                inferFail
                    ( typeMismatchDiag sp
                        "unary operator has no scalar default overload"
                    )
  where
    commit uo = do
        _ <- unifyAt sp operand (uoOperand uo)
        markNumeric operand
        Prelude.pure (uoResult uo)



-- ---------------------------------------------------------------------
-- Pattern inference
-- ---------------------------------------------------------------------


-- | Check that the patterns in a `case` cover every possible value
-- of the scrutinee (LANGUAGE2.md section 8.4 invariant). Misses get a
-- 'NonExhaustivePattern' diagnostic that lists the missing
-- constructors.
--
-- Coverage rules:
--
--   * A 'PWildcard' or 'PVar' alone covers everything (always
--     exhaustive).
--   * A 'PCon' arm covers its own constructor; the case is
--     exhaustive when every constructor of the scrutinee's ADT is
--     covered by some arm.
--   * Literal patterns and record patterns can't be enumerated
--     statically (infinite or open domains), so a case that uses any
--     of them MUST include a wildcard / var arm. Without one, we
--     warn-as-error.
--   * If the scrutinee type isn't a known ADT (e.g. a fresh type
--     var, a function, a record), we don't enforce — there's nothing
--     to enumerate against.
checkExhaustiveness :: SourceSpan -> Env -> Type -> [CaseArm] -> Infer ()
checkExhaustiveness sp env scrutTy arms
    | hasCatchAll arms = Prelude.pure ()
    | Prelude.otherwise = case typeAdtName scrutTy of
        Nothing ->
            -- Scrutinee isn't a known ADT. If patterns are all literal
            -- or record without a catch-all, that's still exhaustive
            -- only if the user knows the literal domain, which we
            -- can't statically check. Soft-pass for initial release to avoid
            -- false positives; tightening lands when we have richer
            -- pattern types (M2.6).
            Prelude.pure ()
        Just tyName ->
            let
                -- Constructors of `tyName` declared in the env.
                allCtors =
                    [ name
                    | (name, info) <- Map.toList (envConstructors env)
                    , ciTypeName info Prelude.== tyName
                    ]
                covered = patternConstructors arms
                missing = Prelude.filter
                    (\c -> Prelude.not (c `Prelude.elem` covered))
                    allCtors
            in
            case missing of
                [] -> Prelude.pure ()
                _ ->
                    inferFail
                        ( Diagnostic
                            { diagSeverity = Error
                            , diagCategory = NonExhaustivePattern
                            , diagSpan = sp
                            , diagMessage =
                                "non-exhaustive `case` over "
                                    Prelude.<> tyName
                                    Prelude.<> ": missing constructor"
                                    Prelude.<> (if Prelude.length missing Prelude.> 1 then "s " else " ")
                                    Prelude.<> T.intercalate ", " missing
                            , diagHint =
                                Just
                                    ( "add an arm for "
                                        Prelude.<> T.intercalate " / " missing
                                        Prelude.<> ", or a wildcard `_` arm to catch the rest"
                                    )
                            }
                        )


-- | True if the arm list has at least one wildcard or variable
-- pattern, which catches everything.
hasCatchAll :: [CaseArm] -> Prelude.Bool
hasCatchAll = Prelude.any (isCatchAll Prelude.. caseArmPattern)
  where
    isCatchAll = \case
        PWildcard _ -> Prelude.True
        PVar _ -> Prelude.True
        _ -> Prelude.False


-- | If the type is a saturated application of an ADT constructor
-- like @Maybe Integer@ or @Logical@, return its name. Otherwise
-- return Nothing (the exhaustiveness checker has nothing to do).
typeAdtName :: Type -> Maybe Text
typeAdtName = \case
    TyCon n -> Just n
    TyApp f _ -> typeAdtName f
    _ -> Nothing


-- | Constructor names matched by the patterns of these arms.
patternConstructors :: [CaseArm] -> [Text]
patternConstructors arms =
    Prelude.concatMap (constructorsOf Prelude.. caseArmPattern) arms
  where
    constructorsOf = \case
        PCon _ n _ -> [upperText n]
        _ -> []


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
    -- Pattern guard (M3.3): typed in the post-pattern environment;
    -- MUST be Logical.
    case caseArmGuard arm of
        Nothing -> Prelude.pure ()
        Just g -> do
            gTy <- inferExprIn env' g
            _ <- unifyAt (caseArmSpan arm) gTy primLogical
            Prelude.pure ()
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
unify (TyDataframe shape1) (TyDataframe shape2)
    | Map.keysSet (dfSchema shape1) Prelude.== Map.keysSet (dfSchema shape2)
        Prelude.&& dfGroupingCols shape1 Prelude.== dfGroupingCols shape2 =
        unifyFields (dfSchema shape1) (dfSchema shape2)
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
    | Prelude.otherwise =
        -- Two tyvars: combine their constraints. The result is the
        -- more restrictive of the two (M3.11).
        case combineConstraints (tyVarConstraint v) (tyVarConstraint u) of
            Nothing -> Nothing  -- conflicting class constraints
            Just c ->
                let
                    -- Bind v to a fresh-style tyvar that carries the
                    -- combined constraint. Keep u's id and name to
                    -- avoid renaming in error messages.
                    merged = u {tyVarConstraint = c}
                in
                Just (Map.singleton v (TyVarT merged))
bindVar v t
    | Set.member v (freeTypeVars t) = Nothing  -- occurs check
    | Prelude.not (satisfiesConstraint (tyVarConstraint v) t) = Nothing
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
    -- Each bound tyvar gets a fresh id; if it carries a class
    -- constraint (M3.11), the fresh tyvar inherits it so unification
    -- can enforce the constraint at the use site.
    pairs <-
        Prelude.traverse
            (\tv -> do
                fresh <- freshVarC (tyVarConstraint tv) (tyVarName tv)
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
