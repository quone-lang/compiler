{-| R code generation.

Implements LANGUAGE.md section 13: maps each AST node to its R
counterpart. Highlights:

* primitive mapping per section 13.2 ('Integer' -> @1L@,
  'Double' -> @1.0@, 'Logical' -> @TRUE@/@FALSE@);
* operator mapping per section 13.2.1 (@//@ -> @%/%@, @%@ -> @%%@,
  @^@ -> @^@);
* curried function definitions and fully-applied calls per
  section 13.3 (single multi-arg R call, no argument curry chains);
* `case` lowering per section 13.6 with the 'Logical'-on-@if@
  optimisation;
* records as named lists per section 13.2;
* record update via @purrr::list_modify@ per section 13.7;
* dataframe verbs as @dplyr::verb(...)@ per section 13.8;
* foreign-import calls qualified as @pkg::fn(...)@ per section 13.9.

The generator never inspects types; it works directly on the AST.
That keeps lowering decoupled from the typer's substitution state.

-}
module Quone.Generate.R
    ( generateProgram
    , generateScript
    , generateExpr
    , generateExprIn
    , runGenerate
    , GenEnv (..)
    , buildGenEnv
    , emptyGenEnv
    )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Generate.Pretty
import qualified Prelude



-- ---------------------------------------------------------------------
-- Generator environment
-- ---------------------------------------------------------------------


-- | Codegen-time context. For v0.0.1 it carries:
--
--   * @gePkgQualifiers@: a foreign-import map from local name to its
--     fully-qualified R form @pkg::fn@ (LANGUAGE.md section 13.9).
data GenEnv = GenEnv
    { gePkgQualifiers :: Map.Map Text Text
    }
    deriving (Prelude.Show, Prelude.Eq)


emptyGenEnv :: GenEnv
emptyGenEnv = GenEnv {gePkgQualifiers = Map.empty}


-- | Walk every foreign-import declaration and build the qualifier map.
--
-- @import readr.read_csv : ...@ ⇒ @\"read_csv\" -> \"readr::read_csv\"@.
-- Multi-segment package paths are joined with dots, then dots are
-- mapped to R's @::@ at use sites (R only ever has one level).
buildGenEnv :: Program -> GenEnv
buildGenEnv prog =
    GenEnv
        { gePkgQualifiers =
            Map.fromList
                [ ( lowerText (foreignFn fname)
                  , qualifierText fname
                  )
                | DImport (ForeignImport _ fname _) <- programDecls prog
                ]
        }


-- | Produce the @pkg::fn@ form. R packages have a single namespace
-- separator, so multi-segment package paths collapse to the LAST
-- segment plus @::@. A two-segment input is the common shape
-- (@dplyr.filter@, @readr.read_csv@); deeper paths are accepted as a
-- forward-compatibility hedge.
qualifierText :: ForeignName -> Text
qualifierText fname =
    let
        pkgSegs = Prelude.fmap lowerText (foreignPackage fname)
        pkg = case pkgSegs of
            [] -> ""
            xs -> Prelude.last xs
    in
    pkg Prelude.<> "::" Prelude.<> lowerText (foreignFn fname)



-- ---------------------------------------------------------------------
-- Programs
-- ---------------------------------------------------------------------


-- | Generate one big R Text for a whole program.
--
-- For a script (no module declaration) the output is the
-- concatenation of all declaration lowerings, separated by blank
-- lines. The optional module header is dropped (it's a Quone-only
-- construct; package-mode generation handles it separately in
-- 'Quone.Generate.Package').
generateProgram :: Program -> Text
generateProgram = generateScript


-- | Synonym used by the CLI's @build --script@ path.
generateScript :: Program -> Text
generateScript prog =
    let
        env = buildGenEnv prog
        decls = generateDecls env (programDecls prog)
    in
    render decls


-- | Convenience for tests.
runGenerate :: Program -> Text
runGenerate = generateScript



-- ---------------------------------------------------------------------
-- Declarations
-- ---------------------------------------------------------------------


generateDecls :: GenEnv -> [Decl] -> Doc
generateDecls env = List.foldl' step empty
  where
    step Empty d = decl env d
    step acc d = acc <+|> decl env d


decl :: GenEnv -> Decl -> Doc
decl env = \case
    DValue v -> valueDecl env v
    DType _ -> empty           -- Custom types have no runtime presence in v0.0.1
    DTypeAlias _ -> empty
    DImport _ -> empty         -- Foreign-import calls qualify themselves at the call site


valueDecl :: GenEnv -> ValueDecl -> Doc
valueDecl env v =
    let
        body = generateExprIn env (valueDeclBody v)
        params = Prelude.fmap lowerText (valueDeclParams v)
        rendered =
            case params of
                [] -> body
                ps ->
                    "function("
                        Prelude.<> sepBy ", " ps
                        Prelude.<> ") "
                        Prelude.<> braces (" " Prelude.<> body Prelude.<> " ")
        docCommentLines = case valueDeclDoc v of
            Nothing -> []
            Just block ->
                Prelude.fmap (\l -> "#' " Prelude.<> l) (docLines block)
        assignLine = lowerText (valueDeclName v) Prelude.<> " <- " Prelude.<> rendered
    in
    foldLines (docCommentLines Prelude.++ [assignLine])


foldLines :: [Text] -> Doc
foldLines = List.foldl' (\d t -> d <+> line t) empty



-- ---------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------


-- | Generate an expression with no foreign-import context. Kept for
-- the test suite and external callers that don't have a Program.
generateExpr :: Expr -> Text
generateExpr = generateExprIn emptyGenEnv


-- | Generate an expression with a known generator environment. Foreign
-- imports map to qualified @pkg::fn@ calls per LANGUAGE.md
-- section 13.9.
generateExprIn :: GenEnv -> Expr -> Text
generateExprIn env = \case
    ELit _ lit -> literal lit
    EVar n -> resolvedName env n
    ECon n -> upperText n   -- constructors are R functions of the same name
    ELambda _ ps body ->
        "function("
            Prelude.<> sepBy ", " (Prelude.fmap lowerText ps)
            Prelude.<> ") "
            Prelude.<> generateExprIn env body
    ECase _ scrut arms ->
        lowerCase env scrut arms
    ELet _ binds body ->
        let
            bs =
                Prelude.fmap
                    (\b ->
                        lowerText (bindingName b)
                            Prelude.<> " <- "
                            Prelude.<> generateExprIn env (bindingBody b)
                    )
                    binds
        in
        "{ "
            Prelude.<> sepBy "; " (bs Prelude.++ [generateExprIn env body])
            Prelude.<> " }"
    call@(EApp _ _ _) ->
        -- Walk left-folded application chain, emit a single multi-arg R call.
        let
            (head_, args) = collectApp call
        in
        case head_ of
            EVar n -> callR (resolvedName env n) (Prelude.fmap (generateExprIn env) args)
            ECon n -> callR (upperText n) (Prelude.fmap (generateExprIn env) args)
            _ -> callR (renderCallHead env head_) (Prelude.fmap (generateExprIn env) args)
    EBinOp _ op l r ->
        renderBinary env op l r
    EUnary _ OpNeg e ->
        "-" Prelude.<> renderUnaryOperand env e
    EPipe _ lhs rhs ->
        renderPipeLhs env lhs Prelude.<> " |> " Prelude.<> generatePipeRhs env rhs
    EField _ record fname ->
        renderFieldBase env record Prelude.<> "$" Prelude.<> lowerText fname
    ERecord _ fields ->
        listCall env fields
    ERecordUpdate _ target fields ->
        "purrr::list_modify("
            Prelude.<> generateExprIn env target
            Prelude.<> ", "
            Prelude.<> namedFields env fields
            Prelude.<> ")"
    EVector _ items ->
        "c(" Prelude.<> sepBy ", " (Prelude.fmap (generateExprIn env) items) Prelude.<> ")"
    EDataframe _ fields ->
        "data.frame("
            Prelude.<> namedFields env fields
            Prelude.<> ")"
    EVerb _ verb args ->
        verbCall env verb args


-- | Resolve a lowercase identifier to its R form. Foreign-imported
-- names are returned as @pkg::fn@; everything else as the bare name.
resolvedName :: GenEnv -> LowerName -> Text
resolvedName env n =
    let
        bare = lowerText n
    in
    Map.findWithDefault bare bare (gePkgQualifiers env)


-- | When a verb appears outside a pipe (rare; the recommended style
-- is `xs |> verb args`), emit a function reference. The compiler does
-- not normally produce this path, but having a sensible lowering keeps
-- the generator total.
verbCall :: GenEnv -> Verb -> [DplyrArg] -> Text
verbCall env verb args =
    let
        fn = "dplyr::" Prelude.<> verbName verb
    in
    case args of
        [] -> fn
        _ -> callR fn (Prelude.fmap (dplyrArgR env) args)


-- | Lower the right-hand side of a pipe. When the RHS is a verb, we
-- emit just the verb call (the dataframe is implicit per R's pipe).
generatePipeRhs :: GenEnv -> Expr -> Text
generatePipeRhs env = \case
    EVerb _ verb args ->
        let
            fn = "dplyr::" Prelude.<> verbName verb
        in
        case args of
            [] -> fn Prelude.<> "()"
            _ -> callR fn (Prelude.fmap (dplyrArgR env) args)
    other -> generateExprIn env other


literal :: Literal -> Text
literal = \case
    LInt n -> T.pack (Prelude.show n) Prelude.<> "L"
    LDouble d -> T.pack (Prelude.show (d :: Prelude.Double))
    LChar t -> "\"" Prelude.<> t Prelude.<> "\""


binOpR :: BinOp -> Text
binOpR = \case
    OpAdd -> "+"
    OpSub -> "-"
    OpMul -> "*"
    OpDiv -> "/"
    OpIntDiv -> "%/%"
    OpMod -> "%%"
    OpExp -> "^"
    OpEq -> "=="
    OpNeq -> "!="
    OpGt -> ">"
    OpLt -> "<"
    OpGe -> ">="
    OpLe -> "<="


data Assoc
    = AssocLeft
    | AssocRight
    deriving (Prelude.Show, Prelude.Eq)


data BinSide
    = BinLeft
    | BinRight
    deriving (Prelude.Show, Prelude.Eq)


renderBinary :: GenEnv -> BinOp -> Expr -> Expr -> Text
renderBinary env op l r =
    renderBinaryOperand env op BinLeft l
        Prelude.<> " "
        Prelude.<> binOpR op
        Prelude.<> " "
        Prelude.<> renderBinaryOperand env op BinRight r


renderBinaryOperand :: GenEnv -> BinOp -> BinSide -> Expr -> Text
renderBinaryOperand env parentOp side child =
    parenthesizeIf
        (needsParensInBinary parentOp side child)
        (generateExprIn env child)


renderUnaryOperand :: GenEnv -> Expr -> Text
renderUnaryOperand env child =
    parenthesizeIf
        (exprPrecedence child Prelude.<= unaryPrecedence)
        (generateExprIn env child)


renderCallHead :: GenEnv -> Expr -> Text
renderCallHead env head_ =
    parenthesizeIf
        (exprPrecedence head_ Prelude.< callPrecedence)
        (generateExprIn env head_)


renderFieldBase :: GenEnv -> Expr -> Text
renderFieldBase env base =
    parenthesizeIf
        (exprPrecedence base Prelude.< fieldPrecedence)
        (generateExprIn env base)


renderPipeLhs :: GenEnv -> Expr -> Text
renderPipeLhs env lhs =
    parenthesizeIf
        (exprPrecedence lhs Prelude.< pipePrecedence)
        (generateExprIn env lhs)


parenthesizeIf :: Prelude.Bool -> Text -> Text
parenthesizeIf needs t =
    if needs
        then parens t
        else t


needsParensInBinary :: BinOp -> BinSide -> Expr -> Prelude.Bool
needsParensInBinary parentOp side = \case
    EBinOp _ childOp _ _ ->
        case Prelude.compare
            (binOpPrecedence childOp)
            (binOpPrecedence parentOp) of
            Prelude.LT -> Prelude.True
            Prelude.GT -> Prelude.False
            Prelude.EQ ->
                case binOpAssoc parentOp of
                    AssocLeft -> side Prelude.== BinRight
                    AssocRight -> side Prelude.== BinLeft
    other ->
        exprPrecedence other Prelude.< binOpPrecedence parentOp


exprPrecedence :: Expr -> Prelude.Int
exprPrecedence = \case
    ELambda _ _ _ -> statementPrecedence
    ECase _ _ _ -> statementPrecedence
    ELet _ _ _ -> statementPrecedence
    EPipe _ _ _ -> pipePrecedence
    EBinOp _ op _ _ -> binOpPrecedence op
    EUnary _ _ _ -> unaryPrecedence
    EApp _ _ _ -> callPrecedence
    EField _ _ _ -> fieldPrecedence
    ERecord _ _ -> callPrecedence
    ERecordUpdate _ _ _ -> callPrecedence
    EVector _ _ -> callPrecedence
    EDataframe _ _ -> callPrecedence
    EVerb _ _ _ -> callPrecedence
    ELit _ _ -> atomPrecedence
    EVar _ -> atomPrecedence
    ECon _ -> atomPrecedence


binOpPrecedence :: BinOp -> Prelude.Int
binOpPrecedence = \case
    OpEq -> comparePrecedence
    OpNeq -> comparePrecedence
    OpGt -> comparePrecedence
    OpLt -> comparePrecedence
    OpGe -> comparePrecedence
    OpLe -> comparePrecedence
    OpAdd -> addPrecedence
    OpSub -> addPrecedence
    OpMul -> multiplyPrecedence
    OpDiv -> multiplyPrecedence
    OpIntDiv -> multiplyPrecedence
    OpMod -> multiplyPrecedence
    OpExp -> exponentPrecedence


binOpAssoc :: BinOp -> Assoc
binOpAssoc = \case
    OpExp -> AssocRight
    _ -> AssocLeft


statementPrecedence :: Prelude.Int
statementPrecedence = 0


pipePrecedence :: Prelude.Int
pipePrecedence = 10


comparePrecedence :: Prelude.Int
comparePrecedence = 20


addPrecedence :: Prelude.Int
addPrecedence = 30


multiplyPrecedence :: Prelude.Int
multiplyPrecedence = 40


unaryPrecedence :: Prelude.Int
unaryPrecedence = 50


exponentPrecedence :: Prelude.Int
exponentPrecedence = 60


callPrecedence :: Prelude.Int
callPrecedence = 70


fieldPrecedence :: Prelude.Int
fieldPrecedence = callPrecedence


atomPrecedence :: Prelude.Int
atomPrecedence = 90


-- | Walk a left-folded EApp chain and return @(head, args)@.
collectApp :: Expr -> (Expr, [Expr])
collectApp = go []
  where
    go acc (EApp _ f x) = go (x : acc) f
    go acc other = (other, acc)


listCall :: GenEnv -> [FieldBinding] -> Text
listCall env fields = "list(" Prelude.<> namedFields env fields Prelude.<> ")"


namedFields :: GenEnv -> [FieldBinding] -> Text
namedFields env fs =
    sepBy
        ", "
        ( Prelude.fmap
            (\fb ->
                lowerText (fieldBindingName fb)
                    Prelude.<> " = "
                    Prelude.<> generateExprIn env (fieldBindingValue fb)
            )
            fs
        )



-- ---------------------------------------------------------------------
-- Case lowering (section 13.6)
-- ---------------------------------------------------------------------


lowerCase :: GenEnv -> Expr -> [CaseArm] -> Text
lowerCase env scrut arms
    | isLogicalIfShape arms =
        ifShape env scrut arms
    | Prelude.otherwise =
        chainShape env scrut arms


-- | LANGUAGE.md section 13.6: when a case has exactly two arms
-- 'True' -> a and 'False' -> b (in either order), lower to R's
-- native 'if'. This is the optimisation that the section 5.3
-- if-desugaring relies on so ordinary 'if' compiles to ordinary R 'if'.
isLogicalIfShape :: [CaseArm] -> Prelude.Bool
isLogicalIfShape arms = case arms of
    [a, b] ->
        case (caseArmPattern a, caseArmPattern b) of
            (PCon _ na [], PCon _ nb [])
                | (upperText na Prelude.== "True"
                    Prelude.&& upperText nb Prelude.== "False")
                    Prelude.|| (upperText na Prelude.== "False"
                        Prelude.&& upperText nb Prelude.== "True") ->
                    Prelude.True
            _ -> Prelude.False
    _ -> Prelude.False


ifShape :: GenEnv -> Expr -> [CaseArm] -> Text
ifShape env scrut arms =
    let
        (trueArm, falseArm) =
            case arms of
                [a, b] -> case caseArmPattern a of
                    PCon _ n [] | upperText n Prelude.== "True" -> (a, b)
                    _ -> (b, a)
                _ -> (Prelude.head arms, Prelude.head arms)
    in
    "if ("
        Prelude.<> generateExprIn env scrut
        Prelude.<> ") "
        Prelude.<> generateExprIn env (caseArmBody trueArm)
        Prelude.<> " else "
        Prelude.<> generateExprIn env (caseArmBody falseArm)


-- | The general case lowering: bind the scrutinee, then a chain of
-- @if (...) { body } else if (...) { body } else stop("non-exhaustive")@.
chainShape :: GenEnv -> Expr -> [CaseArm] -> Text
chainShape env scrut arms =
    let
        scrutVar = "._scrutinee"
        scrutBind = scrutVar Prelude.<> " <- " Prelude.<> generateExprIn env scrut
        chain = buildChain env scrutVar arms
    in
    "{ " Prelude.<> scrutBind Prelude.<> "; " Prelude.<> chain Prelude.<> " }"


buildChain :: GenEnv -> Text -> [CaseArm] -> Text
buildChain env scrutVar = go
  where
    go [] = "stop(\"non-exhaustive case\")"
    go (a : rest) =
        let
            (test, binds) = patternToTest scrutVar (caseArmPattern a)
            body =
                if Prelude.null binds
                    then generateExprIn env (caseArmBody a)
                    else
                        "{ "
                            Prelude.<> sepBy "; " (binds Prelude.++ [generateExprIn env (caseArmBody a)])
                            Prelude.<> " }"
        in
        case test of
            Just predicate ->
                "if ("
                    Prelude.<> predicate
                    Prelude.<> ") "
                    Prelude.<> body
                    Prelude.<> case rest of
                        [] -> ""
                        _ -> " else " Prelude.<> go rest
            Nothing ->
                -- Wildcard / variable pattern: unconditional match.
                body


-- | Translate a pattern into (R predicate, binding statements).
patternToTest :: Text -> Pattern -> (Maybe Text, [Text])
patternToTest scrut = \case
    PWildcard _ -> (Nothing, [])
    PVar n -> (Nothing, [lowerText n Prelude.<> " <- " Prelude.<> scrut])
    PLit _ lit ->
        ( Just (scrut Prelude.<> " == " Prelude.<> literal lit)
        , []
        )
    PCon _ n args ->
        let
            test =
                Just (scrut Prelude.<> "$tag == \"" Prelude.<> upperText n Prelude.<> "\"")
            argBinds =
                Prelude.zipWith
                    (\i arg -> case arg of
                        PVar argName ->
                            lowerText argName
                                Prelude.<> " <- "
                                Prelude.<> scrut
                                Prelude.<> "$values["
                                Prelude.<> T.pack (Prelude.show (i :: Prelude.Int))
                                Prelude.<> "]]"
                        _ -> "")
                    [1 ..]
                    args
        in
        (test, Prelude.filter (Prelude.not Prelude.. T.null) argBinds)
    PRecord _ fields ->
        let
            binds = Prelude.fmap (recordFieldBind scrut) fields
        in
        (Nothing, Prelude.filter (Prelude.not Prelude.. T.null) binds)


recordFieldBind :: Text -> RecordPatField -> Text
recordFieldBind scrut = \case
    RpfShort n ->
        lowerText n Prelude.<> " <- " Prelude.<> scrut Prelude.<> "$" Prelude.<> lowerText n
    RpfFull _ n (PVar v) ->
        lowerText v Prelude.<> " <- " Prelude.<> scrut Prelude.<> "$" Prelude.<> lowerText n
    RpfFull _ n _ ->
        -- Nested patterns are valid; for v0.0.1 lowering we collapse
        -- them to a simple field bind (pattern-matching inside the
        -- bound value would need a recursive call).
        lowerText n Prelude.<> " <- " Prelude.<> scrut Prelude.<> "$" Prelude.<> lowerText n



-- ---------------------------------------------------------------------
-- Verbs (section 13.8)
-- ---------------------------------------------------------------------


verbName :: Verb -> Text
verbName = \case
    VSelect -> "select"
    VFilter -> "filter"
    VMutate -> "mutate"
    VSummarize -> "summarize"
    VGroupBy -> "group_by"
    VUngroup -> "ungroup"
    VArrange -> "arrange"
    VRename -> "rename"
    VDistinct -> "distinct"
    VDistinctAll -> "distinct"
    VCount -> "count"
    VSlice -> "slice"
    VPull -> "pull"
    VRelocate -> "relocate"
    VTransmute -> "transmute"
    VMutateEach -> "mutate"
    VSummarizeEach -> "summarize"
    VLeftJoin -> "left_join"
    VRightJoin -> "right_join"
    VInnerJoin -> "inner_join"
    VFullJoin -> "full_join"
    VAntiJoin -> "anti_join"
    VSemiJoin -> "semi_join"
    VCrossJoin -> "cross_join"


dplyrArgR :: GenEnv -> DplyrArg -> Text
dplyrArgR env = \case
    DAExpr e -> generateExprIn env e
    DARecord _ fields -> namedFields env fields
    DAModifier m -> modifierR env m
    DAJoinOn _ other pairs ->
        generateExprIn env other
            Prelude.<> ", by = c("
            Prelude.<> sepBy ", " (Prelude.fmap pairR pairs)
            Prelude.<> ")"


modifierR :: GenEnv -> Modifier -> Text
modifierR env = \case
    MDesc _ n -> "dplyr::desc(" Prelude.<> lowerText n Prelude.<> ")"
    MAsc _ n -> lowerText n
    MAs _ t -> "\"" Prelude.<> t Prelude.<> "\""
    MWhere _ e -> generateExprIn env e
    MCols _ ns -> "c(" Prelude.<> sepBy ", " (Prelude.fmap (\n -> "\"" Prelude.<> lowerText n Prelude.<> "\"") ns) Prelude.<> ")"


pairR :: JoinPair -> Text
pairR p =
    "\""
        Prelude.<> lowerText (joinPairLeft p)
        Prelude.<> "\" = \""
        Prelude.<> lowerText (joinPairRight p)
        Prelude.<> "\""
