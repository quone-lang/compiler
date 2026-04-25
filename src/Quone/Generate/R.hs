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
import qualified Quone.Prelude.Load as Prelude.Load
import qualified Prelude



-- ---------------------------------------------------------------------
-- Generator environment
-- ---------------------------------------------------------------------


-- | Codegen-time context. For initial release it carries:
--
--   * @gePkgQualifiers@: a foreign-import map from local name to its
--     fully-qualified R form @pkg::fn@ (LANGUAGE.md section 13.9);
--   * @geExternBodies@: per-name R-callable string supplied by an
--     @extern@ declaration in the embedded prelude
--     (LANGUAGE.md section 10). Looked up by 'EApp' lowering to
--     replace the Quone name with the declared R form;
--   * @geOperatorR@: per-'BinOp' R operator string supplied by the
--     prelude's @infix@ declarations. All overloads of a given
--     operator share the same R string.
--   * @geUnaryR@: same for unary operators.
data GenEnv = GenEnv
    { gePkgQualifiers :: Map.Map Text Text
    , geExternBodies :: Map.Map Text ExternBody
    , geOperatorR :: Map.Map BinOp Text
    , geUnaryR :: Map.Map UnaryOp Text
    , geForeignVia :: Map.Map Text Text
    -- ^ Local-name -> R-call template (M3.9). When a call site
    -- resolves to a foreign import that has a `via "..."` clause,
    -- the generator substitutes @$1@, @$2@, ... in this template
    -- instead of emitting a straight `pkg::fn(args...)`.
    , geValueArities :: Map.Map Text Prelude.Int
    -- ^ Quone-defined value functions lower to pipe-friendly R functions:
    -- the last Quone argument is emitted as the first R argument.
    }
    deriving (Prelude.Show, Prelude.Eq)


emptyGenEnv :: GenEnv
emptyGenEnv =
    GenEnv
        { gePkgQualifiers = Map.empty
        , geExternBodies = Map.empty
        , geOperatorR = Map.empty
        , geUnaryR = Map.empty
        , geForeignVia = Map.empty
        , geValueArities = Map.empty
        }


-- | Walk every foreign-import declaration and build the qualifier map.
-- Also seed the extern-body map from the embedded prelude
-- (LANGUAGE.md section 10) so calls to @sqrt@, @mean@, @if_else@,
-- etc. lower to the R callable strings declared in @Prelude.Q@.
--
-- @import readr.read_csv : ...@ ⇒ @\"read_csv\" -> \"readr::read_csv\"@.
-- Multi-segment package paths are joined with dots, then dots are
-- mapped to R's @::@ at use sites (R only ever has one level).
buildGenEnv :: Program -> GenEnv
buildGenEnv prog =
    GenEnv
        { gePkgQualifiers =
            Map.fromList
                [ ( lowerText (foreignBindName fname)
                  , qualifierText fname
                  )
                | DImport (ForeignImport _ _ fname _) <- programDecls prog
                ]
        , geExternBodies =
            -- Prelude bodies first; user extern bodies (which the
            -- validator rejects, but we honour them defensively for
            -- prelude self-compilation) override.
            Map.union
                (collectExternBodies prog)
                preludeExternBodies
        , geOperatorR =
            Map.union
                (collectOperatorR prog)
                preludeOperatorR
        , geUnaryR =
            Map.union
                (collectUnaryR prog)
                preludeUnaryR
        , geForeignVia =
            Map.fromList
                [ (lowerText (foreignBindName fname), template)
                | DImport (ForeignImport _ _ fname _) <- programDecls prog
                , Just template <- [foreignVia fname]
                ]
        , geValueArities =
            Map.union
                (collectValueArities prog)
                preludeValueArities
        }


-- | Extract the @extern@ value declarations from a program into a
-- name-to-body map. Used both for the user program (which normally
-- has none after validation) and for the embedded prelude.
collectExternBodies :: Program -> Map.Map Text ExternBody
collectExternBodies prog =
    Map.fromList
        [ (lowerText name, body)
        | DExtern (ExternValue _ _ name _ body _) <- programDecls prog
        ]


collectValueArities :: Program -> Map.Map Text Prelude.Int
collectValueArities prog =
    Map.fromList
        [ (lowerText (valueDeclName v), Prelude.length (valueDeclParams v))
        | DValue v <- programDecls prog
        ]


-- | Extract operator R strings from @infix@ declarations. All
-- overloads of a given operator share the same R string in initial release;
-- if multiple overloads disagree, the LAST one wins (insertion order).
collectOperatorR :: Program -> Map.Map BinOp Text
collectOperatorR prog =
    Map.fromList
        [ (infixDeclOp d, infixDeclR d)
        | DInfix d <- programDecls prog
        ]


collectUnaryR :: Program -> Map.Map UnaryOp Text
collectUnaryR prog =
    Map.fromList
        [ (prefixDeclOp d, prefixDeclR d)
        | DPrefix d <- programDecls prog
        ]


-- | Extern bodies declared in @Prelude.Q@. Loaded once; cached as a
-- CAF. A failure to load is silently squashed (the typer surfaces the
-- diagnostic; we just lose lowering knowledge here, which is fine
-- because user code wouldn't typecheck either).
preludeExternBodies :: Map.Map Text ExternBody
preludeExternBodies =
    case Prelude.Load.loadPrelude of
        Prelude.Right loaded -> collectExternBodies (Prelude.Load.preludeProgram loaded)
        Prelude.Left _ -> Map.empty


preludeOperatorR :: Map.Map BinOp Text
preludeOperatorR =
    case Prelude.Load.loadPrelude of
        Prelude.Right loaded -> collectOperatorR (Prelude.Load.preludeProgram loaded)
        Prelude.Left _ -> Map.empty


preludeUnaryR :: Map.Map UnaryOp Text
preludeUnaryR =
    case Prelude.Load.loadPrelude of
        Prelude.Right loaded -> collectUnaryR (Prelude.Load.preludeProgram loaded)
        Prelude.Left _ -> Map.empty


preludeValueArities :: Map.Map Text Prelude.Int
preludeValueArities =
    Map.fromList
        [ (lowerText (valueDeclName v), Prelude.length (valueDeclParams v))
        | DValue v <- preludeValueDecls
        ]


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
--
-- Prepends the prelude's Quone-side value bindings (e.g.
-- @with_default@, @result_map@) so user code can call them at
-- runtime. Foreign-style (`extern`) prelude entries don't need
-- emitting — they lower to direct R-call expressions at the call
-- site.
generateScript :: Program -> Text
generateScript prog =
    let
        env = buildGenEnv prog
        userRefs = collectProgramRefs prog
        usedPrelude = preludeValueDeclsUsedBy userRefs
        declDoc =
            generateDecls env (usedPrelude Prelude.++ programDecls prog)
        finalDoc =
            case programFinalExpr prog of
                Nothing -> empty
                Just expr -> line (generateExprIn env expr)
    in
    render (declDoc <+|> finalDoc)


collectProgramRefs :: Program -> Set.Set Text
collectProgramRefs prog =
    Set.unions
        ( collectExprRefs (programDecls prog)
            : case programFinalExpr prog of
                Nothing -> []
                Just expr -> [exprRefs expr]
        )


-- | Walk a list of declarations and collect every name referenced
-- in any expression position. Used to prune the embedded prelude
-- to just the value bindings the user actually calls.
collectExprRefs :: [Decl] -> Set.Set Text
collectExprRefs decls =
    Set.unions
        [ exprRefs (valueDeclBody v)
        | DValue v <- decls
        ]


exprRefs :: Expr -> Set.Set Text
exprRefs = \case
    EVar n -> Set.singleton (lowerText n)
    ELit _ _ -> Set.empty
    ECon _ -> Set.empty
    ELambda _ _ body -> exprRefs body
    ECase _ s arms ->
        Set.unions (exprRefs s : Prelude.fmap (exprRefs Prelude.. caseArmBody) arms)
    ELet _ binds body ->
        Set.unions (exprRefs body : Prelude.fmap (exprRefs Prelude.. bindingBody) binds)
    EApp _ f x -> Set.union (exprRefs f) (exprRefs x)
    EBinOp _ _ l r -> Set.union (exprRefs l) (exprRefs r)
    EUnary _ _ e -> exprRefs e
    EPipe _ l r -> Set.union (exprRefs l) (exprRefs r)
    EField _ e _ -> exprRefs e
    ERecord _ fs ->
        Set.unions (Prelude.fmap (exprRefs Prelude.. fieldBindingValue) fs)
    ERecordUpdate _ tgt fs ->
        Set.unions (exprRefs tgt : Prelude.fmap (exprRefs Prelude.. fieldBindingValue) fs)
    EVector _ items -> Set.unions (Prelude.fmap exprRefs items)
    EDataframe _ binds ->
        Set.unions (Prelude.fmap (exprRefs Prelude.. fieldBindingValue) binds)
    EVerb _ _ args ->
        Set.unions (Prelude.fmap dplyrArgRefs args)
  where
    dplyrArgRefs = \case
        DAExpr e -> exprRefs e
        DARecord _ fs ->
            Set.unions (Prelude.fmap (exprRefs Prelude.. fieldBindingValue) fs)
        DAModifier _ -> Set.empty
        DAJoinOn _ e _pairs ->
            -- JoinPair only references column names, not arbitrary
            -- expressions, so there's nothing to walk for value
            -- references.
            exprRefs e


-- | Subset the prelude's user-style value bindings (i.e. those
-- defined as ordinary `name <- expr` rather than `extern`) to the
-- ones the user's program actually references. Idempotent: if the
-- user already shadows a prelude name, the prelude version is
-- still emitted (the user's later binding overrides at runtime;
-- name resolution prevents accidental self-reference).
preludeValueDeclsUsedBy :: Set.Set Text -> [Decl]
preludeValueDeclsUsedBy used =
    [ DValue v
    | DValue v <- preludeValueDecls
    , Set.member (lowerText (valueDeclName v)) used
    , lowerText (valueDeclName v) Prelude./= "count"
    ]


-- | The prelude's user-style value bindings. Loaded once and cached
-- as a CAF.
preludeValueDecls :: [Decl]
preludeValueDecls =
    case Prelude.Load.loadPrelude of
        Prelude.Right loaded ->
            [ DValue v
            | DValue v <- programDecls (Prelude.Load.preludeProgram loaded)
            ]
        Prelude.Left _ -> []


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
    DType _ -> empty           -- Custom types have no runtime presence in initial release
    DTypeAlias _ -> empty
    DImport _ -> empty         -- Foreign-import calls qualify themselves at the call site
    DExtern _ -> empty
        -- Prelude-only declarations have no runtime presence: their
        -- value is supplied by the embedded R callable string at the
        -- call site, not by emitting a binding.
    DInfix _ -> empty
    DPrefix _ -> empty


valueDecl :: GenEnv -> ValueDecl -> Doc
valueDecl env v =
    let
        body = generateExprIn env (valueDeclBody v)
        params = toRValueOrder (Prelude.fmap lowerText (valueDeclParams v))
        rendered =
            case params of
                [] -> body
                ps ->
                    functionText ps body
        docCommentLines = case valueDeclDoc v of
            Nothing -> []
            Just block ->
                Prelude.fmap formatDocLine (docLines block)
        assignLines =
            prefixFirstLine
                (lowerText (valueDeclName v) Prelude.<> " <- ")
                rendered
    in
    foldLines (docCommentLines Prelude.++ assignLines)


formatDocLine :: Text -> Text
formatDocLine docLine =
    if T.null docLine then
        "#'"
    else
        "#' " Prelude.<> docLine


foldLines :: [Text] -> Doc
foldLines = List.foldl' (\d t -> d <+> line t) empty


functionText :: [Text] -> Text -> Text
functionText params body =
    if T.any (Prelude.== '\n') body then
        T.dropEnd 1
            ( T.unlines
                ( [ "function(" Prelude.<> sepBy ", " params Prelude.<> ") {"
                  ]
                    Prelude.++ indentTextBlock 2 body
                    Prelude.++ [ "}" ]
                )
            )
    else
        "function("
            Prelude.<> sepBy ", " params
            Prelude.<> ") "
            Prelude.<> braces (" " Prelude.<> body Prelude.<> " ")


prefixFirstLine :: Text -> Text -> [Text]
prefixFirstLine prefix body =
    case T.lines body of
        [] -> [prefix]
        first : rest -> (prefix Prelude.<> first) : rest


indentTextBlock :: Prelude.Int -> Text -> [Text]
indentTextBlock n =
    Prelude.fmap (T.replicate (Prelude.fromIntegral n) " " Prelude.<>) Prelude.. T.lines


renderPipe :: GenEnv -> Expr -> Text
renderPipe env pipe =
    case pipeParts pipe of
        [] -> ""
        [one] -> generateExprIn env one
        first : rest ->
            T.intercalate "\n" (pipeLineGroups first rest)
  where
    pipeLineGroups first rest =
        let
            renderedFirst = generateExprIn env first
            renderedRest = Prelude.fmap (generatePipeRhs env) rest
            withPipes =
                appendPipeToLastLine renderedFirst
                    : Prelude.fmap
                        (indentText 2 Prelude.. appendPipeToLastLine)
                        (Prelude.init renderedRest)
            finalPart = case Prelude.reverse renderedRest of
                [] -> []
                lastPart : _ -> [indentText 2 lastPart]
        in
        withPipes Prelude.++ finalPart


pipeParts :: Expr -> [Expr]
pipeParts = \case
    EPipe _ lhs rhs -> pipeParts lhs Prelude.++ [rhs]
    other -> [other]


appendPipeToLastLine :: Text -> Text
appendPipeToLastLine txt =
    case T.lines txt of
        [] -> "|>"
        lines_ ->
            T.intercalate "\n"
                (Prelude.init lines_ Prelude.++ [Prelude.last lines_ Prelude.<> " |>"])


indentText :: Prelude.Int -> Text -> Text
indentText n =
    T.intercalate "\n" Prelude.. indentTextBlock n



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
    EVar n ->
        case Map.lookup (lowerText n) (geExternBodies env) of
            Just body -> externValueR body
            Prelude.Nothing -> resolvedName env n
    ECon n ->
        -- Nullary constructors (`True`, `False`, `Nothing`) lower
        -- directly: `True`/`False` to R booleans, others to a tagged
        -- list. n-ary constructors are caught by the EApp case below
        -- and lowered with their arguments together.
        nullaryCon n
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
        renderApplication env head_ args (Prelude.fmap (generateExprIn env) args)
    EBinOp _ op l r ->
        renderBinary env op l r
    EUnary _ op e ->
        unaryOpR env op Prelude.<> renderUnaryOperand env e
    pipe@(EPipe _ _ _) ->
        renderPipe env pipe
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


-- | Substitute @$1@, @$2@, ... in a `via` template with the
-- corresponding rendered Quone argument (M3.9).
--
-- A reference to @$N@ where N is out of range expands to an empty
-- string; this is intentionally permissive (the template author
-- knows the arity) — a future @validate@ check could tighten this
-- to a diagnostic when the typer's arity disagrees.
expandViaTemplate :: Text -> [Text] -> Text
expandViaTemplate template args =
    Prelude.foldl'
        (\acc (i, arg) ->
            T.replace ("$" Prelude.<> T.pack (Prelude.show i)) arg acc)
        template
        (Prelude.zip [1 :: Prelude.Int ..] args)


-- | Should this foreign-import call swap its first two arguments to
-- match R's data-first convention?
--
-- Today: hardcoded to `purrr::*` callables that follow the
-- @(.x, .f, ...)@ pattern (most of `purrr`). Quone's curry order
-- puts the function first (`map fn xs`), so without a swap the
-- generated R is `purrr::map(fn, xs)` which is broken
-- (KNOWN_FAILURES #3).
--
-- M3 replaces this with a general `import ... via "<template>"`
-- mechanism.
needsPurrrSwap :: Text -> Prelude.Bool
needsPurrrSwap qualified =
    Prelude.any
        (\fn -> qualified Prelude.== "purrr::" Prelude.<> fn)
        purrrSwapList


-- | The `purrr::*` functions whose first parameter is the data.
-- Sourced from <https://purrr.tidyverse.org>.
purrrSwapList :: [Text]
purrrSwapList =
    -- map family
    [ "map", "map_chr", "map_dbl", "map_int", "map_lgl", "map_raw"
    , "map_dfc", "map_dfr", "map_vec"
    , "imap", "imap_chr", "imap_dbl", "imap_int", "imap_lgl"
    , "imap_dfc", "imap_dfr", "imap_vec"
    , "lmap", "lmap_at", "lmap_if"
    -- map2 family (2-vector args; data is .x, .y, fn is .f at position 3)
    -- Don't swap; positional layout already matches if the user knows it
    -- walk family
    , "walk", "walk2", "iwalk"
    -- predicate / reduce family
    , "every", "some", "none", "keep", "discard", "compact"
    , "detect", "detect_index"
    , "reduce", "reduce_right", "reduce2", "accumulate", "accumulate_right"
    -- pluck / modify
    , "pluck", "modify", "modify_at", "modify_if", "modify_in"
    -- transpose / list_*
    , "transpose"
    ]


-- | Lower a nullary constructor reference (`True`, `False`,
-- `Nothing`, or any user ADT constructor with no arguments). The two
-- `Logical` constructors lower to R's reserved boolean names; every
-- other nullary constructor lowers to a tagged list with no values.
nullaryCon :: UpperName -> Text
nullaryCon n = case upperText n of
    "True" -> "TRUE"
    "False" -> "FALSE"
    other -> "list(tag = \"" Prelude.<> other Prelude.<> "\", values = list())"


-- | Lower a constructor application. The args are already-rendered
-- argument expressions; this wraps them in the tagged list shape
-- the case-pattern lowering reads from.
conApply :: UpperName -> [Text] -> Text
conApply n args = case upperText n of
    "True" -> "TRUE"   -- defensive: True/False shouldn't take args, but be safe
    "False" -> "FALSE"
    other ->
        "list(tag = \""
            Prelude.<> other
            Prelude.<> "\", values = list("
            Prelude.<> sepBy ", " args
            Prelude.<> "))"


-- | Lower a fully-applied call to a prelude @extern@ value. Picks
-- the right R callable string based on the body shape:
--
--   * 'ExternSimple r' — emit @r(args...)@ verbatim.
--   * 'ExternDispatch r dispatchOn' — pick a type-suffixed variant
--     of @r@ (e.g. @purrr::map@ → @purrr::map_dbl@) based on the
--     syntactic shape of the function argument, and emit the call
--     with @purrr@'s @(.x, .f)@ argument order (vector first,
--     function second), which is the inverse of Quone's curried
--     @map fn xs@ order.
externCall :: GenEnv -> ExternBody -> [Expr] -> Text
externCall env body args =
    externCallRendered env body args (Prelude.fmap (generateExprIn env) args)


externCallRendered :: GenEnv -> ExternBody -> [Expr] -> [Text] -> Text
externCallRendered _env body args renderedArgs =
    case body of
        ExternSimple _ r ->
            callR (parenthesiseIfFn r) renderedArgs
        ExternDispatch _ r _dispatchOn ->
            -- Curried Quone signature: `map fn xs`. R's purrr::map
            -- takes `(.x, .f)`. Swap the first two arguments and
            -- pick the type suffix from the function body.
            case (args, renderedArgs) of
                (fn : _, fnText : xsText : restText) ->
                    let
                        suffix = mapSuffix fn
                    in
                    callR (r Prelude.<> suffix) (xsText : fnText : restText)
                _ ->
                    -- Underapplied: fall back to the verbatim form so
                    -- the resulting R is at least syntactically valid.
                    callR r renderedArgs


externValueR :: ExternBody -> Text
externValueR = \case
    ExternSimple _ r -> parenthesiseIfFn r
    ExternDispatch _ r _ -> r


-- | If the extern body string looks like an R function expression
-- (e.g. `"function(p, xs) sum(...)"`), wrap it in parens so the
-- subsequent `(args...)` actually applies it. Bare callables
-- (`"+"`, `"mean"`, `"dplyr::if_else"`) pass through unchanged.
parenthesiseIfFn :: Text -> Text
parenthesiseIfFn r =
    if T.isPrefixOf "function" r
        then "(" Prelude.<> r Prelude.<> ")"
        else r


-- | Syntactic guess at the result element type of a function passed
-- to @map@. Returns the @purrr::map_*@ suffix or @""@ for the
-- generic case.
--
-- Best-effort: covers the common cases (lambda returning a literal,
-- a comparison, an integer-only operator, or @sqrt@) and falls back
-- to generic @purrr::map@ otherwise. A complete solution would
-- re-run type inference on the function argument; for initial release the
-- syntactic check is sufficient and documented as @[planned]@ for
-- replacement.
mapSuffix :: Expr -> Text
mapSuffix = \case
    ELambda _ _ body -> bodySuffix body
    -- A bare named function: look up known prelude functions.
    EVar n -> namedFnSuffix (lowerText n)
    _ -> ""
  where
    bodySuffix = \case
        ELit _ (LDouble _) -> "_dbl"
        ELit _ (LInt _) -> "_int"
        ELit _ (LChar _) -> "_chr"
        ECon n
          | upperText n Prelude.== "True" Prelude.|| upperText n Prelude.== "False" -> "_lgl"
        -- Comparison operators always return Logical.
        EBinOp _ op _ _
          | op `Prelude.elem` [OpEq, OpNeq, OpGt, OpLt, OpGe, OpLe] -> "_lgl"
        -- Integer-only operators always return Integer.
        EBinOp _ op _ _
          | op `Prelude.elem` [OpIntDiv, OpMod] -> "_int"
        -- Double-only operators always return Double.
        EBinOp _ OpExp _ _ -> "_dbl"
        -- Arithmetic operators preserve the element type. Try the
        -- left operand's literal kind, then the right's, before
        -- giving up to a generic dispatch.
        EBinOp _ _ l r -> firstNonEmpty (bodySuffix l) (bodySuffix r)
        EUnary _ OpNeg e -> bodySuffix e
        EApp _ f _ -> case collectAppHead f of
            Just n -> namedFnSuffix n
            Nothing -> ""
        _ -> ""

    firstNonEmpty a b = if T.null a then b else a

    namedFnSuffix = \case
        "sqrt" -> "_dbl"
        "to_double" -> "_dbl"
        "mean" -> "_dbl"
        "sum" -> "_dbl"
        "length" -> "_int"
        _ -> ""

    collectAppHead :: Expr -> Maybe Text
    collectAppHead = \case
        EVar n -> Just (lowerText n)
        EApp _ f _ -> collectAppHead f
        _ -> Nothing


-- | When a verb appears outside a pipe (rare; the recommended style
-- is `xs |> verb args`), emit a function reference. The compiler does
-- not normally produce this path, but having a sensible lowering keeps
-- the generator total.
--
-- Two cases:
--
--   1. The verb's name shadows a prelude `extern` (e.g. `count` is
--      both a verb keyword AND a prelude function). When the user
--      writes `count predicate xs` outside a pipe, the parser
--      produces an EVerb but the user's intent is the prelude
--      function. Look up the extern body first and dispatch via
--      'externCall' so the lowered R uses the prelude semantics.
--   2. Otherwise fall back to `dplyr::verbName(args...)`, which
--      mirrors the pipe path.
verbCall :: GenEnv -> Verb -> [DplyrArg] -> Text
verbCall env verb args =
    let
        verbText = verbName verb
        plainArgs = Prelude.fmap dplyrArgExpr args
    in
    case (Map.lookup verbText (geExternBodies env), allJust plainArgs) of
        (Just body, Just exprs) ->
            externCall env body exprs
        _ ->
            let
                fn = "dplyr::" Prelude.<> verbText
            in
            case args of
                [] -> fn
                _ -> callR fn (Prelude.fmap (dplyrArgR env) args)
  where
    dplyrArgExpr = \case
        DAExpr e -> Just e
        _ -> Prelude.Nothing
    allJust :: [Maybe a] -> Maybe [a]
    allJust [] = Just []
    allJust (Just x : xs) = Prelude.fmap (x :) (allJust xs)
    allJust (Prelude.Nothing : _) = Prelude.Nothing


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
            _ -> callRMultiline fn (dplyrArgRsForVerb env verb args)
    EVar n ->
        callR (generateExprIn env (EVar n)) []
    other -> generateExprIn env other


literal :: Literal -> Text
literal = \case
    LInt n -> T.pack (Prelude.show n) Prelude.<> "L"
    LDouble d -> rDouble d
    LChar t -> "\"" Prelude.<> t Prelude.<> "\""


rDouble :: Prelude.Double -> Text
rDouble d =
    let
        rounded = Prelude.round d :: Prelude.Integer
    in
    if d Prelude.== Prelude.fromInteger rounded
        then T.pack (Prelude.show rounded)
        else T.pack (Prelude.show d)


-- | The R spelling of a binary operator. Looked up from
-- 'geOperatorR' (populated from prelude @infix@ declarations); falls
-- back to a hardcoded default when the prelude is unavailable, so
-- callers using 'emptyGenEnv' (e.g. tests) still produce sensible
-- output.
binOpR :: GenEnv -> BinOp -> Text
binOpR env op = Map.findWithDefault (defaultBinOpR op) op (geOperatorR env)


defaultBinOpR :: BinOp -> Text
defaultBinOpR = \case
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


unaryOpR :: GenEnv -> UnaryOp -> Text
unaryOpR env op = Map.findWithDefault (defaultUnaryOpR op) op (geUnaryR env)


defaultUnaryOpR :: UnaryOp -> Text
defaultUnaryOpR = \case
    OpNeg -> "-"


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
        Prelude.<> binOpR env op
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


renderApplication :: GenEnv -> Expr -> [Expr] -> [Text] -> Text
renderApplication env head_ args renderedArgs =
    case head_ of
        EVar n
          | Just template <- Map.lookup (lowerText n) (geForeignVia env) ->
              -- The foreign import declared a `via "<template>"`
              -- clause (M3.9): substitute @$1@, @$2@, ... in the
              -- template with the rendered Quone arguments in source order.
              expandViaTemplate template renderedArgs
        EVar n
          | Just body <- Map.lookup (lowerText n) (geExternBodies env) ->
              -- The callee was declared in the prelude as an `extern` value:
              -- look up its R body and emit `<r>(args...)`.
              externCallRendered env body args renderedArgs
        EVar n ->
            -- Foreign imports of `purrr::*` family functions expect the data
            -- first (`.x`), function second (`.f`), even though Quone's curry
            -- order puts the function first (`map fn xs`). Swap when needed.
            let
                resolved = resolvedName env n
                callArgs = reorderQuoneValueArgs env n renderedArgs
                finalArgs = if needsPurrrSwap resolved Prelude.&& Prelude.length callArgs Prelude.>= 2
                    then case callArgs of
                        (fn : xs : rest) -> xs : fn : rest
                        _ -> callArgs
                    else callArgs
            in
            callR resolved finalArgs
        ECon n ->
            -- Constructor application: build the tagged list representation
            -- directly. The case-pattern lowering reads from the same shape.
            conApply n renderedArgs
        _ -> callR (renderCallHead env head_) renderedArgs


reorderQuoneValueArgs :: GenEnv -> LowerName -> [Text] -> [Text]
reorderQuoneValueArgs env n args =
    let
        name = lowerText n
    in
    case Map.lookup name (geValueArities env) of
        Just arity
          | arity Prelude.> 1
              Prelude.&& Prelude.length args Prelude.== arity
              Prelude.&& Prelude.not (Map.member name (gePkgQualifiers env)) ->
              toRValueOrder args
        _ -> args


toRValueOrder :: [Text] -> [Text]
toRValueOrder args =
    case Prelude.reverse args of
        [] -> []
        lastArg : restReversed -> lastArg : Prelude.reverse restReversed


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
ifShape env scrut [a, b] =
    let
        (trueArm, falseArm) =
            case caseArmPattern a of
                PCon _ n [] | upperText n Prelude.== "True" -> (a, b)
                _ -> (b, a)
    in
    "if ("
        Prelude.<> generateExprIn env scrut
        Prelude.<> ") "
        Prelude.<> generateExprIn env (caseArmBody trueArm)
        Prelude.<> " else "
        Prelude.<> generateExprIn env (caseArmBody falseArm)
ifShape env scrut arms =
    chainShape env scrut arms


-- | The general case lowering. Keep the common no-binding case close
-- to idiomatic R: an @if@ chain is itself an expression, so callers
-- can assign it directly. Pattern forms that introduce variables use
-- a small block with a readable temporary.
chainShape :: GenEnv -> Expr -> [CaseArm] -> Text
chainShape env scrut arms =
    let
        scrutText = generateExprIn env scrut
        needsTemp =
            T.any (Prelude.== '\n') scrutText
                Prelude.|| (Prelude.not (caseScrutineeIsSimple scrut)
                    Prelude.&& Prelude.any (armHasPatternBinds "case_value") arms)
    in
    if needsTemp
        then
            T.intercalate
                "\n"
                ( [ "{"
                  , "  case_value <- " Prelude.<> scrutText
                  , ""
                  ]
                    Prelude.++ indentTextBlock 2 (buildChain env "case_value" arms)
                    Prelude.++ ["}"]
                )
        else buildChain env scrutText arms


buildChain :: GenEnv -> Text -> [CaseArm] -> Text
buildChain env scrutText =
    renderIfChain Prelude.. chainLines
  where
    chainLines [] =
        [ IfArm
            { ifArmPredicate = Nothing
            , ifArmLines = ["stop(\"non-exhaustive case\")"]
            }
        ]
    chainLines (a : rest) =
        let
            (test, binds) = patternToTest scrutText (caseArmPattern a)
            armBody = generateExprIn env (caseArmBody a)
            bodyLines = binds Prelude.++ T.lines armBody
            guardedLines = case caseArmGuard a of
                Nothing -> bodyLines
                Just g ->
                    -- The guard sees pattern bindings, so it must run
                    -- inside this arm. On guard-false, fall through to
                    -- the remaining arms.
                    binds
                        Prelude.++ T.lines
                            ( renderIfChain
                                [ IfArm
                                    { ifArmPredicate = Just (generateExprIn env g)
                                    , ifArmLines = T.lines armBody
                                    }
                                , IfArm
                                    { ifArmPredicate = Nothing
                                    , ifArmLines = T.lines (buildChain env scrutText rest)
                                    }
                                ]
                            )
        in
        IfArm
            { ifArmPredicate = test
            , ifArmLines = guardedLines
            }
            : chainLines rest


data IfArm = IfArm
    { ifArmPredicate :: Maybe Text
    , ifArmLines :: [Text]
    }


renderIfChain :: [IfArm] -> Text
renderIfChain arms =
    T.intercalate "\n" (go Prelude.True arms)
  where
    go _ [] = []
    go isFirst [IfArm Prelude.Nothing bodyLines] =
        let
            head_ = if isFirst then "else {" else "} else {"
        in
        head_
            : indentLines 2 bodyLines
            Prelude.++ ["}"]
    go isFirst (IfArm (Just predicate) bodyLines : rest) =
        let
            head_ =
                (if isFirst then "if (" else "} else if (")
                    Prelude.<> predicate
                    Prelude.<> ") {"
            close = case rest of
                [] -> ["}"]
                _ -> []
        in
        head_
            : indentLines 2 bodyLines
            Prelude.++ close
            Prelude.++ go Prelude.False rest
    go isFirst (IfArm Prelude.Nothing bodyLines : _rest) =
        go isFirst [IfArm Prelude.Nothing bodyLines]


indentLines :: Prelude.Int -> [Text] -> [Text]
indentLines n =
    Prelude.fmap (T.replicate (Prelude.fromIntegral n) " " Prelude.<>)


armHasPatternBinds :: Text -> CaseArm -> Prelude.Bool
armHasPatternBinds scrutText arm =
    let
        (_, binds) = patternToTest scrutText (caseArmPattern arm)
    in
    Prelude.not (Prelude.null binds)


caseScrutineeIsSimple :: Expr -> Prelude.Bool
caseScrutineeIsSimple = \case
    EVar _ -> Prelude.True
    EField _ base _ -> caseScrutineeIsSimple base
    _ -> Prelude.False


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
                            -- R's `[[i]]` extracts a single list element
                            -- (without enclosing it in a one-element list,
                            -- which `[i]` would do). The constructor
                            -- arguments live as positional entries of
                            -- the values list, so we want `[[i]]`.
                            lowerText argName
                                Prelude.<> " <- "
                                Prelude.<> scrut
                                Prelude.<> "$values[["
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
        -- Nested patterns are valid; for initial release lowering we collapse
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
    VLeftJoin -> "left_join"
    VRightJoin -> "right_join"
    VInnerJoin -> "inner_join"


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


dplyrArgRsForVerb :: GenEnv -> Verb -> [DplyrArg] -> [Text]
dplyrArgRsForVerb env verb =
    Prelude.concatMap
        (case verb of
            VSummarize -> dplyrArgRItemsSummarize env
            _ -> dplyrArgRItems env)


dplyrArgRItems :: GenEnv -> DplyrArg -> [Text]
dplyrArgRItems env = \case
    DAExpr e -> [generateExprIn env e]
    DARecord _ fields -> Prelude.fmap (namedField env) fields
    DAModifier m -> [modifierR env m]
    DAJoinOn _ other pairs ->
        [ generateExprIn env other
        , "by = c(" Prelude.<> sepBy ", " (Prelude.fmap pairR pairs) Prelude.<> ")"
        ]


dplyrArgRItemsSummarize :: GenEnv -> DplyrArg -> [Text]
dplyrArgRItemsSummarize env = \case
    DARecord _ fields -> Prelude.fmap (namedFieldSummarize env) fields
    other -> dplyrArgRItems env other


namedField :: GenEnv -> FieldBinding -> Text
namedField env fb =
    lowerText (fieldBindingName fb)
        Prelude.<> " = "
        Prelude.<> generateExprIn env (fieldBindingValue fb)


namedFieldSummarize :: GenEnv -> FieldBinding -> Text
namedFieldSummarize env fb =
    lowerText (fieldBindingName fb)
        Prelude.<> " = "
        Prelude.<> summarizeRhsR env (fieldBindingValue fb)


summarizeRhsR :: GenEnv -> Expr -> Text
summarizeRhsR env = \case
    EVar n
      | lowerText n Prelude.== "count" -> "dplyr::n()"
    ECon n
      | upperText n Prelude.== "CountRows" -> "dplyr::n()"
    other -> generateExprIn env other


callRMultiline :: Text -> [Text] -> Text
callRMultiline fn args =
    let
        oneLine = callR fn args
    in
    if Prelude.length args Prelude.<= 1
        && T.length oneLine Prelude.<= 80
        && Prelude.not (Prelude.any (T.any (Prelude.== '\n')) args)
    then
        oneLine
    else
        T.intercalate "\n"
            ( [fn Prelude.<> "("]
                Prelude.++ commaLines args
                Prelude.++ [")"]
            )


commaLines :: [Text] -> [Text]
commaLines args =
    case args of
        [] -> []
        _ ->
            let
                lastIndex = Prelude.length args Prelude.- 1
            in
            Prelude.fmap
                (\(idx, arg) ->
                    let
                        suffix = if idx Prelude.== lastIndex then "" else ","
                    in
                    indentText 2 arg Prelude.<> suffix
                )
                (Prelude.zip [0 :: Prelude.Int ..] args)


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
