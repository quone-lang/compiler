{-| Dataframe verb typing.

Implements LANGUAGE.md section 8.7's typing rules for the six verbs
that are normative in v0.0.1: 'VSelect', 'VFilter', 'VMutate',
'VSummarize', 'VGroupBy', 'VArrange'. The remaining verbs reserved in
section 3.4 are accepted by the parser (they typecheck as @Dataframe a
-> Dataframe a@ for v0.0.1; tighter rules are @[planned]@ per
section 19.3).

The bare-column-name sugar from section 9.2 is implemented here at
the typing level: when a verb's argument is a record literal, the
record's value expressions are typechecked in an environment where
each column of the input dataframe is bound to its element type. This
matches the explicit-lambda desugaring's typing without producing
syntactic lambdas.

-}
module Quone.Type.Verb
    ( typeVerb
    , verbResultType
    )
where

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
import Quone.Position (SourceSpan)
import Quone.Type.Env
import Quone.Type.Types
import qualified Prelude



-- ---------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------


-- | Type a verb application against a known input dataframe schema.
--
-- The first argument is the schema of the dataframe being piped in
-- (so @students |> filter ...@ calls this with the schema of
-- @students@). The verb is checked, and the resulting schema is
-- returned along with any diagnostics produced.
--
-- The expression typer ('Quone.Type.Infer.inferExprIn') is supplied
-- as a callback; this lets us evaluate predicate / mutator
-- expressions in the row-scoped environment without circular imports.
typeVerb
    :: (Env -> Expr -> Prelude.Either Diagnostic Type)
    -> Env
    -> SourceSpan
    -> Verb
    -> Map.Map Text Type    -- input row schema
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeVerb infer env sp verb schema args = case verb of
    VSelect -> typeSelect sp schema args
    VFilter -> typeFilter infer env sp schema args
    VMutate -> typeMutate infer env sp schema args
    VSummarize -> typeSummarize infer env sp schema args
    VGroupBy -> typeGroupBy sp schema args
    VArrange -> typeArrange sp schema args
    _ ->
        -- Verbs that don't yet have normative typing rules: accept
        -- the input schema unchanged so downstream typing succeeds.
        Prelude.Right schema


-- | Convenience: shape the verb result as a 'TyDataframe'.
verbResultType :: Map.Map Text Type -> Type
verbResultType = TyDataframe



-- ---------------------------------------------------------------------
-- Per-verb rules
-- ---------------------------------------------------------------------


typeSelect
    :: SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeSelect sp schema args = do
    -- select expects exactly one record-shaped argument; its field
    -- names are the columns to keep.
    fields <- argRecordNames sp args
    let
        unknown = Prelude.filter (\n -> Prelude.not (Map.member n schema)) fields
    case unknown of
        [] -> Prelude.Right (Map.filterWithKey (\k _ -> k `Prelude.elem` fields) schema)
        (n : _) -> Prelude.Left (unknownColumn sp n)


typeFilter
    :: (Env -> Expr -> Prelude.Either Diagnostic Type)
    -> Env
    -> SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeFilter infer env sp schema args = do
    -- filter takes one expression argument that produces a Logical
    -- under row scope.
    case Prelude.fmap dplyrArgExpr args of
        [Just predicate] -> do
            let
                envRow = bindColumns env schema
            predTy <- infer envRow predicate
            if predTy Prelude.== primLogical
                then Prelude.Right schema
                else
                    Prelude.Left
                        ( typeMismatch sp
                            ( "filter predicate must be Logical; got "
                                Prelude.<> showType predTy
                            )
                        )
        _ ->
            Prelude.Left
                ( verbShapeError sp "filter" "exactly one expression argument"
                )


typeMutate
    :: (Env -> Expr -> Prelude.Either Diagnostic Type)
    -> Env
    -> SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeMutate infer env sp schema args = do
    fields <- argRecordBindings sp args
    let
        envRow = bindColumns env schema
    newCols <-
        Prelude.traverse
            (\fb -> do
                t <- infer envRow (fieldBindingValue fb)
                Prelude.pure (lowerText (fieldBindingName fb), wrapVector t))
            fields
    Prelude.Right (Map.union (Map.fromList newCols) schema)


typeSummarize
    :: (Env -> Expr -> Prelude.Either Diagnostic Type)
    -> Env
    -> SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeSummarize infer env sp _schema args = do
    fields <- argRecordBindings sp args
    -- summarize uses group scope per LANGUAGE.md section 9.3: each
    -- column is visible as the full Vector being aggregated (so a
    -- caller can write `mean score` where mean : Vector Double ->
    -- Double). We then wrap the per-row result back into a Vector to
    -- preserve the dataframe-of-vectors invariant.
    let
        envGroup = bindColumnsAsVectors env _schema
    cols <-
        Prelude.traverse
            (\fb -> do
                t <- infer envGroup (fieldBindingValue fb)
                Prelude.pure (lowerText (fieldBindingName fb), wrapVector t))
            fields
    Prelude.Right (Map.fromList cols)


typeGroupBy
    :: SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeGroupBy sp schema args = do
    fields <- argRecordNames sp args
    let
        unknown = Prelude.filter (\n -> Prelude.not (Map.member n schema)) fields
    case unknown of
        [] -> Prelude.Right schema
        (n : _) -> Prelude.Left (unknownColumn sp n)


typeArrange
    :: SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeArrange sp schema args = do
    fields <- argSortColumnNames sp args
    let
        unknown = Prelude.filter (\n -> Prelude.not (Map.member n schema)) fields
    case unknown of
        [] -> Prelude.Right schema
        (n : _) -> Prelude.Left (unknownColumn sp n)



-- ---------------------------------------------------------------------
-- Argument shape helpers
-- ---------------------------------------------------------------------


-- | The record-of-bare-column-names form: @select { name, score }@.
-- Returns the names in source order.
argRecordNames :: SourceSpan -> [DplyrArg] -> Prelude.Either Diagnostic [Text]
argRecordNames sp = \case
    [DARecord _ fbs] ->
        Prelude.Right (Prelude.fmap (\fb -> lowerText (fieldBindingName fb)) fbs)
    _ ->
        Prelude.Left
            ( verbShapeError sp "verb"
                "a record argument like { col1, col2 }"
            )


-- | The full record-of-bindings form: @mutate { pct = score / 100.0 }@.
-- Returns the field bindings.
argRecordBindings :: SourceSpan -> [DplyrArg] -> Prelude.Either Diagnostic [FieldBinding]
argRecordBindings sp = \case
    [DARecord _ fbs] -> Prelude.Right fbs
    _ ->
        Prelude.Left
            ( verbShapeError sp "verb"
                "a record argument like { col = expr }"
            )


-- | @arrange@ accepts either bare names or @desc col@ / @asc col@
-- modifiers. Returns the column names referenced.
argSortColumnNames
    :: SourceSpan
    -> [DplyrArg]
    -> Prelude.Either Diagnostic [Text]
argSortColumnNames sp = \case
    [DARecord _ fbs] ->
        Prelude.Right (Prelude.fmap (\fb -> lowerText (fieldBindingName fb)) fbs)
    args ->
        let
            fromMod = \case
                DAModifier (MDesc _ n) -> Just (lowerText n)
                DAModifier (MAsc _ n) -> Just (lowerText n)
                _ -> Nothing
            cols = Prelude.foldr
                (\arg acc -> case fromMod arg of
                    Just c -> c : acc
                    Nothing -> acc)
                []
                args
        in
        if Prelude.null cols
            then
                Prelude.Left
                    ( verbShapeError sp "arrange"
                        "a record of column names or `desc col` / `asc col` modifiers"
                    )
            else Prelude.Right cols


dplyrArgExpr :: DplyrArg -> Maybe Expr
dplyrArgExpr = \case
    DAExpr e -> Just e
    _ -> Nothing



-- ---------------------------------------------------------------------
-- Row scope
-- ---------------------------------------------------------------------


-- | Row scope (LANGUAGE.md section 9.3): each column is bound as a
-- single element. Used by @filter@, @mutate@, @group_by@, @arrange@.
bindColumns :: Env -> Map.Map Text Type -> Env
bindColumns env schema =
    Map.foldlWithKey'
        (\e name colTy ->
            insertValue
                name
                (monoScheme (elementType colTy))
                e
        )
        env
        schema


-- | Group scope (section 9.3): each column is bound as the full
-- 'Vector' being aggregated. Used only by @summarize@.
bindColumnsAsVectors :: Env -> Map.Map Text Type -> Env
bindColumnsAsVectors env schema =
    Map.foldlWithKey'
        (\e name colTy ->
            insertValue name (monoScheme colTy) e
        )
        env
        schema


-- | Strip a single @Vector@ wrapper to recover the element type.
elementType :: Type -> Type
elementType = \case
    TyApp (TyCon "Vector") inner -> inner
    other -> other


-- | Wrap a value type back into a @Vector@ for storage as a column.
-- A column expression that already returns a Vector (e.g. an
-- aggregation function applied to a column) is left alone.
wrapVector :: Type -> Type
wrapVector = \case
    already@(TyApp (TyCon "Vector") _) -> already
    other -> TyApp (TyCon "Vector") other



-- ---------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------


unknownColumn :: SourceSpan -> Text -> Diagnostic
unknownColumn sp col =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = UnknownDataframeColumn
        , diagSpan = sp
        , diagMessage =
            "dataframe has no column "
                Prelude.<> T.pack (Prelude.show col)
        , diagHint = Nothing
        }


verbShapeError :: SourceSpan -> Text -> Text -> Diagnostic
verbShapeError sp verbName what =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = TypeMismatch
        , diagSpan = sp
        , diagMessage =
            verbName Prelude.<> " expects " Prelude.<> what
        , diagHint = Nothing
        }


typeMismatch :: SourceSpan -> Text -> Diagnostic
typeMismatch sp msg =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = TypeMismatch
        , diagSpan = sp
        , diagMessage = msg
        , diagHint = Nothing
        }
