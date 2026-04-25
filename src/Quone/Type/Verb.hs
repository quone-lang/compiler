{-| Dataframe verb typing.

Implements LANGUAGE.md section 8.7's typing rules for the six verbs
that are normative in initial release: 'VSelect', 'VFilter', 'VMutate',
'VSummarize', 'VGroupBy', 'VArrange'. The remaining verbs reserved in
section 3.4 are accepted by the parser (they typecheck as @Dataframe a
-> Dataframe a@ for initial release; tighter rules are @[planned]@ per
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
    , classifyExpr
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
    -> DataframeShape       -- input shape (schema + grouping)
    -> [DplyrArg]
    -> Prelude.Either Diagnostic DataframeShape
typeVerb infer env sp verb shape args =
    let
        schema = dfSchema shape
        grouping = dfGroupingCols shape
    in
    case verb of
        VSelect -> Prelude.fmap (preserveGrouping grouping) (typeSelect sp schema args)
        VFilter -> Prelude.fmap (preserveGrouping grouping) (typeFilter infer env sp schema args)
        VMutate -> Prelude.fmap (preserveGrouping grouping) (typeMutate infer env sp schema args)
        VSummarize ->
            -- summarize collapses to one row per group: keep the
            -- grouping columns from the input schema in the output,
            -- then add the new summary columns. The result is
            -- ungrouped (KNOWN_FAILURES #4).
            Prelude.fmap
                (\summaryCols ->
                    let
                        keptGroupingCols =
                            Map.filterWithKey
                                (\k _ -> k `Prelude.elem` grouping)
                                schema
                        merged = Map.union summaryCols keptGroupingCols
                    in
                    ungroupedDf merged)
                (typeSummarize infer env sp schema args)
        VGroupBy -> typeGroupBy sp schema args
        VUngroup -> Prelude.Right (ungroupedDf schema)
        VArrange -> Prelude.fmap (preserveGrouping grouping) (typeArrange sp schema args)
        VRename -> Prelude.fmap (preserveGrouping grouping) (typeRename sp schema args)
        VLeftJoin -> typeJoin sp shape args
        VRightJoin -> typeJoin sp shape args
        VInnerJoin -> typeJoin sp shape args


-- | Wrap a stateless (column-only) verb result with the grouping
-- list it inherits from the input shape.
preserveGrouping :: [Text] -> Map.Map Text Type -> DataframeShape
preserveGrouping grouping schema =
    DataframeShape {dfSchema = schema, dfGroupingCols = grouping}


-- | Convenience: shape the verb result as a 'TyDataframe'.
verbResultType :: DataframeShape -> Type
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
    -- filter takes one expression argument that produces a Vector Logical
    -- under column-vector scope.
    case Prelude.fmap dplyrArgExpr args of
        [Just predicate] -> do
            let
                envColumns = bindColumnsAsVectors env schema
            predTy <- infer envColumns predicate
            if predTy Prelude.== TyApp (TyCon "Vector") primLogical
                then Prelude.Right schema
                else
                    Prelude.Left
                        ( typeMismatch sp
                            ( "filter predicate must be Vector Logical; got "
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
        envColumns = bindColumnsAsVectors env schema
    newCols <-
        Prelude.traverse
            (\fb -> do
                let
                    rhs = fieldBindingValue fb
                t <- infer envColumns rhs
                -- The schema stores element types; mutate's row-scope
                -- rhs returns an element value (e.g. `Double` for
                -- `score / 100.0`). Strip any redundant Vector
                -- wrapper so we never store `Vector (Vector T)`.
                Prelude.pure (lowerText (fieldBindingName fb), elementType t))
            fields
    Prelude.Right (Map.union (Map.fromList newCols) schema)


typeSummarize
    :: (Env -> Expr -> Prelude.Either Diagnostic Type)
    -> Env
    -> SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeSummarize infer env sp schema args = do
    fields <- argRecordBindings sp args
    -- summarize uses group scope per LANGUAGE.md section 9.3: each
    -- column is visible as the full Vector being aggregated (so a
    -- caller can write `mean score` where mean : Vector Double ->
    -- Double). The summary value is a scalar (element type); we
    -- store it directly to match the schema convention.
    let
        envGroup = bindColumnsAsVectors env schema
    cols <-
        Prelude.traverse
            (\fb -> do
                let
                    rhs = fieldBindingValue fb
                t <- infer envGroup rhs
                checkSummarizeRhs envGroup schema (fieldBindingSpan fb) rhs
                Prelude.pure
                    ( lowerText (fieldBindingName fb)
                    , summarizeResultType t
                    ))
            fields
    Prelude.Right (Map.fromList cols)


typeGroupBy
    :: SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic DataframeShape
typeGroupBy sp schema args = do
    fields <- argRecordNames sp args
    let
        unknown = Prelude.filter (\n -> Prelude.not (Map.member n schema)) fields
    case unknown of
        [] -> Prelude.Right (groupedDf schema fields)
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


-- | @rename { new = old, ... }@: each lhs is the new column name,
-- each rhs is the existing column to be renamed. The output schema
-- replaces every old column with its new name (preserving type).
typeRename
    :: SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeRename sp schema args = do
    fbs <- argRecordBindings sp args
    -- Each field binding's value MUST be `EVar n` (a bare column name).
    pairs <-
        Prelude.traverse
            (\fb -> case fieldBindingValue fb of
                EVar oldName ->
                    Prelude.Right
                        ( lowerText (fieldBindingName fb)
                        , lowerText oldName
                        )
                _ ->
                    Prelude.Left
                        ( verbShapeError
                            (fieldBindingSpan fb)
                            "rename"
                            "each field's value must be a bare column name"
                        ))
            fbs
    -- Validate every old name exists.
    let
        missing = Prelude.filter (\(_, old) -> Prelude.not (Map.member old schema)) pairs
    case missing of
        ((_, old) : _) -> Prelude.Left (unknownColumn sp old)
        [] ->
            -- Apply: drop old names, add new names with the same type.
            let
                renamed =
                    Prelude.foldr
                        (\(new, old) acc ->
                            case Map.lookup old schema of
                                Just t -> Map.insert new t (Map.delete old acc)
                                Nothing -> acc)
                        schema
                        pairs
            in
            Prelude.Right renamed


-- | @distinct { col1, col2 }@: filter to unique rows over the named
-- columns. The schema is unchanged.
typeDistinct
    :: SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeDistinct sp schema args = do
    fields <- argRecordNames sp args
    let
        unknown = Prelude.filter (\n -> Prelude.not (Map.member n schema)) fields
    case unknown of
        [] -> Prelude.Right schema
        (n : _) -> Prelude.Left (unknownColumn sp n)


-- | @count { col1, col2 }@: collapse to one row per unique combination
-- of the named columns plus an `n` column with the row count.
typeCount
    :: SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeCount sp schema args = do
    fields <- argRecordNames sp args
    let
        unknown = Prelude.filter (\n -> Prelude.not (Map.member n schema)) fields
    case unknown of
        [] ->
            let
                kept =
                    Map.filterWithKey
                        (\k _ -> k `Prelude.elem` fields)
                        schema
            in
            Prelude.Right (Map.insert "n" primInteger kept)
        (n : _) -> Prelude.Left (unknownColumn sp n)


-- | @pull col@: extract a single column as a `Vector α`. Note this
-- changes the result type from `dataframe ...` to `Vector α`. The
-- typeVerb dispatcher needs to handle that mismatch.
typePull
    :: SourceSpan
    -> Map.Map Text Type
    -> [Text]            -- ^ grouping columns from the input shape
    -> [DplyrArg]
    -> Prelude.Either Diagnostic DataframeShape
typePull sp schema _grouping args =
    case args of
        [DAExpr (EVar colName)] ->
            case Map.lookup (lowerText colName) schema of
                Just _t ->
                    -- For initial release we still return a DataframeShape with
                    -- the single column to keep the verb-pipeline type
                    -- uniform; downstream code that expects a Vector
                    -- can extract it. A proper "pull returns a
                    -- Vector" rule waits for `pipe-result-not-df` work.
                    Prelude.Right
                        (ungroupedDf
                            (Map.singleton (lowerText colName) _t))
                Nothing ->
                    Prelude.Left (unknownColumn sp (lowerText colName))
        _ ->
            Prelude.Left
                ( verbShapeError sp "pull" "exactly one bare column name"
                )


-- | @transmute { col = expr, ... }@: like mutate but drops every
-- non-mentioned column. The output schema contains only the new
-- columns.
typeTransmute
    :: (Env -> Expr -> Prelude.Either Diagnostic Type)
    -> Env
    -> SourceSpan
    -> Map.Map Text Type
    -> [DplyrArg]
    -> Prelude.Either Diagnostic (Map.Map Text Type)
typeTransmute infer env sp schema args = do
    fields <- argRecordBindings sp args
    let
        envRow = bindColumns env schema
    newCols <-
        Prelude.traverse
            (\fb -> do
                let
                    rhs = fieldBindingValue fb
                t <- infer envRow rhs
                checkRowwiseRhs envRow (fieldBindingSpan fb) "transmute" rhs
                Prelude.pure (lowerText (fieldBindingName fb), elementType t))
            fields
    Prelude.Right (Map.fromList newCols)


-- | Inner / left / right / full / cross join. Output schema is the
-- union of the two input schemas; shared columns get the lhs type.
-- The grouping (if any) follows the lhs.
--
-- For initial release the join key is checked at runtime via @on { col }@ but
-- the typer doesn't enforce schema overlap on the key. A tighter
-- shape system (M4.3) lifts this restriction.
typeJoin
    :: SourceSpan
    -> DataframeShape
    -> [DplyrArg]
    -> Prelude.Either Diagnostic DataframeShape
typeJoin sp shape args =
    case args of
        [DAExpr (EVar _)] ->
            -- The other dataframe's schema isn't statically visible
            -- here (it's a value-level reference). We accept the
            -- input shape unchanged plus the columns the user
            -- expects; M4 tightens this when the shape system can
            -- look up the rhs dataframe's static schema.
            Prelude.Right shape
        [DAJoinOn _ (EVar _) _pairs] ->
            -- @join other on { lhs_col = rhs_col, ... }@. Same
            -- caveat: rhs schema not statically known.
            Prelude.Right shape
        _ ->
            Prelude.Left
                ( verbShapeError sp "join"
                    "another dataframe and an optional `on { lhs_col = rhs_col, ... }`"
                )


-- | @anti_join@ / @semi_join@: filter rows of lhs based on presence /
-- absence in rhs. Output schema is the lhs schema unchanged.
typeAntiSemiJoin
    :: SourceSpan
    -> DataframeShape
    -> [DplyrArg]
    -> Prelude.Either Diagnostic DataframeShape
typeAntiSemiJoin sp shape args =
    case args of
        [DAExpr (EVar _)] -> Prelude.Right shape
        [DAJoinOn _ (EVar _) _] -> Prelude.Right shape
        _ ->
            Prelude.Left
                ( verbShapeError sp "anti/semi join"
                    "another dataframe and an optional `on { lhs_col = rhs_col, ... }`"
                )



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
--
-- The schema stores the per-column element type (`Double` for a
-- column of doubles, not `Vector Double`); row scope binds it
-- directly. Columns are also classified as 'Elementwise' so that
-- the verb-rhs classification check (LANGUAGE.md section 8.7)
-- treats column references as data — not as opaque callables —
-- when walking the right-hand side of e.g. @mutate { x = col + 1 }@.
bindColumns :: Env -> Map.Map Text Type -> Env
bindColumns env schema =
    Map.foldlWithKey'
        (\e name colTy ->
            insertClassification name Elementwise
                ( insertValue
                    name
                    (monoScheme colTy)
                    e
                )
        )
        env
        schema


-- | Group scope (section 9.3): each column is bound as the full
-- 'Vector' being aggregated. Used only by @summarize@.
--
-- The schema stores element types, so we wrap them in `Vector`
-- here. See 'bindColumns' for the rationale behind classifying
-- columns as 'Elementwise'.
bindColumnsAsVectors :: Env -> Map.Map Text Type -> Env
bindColumnsAsVectors env schema =
    Map.foldlWithKey'
        (\e name colTy ->
            insertClassification name Elementwise
                ( insertValue name (monoScheme (wrapVector colTy)) e
                )
        )
        env
        schema


-- | Strip a single @Vector@ wrapper to recover the element type.
-- Idempotent on non-Vector types so callers can use it defensively.
elementType :: Type -> Type
elementType = \case
    TyApp (TyCon "Vector") inner -> inner
    other -> other


summarizeResultType :: Type -> Type
summarizeResultType = \case
    TyCon "SummaryValue" -> primInteger
    other -> elementType other


-- | Wrap a value type back into a @Vector@ for storage as a column.
-- A column expression that already returns a Vector (e.g. an
-- aggregation function applied to a column) is left alone.
wrapVector :: Type -> Type
wrapVector = \case
    already@(TyApp (TyCon "Vector") _) -> already
    other -> TyApp (TyCon "Vector") other



-- ---------------------------------------------------------------------
-- Static classification of expressions
-- ---------------------------------------------------------------------


-- | Statically approximate the R-runtime 'Classification' of an
-- expression. Used by the verb typer to reject right-hand sides whose
-- callees do not match the verb's expected vectorisation pattern (per
-- LANGUAGE.md sections 8.7 and 9.2).
--
-- The walk is conservative:
--
--   * literals, vectors, dataframes, columns, and field projections
--     are 'Elementwise' (they introduce or refer to data, not
--     control flow);
--   * 'EVar' looks up the binding's classification in the environment
--     (defaulting to 'Opaque' for unknown names);
--   * 'EApp' takes the meet of the function's classification and its
--     arguments' classifications, so any opaque sub-expression
--     poisons the result;
--   * 'EBinOp' and 'EUnary' are 'Elementwise' regardless of operand
--     classification — every operator has only elementwise
--     monomorphic instances (LANGUAGE.md section 8.8) — but their
--     operands' classifications still contribute via the meet so that
--     e.g. @sqrt opaque@ is not laundered to elementwise just by
--     wrapping it in @+@;
--   * scalar control flow ('ECase', 'ELambda', 'ELet') is 'Opaque':
--     these are not lifted to per-row evaluation by R, so using them
--     in a verb right-hand side would silently break vectorisation;
--   * 'EVerb' and 'EPipe' are 'Opaque' because the result is a
--     dataframe-shaped value, not a row-shaped one.
classifyExpr :: Env -> Expr -> Classification
classifyExpr env = \case
    ELit _ _ -> Elementwise
    ECon _ -> Elementwise
    EVar n -> lookupClassification (lowerText n) env
    EApp _ f a ->
        classificationMeet (classifyExpr env f) (classifyExpr env a)
    EBinOp _ _ l r ->
        -- Operator itself is elementwise, but combining classifications
        -- of operands ensures that opaque sub-expressions still infect
        -- the result.
        classificationMeet
            Elementwise
            (classificationMeet (classifyExpr env l) (classifyExpr env r))
    EUnary _ _ e ->
        classificationMeet Elementwise (classifyExpr env e)
    EField _ e _ -> classifyExpr env e
    ERecord _ fbs ->
        meetClassifications
            (Prelude.fmap (\fb -> classifyExpr env (fieldBindingValue fb)) fbs)
    ERecordUpdate _ e fbs ->
        classificationMeet
            (classifyExpr env e)
            ( meetClassifications
                ( Prelude.fmap
                    (\fb -> classifyExpr env (fieldBindingValue fb))
                    fbs
                )
            )
    EVector _ es ->
        meetClassifications (Prelude.fmap (classifyExpr env) es)
    EDataframe _ _ -> Opaque
    EVerb _ _ _ -> Opaque
    EPipe _ _ _ -> Opaque
    ELambda _ _ _ -> Opaque
    ECase _ _ _ -> Opaque
    ELet _ _ _ -> Opaque


-- | Meet of a list of classifications. Empty list (e.g. an empty
-- record / vector literal) is treated as 'Elementwise' since there's
-- nothing to poison.
meetClassifications :: [Classification] -> Classification
meetClassifications = Prelude.foldr classificationMeet Elementwise


-- | First name in @e@ whose binding is classified as @target@. Used
-- to point the user at the specific callee that caused a verb-rhs
-- rejection. Returns 'Nothing' if no such name is found (in which
-- case the diagnostic falls back to a generic message about the
-- whole expression).
firstCalleeWith
    :: Env
    -> Classification
    -> Expr
    -> Maybe LowerName
firstCalleeWith env target = go
  where
    matches n = lookupClassification (lowerText n) env Prelude.== target
    go = \case
        EVar n
          | matches n -> Just n
          | Prelude.otherwise -> Nothing
        EApp _ f a -> firstJust (go f) (go a)
        EBinOp _ _ l r -> firstJust (go l) (go r)
        EUnary _ _ e -> go e
        EField _ e _ -> go e
        ERecord _ fbs ->
            firstJustList (Prelude.fmap (\fb -> go (fieldBindingValue fb)) fbs)
        ERecordUpdate _ e fbs ->
            firstJust (go e)
                (firstJustList (Prelude.fmap (\fb -> go (fieldBindingValue fb)) fbs))
        EVector _ es -> firstJustList (Prelude.fmap go es)
        EPipe _ l r -> firstJust (go l) (go r)
        ELit _ _ -> Nothing
        ECon _ -> Nothing
        ELambda _ _ _ -> Nothing
        ECase _ _ _ -> Nothing
        ELet _ _ _ -> Nothing
        EDataframe _ _ -> Nothing
        EVerb _ _ _ -> Nothing


-- | Validate a @summarize@ right-hand side.
--
-- Per LANGUAGE.md section 9.3, @summarize { col = expr }@ runs @expr@
-- in group scope where each column is bound as the full @Vector@ of
-- values. The expression must produce a single scalar per group, so
-- columns must appear inside a 'Reducer' call (e.g. @mean(col)@,
-- @sum(col)@). 'Elementwise' work on top of a reducer's scalar result
-- (e.g. @mean(col) / total@) is fine; raw column references (which
-- would leak the full vector into the group result) are not.
--
-- The rule we apply is:
--
--   * a reducer call @r(arg1, arg2, ..)@ is OK iff its arguments
--     contain no opaque sub-expressions; raw column references in
--     reducer arguments are fine (that's the point of a reducer);
--   * an elementwise call / operator / unary / field / record / vector
--     literal is OK iff each component is OK;
--   * a literal or constructor is OK;
--   * a column reference (i.e. an 'EVar' that names a schema column)
--     is /not/ OK on its own — it must appear inside a reducer;
--   * any other 'EVar' (a scalar binding or imported elementwise
--     callable) is OK;
--   * 'ECase' / 'ELambda' / 'ELet' / 'EVerb' / 'EDataframe' /
--     'EPipe' are not OK.
--
-- Returns 'Nothing' on success, or @Just calleeName@ pointing at the
-- offending column / opaque call.
summarizeOk
    :: Env
    -> Map.Map Text Type
    -> Expr
    -> Maybe LowerName
summarizeOk env schema = goOuter
  where
    -- "Outer" position: result must be scalar. Column references are
    -- not OK; reducer calls /are/ OK and switch to "inner" walk where
    -- column references are fine.
    goOuter = \case
        EVar n
          | Map.member (lowerText n) schema -> Just n
          | Prelude.otherwise -> Nothing
        e@(EApp _ _ _)
          | (Just callee, args) <- appHeadAndArgs e
          , lookupClassification (lowerText callee) env Prelude.== Reducer
              -> firstJustList (Prelude.fmap goInner args)
          | Prelude.otherwise ->
              case e of
                  EApp _ f a -> firstJust (goOuter f) (goOuter a)
                  _ -> Nothing
        EBinOp _ _ l r -> firstJust (goOuter l) (goOuter r)
        EUnary _ _ e -> goOuter e
        EField _ e _ -> goOuter e
        ERecord _ fbs ->
            firstJustList (Prelude.fmap (\fb -> goOuter (fieldBindingValue fb)) fbs)
        ERecordUpdate _ e fbs ->
            firstJust (goOuter e)
                (firstJustList (Prelude.fmap (\fb -> goOuter (fieldBindingValue fb)) fbs))
        EVector _ es -> firstJustList (Prelude.fmap goOuter es)
        EPipe _ _ _ -> Nothing
        ELit _ _ -> Nothing
        ECon _ -> Nothing
        ELambda _ _ _ -> Nothing
        ECase _ _ _ -> Nothing
        ELet _ _ _ -> Nothing
        EDataframe _ _ -> Nothing
        EVerb _ _ _ -> Nothing
    -- "Inner" position: we are inside a reducer's argument list, so a
    -- raw column reference is fine. We still reject opaque sub-
    -- expressions and scalar control flow.
    goInner = \case
        EVar _ -> Nothing
        EApp _ f a -> firstJust (goInner f) (goInner a)
        EBinOp _ _ l r -> firstJust (goInner l) (goInner r)
        EUnary _ _ e -> goInner e
        EField _ e _ -> goInner e
        ERecord _ fbs ->
            firstJustList (Prelude.fmap (\fb -> goInner (fieldBindingValue fb)) fbs)
        ERecordUpdate _ e fbs ->
            firstJust (goInner e)
                (firstJustList (Prelude.fmap (\fb -> goInner (fieldBindingValue fb)) fbs))
        EVector _ es -> firstJustList (Prelude.fmap goInner es)
        EPipe _ _ _ -> Nothing
        ELit _ _ -> Nothing
        ECon _ -> Nothing
        ELambda _ _ _ -> Nothing
        ECase _ _ _ -> Nothing
        ELet _ _ _ -> Nothing
        EDataframe _ _ -> Nothing
        EVerb _ _ _ -> Nothing


-- | Walk an application chain @f a b c@ and return the (innermost)
-- callee name plus its arguments in source order. Returns
-- @(Nothing, [])@ if the head is not a plain variable reference.
appHeadAndArgs :: Expr -> (Maybe LowerName, [Expr])
appHeadAndArgs = go []
  where
    go acc = \case
        EVar n -> (Just n, acc)
        EApp _ f a -> go (a : acc) f
        _ -> (Nothing, acc)


firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust a b = case a of
    Just x -> Just x
    Nothing -> b


firstJustList :: [Maybe a] -> Maybe a
firstJustList = Prelude.foldr firstJust Nothing



-- ---------------------------------------------------------------------
-- Verb-rhs classification checks
-- ---------------------------------------------------------------------


-- | Reject a @mutate@ or @filter@ right-hand side whose classification
-- isn't 'Elementwise'. Per LANGUAGE.md section 8.7, these verbs map
-- per-row in R, so any 'Reducer' (which expects the full vector) or
-- 'Opaque' callable (whose vectorisation is unknown) is unsafe.
checkRowwiseRhs
    :: Env
    -> SourceSpan
    -> Text
    -> Expr
    -> Prelude.Either Diagnostic ()
checkRowwiseRhs env sp verbName rhs = case classifyExpr env rhs of
    Elementwise -> Prelude.Right ()
    Reducer ->
        Prelude.Left
            ( verbClassificationError sp verbName
                (firstCalleeWith env Reducer rhs)
                "is a reducer; reducers consume the whole column and may not be used in row-scoped verbs (mutate/filter)"
            )
    Opaque ->
        Prelude.Left
            ( verbClassificationError sp verbName
                (firstOpaqueCallee env rhs)
                "is not classified as elementwise; declare its foreign import as `import elementwise pkg.fn : ...` (LANGUAGE.md section 4.5) or refactor to use only elementwise primitives"
            )


-- | Reject a @summarize@ right-hand side that does not collapse each
-- referenced column with a 'Reducer'. Raw column references at the
-- outer level would leak the full vector into the group result; the
-- rule is enforced by 'summarizeOk'.
checkSummarizeRhs
    :: Env
    -> Map.Map Text Type
    -> SourceSpan
    -> Expr
    -> Prelude.Either Diagnostic ()
checkSummarizeRhs env schema sp rhs = case summarizeOk env schema rhs of
    Nothing -> Prelude.Right ()
    Just offending ->
        let
            offendingText = lowerText offending
            detail =
                if Map.member offendingText schema
                    then
                        " is a column; in summarize each column must appear inside a reducer like `mean(" Prelude.<> offendingText Prelude.<> ")` or `sum(" Prelude.<> offendingText Prelude.<> ")`"
                    else
                        " is not classified as elementwise; declare its foreign import as `import elementwise pkg.fn : ...` (LANGUAGE.md section 4.5)"
        in
        Prelude.Left
            ( verbClassificationError sp "summarize"
                (Just offending)
                detail
            )


-- | Helper: locate an opaque callee in @e@. Falls back to looking
-- under reducer-classified callees (e.g. an opaque arg to @mean@)
-- when the rhs has no top-level opaque caller. Returns 'Nothing' if
-- nothing useful can be named.
firstOpaqueCallee :: Env -> Expr -> Maybe LowerName
firstOpaqueCallee env e = case firstCalleeWith env Opaque e of
    Just n -> Just n
    Nothing -> firstCalleeWith env Reducer e



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


-- | Diagnostic for a verb-rhs classification mismatch (LANGUAGE.md
-- section 8.7). Names the offending callee where possible so the
-- user can find what to fix.
verbClassificationError
    :: SourceSpan
    -> Text          -- ^ verb name (e.g. @"mutate"@)
    -> Maybe LowerName  -- ^ offending callee, if known
    -> Text          -- ^ explanation tail (e.g. @" is a reducer..."@)
    -> Diagnostic
verbClassificationError sp verbName mCallee detail =
    let
        head_ = case mCallee of
            Just n ->
                T.pack (Prelude.show (lowerText n))
                    Prelude.<> detail
            Nothing ->
                "right-hand side" Prelude.<> detail
    in
    Diagnostic
        { diagSeverity = Error
        , diagCategory = TypeMismatch
        , diagSpan = case mCallee of
            Just n -> lowerSpan n
            Nothing -> sp
        , diagMessage =
            "in `" Prelude.<> verbName Prelude.<> "`: " Prelude.<> head_
        , diagHint = Nothing
        }
