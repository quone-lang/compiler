{-| Dataframe verb typing tests.

Covers stage 7: each of the six normatively-typed verbs from
LANGUAGE.md section 8.7 plus the bare-column-name sugar from
section 9.2.

-}
module Test.VerbTests (suite) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Parse.Desugar (desugarSource)
import Quone.Type.Infer (inferProgram, typedBindings)
import Quone.Type.Types
    ( Scheme (..)
    , Type (..)
    , primDouble
    , primInteger
    , primLogical
    , showType
    )
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , assert
    , assertLeft
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "verb"
        ( selectTests
            ++ filterTests
            ++ mutateTests
            ++ summarizeTests
            ++ groupByTests
            ++ arrangeTests
        )



-- ---------------------------------------------------------------------
-- select
-- ---------------------------------------------------------------------


selectTests :: [Test]
selectTests =
    [ test "verb/select_keeps_named_columns_section_8_7" <|
        case infer (studentsDf ++ "y <- students |> select { name }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe fs ->
                        Prelude.pure
                            ( assert
                                (Map.keysSet fs Prelude.== Map.keysSet (Map.singleton "name" ()))
                                ("expected schema {name}; got " Prelude.<> showType (TyDataframe fs))
                            )
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    , test "verb/select_unknown_column_rejected_section_8_7" <|
        Prelude.pure
            ( assertLeft
                ( inferRaw
                    (studentsDf ++ "y <- students |> select { missing }")
                )
            )
    ]



-- ---------------------------------------------------------------------
-- filter
-- ---------------------------------------------------------------------


filterTests :: [Test]
filterTests =
    [ test "verb/filter_predicate_uses_row_scope_section_9_2" <|
        case infer (studentsDf ++ "y <- students |> filter (score > 70.0)") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    , test "verb/filter_predicate_must_be_logical_section_8_7" <|
        Prelude.pure
            ( assertLeft
                ( inferRaw
                    (studentsDf ++ "y <- students |> filter (score)")
                )
            )
    , test "verb/filter_unknown_column_rejected_section_8_7" <|
        Prelude.pure
            ( assertLeft
                ( inferRaw
                    (studentsDf ++ "y <- students |> filter (notACol > 70.0)")
                )
            )
    ]



-- ---------------------------------------------------------------------
-- mutate
-- ---------------------------------------------------------------------


mutateTests :: [Test]
mutateTests =
    [ test "verb/mutate_extends_schema_section_8_7" <|
        case infer
            (studentsDf ++ "y <- students |> mutate { pct = score / 100.0 }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe fs ->
                        Prelude.pure
                            ( assert
                                (Map.member "pct" fs Prelude.&& Map.member "score" fs)
                                ("expected pct + original cols; got " Prelude.<> showType (TyDataframe fs))
                            )
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    ]



-- ---------------------------------------------------------------------
-- summarize
-- ---------------------------------------------------------------------


summarizeTests :: [Test]
summarizeTests =
    [ test "verb/summarize_replaces_schema_section_8_7" <|
        case infer
            (studentsDf ++ "y <- students |> summarize { avg = mean score }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe fs ->
                        Prelude.pure
                            ( assert
                                (Map.keysSet fs Prelude.== Map.keysSet (Map.singleton "avg" ()))
                                ("expected only {avg}; got " Prelude.<> showType (TyDataframe fs))
                            )
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    ]



-- ---------------------------------------------------------------------
-- group_by
-- ---------------------------------------------------------------------


groupByTests :: [Test]
groupByTests =
    [ test "verb/group_by_keeps_schema_section_8_7" <|
        case infer (studentsDf ++ "y <- students |> group_by { dept }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    , test "verb/group_by_unknown_column_rejected_section_8_7" <|
        Prelude.pure
            ( assertLeft
                ( inferRaw
                    (studentsDf ++ "y <- students |> group_by { missing }")
                )
            )
    ]



-- ---------------------------------------------------------------------
-- arrange
-- ---------------------------------------------------------------------


arrangeTests :: [Test]
arrangeTests =
    [ test "verb/arrange_with_record_keeps_schema_section_8_7" <|
        case infer (studentsDf ++ "y <- students |> arrange { score }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    , test "verb/arrange_with_desc_modifier_section_8_7" <|
        case infer (studentsDf ++ "y <- students |> arrange (desc score)") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    ]



-- ---------------------------------------------------------------------
-- Fixtures and helpers
-- ---------------------------------------------------------------------


studentsDf :: Text
studentsDf =
    T.unlines
        [ "students <-"
        , "    dataframe { name = [\"Alice\"], score = [92.0], dept = [\"math\"] }"
        ]


infer :: Text -> Prelude.Either Text (Map.Map Text Scheme)
infer src =
    case desugarSource src of
        Prelude.Left d -> Prelude.Left (T.pack (Prelude.show d))
        Prelude.Right p ->
            case inferProgram p of
                Prelude.Left d -> Prelude.Left (T.pack (Prelude.show d))
                Prelude.Right tp -> Prelude.Right (typedBindings tp)


inferRaw :: Text -> Prelude.Either Text (Map.Map Text Scheme)
inferRaw = infer
