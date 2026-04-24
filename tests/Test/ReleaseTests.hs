module Test.ReleaseTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Cli.Commands (compileScript)
import qualified Quone.Format.Format as Format
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..), (===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "initial-release"
        [ Harness.test "release/checks_simple_binding" <|
            expectOk "answer <- 42"
        , Harness.test "release/checks_function_definition" <|
            expectOk
                ( T.unlines
                    [ "add : Double -> Double -> Double"
                    , "add x y <- x + y"
                    , "total <- add 1 2"
                    ]
                )
        , Harness.test "release/checks_lambda_and_map" <|
            expectOk
                ( T.unlines
                    [ "xs : Vector Double"
                    , "xs <- [1, 2, 3]"
                    , "ys <- map (\\x -> x + 1) xs"
                    ]
                )
        , Harness.test "release/checks_let_expression" <|
            expectOk "x <- let y <- 1 in y + 2"
        , Harness.test "release/checks_record_field_access" <|
            expectOk
                ( T.unlines
                    [ "student <- { name = \"Ada\", score = 95 }"
                    , "name <- student.name"
                    ]
                )
        , Harness.test "release/checks_record_update" <|
            expectOk
                ( T.unlines
                    [ "student <- { name = \"Ada\", score = 95 }"
                    , "updated <- { student | score = 100 }"
                    ]
                )
        , Harness.test "release/checks_custom_type_case" <|
            expectOk
                ( T.unlines
                    [ "maybe <- Just 1"
                    , "label <- case maybe of"
                    , "    Just n -> \"known\""
                    , "    Nothing -> \"missing\""
                    ]
                )
        , Harness.test "release/rejects_non_exhaustive_case" <|
            expectFail
                ( T.unlines
                    [ "maybe <- Just 1"
                    , "label <- case maybe of"
                    , "    Just n -> \"known\""
                    ]
                )
        , Harness.test "release/checks_if_expression" <|
            expectOk "x <- if True then 1 else 2"
        , Harness.test "release/checks_empty_vector_with_annotation" <|
            expectOk
                ( T.unlines
                    [ "xs : Vector Double"
                    , "xs <- []"
                    ]
                )
        , Harness.test "release/rejects_heterogeneous_vector" <|
            expectFail "xs <- [1, \"a\"]"
        , Harness.test "release/rejects_mixed_numeric_without_conversion" <|
            expectFail "x <- 1L + 2"
        , Harness.test "release/checks_explicit_numeric_conversion" <|
            expectOk "x <- to_double 1L + 2"
        , Harness.test "release/checks_dataframe_pipeline" <|
            expectOk
                ( T.unlines
                    [ "students <- dataframe { name = [\"Ada\", \"Bob\"], score = [90, 70] }"
                    , "passing <- students |> filter (score > 80) |> mutate { pct = score / 100 } |> select { name, pct }"
                    ]
                )
        , Harness.test "release/checks_grouped_summary" <|
            expectOk
                ( T.unlines
                    [ "students <- dataframe { class = [\"a\", \"a\", \"b\"], score = [90, 70, 80] }"
                    , "summary <- students |> group_by { class } |> summarize { avg = mean score } |> arrange (desc avg)"
                    ]
                )
        , Harness.test "release/checks_inner_join" <|
            expectOk
                ( T.unlines
                    [ "students <- dataframe { school_id = [\"s1\"], name = [\"Ada\"] }"
                    , "schools <- dataframe { id = [\"s1\"], region = [\"north\"] }"
                    , "joined <- students |> inner_join schools { school_id = id }"
                    ]
                )
        , Harness.test "release/rejects_deferred_verb_keyword" <|
            expectFail "x <- students |> full_join other { id = id }"
        , Harness.test "release/checks_foreign_import_via" <|
            expectOk
                ( T.unlines
                    [ "import elementwise stringr.str_detect as str_detect : Character -> Character -> Logical via \"stringr::str_detect($2, $1)\""
                    , "x <- str_detect \"Ada\" \"Ada Lovelace\""
                    ]
                )
        , Harness.test "release/generates_readable_r" <|
            expectGenerated "x <- sqrt 9" "x <- sqrt(9)"
        , Harness.test "release/generates_record_r" <|
            expectGenerated "student <- { name = \"Ada\", score = 95 }" "student <- list(name = \"Ada\", score = 95)"
        , Harness.test "release/formatter_is_idempotent" <|
            case Format.format "<test>" "x<-1+2" of
                Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
                Prelude.Right out ->
                    case Format.format "<test>" out of
                        Prelude.Right out2 -> Prelude.pure (out2 === out)
                        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
        ]


expectOk :: Text -> Prelude.IO TestResult
expectOk src =
    case compileScript "<test>" src of
        Prelude.Right _ -> Prelude.pure Pass
        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))


expectFail :: Text -> Prelude.IO TestResult
expectFail src =
    case compileScript "<test>" src of
        Prelude.Left _ -> Prelude.pure Pass
        Prelude.Right _ -> Prelude.pure (Fail "expected failure")


expectGenerated :: Text -> Text -> Prelude.IO TestResult
expectGenerated src expected =
    case compileScript "<test>" src of
        Prelude.Right (_, r) -> Prelude.pure (r === expected)
        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))

