module Test.TypeTests (suite) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import Quone.Parse.Desugar (desugarSource)
import Quone.Type.Infer (TypedProgram (..), inferProgram)
import Quone.Type.Types (showScheme)
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..), (===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "type"
        [ Harness.test "type/infers_function" <|
            inferBinding "add a b <- a + b" "add" "Double -> Double -> Double"
        , Harness.test "type/rejects_mixed_numeric" <|
            expectTypeFail "x <- 1L + 2"
        , Harness.test "type/explicit_conversion" <|
            inferBinding "x <- to_double 1L + 2" "x" "Double"
        , Harness.test "type/record_exact_field_access" <|
            inferBinding
                ( T.unlines
                    [ "student <- { name = \"Ada\", score = 95 }"
                    , "x <- student.score"
                    ]
                )
                "x"
                "Double"
        , Harness.test "type/maybe_case_result" <|
            inferBinding
                ( T.unlines
                    [ "maybe <- Just 1"
                    , "label <- case maybe of"
                    , "    Just n -> \"known\""
                    , "    Nothing -> \"missing\""
                    ]
                )
                "label"
                "Character"
        , Harness.test "type/displays_dataframe_columns_as_vectors" <|
            inferBinding
                ( T.unlines
                    [ "mtcars_demo <-"
                    , "    mtcars"
                    , "        |> filter (mpg > mean mpg)"
                    , "        |> group_by { cyl }"
                    , "        |> summarize { n_cars = count, avg_mpg = mean mpg }"
                    ]
                )
                "mtcars_demo"
                "dataframe { avg_mpg : Vector Double, cyl : Vector Integer, n_cars : Vector Integer }"
        ]


inferBinding :: Text -> Text -> Text -> Prelude.IO TestResult
inferBinding src name expected =
    case desugarSource src >>= inferProgram of
        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
        Prelude.Right typed ->
            case Map.lookup name (typedBindings typed) of
                Just scheme -> Prelude.pure (showScheme scheme === expected)
                Nothing -> Prelude.pure (Fail ("missing binding: " ++ name))


expectTypeFail :: Text -> Prelude.IO TestResult
expectTypeFail src =
    case desugarSource src >>= inferProgram of
        Prelude.Left _ -> Prelude.pure Pass
        Prelude.Right _ -> Prelude.pure (Fail "expected type failure")

