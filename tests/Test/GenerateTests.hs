module Test.GenerateTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Cli.Commands (compileScript)
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..), (===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "generate"
        [ Harness.test "generate/double_literals_are_idiomatic" <|
            expectGenerated "x <- 1" "x <- 1"
        , Harness.test "generate/record_literal" <|
            expectGenerated
                "student <- { name = \"Ada\", score = 95 }"
                "student <- list(name = \"Ada\", score = 95)"
        , Harness.test "generate/foreign_import_via" <|
            expectGenerated
                ( T.unlines
                    [ "import elementwise stringr.str_detect as str_detect : Character -> Character -> Logical via \"stringr::str_detect($2, $1)\""
                    , "x <- str_detect \"Ada\" \"Ada Lovelace\""
                    ]
                )
                "x <- stringr::str_detect(\"Ada Lovelace\", \"Ada\")"
        , Harness.test "generate/dataframe_pipeline" <|
            expectGeneratedContains
                "passing <- dataframe { name = [\"Ada\"], score = [90] } |> filter (score > 80)"
                "dplyr::filter(score > 80)"
        , Harness.test "generate/generic_pipe_rhs_function_names_are_calls" <|
            expectGeneratedContains
                ( T.unlines
                    [ "rmse : Vector Double -> Vector Double -> Double"
                    , "rmse actuals predictions <-"
                    , "    (predictions - actuals) ^ 2"
                    , "        |> mean"
                    , "        |> sqrt"
                    , ""
                    , "actuals : Vector Double"
                    , "actuals <- [2.1, 3.4, 4.0]"
                    , ""
                    , "predictions : Vector Double"
                    , "predictions <- [2.0, 3.7, 3.8]"
                    , ""
                    , "error <- rmse actuals predictions"
                    ]
                )
                (T.unlines ["mean() |>", "    sqrt()"])
        , Harness.test "generate/formats_dataframe_pipeline_readably" <|
            expectGenerated mtcarsSource mtcarsExpectedR
        , Harness.test "generate/builtin_mtcars_dataset" <|
            expectGeneratedContains
                ( T.unlines
                    [ "demo <- mtcars |> group_by { cyl } |> summarize { n_cars = count }"
                    ]
                )
                "demo <- datasets::mtcars |>"
        , Harness.test "generate/builtin_example_datasets" <|
            expectGeneratedContains
                ( T.unlines
                    [ "iris_demo <- iris"
                    , "air_demo <- airquality"
                    , "plant_demo <- plant_growth"
                    , "tooth_demo <- tooth_growth"
                    ]
                )
                "tooth_demo <- dplyr::mutate(datasets::ToothGrowth"
        ]


expectGenerated :: Text -> Text -> Prelude.IO TestResult
expectGenerated src expected =
    case compileScript "<test>" src of
        Prelude.Right (_, r) -> Prelude.pure (r === expected)
        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))


expectGeneratedContains :: Text -> Text -> Prelude.IO TestResult
expectGeneratedContains src expected =
    case compileScript "<test>" src of
        Prelude.Right (_, r) ->
            if expected `T.isInfixOf` r then
                Prelude.pure Pass
            else
                Prelude.pure (Fail ("expected generated R to contain: " ++ expected ++ "\nactual:\n" ++ r))
        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))


mtcarsSource :: Text
mtcarsSource =
    T.unlines
        [ "type alias Cars <- dataframe { model : Vector Character, mpg : Vector Double, cyl : Vector Integer, hp : Vector Double, wt : Vector Double }"
        , ""
        , "mtcars_demo : Cars -> dataframe { cyl : Vector Integer, n_cars : Vector Integer, avg_mpg : Vector Double, avg_hp : Vector Double }"
        , "mtcars_demo cars <- cars |> filter (mpg > mean mpg) |> mutate { power_to_weight = hp / wt } |> group_by { cyl } |> summarize { n_cars = count, avg_mpg = mean mpg, avg_hp = mean hp } |> arrange (desc avg_mpg)"
        ]


mtcarsExpectedR :: Text
mtcarsExpectedR =
    T.dropEnd 1
        ( T.unlines
            [ "mtcars_demo <- function(cars) {"
            , "  cars |>"
            , "    dplyr::filter(mpg > mean(mpg)) |>"
            , "    dplyr::mutate(power_to_weight = hp / wt) |>"
            , "    dplyr::group_by(cyl = cyl) |>"
            , "    dplyr::summarize("
            , "      n_cars = dplyr::n(),"
            , "      avg_mpg = mean(mpg),"
            , "      avg_hp = mean(hp)"
            , "    ) |>"
            , "    dplyr::arrange(dplyr::desc(avg_mpg))"
            , "}"
            ]
        )

