module Test.FormatTests (suite) where

import qualified Data.Text as T
import NriPrelude
import qualified Quone.Format.Format as Format
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..), (===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "format"
        [ Harness.test "format/canonicalizes_whitespace" <|
            Prelude.pure
                ( assertFormatted
                    "x<-1+2"
                    "x <- 1 + 2\n"
                )
        , Harness.test "format/is_formatted_matches_elm_format_check" <|
            Prelude.pure
                ( if Format.isFormatted "<test>" "x<-1+2" then
                    Fail "unformatted source was reported as formatted"
                  else if Prelude.not (Format.isFormatted "<test>" "x <- 1 + 2\n") then
                    Fail "canonical source was reported as unformatted"
                  else
                    Pass
                )
        , Harness.test "format/preserves_numeric_literal_spelling" <|
            Prelude.pure
                ( assertFormatted
                    "x <- 1\ny <- 1L"
                    "x <- 1\n\ny <- 1L\n"
                )
        , Harness.test "format/preserves_top_of_file_comment" <|
            Prelude.pure
                ( assertFormatted
                    "# keep me\nx <- 1"
                    "# keep me\nx <- 1\n"
                )
        , Harness.test "format/preserves_top_of_file_doc_comment" <|
            Prelude.pure
                ( assertFormatted
                    "#' Hello\n#'\nx <- 1"
                    "#' Hello\n#'\nx <- 1\n"
                )
        , Harness.test "format/preserves_between_decl_comment" <|
            Prelude.pure
                ( assertFormatted
                    (T.unlines ["x<-1", "# explain y", "y<-2"])
                    (T.unlines ["x <- 1", "", "# explain y", "y <- 2"])
                )
        , Harness.test "format/short_record_literal_stays_inline" <|
            Prelude.pure
                ( assertFormatted
                    "row<-{a=1,b=2}"
                    "row <- { a = 1, b = 2 }\n"
                )
        , Harness.test "format/function_body_stays_below_bind" <|
            Prelude.pure
                ( assertFormatted
                    "add x y <- x + y"
                    (T.unlines ["add x y <-", "    x + y"])
                )
        , Harness.test "format/preserves_foreign_import_alias_and_via" <|
            Prelude.pure
                ( assertFormatted
                    "import pkg.fn as f : a -> a via \"pkg::fn($1)\""
                    "import pkg.fn as f : a -> a via \"pkg::fn($1)\"\n"
                )
        , Harness.test "format/join_output_parses_without_on_keyword" <|
            Prelude.pure
                ( assertFormatted
                    "joined <- students |> left_join schools { school_id = id }"
                    ( T.unlines
                        [ "joined <-"
                        , "    students"
                        , "        |> left_join schools { school_id = id }"
                        ]
                    )
                )
        , Harness.test "format/mtcars_pipeline_matches_canonical_output" <|
            Prelude.pure (assertFormatted mtcarsSource mtcarsExpected)
        , Harness.test "format/reindents_multiline_pipeline_body" <|
            Prelude.pure
                ( assertFormatted
                    ( T.unlines
                        [ "demo xs <-"
                        , "    xs"
                        , "    |> filter (score > 0)"
                        , "    |> arrange { desc score }"
                        ]
                    )
                    ( T.unlines
                        [ "demo xs <-"
                        , "    xs"
                        , "        |> filter (score > 0)"
                        , "        |> arrange { desc score }"
                        ]
                    )
                )
        , Harness.test "format/custom_type_output_parses" <|
            Prelude.pure
                ( assertFormatted
                    "type A <- First | Second"
                    (T.unlines ["type A", "    <- First", "    | Second"])
                )
        ]


assertFormatted :: Text -> Text -> TestResult
assertFormatted input expected =
    case Format.format "<test>" input of
        Prelude.Left d -> Fail (T.pack (Prelude.show d))
        Prelude.Right once ->
            case Format.format "<test>" once of
                Prelude.Left d -> Fail (T.pack (Prelude.show d))
                Prelude.Right twice ->
                    if once Prelude./= expected then
                        once === expected
                    else if twice Prelude./= once then
                        Fail ("not idempotent:\n" ++ once)
                    else if Prelude.any (\line -> T.length line Prelude.> 80) (T.lines once) then
                        Fail ("line over 80 columns:\n" ++ once)
                    else
                        Pass


mtcarsSource :: Text
mtcarsSource =
    T.unlines
        [ "type alias Cars <- dataframe { model : Vector Character, mpg : Vector Double, cyl : Vector Integer, hp : Vector Double, wt : Vector Double }"
        , ""
        , "mtcars_demo : Cars -> dataframe { cyl : Vector Integer, n_cars : Vector Integer, avg_mpg : Vector Double, avg_hp : Vector Double }"
        , "mtcars_demo cars <- cars |> filter (mpg > mean mpg) |> mutate { power_to_weight = hp / wt } |> group_by { cyl } |> summarize { n_cars = count, avg_mpg = mean mpg, avg_hp = mean hp } |> arrange (desc avg_mpg)"
        ]


mtcarsExpected :: Text
mtcarsExpected =
    T.unlines
        [ "type alias Cars <-"
        , "    dataframe"
        , "        { model : Vector Character"
        , "        , mpg : Vector Double"
        , "        , cyl : Vector Integer"
        , "        , hp : Vector Double"
        , "        , wt : Vector Double"
        , "        }"
        , ""
        , "mtcars_demo :"
        , "    Cars ->"
        , "    dataframe"
        , "        { cyl : Vector Integer"
        , "        , n_cars : Vector Integer"
        , "        , avg_mpg : Vector Double"
        , "        , avg_hp : Vector Double"
        , "        }"
        , "mtcars_demo cars <-"
        , "    cars"
        , "        |> filter (mpg > mean mpg)"
        , "        |> mutate { power_to_weight = hp / wt }"
        , "        |> group_by { cyl }"
        , "        |> summarize"
        , "            { n_cars = count"
        , "            , avg_mpg = mean mpg"
        , "            , avg_hp = mean hp"
        , "            }"
        , "        |> arrange { desc avg_mpg }"
        ]

