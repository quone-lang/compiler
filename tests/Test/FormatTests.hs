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
                    (T.unlines ["x <-", "    1 + 2"])
                )
        , Harness.test "format/is_formatted_matches_elm_format_check" <|
            Prelude.pure
                ( if Format.isFormatted "<test>" "x<-1+2" then
                    Fail "unformatted source was reported as formatted"
                  else if Prelude.not (Format.isFormatted "<test>" (T.unlines ["x <-", "    1 + 2"])) then
                    Fail "canonical source was reported as unformatted"
                  else
                    Pass
                )
        , Harness.test "format/preserves_numeric_literal_spelling" <|
            Prelude.pure
                ( assertFormatted
                    "x <- 1\ny <- 1L"
                    (T.unlines ["x <-", "    1", "", "y <-", "    1L"])
                )
        , Harness.test "format/preserves_top_of_file_comment" <|
            Prelude.pure
                ( assertFormatted
                    "# keep me\nx <- 1"
                    (T.unlines ["# keep me", "x <-", "    1"])
                )
        , Harness.test "format/preserves_top_of_file_doc_comment" <|
            Prelude.pure
                ( assertFormatted
                    "#' Hello\n#'\nx <- 1"
                    (T.unlines ["#' Hello", "#'", "x <-", "    1"])
                )
        , Harness.test "format/preserves_between_decl_comment" <|
            Prelude.pure
                ( assertFormatted
                    (T.unlines ["x<-1", "# explain y", "y<-2"])
                    (T.unlines ["x <-", "    1", "", "# explain y", "y <-", "    2"])
                )
        , Harness.test "format/final_bare_expression" <|
            Prelude.pure
                ( assertFormatted
                    (T.unlines ["x<-1", "x+1"])
                    (T.unlines ["x <-", "    1", "", "x + 1"])
                )
        , Harness.test "format/short_record_literal_stays_inline" <|
            Prelude.pure
                ( assertFormatted
                    "row<-{a=1,b=2}"
                    (T.unlines ["row <-", "    { a = 1, b = 2 }"])
                )
        , Harness.test "format/multiline_record_literal_breaks_fields" <|
            Prelude.pure
                ( assertFormatted
                    (T.unlines ["row <- { a = 1,", "b = 2, c = 3 }"])
                    ( T.unlines
                        [ "row <-"
                        , "    { a = 1"
                        , "    , b = 2"
                        , "    , c = 3"
                        , "    }"
                        ]
                    )
                )
        , Harness.test "format/wide_record_literal_breaks_fields" <|
            Prelude.pure
                ( assertFormatted
                    "row <- { alpha_value = 1, beta_value = 2, gamma_value = 3, delta_value = 4, epsilon_value = 5 }"
                    ( T.unlines
                        [ "row <-"
                        , "    { alpha_value = 1"
                        , "    , beta_value = 2"
                        , "    , gamma_value = 3"
                        , "    , delta_value = 4"
                        , "    , epsilon_value = 5"
                        , "    }"
                        ]
                    )
                )
        , Harness.test "format/multiline_record_type_breaks_fields" <|
            Prelude.pure
                ( assertFormatted
                    ( T.unlines
                        [ "row : { a : Double,"
                        , "b : Double, c : Double }"
                        , "row <- { a = 1, b = 2, c = 3 }"
                        ]
                    )
                    ( T.unlines
                        [ "row :"
                        , "    { a : Double"
                        , "    , b : Double"
                        , "    , c : Double"
                        , "    }"
                        , "row <-"
                        , "    { a = 1, b = 2, c = 3 }"
                        ]
                    )
                )
        , Harness.test "format/function_body_stays_below_bind" <|
            Prelude.pure
                ( assertFormatted
                    "add x y <- x + y"
                    (T.unlines ["add x y <-", "    x + y"])
                )
        , Harness.test "format/short_if_stays_inline" <|
            Prelude.pure
                ( assertFormatted
                    "b <- if z then 1 else 2"
                    (T.unlines ["b <-", "    if z then 1 else 2"])
                )
        , Harness.test "format/multiline_if_breaks_branches" <|
            Prelude.pure
                ( assertFormatted
                    ( T.unlines
                        [ "b <-"
                        , "    if z then "
                        , "        1 else 2"
                        ]
                    )
                    ( T.unlines
                        [ "b <-"
                        , "    if z then"
                        , "        1"
                        , "    else"
                        , "        2"
                        ]
                    )
                )
        , Harness.test "format/wide_if_breaks_branches" <|
            Prelude.pure
                ( assertFormatted
                    "b <- if z then this_branch_name_is_long_enough_to_force_a_break else that_branch_name_is_also_long"
                    ( T.unlines
                        [ "b <-"
                        , "    if z then"
                        , "        this_branch_name_is_long_enough_to_force_a_break"
                        , "    else"
                        , "        that_branch_name_is_also_long"
                        ]
                    )
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
        , "            { n_cars = count, avg_mpg = mean mpg, avg_hp = mean hp }"
        , "        |> arrange { desc avg_mpg }"
        ]

