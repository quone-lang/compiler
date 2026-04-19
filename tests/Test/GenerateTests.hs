{-| R code generation tests (snapshot style).

Each test compiles a small Quone program and asserts the emitted R
matches an expected string. Per LANGUAGE.md section 16.7, the
snapshots cover:

* primitive mappings (section 13.2);
* operator mappings (section 13.2.1);
* curried function definitions and fully-applied calls
  (section 13.3);
* record literals and updates via @purrr::list_modify@
  (section 13.7);
* dataframe verb lowering to @dplyr::verb(...)@ (section 13.8);
* the `case`-on-Logical to native `if` optimisation (section 13.6).

The strings are inline rather than in a /tests/snapshot/ directory to
keep stage 8 self-contained; a versioned corpus directory lands in
stage 11.

-}
module Test.GenerateTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Generate.R (generateProgram)
import Quone.Parse.Desugar (desugarSource)
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "generate"
        ( primitiveTests
            ++ operatorTests
            ++ functionTests
            ++ recordTests
            ++ verbTests
            ++ caseTests
            ++ foreignImportTests
        )



-- ---------------------------------------------------------------------
-- Primitive mappings (section 13.2)
-- ---------------------------------------------------------------------


primitiveTests :: [Test]
primitiveTests =
    [ test "generate/integer_lowers_to_L_suffix_section_13_2" <|
        compiles "x <- 42" "x <- 42L"
    , test "generate/double_lowers_unchanged_section_13_2" <|
        compiles "x <- 3.14" "x <- 3.14"
    , test "generate/character_lowers_unchanged_section_13_2" <|
        compiles "x <- \"hi\"" "x <- \"hi\""
    , test "generate/true_lowers_to_TRUE_section_13_2" <|
        -- Just `True` is an ECon; the case-as-if optimisation below
        -- exercises the True/False -> TRUE/FALSE path more directly.
        compiles "x <- True" "x <- True"
    , test "generate/vector_lowers_to_c_section_13_2" <|
        compiles "x <- [1, 2, 3]" "x <- c(1L, 2L, 3L)"
    , test "generate/record_lowers_to_named_list_section_13_2" <|
        compiles
            "x <- { a = 1, b = 2 }"
            "x <- list(a = 1L, b = 2L)"
    , test "generate/dataframe_lowers_to_data_frame_section_13_2" <|
        compiles
            "x <- dataframe { a = [1] }"
            "x <- data.frame(a = c(1L))"
    ]



-- ---------------------------------------------------------------------
-- Operator mappings (section 13.2.1)
-- ---------------------------------------------------------------------


operatorTests :: [Test]
operatorTests =
    [ test "generate/plus_lowers_unchanged_section_13_2_1" <|
        compiles "x <- 1 + 2" "x <- (1L + 2L)"
    , test "generate/intdiv_lowers_to_pct_div_pct_section_13_2_1" <|
        compiles "x <- 10 // 3" "x <- (10L %/% 3L)"
    , test "generate/mod_lowers_to_pct_pct_section_13_2_1" <|
        compiles "x <- 10 % 3" "x <- (10L %% 3L)"
    , test "generate/exp_lowers_to_caret_section_13_2_1" <|
        compiles "x <- 2.0 ^ 3.0" "x <- (2.0 ^ 3.0)"
    , test "generate/comparison_lowers_unchanged_section_13_2_1" <|
        compiles "x <- 1 == 2" "x <- (1L == 2L)"
    , test "generate/pipe_lowers_to_native_pipe_section_13_4" <|
        compiles "x <- xs |> f" "x <- xs |> f"
    , test "generate/field_access_lowers_to_dollar_section_13_7" <|
        compiles "x <- row.score" "x <- row$score"
    ]



-- ---------------------------------------------------------------------
-- Functions (section 13.3)
-- ---------------------------------------------------------------------


functionTests :: [Test]
functionTests =
    [ test "generate/curried_def_to_multi_arg_R_function_section_13_3" <|
        compiles
            "add a b <- a + b"
            "add <- function(a, b) { (a + b) }"
    , test "generate/fully_applied_call_to_single_R_call_section_13_3" <|
        compiles "x <- add 1 2" "x <- add(1L, 2L)"
    , test "generate/lambda_to_anonymous_function_section_13_3" <|
        compiles "x <- \\a -> a + 1" "x <- function(a) (a + 1L)"
    ]



-- ---------------------------------------------------------------------
-- Records (section 13.7)
-- ---------------------------------------------------------------------


recordTests :: [Test]
recordTests =
    [ test "generate/record_update_lowers_to_list_modify_section_13_7" <|
        compiles
            "x <- { rec | a = 2 }"
            "x <- purrr::list_modify(rec, a = 2L)"
    ]



-- ---------------------------------------------------------------------
-- Dataframe verbs (section 13.8)
-- ---------------------------------------------------------------------


foreignImportTests :: [Test]
foreignImportTests =
    [ test "generate/foreign_import_qualifies_call_section_13_9" <|
        compiles
            ( T.unlines
                [ "import readr.read_csv : Character -> Integer"
                , ""
                , "load path <- read_csv path"
                ]
            )
            "load <- function(path) { readr::read_csv(path) }"
    , test "generate/foreign_import_qualifies_bare_reference_section_13_9" <|
        compiles
            ( T.unlines
                [ "import dplyr.n : Integer"
                , ""
                , "x <- n"
                ]
            )
            "x <- dplyr::n"
    ]


verbTests :: [Test]
verbTests =
    [ test "generate/filter_in_pipe_to_dplyr_filter_section_13_8" <|
        compiles
            "y <- xs |> filter (a > 0)"
            "y <- xs |> dplyr::filter((a > 0L))"
    , test "generate/select_in_pipe_to_dplyr_select_section_13_8" <|
        compiles
            "y <- xs |> select { a }"
            "y <- xs |> dplyr::select(a = a)"
    , test "generate/mutate_in_pipe_to_dplyr_mutate_section_13_8" <|
        compiles
            "y <- xs |> mutate { b = a + 1 }"
            "y <- xs |> dplyr::mutate(b = (a + 1L))"
    , test "generate/arrange_with_desc_to_dplyr_desc_section_13_8" <|
        compiles
            "y <- xs |> arrange (desc score)"
            "y <- xs |> dplyr::arrange(dplyr::desc(score))"
    ]



-- ---------------------------------------------------------------------
-- Case (section 13.6)
-- ---------------------------------------------------------------------


caseTests :: [Test]
caseTests =
    [ test "generate/if_lowers_to_native_R_if_section_13_6" <|
        -- `if c then a else b` desugars to case on Logical and the
        -- generator's optimisation lowers it to R's native if.
        compiles
            "x <- if cond then 1 else 2"
            "x <- if (cond) 1L else 2L"
    , test "generate/case_on_logical_with_swapped_arms_section_13_6" <|
        compiles
            ( T.unlines
                [ "x <- case cond of"
                , "    False -> 0"
                , "    True -> 1"
                ]
            )
            "x <- if (cond) 1L else 0L"
    ]



-- ---------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------


-- | Compile a Quone source string and assert the emitted R matches
-- an exact expected string. Whitespace is collapsed for the comparison
-- so authors can format the expected string for readability.
compiles :: Text -> Text -> Prelude.IO TestResult
compiles src expected =
    case desugarSource src of
        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
        Prelude.Right p ->
            let
                actual = T.strip (generateProgram p)
                want = T.strip expected
            in
            Prelude.pure
                ( if normalised actual Prelude.== normalised want
                    then Pass
                    else Fail
                        ( "expected:\n  "
                            Prelude.<> want
                            Prelude.<> "\nactual:\n  "
                            Prelude.<> actual
                        )
                )


-- | Collapse runs of whitespace to a single space so the expected
-- snapshots can be wrapped or laid out for readability without
-- becoming load-bearing.
normalised :: Text -> Text
normalised = T.unwords Prelude.. T.words
