{-| Formatter tests.

Compiler track A4 (project plan): @quonec fmt@ rewrites a file to a
single canonical form. The full elm-format-style printer is large
enough that v0.0.1 only verifies the foundational properties:

* parses what it produces (round-trip);
* idempotent (two runs equal one);
* leaves a valid program valid.

Hand-curated paired @<name>.in.Q@ / @<name>.out.Q@ snapshots can be
added as the formatter rules grow; the property tests below already
catch the most common regressions.

-}
module Test.FormatTests (suite) where

import qualified Data.Text as T
import NriPrelude
import qualified Quone.Format.Format as Fmt
import Quone.Parse.Desugar (desugarSource)
import Test.Harness
    ( Suite
    , Test
    , TestResult (..)
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "format"
        ( idempotenceTests
            ++ roundTripTests
            ++ commentPreservationTests
        )


inputs :: [(Text, Text)]
inputs =
    [ ("hello", "main <- 1\n")
    , ("scores", "scores <- [1.0, 2.0, 3.0]\n")
    , ("annotated", "double : Double\ndouble <- 2.0\n")
    , ("imports", "import readr.read_csv : Character -> Character\n")
    , ("module-header"
      , "module Stats.Summary exporting (mean_score)\n\nmean_score <- 1.0\n"
      )
    ]


idempotenceTests :: [Test]
idempotenceTests =
    [ test
        ("format/idempotent_" ++ name ++ "_section_a4")
        ( do
            case Fmt.format ("<test>." ++ name) src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "first format failed to parse")
                Prelude.Right once ->
                    case Fmt.format ("<test>." ++ name) once of
                        Prelude.Left _ ->
                            Prelude.pure
                                (Fail "second format failed to parse")
                        Prelude.Right twice ->
                            Prelude.pure (twice === once)
        )
    | (name, src) <- inputs
    ]


roundTripTests :: [Test]
roundTripTests =
    [ test
        ("format/output_reparses_" ++ name ++ "_section_a4")
        ( do
            case Fmt.format ("<test>." ++ name) src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "format failed")
                Prelude.Right out ->
                    case desugarSource out of
                        Prelude.Right _ -> Prelude.pure Pass
                        Prelude.Left d ->
                            Prelude.pure
                                (Fail
                                    ("formatted output failed to parse: "
                                        ++ T.pack (Prelude.show d)))
        )
    | (name, src) <- inputs
    ]


commentPreservationTests :: [Test]
commentPreservationTests =
    [ test "format/preserves_top_of_file_comment_section_c1"
        ( do
            let
                src = "# this is a top comment\nmain <- 1\n"
            case Fmt.format "<test>" src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "format failed")
                Prelude.Right out ->
                    Prelude.pure
                        (assertContains "this is a top comment" out)
        )
    , test "format/preserves_between_decl_comment_section_c1"
        ( do
            let
                src =
                    T.unlines
                        [ "x <- 1"
                        , ""
                        , "# explain y"
                        , "y <- 2"
                        ]
            case Fmt.format "<test>" src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "format failed")
                Prelude.Right out ->
                    Prelude.pure (assertContains "explain y" out)
        )
    , test "format/preserves_trailing_comment_section_c1"
        ( do
            let
                src =
                    T.unlines
                        [ "main <- 1"
                        , ""
                        , "# end-of-file note"
                        ]
            case Fmt.format "<test>" src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "format failed")
                Prelude.Right out ->
                    Prelude.pure (assertContains "end-of-file note" out)
        )
    , test "format/idempotent_with_comments_section_c1"
        ( do
            let
                src =
                    T.unlines
                        [ "# top"
                        , "x <- 1"
                        , ""
                        , "# inline"
                        , "y <- 2"
                        , "# trailing"
                        ]
            case Fmt.format "<test>" src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "first format failed")
                Prelude.Right once ->
                    case Fmt.format "<test>" once of
                        Prelude.Left _ ->
                            Prelude.pure (Fail "second format failed")
                        Prelude.Right twice ->
                            Prelude.pure (twice === once)
        )
    ]


assertContains :: Text -> Text -> TestResult
assertContains needle haystack =
    if T.isInfixOf needle haystack
        then Pass
        else Fail ("did not contain " ++ needle ++ "; got: " ++ haystack)
