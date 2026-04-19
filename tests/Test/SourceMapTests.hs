{-| Tests for source maps emitted alongside generated R.

Compiler track A2 (project plan): every @--emit-sourcemap@ produces
a sidecar @.R.map@ NDJSON file mapping generated R lines back to
their @.Q@ origin. The coarse-grained fallback in
'Quone.Generate.SourceMap.buildSourceMap' is exercised here.

-}
module Test.SourceMapTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Generate.SourceMap
    ( Entry (..)
    , SourceMap (..)
    , buildSourceMap
    , encodeSourceMap
    )
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
        "sourcemap"
        ( buildTests
            ++ encodingTests
        )


parseProgram :: Text -> Prelude.IO (Maybe ())
parseProgram src = case desugarSource src of
    Prelude.Right _ -> Prelude.pure (Just ())
    Prelude.Left _ -> Prelude.pure Prelude.Nothing


buildTests :: [Test]
buildTests =
    [ test "sourcemap/one_entry_per_decl_section_a2"
        ( do
            let
                src =
                    T.unlines
                        [ "x <- 1"
                        , "y <- 2"
                        , "z <- x + y"
                        ]
            case desugarSource src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "parse failed")
                Prelude.Right prog -> do
                    let
                        entries = buildSourceMap prog
                    Prelude.pure (Prelude.length entries === 3)
        )
    , test "sourcemap/maps_first_decl_to_line_1_section_a2"
        ( do
            let
                src = "x <- 1"
            case desugarSource src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "parse failed")
                Prelude.Right prog -> do
                    case buildSourceMap prog of
                        (e : _) ->
                            Prelude.pure (entryQLine e === 1)
                        [] ->
                            Prelude.pure (Fail "no entries")
        )
    ]


encodingTests :: [Test]
encodingTests =
    [ test "sourcemap/header_includes_paths_section_a2"
        ( do
            let
                sm =
                    SourceMap
                        { smGenerated = "out/foo.R"
                        , smSource = "src/foo.Q"
                        , smEntries = []
                        }
                encoded = encodeSourceMap sm
            Prelude.pure
                (assertCond
                    "header has source path"
                    (T.isInfixOf "\"source\":\"src/foo.Q\"" encoded))
        )
    , test "sourcemap/entry_uses_r_and_q_keys_section_a2"
        ( do
            let
                sm =
                    SourceMap
                        { smGenerated = "out.R"
                        , smSource = "in.Q"
                        , smEntries = [Entry 1 1 5 7]
                        }
                encoded = encodeSourceMap sm
            Prelude.pure
                (assertCond
                    "entry has r and q sub-objects"
                    (T.isInfixOf "\"r\":{" encoded
                        Prelude.&& T.isInfixOf "\"q\":{" encoded))
        )
    , test "sourcemap/entry_columns_round_trip_section_a2"
        ( do
            let
                sm =
                    SourceMap
                        { smGenerated = "out.R"
                        , smSource = "in.Q"
                        , smEntries = [Entry 2 1 9 12]
                        }
                encoded = encodeSourceMap sm
            Prelude.pure
                (assertCond
                    "encodes line/col"
                    (T.isInfixOf "\"line\":9" encoded
                        Prelude.&& T.isInfixOf "\"col\":12" encoded))
        )
    ]


assertCond :: Text -> Prelude.Bool -> TestResult
assertCond msg cond = if cond then Pass else Fail msg
