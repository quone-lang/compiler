{-| Snapshot tests for the elm-format-style Quone formatter.

Each fixture in @tests/format/@ is a paired input + expected-output:

  * @<NN>_<name>.in.Q@   - source the user might write (often deliberately
                            messy so the test asserts the formatter
                            normalises it).
  * @<NN>_<name>.out.Q@  - the canonical elm-format-style rendering of
                            that input.

The harness:

  1. Discovers all @.in.Q@ files in 'fixtureDir' at runtime so adding a
     fixture requires no Haskell recompile (same pattern as
     'Test.CorpusTests' and 'Test.ScenarioTests').
  2. For each pair, formats the @.in.Q@ via 'Quone.Format.Format.format'
     and compares the result, byte for byte, against the @.out.Q@. A
     mismatch is reported with a unified diff so reviewers see exactly
     which line drifted.
  3. Asserts /idempotence/: re-formatting the @.out.Q@ returns the
     same text.
  4. Asserts /round-trip/: the formatted output still parses (so we
     never produce unparseable Quone).

The directory is intended to read like the elm-format-for-Quone spec:
each fixture is short, focused on one rule, and named after that rule.
A reviewer should be able to read @tests/format/@ in alphabetical
order and understand the entire formatter contract.

-}
module Test.FormatSnapshotTests (suite) where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import qualified Quone.Format.Format as Fmt
import Quone.Parse.Desugar (desugarSource)
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.IO.Unsafe as U
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , describe
    , test
    )
import qualified Prelude



suite :: Suite
suite = describe "format-snapshot" snapshotTests



-- ---------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------


-- | Where to look for fixtures on disk.
fixtureDir :: Prelude.FilePath
fixtureDir = "tests/format"


inSuffix :: Prelude.String
inSuffix = ".in.Q"


outSuffix :: Prelude.String
outSuffix = ".out.Q"



-- ---------------------------------------------------------------------
-- Discovery (mirrors Test.ScenarioTests)
-- ---------------------------------------------------------------------


snapshotTests :: [Test]
snapshotTests =
    Prelude.concatMap mkFixture discoverFixtures


discoverFixtures :: [Prelude.FilePath]
discoverFixtures = U.unsafePerformIO <| do
    exists <- Dir.doesDirectoryExist fixtureDir
    if Prelude.not exists
        then Prelude.pure []
        else do
            entries <- Dir.listDirectory fixtureDir
            Prelude.pure
                ( List.sort
                    ( Prelude.fmap (\name -> fixtureDir FP.</> name)
                        ( Prelude.filter
                            (\name -> inSuffix `List.isSuffixOf` name)
                            entries
                        )
                    )
                )



-- ---------------------------------------------------------------------
-- Test construction
-- ---------------------------------------------------------------------


-- | Each fixture contributes two tests: one for the formatter output
-- (matches the @.out.Q@ golden) and one for idempotence (re-formatting
-- the @.out.Q@ produces the same text). A combined re-parse check is
-- folded into the output test so we don't need a third per-fixture
-- entry.
mkFixture :: Prelude.FilePath -> [Test]
mkFixture inPath =
    [ outputTest inPath
    , idempotenceTest inPath
    ]


-- | @format-snapshot/<name>/output@. Reads the @.in.Q@, formats it,
-- byte-compares to @.out.Q@. Also asserts the formatted output
-- re-parses, so a regression that produces an unparseable program is
-- reported here too.
outputTest :: Prelude.FilePath -> Test
outputTest inPath =
    test
        ("format-snapshot/" Prelude.<> T.pack name Prelude.<> "/output")
        ( do
            inSrc <- TIO.readFile inPath
            outExists <- Dir.doesFileExist outPath
            if Prelude.not outExists
                then Prelude.pure (Fail (missingGolden outPath))
                else do
                    expected <- TIO.readFile outPath
                    case Fmt.format (T.pack inPath) inSrc of
                        Prelude.Left d ->
                            Prelude.pure
                                (Fail
                                    ( "format failed on .in.Q:\n"
                                        Prelude.<> T.pack
                                            (Prelude.show d)
                                    )
                                )
                        Prelude.Right actual ->
                            if actual Prelude.== expected
                                then Prelude.pure
                                    (checkReparses actual)
                                else Prelude.pure
                                    (Fail (renderDiff expected actual))
        )
  where
    name = baseName inPath
    outPath = outPathFor inPath


-- | @format-snapshot/<name>/idempotence@. Formats the @.out.Q@; the
-- result must be byte-identical to the @.out.Q@ itself. Catches
-- regressions where the formatter is non-stable: if a second run
-- changes the output, the formatter is broken.
idempotenceTest :: Prelude.FilePath -> Test
idempotenceTest inPath =
    test
        ("format-snapshot/" Prelude.<> T.pack name Prelude.<> "/idempotence")
        ( do
            outExists <- Dir.doesFileExist outPath
            if Prelude.not outExists
                then Prelude.pure (Fail (missingGolden outPath))
                else do
                    expected <- TIO.readFile outPath
                    case Fmt.format (T.pack outPath) expected of
                        Prelude.Left d ->
                            Prelude.pure
                                (Fail
                                    ( "format failed on .out.Q:\n"
                                        Prelude.<> T.pack
                                            (Prelude.show d)
                                    )
                                )
                        Prelude.Right twice ->
                            if twice Prelude.== expected
                                then Prelude.pure Pass
                                else Prelude.pure
                                    (Fail
                                        ( "not idempotent: a second "
                                            Prelude.<> "format pass changed the output\n"
                                            Prelude.<> renderDiff expected twice
                                        )
                                    )
        )
  where
    name = baseName inPath
    outPath = outPathFor inPath


checkReparses :: Text -> TestResult
checkReparses formatted =
    case desugarSource formatted of
        Prelude.Right _ -> Pass
        Prelude.Left d ->
            Fail
                ( "formatted output failed to re-parse:\n"
                    Prelude.<> T.pack (Prelude.show d)
                )


missingGolden :: Prelude.FilePath -> Text
missingGolden p =
    "missing golden file: " Prelude.<> T.pack p Prelude.<> "\n"
        Prelude.<> "create it with the canonical formatted output."


outPathFor :: Prelude.FilePath -> Prelude.FilePath
outPathFor inPath =
    FP.dropExtension (FP.dropExtension inPath) Prelude.<> outSuffix


-- | @tests/format/01_value_decl.in.Q@ -> @01_value_decl@.
baseName :: Prelude.FilePath -> Prelude.String
baseName =
    FP.dropExtension Prelude.. FP.dropExtension Prelude.. FP.takeFileName



-- ---------------------------------------------------------------------
-- Diff rendering
-- ---------------------------------------------------------------------


renderDiff :: Text -> Text -> Text
renderDiff expected actual =
    T.intercalate "\n"
        ( [ "format drift: output does not match .out.Q golden"
          , ""
          , unifiedDiff expected actual
          ]
        )


-- | Same simple positional diff renderer used by 'Test.ScenarioTests'.
-- Pulls in zero new dependencies and is sufficient for the small
-- (5-30 line) fixtures the format snapshots produce.
unifiedDiff :: Text -> Text -> Text
unifiedDiff want got =
    let
        wantLines = T.lines want
        gotLines = T.lines got
        n = Prelude.max
            (Prelude.length wantLines)
            (Prelude.length gotLines)
        wantLines' = padTo n wantLines
        gotLines' = padTo n gotLines
        rows =
            Prelude.zipWith3
                renderRow
                [1 :: Prelude.Int ..]
                wantLines'
                gotLines'
    in
    T.intercalate "\n"
        ( ["  expected (-) vs actual (+):"] Prelude.++ rows
        )


padTo :: Prelude.Int -> [Text] -> [Text]
padTo n xs =
    xs Prelude.++ Prelude.replicate (n Prelude.- Prelude.length xs) "<missing>"


renderRow :: Prelude.Int -> Text -> Text -> Text
renderRow n w g
    | w Prelude.== g =
        "    " Prelude.<> lineNum n Prelude.<> "  " Prelude.<> visible w
    | Prelude.otherwise =
        T.intercalate "\n"
            [ "  - " Prelude.<> lineNum n Prelude.<> "  " Prelude.<> visible w
            , "  + " Prelude.<> lineNum n Prelude.<> "  " Prelude.<> visible g
            ]


-- | Make trailing whitespace visible in the diff so leading-comma
-- mistakes and stray spaces are obvious. Tabs are flagged because
-- elm-format-style Quone is spaces-only.
visible :: Text -> Text
visible =
    T.replace "\t" "<TAB>"


lineNum :: Prelude.Int -> Text
lineNum n =
    let
        s = T.pack (Prelude.show n)
    in
    T.justifyRight 3 ' ' s Prelude.<> "|"
