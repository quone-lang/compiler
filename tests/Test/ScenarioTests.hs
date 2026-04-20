{-| End-to-end scenario tests.

Each scenario in @tests/scenarios/@ is a small but realistic Quone
program paired with two goldens:

  * @<name>.R@   - the lowered R the compiler produces (whitespace
                   normalised, same as 'Test.CorpusTests'). Catches
                   /codegen drift/: a lowering change that produces
                   different R, even if the new R happens to behave
                   the same at runtime.
  * @<name>.out@ - what that lowered R prints when fed to @Rscript@.
                   Catches /runtime drift/: a change in what the
                   program actually computes (wrong value, dropped
                   row, missing column, etc.).

Having both layers means a CI failure tells you which dimension
broke. If only @.R@ diverges you have a codegen change that's
behaviourally equivalent (often expected, just refresh the golden).
If only @.out@ diverges, the lowered R looks the same but R itself
is now producing different output (e.g. a behavioural regression in
a runtime helper or a dependent package). If both diverge, the
codegen change actually altered runtime behaviour.

The harness emits two tests per scenario:

  * @scenarios/<name>/codegen@: compile, normalise whitespace, compare
    to @<name>.R@. Does not need @Rscript@ on PATH, so it always runs.
  * @scenarios/<name>/runtime@: compile, wrap with @print(main)@,
    spawn @Rscript --no-save --no-restore --vanilla@ on stdin, capture
    stdout + stderr, diff against @<name>.out@. Skipped (passes) if
    @Rscript@ is not on PATH so CI without R stays green.

Both renderers use the same unified-diff helper so failures are easy
to read.

-}
module Test.ScenarioTests (suite) where

import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Cli.Commands (compileScript)
import Quone.Diagnostic (render)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import qualified System.IO.Unsafe as U
import qualified System.Process as Proc
import qualified System.Timeout as Timeout
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , describe
    , test
    )
import qualified Prelude



suite :: Suite
suite = describe "scenarios" scenarioTests



-- ---------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------


-- | Where to look for scenarios on disk.
scenarioDir :: Prelude.FilePath
scenarioDir = "tests/scenarios"


-- | Cap each Rscript spawn so a runaway scenario can't hang CI.
-- 30 seconds is generous; in practice every scenario runs in well
-- under a second on a developer laptop.
scenarioTimeoutMicros :: Prelude.Int
scenarioTimeoutMicros = 30 Prelude.* 1000 Prelude.* 1000



-- ---------------------------------------------------------------------
-- Discovery (mirrors Test.CorpusTests)
-- ---------------------------------------------------------------------


-- | Synchronous file-system walk via 'unsafePerformIO'. Same pattern
-- as 'Test.CorpusTests': tests are enumerated once at suite-construction
-- time so adding a new scenario requires no Haskell recompile.
scenarioTests :: [Test]
scenarioTests =
    Prelude.concatMap mkScenario discoverScenarios


discoverScenarios :: [Prelude.FilePath]
discoverScenarios = U.unsafePerformIO <| do
    exists <- Dir.doesDirectoryExist scenarioDir
    if Prelude.not exists
        then Prelude.pure []
        else do
            entries <- Dir.listDirectory scenarioDir
            Prelude.pure
                ( List.sort
                    ( Prelude.fmap (\name -> scenarioDir FP.</> name)
                        ( Prelude.filter
                            (\name -> ".Q" `List.isSuffixOf` name)
                            entries
                        )
                    )
                )



-- ---------------------------------------------------------------------
-- Test construction
-- ---------------------------------------------------------------------


-- | Each scenario contributes two tests: one for the lowered R
-- (always runnable) and one for the runtime output (skipped if
-- 'Rscript' is missing).
mkScenario :: Prelude.FilePath -> [Test]
mkScenario qPath =
    [ codegenTest qPath
    , runtimeTest qPath
    ]


-- | @scenarios/<name>/codegen@. Runs offline; fails if the compiler
-- errors or if the lowered R drifts from the @.R@ golden under
-- whitespace-normalised comparison.
codegenTest :: Prelude.FilePath -> Test
codegenTest qPath =
    test
        ("scenarios/" Prelude.<> T.pack name Prelude.<> "/codegen")
        ( do
            src <- TIO.readFile qPath
            goldenExists <- Dir.doesFileExist rPath
            if Prelude.not goldenExists
                then Prelude.pure (Fail (missingGolden rPath))
                else do
                    expected <- TIO.readFile rPath
                    case compileScript (T.pack qPath) src of
                        Prelude.Left d ->
                            Prelude.pure
                                (Fail
                                    ( "compile failed:\n"
                                        Prelude.<> render d
                                    )
                                )
                        Prelude.Right (_prog, rcode) ->
                            Prelude.pure (compareCodegen expected rcode)
        )
  where
    name = FP.takeBaseName qPath
    rPath = FP.replaceExtension qPath "R"


-- | @scenarios/<name>/runtime@. Skipped (passes) if 'Rscript' is not
-- on PATH so CI without R stays green. Otherwise compiles, wraps the
-- generated R with a @print(main)@ epilogue, runs it, and diffs the
-- captured stdout against the @.out@ golden.
runtimeTest :: Prelude.FilePath -> Test
runtimeTest qPath =
    test
        ("scenarios/" Prelude.<> T.pack name Prelude.<> "/runtime")
        ( do
            rscript <- Dir.findExecutable "Rscript"
            case rscript of
                Prelude.Nothing -> Prelude.pure Pass
                Just path -> runRuntimeScenario path qPath outPath
        )
  where
    name = FP.takeBaseName qPath
    outPath = FP.replaceExtension qPath "out"


runRuntimeScenario
    :: Prelude.FilePath  -- ^ resolved 'Rscript' path
    -> Prelude.FilePath  -- ^ scenario .Q
    -> Prelude.FilePath  -- ^ expected .out
    -> Prelude.IO TestResult
runRuntimeScenario rscript qPath outPath = do
    src <- TIO.readFile qPath
    expectedExists <- Dir.doesFileExist outPath
    if Prelude.not expectedExists
        then Prelude.pure (Fail (missingGolden outPath))
        else do
            expected <- TIO.readFile outPath
            case compileScript (T.pack qPath) src of
                Prelude.Left d ->
                    Prelude.pure
                        (Fail
                            ( "compile failed:\n"
                                Prelude.<> render d
                            )
                        )
                Prelude.Right (_prog, rcode) -> do
                    let
                        wrapped = wrapR rcode
                    captured <- runRscript rscript wrapped
                    Prelude.pure (compareOutput expected captured)


missingGolden :: Prelude.FilePath -> Text
missingGolden p =
    "missing golden file: " Prelude.<> T.pack p Prelude.<> "\n"
        Prelude.<> "create it with the actual output, then re-run."


-- ---------------------------------------------------------------------
-- Codegen comparison
-- ---------------------------------------------------------------------


-- | Compare lowered R against the @.R@ golden. Whitespace is
-- normalised the same way 'Test.CorpusTests' normalises snapshot
-- comparisons (collapse all whitespace runs to single spaces) so the
-- harness isn't brittle to formatter tweaks. The /diff/ shown on
-- failure uses the un-normalised text on both sides so a reviewer
-- can see structural differences clearly.
compareCodegen :: Text -> Text -> TestResult
compareCodegen expected actual
    | normaliseR (T.strip actual) Prelude.== normaliseR (T.strip expected) =
        Pass
    | Prelude.otherwise =
        Fail
            ( T.intercalate "\n"
                [ "codegen drift: lowered R does not match .R golden"
                , ""
                , unifiedDiff (T.strip expected) (T.strip actual)
                ]
            )


-- | Same normalisation as @Test.CorpusTests.normalise@: collapse all
-- runs of whitespace (including newlines) to a single space. Two R
-- programs that match under this comparison are guaranteed to parse
-- to the same R AST modulo formatting.
normaliseR :: Text -> Text
normaliseR = T.unwords Prelude.. T.words



-- ---------------------------------------------------------------------
-- R execution
-- ---------------------------------------------------------------------


-- | What the harness actually sends to Rscript: the lowered R, then a
-- final @print(main)@ that mirrors what 'Quone.Repl.Session' does so
-- the scenario's @main@ binding shows up as the captured stdout.
wrapR :: Text -> Text
wrapR rcode =
    T.intercalate "\n"
        [ rcode
        , "if (exists(\"main\")) print(main)"
        ]


-- | Captured output from a single Rscript run. We don't separate
-- stdout vs stderr in the golden because realistic R output (like a
-- tibble print) goes to stdout, and warnings/errors going to stderr
-- ARE part of what the user sees.
data Captured = Captured
    { capStdout :: Text
    , capStderr :: Text
    , capExit :: Maybe Prelude.Int  -- 'Nothing' means timed out
    }


-- | Spawn @Rscript@ with the program on stdin and capture stdout +
-- stderr. We use 'Proc.readCreateProcessWithExitCode' rather than
-- raw 'Proc.createProcess' + 'Conc.forkIO' because the test suite
-- is built without @-threaded@; a hand-rolled drain loop would let
-- a blocking I/O call starve every other Haskell thread (which is
-- exactly the deadlock we hit on the first cut of this harness).
runRscript :: Prelude.FilePath -> Text -> Prelude.IO Captured
runRscript rscript program = do
    let
        spec =
            Proc.proc rscript
                ["--no-save", "--no-restore", "--vanilla", "-"]
        call =
            Proc.readCreateProcessWithExitCode
                spec
                (T.unpack program)
    result <-
        Timeout.timeout scenarioTimeoutMicros
            (Exception.try call
                :: Prelude.IO
                    (Prelude.Either
                        Exception.SomeException
                        (Exit.ExitCode, Prelude.String, Prelude.String)
                    )
            )
    case result of
        Prelude.Nothing ->
            Prelude.pure
                Captured
                    { capStdout = T.empty
                    , capStderr =
                        "Rscript timed out (or could not be started)"
                    , capExit = Prelude.Nothing
                    }
        Just (Prelude.Left ex) ->
            Prelude.pure
                Captured
                    { capStdout = T.empty
                    , capStderr =
                        "Rscript invocation failed: "
                            Prelude.<> T.pack (Prelude.show ex)
                    , capExit = Just (-1)
                    }
        Just (Prelude.Right (code, sout, serr)) ->
            Prelude.pure
                Captured
                    { capStdout = T.pack sout
                    , capStderr = T.pack serr
                    , capExit = Just (exitCodeInt code)
                    }


exitCodeInt :: Exit.ExitCode -> Prelude.Int
exitCodeInt = \case
    Exit.ExitSuccess -> 0
    Exit.ExitFailure n -> n



-- ---------------------------------------------------------------------
-- Comparison + diff rendering
-- ---------------------------------------------------------------------


-- | Compare captured stdout to the golden, normalising trailing
-- whitespace on each line (R's print routines like to pad with
-- spaces, and editors sometimes strip trailing whitespace from
-- the golden file).
compareOutput :: Text -> Captured -> TestResult
compareOutput expected captured
    | capExit captured Prelude.== Prelude.Nothing =
        Fail
            ( "Rscript timed out after "
                Prelude.<> T.pack
                    (Prelude.show (scenarioTimeoutMicros `Prelude.div` 1000000))
                Prelude.<> "s\n"
                Prelude.<> "stderr:\n"
                Prelude.<> indent (capStderr captured)
            )
    | normaliseLines (capStdout captured)
        Prelude.== normaliseLines expected =
        Pass
    | Prelude.otherwise =
        Fail (renderMismatch expected captured)


renderMismatch :: Text -> Captured -> Text
renderMismatch expected captured =
    T.intercalate "\n"
        ( [ "runtime drift: Rscript stdout does not match .out golden"
          , "exit code: "
                Prelude.<> case capExit captured of
                    Prelude.Nothing -> "<timeout>"
                    Just n -> T.pack (Prelude.show n)
          , ""
          , unifiedDiff expected (capStdout captured)
          ]
            Prelude.++ stderrSection (capStderr captured)
        )


stderrSection :: Text -> [Text]
stderrSection serr
    | T.null (T.strip serr) = []
    | Prelude.otherwise =
        ["", "Rscript stderr:", indent serr]


indent :: Text -> Text
indent =
    T.intercalate "\n"
        Prelude.. Prelude.fmap ("    " Prelude.<>)
        Prelude.. T.lines


-- | Trim trailing whitespace on each line. R uses padding spaces
-- in tibble output that vary by terminal width, and editors strip
-- trailing whitespace from .out files. Normalising here makes the
-- comparison robust without making the diff lie.
normaliseLines :: Text -> Text
normaliseLines =
    T.intercalate "\n"
        Prelude.. Prelude.fmap T.stripEnd
        Prelude.. T.lines


-- | A small unified-diff renderer. Not a full LCS algorithm: lines
-- are compared positionally, with a context window of 1 around each
-- difference. Sufficient for the small (5-30 line) goldens scenarios
-- produce, and pulls in zero new dependencies.
unifiedDiff :: Text -> Text -> Text
unifiedDiff want got =
    let
        wantLines = padTo (Prelude.length gotLines) (T.lines want)
        gotLines = T.lines got
        wantLines' =
            padTo (Prelude.length wantLines) wantLines
        gotLines' =
            padTo (Prelude.length wantLines) gotLines
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
    | T.stripEnd w Prelude.== T.stripEnd g =
        "    " Prelude.<> lineNum n Prelude.<> "  " Prelude.<> w
    | Prelude.otherwise =
        T.intercalate "\n"
            [ "  - " Prelude.<> lineNum n Prelude.<> "  " Prelude.<> w
            , "  + " Prelude.<> lineNum n Prelude.<> "  " Prelude.<> g
            ]


lineNum :: Prelude.Int -> Text
lineNum n =
    let
        s = T.pack (Prelude.show n)
    in
    T.justifyRight 3 ' ' s Prelude.<> "|"
