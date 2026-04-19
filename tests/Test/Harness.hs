{-| A tiny test harness used by the compiler test suite.

Per LANGUAGE.md section 16, every normative rule needs at least one
test. We use a local harness rather than a third-party framework so
the test names track the spec sections directly (section 16.14) and
the runner has no dependencies beyond what the compiler already uses.

Usage:

@
suite :: 'Suite'
suite = 'describe' "lexer"
    [ 'test' "lexer/keywords_section_3_4" $ ...
    , 'test' "lexer/operators_section_3_5" $ ...
    ]

main = 'runSuite' [suite]
@

A 'TestResult' is either 'Pass' or 'Fail' with a human-readable
message; the runner exits 0 on all passes, 1 otherwise.

-}
module Test.Harness
    ( Suite
    , Test
    , TestResult (..)
    , describe
    , test
    , runSuite
    , runSuites
    , (===)
    , assert
    , assertContains
    , assertLeft
    , assertRight
    )
where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import qualified System.Exit as Exit
import qualified Prelude


data TestResult
    = Pass
    | Fail Text
    deriving (Prelude.Show, Prelude.Eq)


data Test = Test
    { testName :: Text
    , testRun :: Prelude.IO TestResult
    }


data Suite = Suite
    { suiteName :: Text
    , suiteTests :: [Test]
    }


-- | Build a test from a name and an action. The action returns 'Pass'
-- or 'Fail msg'; uncaught exceptions are not caught (we want them to
-- surface as test failures with a stack trace).
test :: Text -> Prelude.IO TestResult -> Test
test name action = Test {testName = name, testRun = action}


-- | Group related tests under a label.
describe :: Text -> [Test] -> Suite
describe name tests = Suite {suiteName = name, suiteTests = tests}


-- | Run every suite, print a per-test status line, and exit with code
-- 1 if any test failed.
runSuites :: [Suite] -> Prelude.IO ()
runSuites suites = do
    results <- Prelude.traverse runSuite suites
    let
        total :: Prelude.Int
        total = Prelude.sum (Prelude.fmap Prelude.fst results)
        failed :: Prelude.Int
        failed = Prelude.sum (Prelude.fmap Prelude.snd results)
    TIO.putStrLn ""
    if failed Prelude.== 0
        then do
            TIO.putStrLn
                ( "quonec-test: "
                    ++ T.pack (Prelude.show total)
                    ++ " tests, 0 failures."
                )
            Exit.exitWith Exit.ExitSuccess
        else do
            TIO.putStrLn
                ( "quonec-test: "
                    ++ T.pack (Prelude.show total)
                    ++ " tests, "
                    ++ T.pack (Prelude.show failed)
                    ++ " failures."
                )
            Exit.exitWith (Exit.ExitFailure 1)


-- | Run a single suite. Returns @(total, failed)@ for the summary.
runSuite :: Suite -> Prelude.IO (Prelude.Int, Prelude.Int)
runSuite suite = do
    TIO.putStrLn ("# " ++ suiteName suite)
    pairs <- Prelude.traverse runOne (suiteTests suite)
    let
        total :: Prelude.Int
        total = Prelude.length pairs
        failed :: Prelude.Int
        failed = Prelude.length (Prelude.filter Prelude.not pairs)
    Prelude.pure (total, failed)


runOne :: Test -> Prelude.IO Prelude.Bool
runOne t = do
    result <- testRun t
    case result of
        Pass -> do
            TIO.putStrLn ("  ok  " ++ testName t)
            Prelude.pure Prelude.True
        Fail msg -> do
            TIO.putStrLn ("  FAIL " ++ testName t)
            TIO.putStrLn ("       " ++ T.replace "\n" "\n       " msg)
            Prelude.pure Prelude.False



-- | Equality assertion. The error message includes both sides.
(===) :: (Prelude.Show a, Prelude.Eq a) => a -> a -> TestResult
actual === expected =
    if actual Prelude.== expected
        then Pass
        else
            Fail
                ( "expected: "
                    ++ T.pack (Prelude.show expected)
                    ++ "\n  actual: "
                    ++ T.pack (Prelude.show actual)
                )


infix 1 ===


assert :: Prelude.Bool -> Text -> TestResult
assert cond msg =
    if cond then Pass else Fail msg


assertContains :: (Prelude.Show a, Prelude.Eq a) => a -> [a] -> TestResult
assertContains needle haystack =
    if needle `List.elem` haystack
        then Pass
        else
            Fail
                ( T.pack (Prelude.show needle)
                    ++ " was not present in "
                    ++ T.pack (Prelude.show haystack)
                )


assertLeft :: Prelude.Show b => Prelude.Either a b -> TestResult
assertLeft = \case
    Prelude.Left _ -> Pass
    Prelude.Right b -> Fail ("expected Left, got Right " ++ T.pack (Prelude.show b))


assertRight :: Prelude.Show a => Prelude.Either a b -> TestResult
assertRight = \case
    Prelude.Right _ -> Pass
    Prelude.Left a -> Fail ("expected Right, got Left " ++ T.pack (Prelude.show a))
