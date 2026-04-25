module Test.ExampleTests (suite) where

import qualified Control.Exception as Exception
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Cli.Commands (compileScript)
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..))
import qualified Prelude


data ExampleFile = ExampleFile
    { exampleName :: Text
    , examplePath :: Prelude.FilePath
    }


suite :: Harness.Suite
suite =
    Harness.describe "examples" (Prelude.fmap exampleTest exampleFiles)


exampleFiles :: [ExampleFile]
exampleFiles =
    [ ExampleFile "compiler_hello" "examples/hello.Q"
    , ExampleFile "hello" "../examples/hello.Q"
    , ExampleFile "dataframe_pipeline" "../examples/dataframe-pipeline/pipeline.Q"
    , ExampleFile "decoders" "../examples/decoders/decoders.Q"
    , ExampleFile "scores" "../examples/scores/scores.Q"
    , ExampleFile "pharma_analysis" "../examples/pharma-analysis/pharma-analysis.Q"
    , ExampleFile "r_package_mtcars_summary" "../quone/inst/examples/mtcars_summary.Q"
    ]


exampleTest :: ExampleFile -> Harness.Test
exampleTest example =
    Harness.test
        ("examples/" ++ exampleName example ++ "_compiles_to_valid_readable_r")
        (checkExample example)


checkExample :: ExampleFile -> Prelude.IO TestResult
checkExample example = do
    src <- TIO.readFile (examplePath example)
    case compileScript (T.pack (examplePath example)) src of
        Prelude.Left diagnostic ->
            Prelude.pure (Fail (T.pack (Prelude.show diagnostic)))
        Prelude.Right (_, generatedR) ->
            Temp.withSystemTempFile (FP.takeBaseName (examplePath example) Prelude.++ ".R") <| \path handle -> do
                TIO.hPutStr handle generatedR
                IO.hClose handle
                rParseResult <- parseR path
                Prelude.pure (combineResults [readableR (exampleName example) generatedR, rParseResult])


readableR :: Text -> Text -> TestResult
readableR name generatedR =
    let
        lines_ = T.lines generatedR
        trailingWhitespace line = T.dropWhileEnd isHorizontalSpace line Prelude./= line
        longLines = Prelude.filter (\line -> T.length line Prelude.> 100) lines_
    in
    if T.null (T.strip generatedR) then
        Fail (name ++ " generated empty R")
    else if "\r" `T.isInfixOf` generatedR then
        Fail (name ++ " generated CRLF line endings")
    else if Prelude.any trailingWhitespace lines_ then
        Fail (name ++ " generated trailing whitespace")
    else case longLines of
        [] -> Pass
        firstLong : _ ->
            Fail
                ( name
                    ++ " generated an R line over 100 columns:\n"
                    ++ firstLong
                )


parseR :: Prelude.FilePath -> Prelude.IO TestResult
parseR path = do
    result <-
        Exception.try
            ( Process.readProcessWithExitCode
                "Rscript"
                ["-e", "parse(file = commandArgs(TRUE)[1])", path]
                ""
            )
    case (result :: Prelude.Either Exception.IOException (Exit.ExitCode, Prelude.String, Prelude.String)) of
        Prelude.Left err ->
            Prelude.pure (Fail ("failed to run Rscript: " ++ T.pack (Prelude.show err)))
        Prelude.Right (Exit.ExitSuccess, _, _) ->
            Prelude.pure Pass
        Prelude.Right (exitCode, stdout_, stderr_) ->
            Prelude.pure
                ( Fail
                    ( "R parser rejected generated output with "
                        ++ T.pack (Prelude.show exitCode)
                        ++ "\nstdout:\n"
                        ++ T.pack stdout_
                        ++ "\nstderr:\n"
                        ++ T.pack stderr_
                    )
                )


combineResults :: [TestResult] -> TestResult
combineResults results =
    case [msg | Fail msg <- results] of
        [] -> Pass
        failures -> Fail (T.intercalate "\n" failures)


isHorizontalSpace :: Char -> Prelude.Bool
isHorizontalSpace c =
    c Prelude.== ' ' || c Prelude.== '\t'
