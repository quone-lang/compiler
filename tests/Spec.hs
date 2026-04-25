module Main where

import qualified Prelude
import qualified Test.CliTests
import qualified Test.ExampleTests
import qualified Test.FormatTests
import qualified Test.GenerateTests
import qualified Test.Harness as Harness
import qualified Test.LexTests
import qualified Test.LspTests
import qualified Test.ParseTests
import qualified Test.ReleaseTests
import qualified Test.ResolveTests
import qualified Test.TypeTests
import qualified Test.VerbTests


main :: Prelude.IO ()
main =
    Harness.runSuites
        [ Test.LexTests.suite
        , Test.ParseTests.suite
        , Test.TypeTests.suite
        , Test.VerbTests.suite
        , Test.GenerateTests.suite
        , Test.FormatTests.suite
        , Test.ExampleTests.suite
        , Test.ResolveTests.suite
        , Test.LspTests.suite
        , Test.CliTests.suite
        , Test.ReleaseTests.suite
        ]

