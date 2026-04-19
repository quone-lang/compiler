{-| Test entrypoint.

Each compiler stage adds its suite to this list. Names follow
@\<phase\>/\<rule\>_section_\<n_m\>@ per LANGUAGE.md section 16.14 so
failures are traceable back to the exact spec rule.

-}
module Main where

import NriPrelude
import qualified Test.Harness as Harness
import qualified Test.AstTests
import qualified Test.CliTests
import qualified Test.CorpusTests
import qualified Test.FormatTests
import qualified Test.GenerateTests
import qualified Test.JsonDiagnosticTests
import qualified Test.LexTests
import qualified Test.LspTests
import qualified Test.PackageTests
import qualified Test.ParseTests
import qualified Test.PropertyTests
import qualified Test.ReplTests
import qualified Test.ResolveTests
import qualified Test.SourceMapTests
import qualified Test.TypeTests
import qualified Test.VerbTests
import qualified Prelude


main :: Prelude.IO ()
main =
    Harness.runSuites
        [ Test.LexTests.suite
        , Test.ParseTests.suite
        , Test.AstTests.suite
        , Test.ResolveTests.suite
        , Test.TypeTests.suite
        , Test.VerbTests.suite
        , Test.GenerateTests.suite
        , Test.PackageTests.suite
        , Test.PropertyTests.suite
        , Test.CorpusTests.suite
        , Test.JsonDiagnosticTests.suite
        , Test.SourceMapTests.suite
        , Test.FormatTests.suite
        , Test.ReplTests.suite
        , Test.LspTests.suite
        , Test.CliTests.suite
        ]
