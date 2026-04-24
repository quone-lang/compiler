module Test.CliTests (suite) where

import NriPrelude
import qualified Quone.Cli.Main
import Quone.Cli.Main
    ( Command (..)
    , parseArgs
    )
import Quone.Diagnostic (DiagnosticsFormat (..))
import qualified Test.Harness as Harness
import Test.Harness ((===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "cli"
        [ Harness.test "cli/parses_compile" <|
            Prelude.pure
                (parseArgs ["compile", "main.Q"] === CmdBuildScript defaultOpts "main.Q")
        , Harness.test "cli/parses_compile_dir" <|
            Prelude.pure
                (parseArgs ["compile-dir", "src"] === CmdCompileDir defaultOpts "src")
        , Harness.test "cli/parses_check_json" <|
            Prelude.pure
                (parseArgs ["--json", "check", "main.Q"] === CmdCheck JsonDiagnostics "main.Q")
        , Harness.test "cli/parses_lsp" <|
            Prelude.pure
                (parseArgs ["lsp"] === CmdLsp Prelude.Nothing)
        ]


defaultOpts :: Quone.Cli.Main.BuildOpts
defaultOpts =
    Quone.Cli.Main.defaultBuildOpts

