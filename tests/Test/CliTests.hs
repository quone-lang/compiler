{-| CLI argument-parsing tests.

Compiler track A1/A3/A6 (project plan): the @quonec@ CLI grew a
@--diagnostics-format@ flag, an @--out@ flag, a @--rscript@ flag, a
@--emit-sourcemap@ flag, an @--log-file@ flag, and three new
subcommands (@deps@, @lsp@, plus the no-longer-stub @repl@ and
@fmt@). This module checks that argv parses into the right
'Quone.Cli.Main.Command' shape.

-}
module Test.CliTests (suite) where

import NriPrelude
import Quone.Cli.Main
    ( BuildOpts (..)
    , Command (..)
    , RunOpts (..)
    , defaultBuildOpts
    , defaultRunOpts
    , parseArgs
    )
import Quone.Diagnostic (DiagnosticsFormat (..))
import Test.Harness
    ( Suite
    , Test
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "cli"
        ( basicTests
            ++ diagnosticsFormatTests
            ++ buildTests
            ++ runTests
            ++ depsTests
            ++ lspTests
        )


basicTests :: [Test]
basicTests =
    [ test "cli/no_args_prints_help_section_a3"
        (Prelude.pure (parseArgs [] === CmdHelp))
    , test "cli/version_command_section_a3"
        (Prelude.pure (parseArgs ["version"] === CmdVersion))
    , test "cli/repl_command_section_a5"
        (Prelude.pure (parseArgs ["repl"] === CmdRepl))
    , test "cli/fmt_command_section_a4"
        ( Prelude.pure
            (parseArgs ["fmt", "src/Foo.Q"]
                === CmdFmt "src/Foo.Q"))
    ]


diagnosticsFormatTests :: [Test]
diagnosticsFormatTests =
    [ test "cli/check_defaults_to_human_section_a1"
        ( Prelude.pure
            (parseArgs ["check", "src/Foo.Q"]
                === CmdCheck HumanDiagnostics "src/Foo.Q"))
    , test "cli/check_accepts_json_section_a1"
        ( Prelude.pure
            (parseArgs ["check", "--diagnostics-format=json", "src/Foo.Q"]
                === CmdCheck JsonDiagnostics "src/Foo.Q"))
    , test "cli/check_accepts_human_explicit_section_a1"
        ( Prelude.pure
            (parseArgs
                ["check", "--diagnostics-format=human", "src/Foo.Q"]
                === CmdCheck HumanDiagnostics "src/Foo.Q"))
    , test "cli/check_accepts_short_json_section_a1"
        ( Prelude.pure
            (parseArgs ["check", "--json", "src/Foo.Q"]
                === CmdCheck JsonDiagnostics "src/Foo.Q"))
    ]


buildTests :: [Test]
buildTests =
    [ test "cli/build_defaults_to_script_section_a3"
        ( Prelude.pure
            (parseArgs ["build", "src/Foo.Q"]
                === CmdBuildScript defaultBuildOpts "src/Foo.Q"))
    , test "cli/build_accepts_out_dir_section_a3"
        ( Prelude.pure
            (parseArgs ["build", "--out=out", "src/Foo.Q"]
                === CmdBuildScript
                    defaultBuildOpts
                        {boOut = Just "out"}
                    "src/Foo.Q"))
    , test "cli/build_accepts_emit_sourcemap_section_a2"
        ( Prelude.pure
            (parseArgs ["build", "--emit-sourcemap", "src/Foo.Q"]
                === CmdBuildScript
                    defaultBuildOpts
                        {boSourcemap = Prelude.True}
                    "src/Foo.Q"))
    , test "cli/build_package_with_path_section_a3"
        ( Prelude.pure
            (parseArgs ["build", "--package", "proj"]
                === CmdBuildPackage defaultBuildOpts "proj"))
    , test "cli/build_package_no_path_uses_dot_section_a3"
        ( Prelude.pure
            (parseArgs ["build", "--package"]
                === CmdBuildPackage defaultBuildOpts "."))
    ]


runTests :: [Test]
runTests =
    [ test "cli/run_default_no_rscript_section_a3"
        ( Prelude.pure
            (parseArgs ["run", "src/Foo.Q"]
                === CmdRun defaultRunOpts "src/Foo.Q"))
    , test "cli/run_with_rscript_section_a3"
        ( Prelude.pure
            (parseArgs ["run", "--rscript", "src/Foo.Q"]
                === CmdRun
                    defaultRunOpts {roRscript = Prelude.True}
                    "src/Foo.Q"))
    ]


depsTests :: [Test]
depsTests =
    [ test "cli/deps_default_path_is_dot_section_a3"
        ( Prelude.pure
            (parseArgs ["deps"]
                === CmdDeps HumanDiagnostics "."))
    , test "cli/deps_accepts_path_section_a3"
        ( Prelude.pure
            (parseArgs ["deps", "examples/stats-package"]
                === CmdDeps HumanDiagnostics "examples/stats-package"))
    , test "cli/deps_accepts_json_section_a3"
        ( Prelude.pure
            (parseArgs
                ["deps", "--diagnostics-format=json", "proj"]
                === CmdDeps JsonDiagnostics "proj"))
    ]


lspTests :: [Test]
lspTests =
    [ test "cli/lsp_no_log_section_a6"
        ( Prelude.pure
            (parseArgs ["lsp"] === CmdLsp Prelude.Nothing))
    , test "cli/lsp_with_log_file_section_a6"
        ( Prelude.pure
            (parseArgs ["lsp", "--log-file=/tmp/lsp.log"]
                === CmdLsp (Just "/tmp/lsp.log")))
    ]
