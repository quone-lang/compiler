{-| Argument parsing for the @quonec@ CLI.

Parses the command-line, then dispatches to 'Quone.Cli.Commands'.
For v0.0.1 we use a hand-rolled argument parser to avoid pulling in
@optparse-applicative@ as a dependency for one subcommand layer.
A migration to @optparse-applicative@ is `[planned]` once the
subcommand surface stabilises.

-}
module Quone.Cli.Main
    ( runCli
    , parseArgs
    , Command (..)
    )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Cli.Commands
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Prelude



-- | The parsed subcommand to run.
data Command
    = CmdVersion
    | CmdHelp
    | CmdCheck Prelude.FilePath
    | CmdBuildScript Prelude.FilePath
    | CmdBuildPackage Prelude.FilePath
    | CmdRun Prelude.FilePath
    | CmdFmt Prelude.FilePath
    | CmdNew Prelude.FilePath
    | CmdRepl
    | CmdUnknown Text
    deriving (Prelude.Show, Prelude.Eq)


-- | Entry point: parse argv, dispatch, exit with the command's code.
runCli :: [Prelude.String] -> Prelude.IO ()
runCli argv = do
    code <- dispatch (parseArgs argv)
    Exit.exitWith code


parseArgs :: [Prelude.String] -> Command
parseArgs = \case
    [] -> CmdHelp
    ("-h" : _) -> CmdHelp
    ("--help" : _) -> CmdHelp
    ("version" : _) -> CmdVersion
    ("--version" : _) -> CmdVersion
    ("check" : path : _) -> CmdCheck path
    ("build" : "--script" : path : _) -> CmdBuildScript path
    ("build" : "--package" : path : _) -> CmdBuildPackage path
    ("build" : "--package" : []) -> CmdBuildPackage "."
    ("build" : path : _) -> CmdBuildScript path
    ("run" : path : _) -> CmdRun path
    ("fmt" : path : _) -> CmdFmt path
    ("new" : name : _) -> CmdNew name
    ("repl" : _) -> CmdRepl
    (cmd : _) -> CmdUnknown (T.pack cmd)


dispatch :: Command -> Prelude.IO Exit.ExitCode
dispatch = \case
    CmdHelp -> printHelp Prelude.>> Prelude.pure Exit.ExitSuccess
    CmdVersion -> cmdVersion
    CmdCheck path -> cmdCheck path
    CmdBuildScript path -> cmdBuild path
    CmdBuildPackage path -> cmdBuildPackage path
    CmdRun path -> cmdRun path
    CmdFmt path -> cmdFmt path
    CmdNew name -> cmdNew name
    CmdRepl -> cmdRepl
    CmdUnknown name -> do
        TIO.hPutStrLn IO.stderr ("unknown command: " Prelude.<> name)
        printHelp
        Prelude.pure (Exit.ExitFailure 2)


printHelp :: Prelude.IO ()
printHelp =
    TIO.putStr
        ( T.unlines
            [ "quonec 0.0.1 - the Quone language compiler"
            , ""
            , "Usage:"
            , "  quonec new <name>            scaffold a new project"
            , "  quonec build <file.Q>        compile a script to .R"
            , "  quonec build --script <file> same as above"
            , "  quonec build --package [dir] compile an R package into <dir>/build/"
            , "  quonec run <file.Q>          compile, then print Rscript instructions"
            , "  quonec check <file.Q>        typecheck without emitting"
            , "  quonec fmt <file.Q>          format in place (planned)"
            , "  quonec repl                  REPL (planned)"
            , "  quonec version               print version"
            , "  quonec --help                print this message"
            ]
        )
