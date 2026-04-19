{-| Argument parsing for the @quonec@ CLI.

Parses the command-line, then dispatches to 'Quone.Cli.Commands'.
For v0.0.1 we use a hand-rolled argument parser to avoid pulling in
@optparse-applicative@ as a dependency for one subcommand layer.
A migration to @optparse-applicative@ is `[planned]` once the
subcommand surface stabilises.

The parser accepts a small set of cross-cutting options before or
after the subcommand:

* @--diagnostics-format=human|json@ -- machine-readable diagnostic
  output for editors and the R companion package.
* @--out=DIR@ -- override the output directory for @build@ /
  @build --package@.
* @--rscript@ -- on @run@, actually invoke @Rscript@ rather than
  printing the suggested command.

-}
module Quone.Cli.Main
    ( runCli
    , parseArgs
    , Command (..)
    , BuildOpts (..)
    , RunOpts (..)
    , defaultBuildOpts
    , defaultRunOpts
    )
where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Cli.Commands
import Quone.Diagnostic (DiagnosticsFormat (..))
import qualified Quone.Lsp.Server as Lsp
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Prelude



-- | Options that influence script and package builds.
data BuildOpts = BuildOpts
    { boFormat :: DiagnosticsFormat
    , boOut :: Maybe Prelude.FilePath
    , boSourcemap :: Prelude.Bool
    }
    deriving (Prelude.Show, Prelude.Eq)


defaultBuildOpts :: BuildOpts
defaultBuildOpts =
    BuildOpts
        { boFormat = HumanDiagnostics
        , boOut = Prelude.Nothing
        , boSourcemap = Prelude.False
        }


-- | Options that influence @quonec run@.
data RunOpts = RunOpts
    { roBuild :: BuildOpts
    , roRscript :: Prelude.Bool
    }
    deriving (Prelude.Show, Prelude.Eq)


defaultRunOpts :: RunOpts
defaultRunOpts =
    RunOpts
        { roBuild = defaultBuildOpts
        , roRscript = Prelude.False
        }


-- | The parsed subcommand to run.
data Command
    = CmdVersion
    | CmdHelp
    | CmdCheck DiagnosticsFormat Prelude.FilePath
    | CmdBuildScript BuildOpts Prelude.FilePath
    | CmdBuildPackage BuildOpts Prelude.FilePath
    | CmdRun RunOpts Prelude.FilePath
    | CmdDeps DiagnosticsFormat Prelude.FilePath
    | CmdFmt Prelude.FilePath
    | CmdNew Prelude.FilePath
    | CmdRepl
    | CmdLsp (Maybe Prelude.FilePath)
    | CmdMissingArg Text
    | CmdUnknown Text
    deriving (Prelude.Show, Prelude.Eq)


-- | Entry point: parse argv, dispatch, exit with the command's code.
runCli :: [Prelude.String] -> Prelude.IO ()
runCli argv = do
    code <- dispatch (parseArgs argv)
    Exit.exitWith code


parseArgs :: [Prelude.String] -> Command
parseArgs argv = case Prelude.fmap T.pack argv of
    [] -> CmdHelp
    raw ->
        let
            (flags, rest) = partitionFlags raw
        in
        case rest of
            [] -> CmdHelp
            (cmd : args) -> case T.unpack cmd of
                "-h" -> CmdHelp
                "--help" -> CmdHelp
                "version" -> CmdVersion
                "--version" -> CmdVersion
                "check" -> case args of
                    (path : _) -> CmdCheck (lookupFormat flags) (T.unpack path)
                    [] -> CmdMissingArg "check"
                "build" -> parseBuild flags args
                "run" -> parseRun flags args
                "deps" -> case args of
                    (path : _) -> CmdDeps (lookupFormat flags) (T.unpack path)
                    [] -> CmdDeps (lookupFormat flags) "."
                "fmt" -> case args of
                    (path : _) -> CmdFmt (T.unpack path)
                    [] -> CmdMissingArg "fmt"
                "new" -> case args of
                    (name : _) -> CmdNew (T.unpack name)
                    [] -> CmdMissingArg "new"
                "repl" -> CmdRepl
                "lsp" -> CmdLsp (lookupLogFile flags)
                _ -> CmdUnknown cmd


parseBuild :: [Flag] -> [Text] -> Command
parseBuild flags args = case args of
    ("--script" : path : _) -> CmdBuildScript (buildOpts flags) (T.unpack path)
    ["--script"] -> CmdMissingArg "build --script"
    ["--package"] -> CmdBuildPackage (buildOpts flags) "."
    ("--package" : path : _) -> CmdBuildPackage (buildOpts flags) (T.unpack path)
    (path : _) -> CmdBuildScript (buildOpts flags) (T.unpack path)
    [] -> CmdMissingArg "build"


parseRun :: [Flag] -> [Text] -> Command
parseRun flags args = case args of
    (path : _) -> CmdRun (runOpts flags) (T.unpack path)
    [] -> CmdMissingArg "run"


-- ---------------------------------------------------------------------
-- Cross-cutting flag handling
-- ---------------------------------------------------------------------


-- | A parsed cross-cutting flag. Cheap and union-ish so the body of
-- 'parseArgs' stays linear.
data Flag
    = FlagFormat DiagnosticsFormat
    | FlagOut Prelude.FilePath
    | FlagSourcemap
    | FlagRscript
    | FlagLogFile Prelude.FilePath
    | FlagUnknown Text
    deriving (Prelude.Show, Prelude.Eq)


-- | Split argv into flags (anything starting with @--@ that we
-- recognise) and the rest. Unknown @--@ tokens are passed through
-- as positional so subcommand-specific parsers can decide.
partitionFlags :: [Text] -> ([Flag], [Text])
partitionFlags toks =
    Prelude.foldr step ([], []) toks
  where
    step tok (fs, ps) =
        case parseFlag tok of
            Just (FlagUnknown _) -> (fs, tok : ps)
            Just f -> (f : fs, ps)
            Nothing -> (fs, tok : ps)


parseFlag :: Text -> Maybe Flag
parseFlag tok
    | tok == "--diagnostics-format=human" = Just (FlagFormat HumanDiagnostics)
    | tok == "--diagnostics-format=json" = Just (FlagFormat JsonDiagnostics)
    | tok == "--json" = Just (FlagFormat JsonDiagnostics)
    | tok == "--emit-sourcemap" = Just FlagSourcemap
    | tok == "--rscript" = Just FlagRscript
    | "--out=" `T.isPrefixOf` tok =
        Just (FlagOut (T.unpack (T.drop (T.length "--out=") tok)))
    | "--log-file=" `T.isPrefixOf` tok =
        Just (FlagLogFile (T.unpack (T.drop (T.length "--log-file=") tok)))
    | "--diagnostics-format=" `T.isPrefixOf` tok =
        Just (FlagUnknown tok)
    | "--" `T.isPrefixOf` tok = Nothing
    | Prelude.otherwise = Nothing


lookupFormat :: [Flag] -> DiagnosticsFormat
lookupFormat flags =
    case [f | FlagFormat f <- flags] of
        (f : _) -> f
        [] -> HumanDiagnostics


lookupOut :: [Flag] -> Maybe Prelude.FilePath
lookupOut flags =
    case [d | FlagOut d <- flags] of
        (d : _) -> Just d
        [] -> Prelude.Nothing


hasSourcemap :: [Flag] -> Prelude.Bool
hasSourcemap flags = Prelude.not (Prelude.null [() | FlagSourcemap <- flags])


hasRscript :: [Flag] -> Prelude.Bool
hasRscript flags = Prelude.not (Prelude.null [() | FlagRscript <- flags])


lookupLogFile :: [Flag] -> Maybe Prelude.FilePath
lookupLogFile flags =
    case [p | FlagLogFile p <- flags] of
        (p : _) -> Just p
        [] -> Prelude.Nothing


buildOpts :: [Flag] -> BuildOpts
buildOpts flags =
    BuildOpts
        { boFormat = lookupFormat flags
        , boOut = lookupOut flags
        , boSourcemap = hasSourcemap flags
        }


runOpts :: [Flag] -> RunOpts
runOpts flags =
    RunOpts
        { roBuild = buildOpts flags
        , roRscript = hasRscript flags
        }


-- ---------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------


dispatch :: Command -> Prelude.IO Exit.ExitCode
dispatch = \case
    CmdHelp -> printHelp Prelude.>> Prelude.pure Exit.ExitSuccess
    CmdVersion -> cmdVersion
    CmdCheck fmt path -> cmdCheck fmt path
    CmdBuildScript opts path ->
        cmdBuild
            (boFormat opts)
            (boOut opts)
            (boSourcemap opts)
            path
    CmdBuildPackage opts path ->
        cmdBuildPackage
            (boFormat opts)
            (boOut opts)
            (boSourcemap opts)
            path
    CmdRun opts path ->
        cmdRun
            (boFormat (roBuild opts))
            (boOut (roBuild opts))
            (boSourcemap (roBuild opts))
            (roRscript opts)
            path
    CmdDeps fmt path -> cmdDeps fmt path
    CmdFmt path -> cmdFmt path
    CmdNew name -> cmdNew name
    CmdRepl -> cmdRepl
    CmdLsp logPath ->
        Lsp.runServer Lsp.defaultOpts {Lsp.optLogFile = logPath}
    CmdMissingArg name -> do
        TIO.hPutStrLn IO.stderr ("missing argument for: " Prelude.<> name)
        printHelp
        Prelude.pure (Exit.ExitFailure 2)
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
            , "  quonec run --rscript <file>  compile then invoke Rscript on the result"
            , "  quonec check <file.Q>        typecheck without emitting"
            , "  quonec deps [dir]            print the auto-derived runtime deps"
            , "  quonec fmt <file.Q>          format in place"
            , "  quonec repl                  start an interactive session"
            , "  quonec lsp                   speak LSP over stdin/stdout"
            , "  quonec version               print version"
            , "  quonec --help                print this message"
            , ""
            , "Cross-cutting options:"
            , "  --diagnostics-format=FMT     human (default) or json (NDJSON)"
            , "  --out=DIR                    write generated R into DIR"
            , "  --emit-sourcemap             also emit .R.map sidecars"
            , "  --rscript                    on `run`, invoke Rscript directly"
            , "  --log-file=PATH              on `lsp`, mirror traffic to PATH"
            ]
        )
