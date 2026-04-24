{-| Argument parsing for the @quonec@ CLI.

Parses the command-line, then dispatches to 'Quone.Cli.Commands'.
For the initial release we use a hand-rolled argument parser to avoid pulling in
@optparse-applicative@ as a dependency for one subcommand layer.

The parser accepts a small set of cross-cutting options before or
after the subcommand:

* @--diagnostics-format=human|json@ -- machine-readable diagnostic
  output for editors and the R companion package.
* @--out=DIR@ -- override the output directory for @compile@.

-}
module Quone.Cli.Main
    ( runCli
    , parseArgs
    , Command (..)
    , BuildOpts (..)
    , defaultBuildOpts
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
    }
    deriving (Prelude.Show, Prelude.Eq)


defaultBuildOpts :: BuildOpts
defaultBuildOpts =
    BuildOpts
        { boFormat = HumanDiagnostics
        , boOut = Prelude.Nothing
        }


-- | The parsed subcommand to run.
data Command
    = CmdVersion
    | CmdHelp
    | CmdCheck DiagnosticsFormat Prelude.FilePath
    | CmdBuildScript BuildOpts Prelude.FilePath
    | CmdCompileDir BuildOpts Prelude.FilePath
    | CmdFmt Prelude.FilePath
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
                "compile" -> parseBuild flags args
                "compile-dir" -> case args of
                    (path : _) -> CmdCompileDir (buildOpts flags) (T.unpack path)
                    [] -> CmdMissingArg "compile-dir"
                "build" -> parseBuild flags args
                "fmt" -> case args of
                    (path : _) -> CmdFmt (T.unpack path)
                    [] -> CmdMissingArg "fmt"
                "lsp" -> CmdLsp (lookupLogFile flags)
                _ -> CmdUnknown cmd


parseBuild :: [Flag] -> [Text] -> Command
parseBuild flags args = case args of
    ("--script" : path : _) -> CmdBuildScript (buildOpts flags) (T.unpack path)
    ["--script"] -> CmdMissingArg "build --script"
    (path : _) -> CmdBuildScript (buildOpts flags) (T.unpack path)
    [] -> CmdMissingArg "build"


-- ---------------------------------------------------------------------
-- Cross-cutting flag handling
-- ---------------------------------------------------------------------


-- | A parsed cross-cutting flag. Cheap and union-ish so the body of
-- 'parseArgs' stays linear.
data Flag
    = FlagFormat DiagnosticsFormat
    | FlagOut Prelude.FilePath
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
            path
    CmdCompileDir opts path ->
        cmdCompileDir
            (boFormat opts)
            (boOut opts)
            path
    CmdFmt path -> cmdFmt path
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
            [ "quonec - the Quone language compiler"
            , ""
            , "Usage:"
            , "  quonec compile <file.Q>      compile a file to .R"
            , "  quonec compile-dir <dir>     compile all .Q files under a directory"
            , "  quonec build <file.Q>        alias for compile"
            , "  quonec check <file.Q>        typecheck without emitting"
            , "  quonec fmt <file.Q>          format in place"
            , "  quonec lsp                   speak LSP over stdin/stdout"
            , "  quonec version               print version"
            , "  quonec --help                print this message"
            , ""
            , "Cross-cutting options:"
            , "  --diagnostics-format=FMT     human (default) or json (NDJSON)"
            , "  --out=DIR                    write generated R into DIR"
            , "  --log-file=PATH              on `lsp`, mirror traffic to PATH"
            ]
        )
