{-| Implementation of each @quonec@ subcommand.

The argument parser lives in 'Quone.Cli.Main'; this module owns the
actual work each command performs.

Every diagnostic-emitting command takes a 'DiagnosticsFormat' so the
R companion package and editors can request NDJSON output instead of
the default human-formatted blocks.

-}
module Quone.Cli.Commands
    ( cmdVersion
    , cmdCheck
    , cmdBuild
    , cmdCompileDir
    , cmdFmt
      -- * shared helpers
    , compileScript
    , emitDiagnostic
    , emitDiagnostics
    )
where

import qualified Control.Monad as CMonad
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Ast.Source (Program)
import Quone.Ast.Validate (validate)
import qualified Quone.Diagnostic as Diag
import Quone.Diagnostic
    ( Diagnostic (..)
    , DiagnosticsFormat (..)
    , render
    )
import qualified Quone.Diagnostic.Json as DiagJson
import qualified Quone.Format.Format as Fmt
import Quone.Generate.R (generateProgram)
import Quone.Parse.Desugar (desugarFile)
import Quone.Prelude.Load (LoadedPrelude (..), loadPrelude)
import Quone.Type.Infer
    ( inferProgramFrom
    )
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified Prelude



-- | Print the compiler version. Returns ExitSuccess.
cmdVersion :: Prelude.IO Exit.ExitCode
cmdVersion = do
    TIO.putStrLn "quonec 0.0.1"
    Prelude.pure Exit.ExitSuccess


-- | Type-check without emitting R. Used by editors / CI.
cmdCheck :: DiagnosticsFormat -> Prelude.FilePath -> Prelude.IO Exit.ExitCode
cmdCheck fmt path = do
    src <- TIO.readFile path
    case compileScript (T.pack path) src of
        Prelude.Left d -> do
            emitDiagnostic fmt d
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right _ ->
            case fmt of
                JsonDiagnostics -> Prelude.pure Exit.ExitSuccess
                HumanDiagnostics -> do
                    TIO.putStrLn "ok"
                    Prelude.pure Exit.ExitSuccess


-- | Compile a single .Q file to .R.
--
-- @outDir@ overrides the directory the @.R@ is written to (and the
-- @.R.map@ sidecar if @sourcemap@ is set). When @Nothing@, the @.R@
-- is written next to the input file as before.
cmdBuild
    :: DiagnosticsFormat
    -> Maybe Prelude.FilePath
    -> Prelude.FilePath
    -> Prelude.IO Exit.ExitCode
cmdBuild fmt outDir path = do
    src <- TIO.readFile path
    case compileScript (T.pack path) src of
        Prelude.Left d -> do
            emitDiagnostic fmt d
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right (_prog, rcode) -> do
            let
                outPath = case outDir of
                    Prelude.Nothing -> FP.replaceExtension path "R"
                    Just dir ->
                        dir
                            FP.</> FP.takeFileName
                                (FP.replaceExtension path "R")
            Dir.createDirectoryIfMissing Prelude.True (FP.takeDirectory outPath)
            TIO.writeFile outPath rcode
            case fmt of
                JsonDiagnostics -> Prelude.pure Exit.ExitSuccess
                HumanDiagnostics -> do
                    TIO.putStrLn (T.pack ("wrote " Prelude.++ outPath))
                    Prelude.pure Exit.ExitSuccess


cmdCompileDir
    :: DiagnosticsFormat
    -> Maybe Prelude.FilePath
    -> Prelude.FilePath
    -> Prelude.IO Exit.ExitCode
cmdCompileDir fmt outDir dir = do
    qFiles <- listSourceFiles dir
    codes <- Prelude.traverse (cmdBuild fmt outDir) qFiles
    Prelude.pure (combineExit codes)


-- | Format a .Q file in place.
--
-- Wraps the elm-format-style formatter in 'Quone.Format.Format'. See
-- LANGUAGE2.md section 3.2.1 (naming conventions) and the planned
-- canonical layout rules described in the project plan.
cmdFmt :: Prelude.FilePath -> Prelude.IO Exit.ExitCode
cmdFmt path = do
    isDir <- Dir.doesDirectoryExist path
    if isDir
        then formatProject path
        else formatFile path


formatFile :: Prelude.FilePath -> Prelude.IO Exit.ExitCode
formatFile path = do
    src <- TIO.readFile path
    case Fmt.format (T.pack path) src of
        Prelude.Left d -> do
            emitDiagnostic HumanDiagnostics d
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right out -> do
            CMonad.when (out Prelude./= src) (TIO.writeFile path out)
            Prelude.pure Exit.ExitSuccess


formatProject :: Prelude.FilePath -> Prelude.IO Exit.ExitCode
formatProject root = do
    qFiles <- listSourceFiles (root FP.</> "src")
    codes <- Prelude.traverse formatFile qFiles
    Prelude.pure (combineExit codes)


combineExit :: [Exit.ExitCode] -> Exit.ExitCode
combineExit = Prelude.foldr step Exit.ExitSuccess
  where
    step Exit.ExitSuccess acc = acc
    step e _ = e


-- ---------------------------------------------------------------------
-- Diagnostic emission
-- ---------------------------------------------------------------------


-- | Print a single diagnostic to stderr in the requested format.
emitDiagnostic :: DiagnosticsFormat -> Diagnostic -> Prelude.IO ()
emitDiagnostic fmt d = case fmt of
    HumanDiagnostics -> TIO.hPutStrLn IO.stderr (render d)
    JsonDiagnostics -> TIO.hPutStrLn IO.stderr (DiagJson.encodeDiagnostic d)


-- | Print a list of diagnostics to stderr in the requested format.
emitDiagnostics :: DiagnosticsFormat -> [Diagnostic] -> Prelude.IO ()
emitDiagnostics fmt = Prelude.mapM_ (emitDiagnostic fmt)

-- ---------------------------------------------------------------------
-- Pipeline drivers
-- ---------------------------------------------------------------------


-- | Compile one source file. Returns the typed AST and emitted R, or
-- the first diagnostic produced.
--
-- The embedded prelude is loaded once at the start of compilation
-- (see 'Quone.Prelude.Load.loadPrelude'); its typing environment
-- seeds the user-program inference. A failure to load the prelude
-- itself is treated as a compiler-development bug surfaced as the
-- first diagnostic.
compileScript
    :: Text                                  -- ^ filename for diagnostics
    -> Text                                  -- ^ source contents
    -> Prelude.Either Diagnostic (Program, Text)
compileScript filename src = do
    loaded <- loadPrelude
    prog <- desugarFile filename src
    case validate prog of
        (d : _) -> Prelude.Left d
        [] -> Prelude.pure ()
    _typed <- inferProgramFrom (preludeEnv loaded) prog
    Prelude.pure (prog, generateProgram prog)


-- | Find every .Q file under a directory, recursively.
listSourceFiles :: Prelude.FilePath -> Prelude.IO [Prelude.FilePath]
listSourceFiles dir = do
    exists <- Dir.doesDirectoryExist dir
    if Prelude.not exists
        then Prelude.pure []
        else do
            entries <- Dir.listDirectory dir
            let
                full = Prelude.fmap (dir FP.</>) entries
            collected <-
                Prelude.traverse
                    (\p -> do
                        isDir <- Dir.doesDirectoryExist p
                        if isDir
                            then listSourceFiles p
                            else
                                if FP.takeExtension p Prelude.== ".Q"
                                    then Prelude.pure [p]
                                    else Prelude.pure [])
                    full
            Prelude.pure (Prelude.concat collected)
