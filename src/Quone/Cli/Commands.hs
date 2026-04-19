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
    , cmdBuildPackage
    , cmdRun
    , cmdDeps
    , cmdFmt
    , cmdNew
    , cmdRepl
      -- * shared helpers
    , compileScript
    , compilePackage
    , writePackage
    , emitDiagnostic
    , emitDiagnostics
    )
where

import qualified Control.Monad as CMonad
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
import Quone.Generate.Module (generateModule, ModuleArtifact (..))
import Quone.Generate.Package
    ( PackageArtifact (..)
    , defaultPackageInputs
    , generatePackage
    )
import Quone.Generate.R (generateProgram)
import Quone.Generate.SourceMap
    ( SourceMap (..)
    , buildSourceMap
    , encodeSourceMap
    )
import Quone.Parse.Desugar (desugarFile)
import qualified Quone.Position as Position
import qualified Quone.Repl.Driver as Repl
import qualified Quone.Resolve.Project as Project
import Quone.Type.Infer (inferProgram)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import qualified System.IO as IO
import qualified System.Process as Proc
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
    -> Prelude.Bool
    -> Prelude.FilePath
    -> Prelude.IO Exit.ExitCode
cmdBuild fmt outDir sourcemap path = do
    src <- TIO.readFile path
    case compileScript (T.pack path) src of
        Prelude.Left d -> do
            emitDiagnostic fmt d
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right (prog, rcode) -> do
            let
                outPath = case outDir of
                    Prelude.Nothing -> FP.replaceExtension path "R"
                    Just dir ->
                        dir
                            FP.</> FP.takeFileName
                                (FP.replaceExtension path "R")
            Dir.createDirectoryIfMissing Prelude.True (FP.takeDirectory outPath)
            TIO.writeFile outPath rcode
            CMonad.when sourcemap (writeSourceMap path outPath prog rcode)
            case fmt of
                JsonDiagnostics -> Prelude.pure Exit.ExitSuccess
                HumanDiagnostics -> do
                    TIO.putStrLn (T.pack ("wrote " Prelude.++ outPath))
                    Prelude.pure Exit.ExitSuccess


-- | Compile and then optionally invoke @Rscript@ on the result.
--
-- When @rscript@ is 'Prelude.False' the command keeps its v0.0.1
-- behaviour of just printing the suggested @Rscript ...@ command. When
-- 'Prelude.True' it shells out to @Rscript@ and returns its exit code.
cmdRun
    :: DiagnosticsFormat
    -> Maybe Prelude.FilePath
    -> Prelude.Bool
    -> Prelude.Bool
    -> Prelude.FilePath
    -> Prelude.IO Exit.ExitCode
cmdRun fmt outDir sourcemap rscript path = do
    code <- cmdBuild fmt outDir sourcemap path
    case code of
        Exit.ExitSuccess -> do
            let
                outPath = case outDir of
                    Prelude.Nothing -> FP.replaceExtension path "R"
                    Just dir ->
                        dir
                            FP.</> FP.takeFileName
                                (FP.replaceExtension path "R")
            if rscript
                then do
                    (_, _, _, ph) <-
                        Proc.createProcess
                            (Proc.proc "Rscript" [outPath])
                    Proc.waitForProcess ph
                else do
                    CMonad.when
                        (fmt Prelude.== HumanDiagnostics)
                        (TIO.putStrLn
                            ( T.pack
                                ( "Run with: Rscript " Prelude.++ outPath)))
                    Prelude.pure Exit.ExitSuccess
        other -> Prelude.pure other


-- | Build a multi-module Quone project into an R package directory
-- tree under @<projectDir>/build/@ (or @<outDir>@ when set).
--
-- Layout per LANGUAGE.md section 14.6:
--
--     build/
--       DESCRIPTION
--       NAMESPACE
--       R/
--         <module>.R   (one per .Q file, kebab-cased)
--
-- @roxygen2::roxygenise@ should be invoked over the resulting
-- directory to generate @man/@ entries; the CLI prints the suggested
-- command on success.
cmdBuildPackage
    :: DiagnosticsFormat
    -> Maybe Prelude.FilePath
    -> Prelude.Bool
    -> Prelude.FilePath
    -> Prelude.IO Exit.ExitCode
cmdBuildPackage fmt outDir _sourcemap projectDir = do
    result <- compilePackage projectDir
    case result of
        Prelude.Left d -> do
            emitDiagnostic fmt d
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right pa -> do
            let
                outRoot = case outDir of
                    Just d -> d
                    Prelude.Nothing -> projectDir FP.</> "build"
            writePackage outRoot pa
            case fmt of
                JsonDiagnostics -> Prelude.pure Exit.ExitSuccess
                HumanDiagnostics -> do
                    TIO.putStrLn (T.pack ("wrote " Prelude.++ outRoot))
                    TIO.putStrLn
                        ( T.pack
                            ( "Generate man/ with: R -e \"roxygen2::roxygenise('"
                                Prelude.++ outRoot
                                Prelude.++ "')\""
                            )
                        )
                    Prelude.pure Exit.ExitSuccess


-- | Print the auto-derived runtime dependency set for a project.
--
-- Implements LANGUAGE.md section 13.9: the set is the union of all R
-- packages the compiled output calls. Output format mirrors NDJSON
-- when 'JsonDiagnostics' is selected, one JSON object per line:
--
-- @
-- {"package":"dplyr"}
-- {"package":"purrr"}
-- {"package":"readr"}
-- @
--
-- The human format prints one package name per line.
cmdDeps :: DiagnosticsFormat -> Prelude.FilePath -> Prelude.IO Exit.ExitCode
cmdDeps fmt projectDir = do
    result <- compilePackage projectDir
    case result of
        Prelude.Left d -> do
            emitDiagnostic fmt d
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right pa -> do
            let
                deps = paDependencies pa
            case fmt of
                JsonDiagnostics ->
                    Prelude.mapM_ (TIO.putStrLn Prelude.. depJson) deps
                HumanDiagnostics ->
                    Prelude.mapM_ TIO.putStrLn deps
            Prelude.pure Exit.ExitSuccess


depJson :: Text -> Text
depJson pkg =
    "{\"package\":\""
        Prelude.<> T.replace "\"" "\\\"" pkg
        Prelude.<> "\"}"


-- | Write a 'PackageArtifact' to a directory tree.
writePackage
    :: Prelude.FilePath
    -> PackageArtifact
    -> Prelude.IO ()
writePackage outDir pa = do
    Dir.createDirectoryIfMissing Prelude.True outDir
    Dir.createDirectoryIfMissing Prelude.True (outDir FP.</> "R")
    TIO.writeFile (outDir FP.</> "DESCRIPTION") (paDescription pa)
    TIO.writeFile (outDir FP.</> "NAMESPACE") (paNamespace pa)
    Prelude.mapM_
        (\m -> do
            let
                target = outDir FP.</> artifactPath m
            Dir.createDirectoryIfMissing Prelude.True (FP.takeDirectory target)
            TIO.writeFile target (artifactBody m))
        (paModules pa)


-- | Format a .Q file in place.
--
-- Wraps the elm-format-style formatter in 'Quone.Format.Format'. See
-- LANGUAGE.md section 3.2.1 (naming conventions) and the planned
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


-- | Scaffold a new project: a quone.toml plus src/Main.Q.
cmdNew :: Prelude.FilePath -> Prelude.IO Exit.ExitCode
cmdNew name = do
    let
        root = name
        srcDir = root FP.</> "src"
        toml = root FP.</> "quone.toml"
        main_ = srcDir FP.</> "Main.Q"

        tomlContent =
            T.unlines
                [ "[package]"
                , "name = \"" Prelude.<> T.pack name Prelude.<> "\""
                , "version = \"0.0.1\""
                ]
        mainContent =
            T.unlines
                [ "module Main exporting (main)"
                , ""
                , "#' Entry point."
                , "#' @export"
                , "main <- 1 + 1"
                ]

    Dir.createDirectoryIfMissing Prelude.True srcDir
    TIO.writeFile toml tomlContent
    TIO.writeFile main_ mainContent
    TIO.putStrLn (T.pack ("created " Prelude.++ root))
    Prelude.pure Exit.ExitSuccess


-- | Start an interactive REPL session.
--
-- Delegates to 'Quone.Repl.Driver.runRepl' which spawns a long-lived
-- @Rscript@ subprocess and feeds it the lowering of every entered
-- expression.
cmdRepl :: Prelude.IO Exit.ExitCode
cmdRepl = Repl.runRepl Repl.defaultOpts



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
-- Source map writer
-- ---------------------------------------------------------------------


writeSourceMap
    :: Prelude.FilePath
    -> Prelude.FilePath
    -> Program
    -> Text
    -> Prelude.IO ()
writeSourceMap srcPath outPath prog _rcode =
    let
        sm =
            SourceMap
                { smGenerated = outPath
                , smSource = srcPath
                , smEntries = buildSourceMap prog
                }
    in
    TIO.writeFile (outPath Prelude.++ ".map") (encodeSourceMap sm)


-- ---------------------------------------------------------------------
-- Pipeline drivers
-- ---------------------------------------------------------------------


-- | Compile one source file. Returns the typed AST and emitted R, or
-- the first diagnostic produced.
compileScript
    :: Text                                  -- ^ filename for diagnostics
    -> Text                                  -- ^ source contents
    -> Prelude.Either Diagnostic (Program, Text)
compileScript filename src = do
    prog <- desugarFile filename src
    case validate prog of
        (d : _) -> Prelude.Left d
        [] -> Prelude.pure ()
    _typed <- inferProgram prog
    Prelude.pure (prog, generateProgram prog)


-- | Compile a multi-module project rooted at @projectDir@.
--
-- Looks for @quone.toml@ in the directory, parses it, then walks
-- @src\/**\/*.Q@ and produces a 'PackageArtifact'. The CLI is
-- responsible for writing the artifact to disk (and shelling out to
-- @roxygen2@ if requested).
compilePackage
    :: Prelude.FilePath
    -> Prelude.IO (Prelude.Either Diagnostic PackageArtifact)
compilePackage projectDir = do
    let tomlPath = projectDir FP.</> "quone.toml"
    tomlExists <- Dir.doesFileExist tomlPath
    if Prelude.not tomlExists
        then
            Prelude.pure
                ( Prelude.Left
                    Diagnostic
                        { diagSeverity = Diag.Error
                        , diagCategory = Diag.FileLoading
                        , diagSpan = Position.emptySpan
                        , diagMessage = "no quone.toml found in " Prelude.<> T.pack projectDir
                        , diagHint = Just "run `quonec new <name>` first"
                        }
                )
        else do
            tomlSrc <- TIO.readFile tomlPath
            case Project.parseProjectToml (T.pack tomlPath) tomlSrc of
                Prelude.Left d -> Prelude.pure (Prelude.Left d)
                Prelude.Right proj -> do
                    qFiles <- listSourceFiles (projectDir FP.</> "src")
                    progs <- Prelude.traverse readProgram qFiles
                    case sequenceErrors progs of
                        Prelude.Left d -> Prelude.pure (Prelude.Left d)
                        Prelude.Right okProgs ->
                            Prelude.pure
                                (generatePackage
                                    (defaultPackageInputs proj okProgs))


readProgram
    :: Prelude.FilePath
    -> Prelude.IO (Prelude.Either Diagnostic Program)
readProgram path = do
    src <- TIO.readFile path
    Prelude.pure (desugarFile (T.pack path) src)


sequenceErrors
    :: [Prelude.Either Diagnostic a]
    -> Prelude.Either Diagnostic [a]
sequenceErrors = List.foldr step (Prelude.Right [])
  where
    step (Prelude.Left d) _ = Prelude.Left d
    step (Prelude.Right _) (Prelude.Left d) = Prelude.Left d
    step (Prelude.Right x) (Prelude.Right xs) = Prelude.Right (x : xs)


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
