{-| Implementation of each @quonec@ subcommand.

The argument parser lives in 'Quone.Cli.Main'; this module owns the
actual work each command performs.

-}
module Quone.Cli.Commands
    ( cmdVersion
    , cmdCheck
    , cmdBuild
    , cmdBuildPackage
    , cmdRun
    , cmdFmt
    , cmdNew
    , cmdRepl
      -- * shared helpers
    , compileScript
    , compilePackage
    , writePackage
    )
where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Ast.Source (Program)
import Quone.Ast.Validate (validate)
import qualified Quone.Diagnostic as Diag
import Quone.Diagnostic (Diagnostic (..), render)
import Quone.Generate.Module (generateModule, ModuleArtifact (..))
import Quone.Generate.Package
    ( PackageArtifact (..)
    , defaultPackageInputs
    , generatePackage
    )
import Quone.Generate.R (generateProgram)
import Quone.Parse.Desugar (desugarFile)
import qualified Quone.Position as Position
import qualified Quone.Resolve.Project as Project
import Quone.Type.Infer (inferProgram)
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
cmdCheck :: Prelude.FilePath -> Prelude.IO Exit.ExitCode
cmdCheck path = do
    src <- TIO.readFile path
    case compileScript (T.pack path) src of
        Prelude.Left d -> do
            TIO.hPutStrLn IO.stderr (render d)
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right _ -> do
            TIO.putStrLn "ok"
            Prelude.pure Exit.ExitSuccess


-- | Compile a single .Q file to .R alongside it.
cmdBuild :: Prelude.FilePath -> Prelude.IO Exit.ExitCode
cmdBuild path = do
    src <- TIO.readFile path
    case compileScript (T.pack path) src of
        Prelude.Left d -> do
            TIO.hPutStrLn IO.stderr (render d)
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right (_, rcode) -> do
            let
                outPath = FP.replaceExtension path "R"
            TIO.writeFile outPath rcode
            TIO.putStrLn (T.pack ("wrote " Prelude.++ outPath))
            Prelude.pure Exit.ExitSuccess


-- | Compile and then run the resulting .R via the system R interpreter.
--
-- For v0.0.1 we don't shell out to R from Haskell; we just print the
-- path so the user can run it themselves. A proper @cmdRun@ that
-- invokes Rscript is `[planned]`.
cmdRun :: Prelude.FilePath -> Prelude.IO Exit.ExitCode
cmdRun path = do
    code <- cmdBuild path
    case code of
        Exit.ExitSuccess -> do
            TIO.putStrLn
                ( T.pack ("Run with: Rscript " Prelude.++ FP.replaceExtension path "R"))
            Prelude.pure Exit.ExitSuccess
        other -> Prelude.pure other


-- | Build a multi-module Quone project into an R package directory
-- tree under @<projectDir>/build/@.
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
cmdBuildPackage :: Prelude.FilePath -> Prelude.IO Exit.ExitCode
cmdBuildPackage projectDir = do
    result <- compilePackage projectDir
    case result of
        Prelude.Left d -> do
            TIO.hPutStrLn IO.stderr (render d)
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right pa -> do
            let
                outDir = projectDir FP.</> "build"
            writePackage outDir pa
            TIO.putStrLn (T.pack ("wrote " Prelude.++ outDir))
            TIO.putStrLn
                ( T.pack
                    ( "Generate man/ with: R -e \"roxygen2::roxygenise('"
                        Prelude.++ outDir
                        Prelude.++ "')\""
                    )
                )
            Prelude.pure Exit.ExitSuccess


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


-- | Format a .Q file in place. v0.0.1 stub per
-- LANGUAGE.md section 19.7.
cmdFmt :: Prelude.FilePath -> Prelude.IO Exit.ExitCode
cmdFmt _ = do
    TIO.putStrLn "fmt is not yet implemented (LANGUAGE.md section 19.7)"
    Prelude.pure Exit.ExitSuccess


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


-- | REPL stub for v0.0.1 per LANGUAGE.md section 19.7.
cmdRepl :: Prelude.IO Exit.ExitCode
cmdRepl = do
    TIO.putStrLn "repl is not yet implemented (LANGUAGE.md section 19.7)"
    Prelude.pure Exit.ExitSuccess



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
