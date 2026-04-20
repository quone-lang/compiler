{-| Package-mode generation tests.

Covers stage 9: file-name mapping, NAMESPACE generation,
DESCRIPTION generation, and the package-wide name-collision check
from LANGUAGE.md section 14.6.

E2E tests that actually invoke @roxygen2::roxygenise@ are deferred
to stage 11 (corpus); they require an R installation in CI.

-}
module Test.PackageTests (suite) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Ast.Source
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.IO.Temp as Temp
import Quone.Generate.Module
    ( ModuleArtifact (..)
    , generateModule
    , moduleFileName
    )
import Quone.Cli.Commands (compilePackage, writePackage)
import Quone.Generate.Module (artifactBody, artifactPath)
import Quone.Generate.Package
    ( PackageArtifact (..)
    , PackageInputs
    , defaultPackageInputs
    , generateNamespace
    , generatePackage
    )
import Quone.Parse.Desugar (desugarSource)
import Quone.Resolve.Project
    ( PackageMeta (..)
    , Project (..)
    )
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , assert
    , assertLeft
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "package"
        ( fileNameTests
            ++ moduleArtifactTests
            ++ packageTests
            ++ collisionTests
            ++ e2eTests
        )



-- ---------------------------------------------------------------------
-- File name mapping
-- ---------------------------------------------------------------------


fileNameTests :: [Test]
fileNameTests =
    [ test "package/filename_simple_section_14_6" <|
        Prelude.pure (moduleFileName ["Foo"] === "R/foo.R")
    , test "package/filename_dotted_to_kebab_section_14_6" <|
        Prelude.pure (moduleFileName ["Stats", "Transform"] === "R/stats-transform.R")
    , test "package/filename_three_segments_section_14_6" <|
        Prelude.pure (moduleFileName ["A", "B", "C"] === "R/a-b-c.R")
    ]



-- ---------------------------------------------------------------------
-- Module artifact
-- ---------------------------------------------------------------------


moduleArtifactTests :: [Test]
moduleArtifactTests =
    [ test "package/module_artifact_emits_R_section_14_6" <|
        case desugarSource
            ( T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "x <- 1L"
                ]
            ) of
            Prelude.Right p ->
                let
                    art = generateModule p
                in
                Prelude.pure
                    ( assert
                        (artifactPath art Prelude.== "R/foo.R"
                            Prelude.&& T.isInfixOf "x <- 1L" (artifactBody art))
                        ("got " Prelude.<> T.pack (Prelude.show art))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "package/module_export_requires_at_export_tag_section_14_6" <|
        case desugarSource
            ( T.unlines
                [ "module Foo exporting (x, y)"
                , ""
                , "#' @export"
                , "x <- 1"
                , ""
                , "y <- 2"
                ]
            ) of
            Prelude.Right p ->
                let
                    art = generateModule p
                in
                Prelude.pure (artifactExports art === ["x"])
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "package/module_collects_dplyr_dep_for_verb_section_13_9" <|
        case desugarSource
            ( T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "x <- xs |> filter (a > 0)"
                ]
            ) of
            Prelude.Right p ->
                let
                    art = generateModule p
                in
                Prelude.pure
                    ( assert
                        ("dplyr" `Prelude.elem` artifactDeps art)
                        ("expected dplyr in deps; got " Prelude.<> T.pack (Prelude.show (artifactDeps art)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "package/module_collects_purrr_dep_for_record_update_section_13_7" <|
        case desugarSource
            ( T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "x <- { rec | a = 1 }"
                ]
            ) of
            Prelude.Right p ->
                let
                    art = generateModule p
                in
                Prelude.pure
                    ( assert
                        ("purrr" `Prelude.elem` artifactDeps art)
                        ("expected purrr in deps; got " Prelude.<> T.pack (Prelude.show (artifactDeps art)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]



-- ---------------------------------------------------------------------
-- Package artifact
-- ---------------------------------------------------------------------


packageTests :: [Test]
packageTests =
    [ test "package/generates_description_with_metadata_section_14_6" <|
        case generatePackage (oneModuleInputs simpleProject simpleSrc) of
            Prelude.Right pa ->
                Prelude.pure
                    ( assert
                        (T.isInfixOf "Package: stats" (paDescription pa)
                            Prelude.&& T.isInfixOf "Version: 0.0.1" (paDescription pa))
                        ("got " Prelude.<> paDescription pa)
                    )
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "package/description_includes_roxygen_marker_section_14_6" <|
        case generatePackage (oneModuleInputs simpleProject simpleSrc) of
            Prelude.Right pa ->
                Prelude.pure
                    ( assert
                        (T.isInfixOf "Roxygen: list(markdown = TRUE)" (paDescription pa))
                        "expected Roxygen: line"
                    )
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "package/namespace_lists_at_export_bindings_section_14_6" <|
        case generatePackage (oneModuleInputs simpleProject simpleSrcExported) of
            Prelude.Right pa ->
                Prelude.pure
                    ( assert
                        (T.isInfixOf "export(x)" (paNamespace pa))
                        ("got " Prelude.<> paNamespace pa)
                    )
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "package/dependency_set_includes_dplyr_section_13_9" <|
        case generatePackage (oneModuleInputs simpleProject verbSrc) of
            Prelude.Right pa ->
                Prelude.pure
                    ( assert
                        ("dplyr" `Prelude.elem` paDependencies pa)
                        ("got " Prelude.<> T.pack (Prelude.show (paDependencies pa)))
                    )
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    ]



-- ---------------------------------------------------------------------
-- Collision check
-- ---------------------------------------------------------------------


collisionTests :: [Test]
collisionTests =
    [ test "package/collision_in_two_modules_rejected_section_14_6" <|
        case (desugarSource modA, desugarSource modB) of
            (Prelude.Right pA, Prelude.Right pB) ->
                Prelude.pure
                    ( assertLeft
                        ( generatePackage
                            ( defaultPackageInputs
                                simpleProject
                                [pA, pB]
                            )
                        )
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "package/no_collision_passes_section_14_6" <|
        case (desugarSource modA, desugarSource modCdistinct) of
            (Prelude.Right pA, Prelude.Right pC) ->
                Prelude.pure
                    ( case generatePackage
                            ( defaultPackageInputs
                                simpleProject
                                [pA, pC]
                            )
                      of
                        Prelude.Right _ -> Pass
                        Prelude.Left d -> Fail (T.pack (Prelude.show d))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]



-- ---------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------


simpleProject :: Project
simpleProject =
    Project
        { projectMeta =
            PackageMeta
                { metaName = "stats"
                , metaVersion = "0.0.1"
                , metaDescription = Just "Test package"
                , metaAuthors = ["Test"]
                }
        , projectDependencies = []
        }


oneModuleInputs :: Project -> Text -> PackageInputs
oneModuleInputs proj src =
    case desugarSource src of
        Prelude.Right p -> defaultPackageInputs proj [p]
        Prelude.Left _ -> defaultPackageInputs proj []
  where
    _ = generateNamespace -- silence unused-import warning if any


simpleSrc :: Text
simpleSrc =
    T.unlines
        [ "module Foo exporting (x)"
        , ""
        , "x <- 1"
        ]


simpleSrcExported :: Text
simpleSrcExported =
    T.unlines
        [ "module Foo exporting (x)"
        , ""
        , "#' @export"
        , "x <- 1"
        ]


verbSrc :: Text
verbSrc =
    T.unlines
        [ "module Foo exporting (run)"
        , ""
        , "#' @export"
        , "run <- xs |> filter (a > 0)"
        ]


modA :: Text
modA =
    T.unlines
        [ "module Foo exporting (shared)"
        , ""
        , "#' @export"
        , "shared <- 1"
        ]


modB :: Text
modB =
    T.unlines
        [ "module Bar exporting (shared)"
        , ""
        , "#' @export"
        , "shared <- 2"
        ]


modCdistinct :: Text
modCdistinct =
    T.unlines
        [ "module Bar exporting (other)"
        , ""
        , "#' @export"
        , "other <- 2"
        ]



-- ---------------------------------------------------------------------
-- End-to-end (compile project on disk, write artifact tree)
-- ---------------------------------------------------------------------


e2eTests :: [Test]
e2eTests =
    [ test "package/e2e_compiles_multimodule_project_section_14_6" <|
        Temp.withSystemTempDirectory "quone-e2e-" <| \root -> do
            -- Lay out a minimal two-module project on disk.
            let
                srcDir = root FP.</> "src"
                fooDir = srcDir FP.</> "Foo"
            Dir.createDirectoryIfMissing Prelude.True fooDir
            TIO.writeFile
                (root FP.</> "quone.toml")
                ( T.unlines
                    [ "[package]"
                    , "name = \"e2e\""
                    , "version = \"0.0.1\""
                    ]
                )
            TIO.writeFile
                (srcDir FP.</> "Bar.Q")
                ( T.unlines
                    [ "module Bar exporting (greet)"
                    , ""
                    , "#' @export"
                    , "greet <- \"hi\""
                    ]
                )
            TIO.writeFile
                (fooDir FP.</> "Baz.Q")
                ( T.unlines
                    [ "module Foo.Baz exporting (n)"
                    , ""
                    , "#' @export"
                    , "n <- 7"
                    ]
                )
            -- Compile + write.
            result <- compilePackage root
            case result of
                Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
                Prelude.Right pa -> do
                    let outDir = root FP.</> "build"
                    writePackage outDir pa
                    -- All the expected files should exist.
                    descExists <- Dir.doesFileExist (outDir FP.</> "DESCRIPTION")
                    nsExists <- Dir.doesFileExist (outDir FP.</> "NAMESPACE")
                    barExists <- Dir.doesFileExist (outDir FP.</> "R" FP.</> "bar.R")
                    bazExists <- Dir.doesFileExist (outDir FP.</> "R" FP.</> "foo-baz.R")
                    Prelude.pure
                        ( assert
                            (descExists Prelude.&& nsExists Prelude.&& barExists Prelude.&& bazExists)
                            ( T.pack
                                ( "missing artifact: DESCRIPTION="
                                    Prelude.++ Prelude.show descExists
                                    Prelude.++ " NAMESPACE="
                                    Prelude.++ Prelude.show nsExists
                                    Prelude.++ " bar.R="
                                    Prelude.++ Prelude.show barExists
                                    Prelude.++ " foo-baz.R="
                                    Prelude.++ Prelude.show bazExists
                                )
                            )
                        )
    , test "package/e2e_emits_export_for_at_export_only_section_14_6" <|
        Temp.withSystemTempDirectory "quone-e2e-" <| \root -> do
            let srcDir = root FP.</> "src"
            Dir.createDirectoryIfMissing Prelude.True srcDir
            TIO.writeFile
                (root FP.</> "quone.toml")
                ( T.unlines
                    [ "[package]"
                    , "name = \"e2e\""
                    , "version = \"0.0.1\""
                    ]
                )
            TIO.writeFile
                (srcDir FP.</> "Mixed.Q")
                ( T.unlines
                    [ "module Mixed exporting (a, b)"
                    , ""
                    , "#' @export"
                    , "a <- 1"
                    , ""
                    , "b <- 2"
                    ]
                )
            result <- compilePackage root
            case result of
                Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
                Prelude.Right pa ->
                    Prelude.pure
                        ( assert
                            ( T.isInfixOf "export(a)" (paNamespace pa)
                                Prelude.&& Prelude.not
                                    (T.isInfixOf "export(b)" (paNamespace pa))
                            )
                            ("got NAMESPACE: " Prelude.<> paNamespace pa)
                        )
    ]
