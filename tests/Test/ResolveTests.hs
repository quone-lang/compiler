{-| Resolver and project-model tests.

Covers stage 5: cross-module visibility per LANGUAGE.md section 4.5
and the minimal `quone.toml` schema per section 14.

-}
module Test.ResolveTests (suite) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Parse.Desugar (desugarSource)
import Quone.Position (emptySpan)
import Quone.Resolve.Names
    ( LocalName
    , ModuleSymbols
    , collectSymbols
    , modSymLocals
    , resolveProgram
    , resolveProject
    )
import Quone.Resolve.Project
    ( Dependency (..)
    , PackageMeta (..)
    , Project (..)
    , discoverModulePath
    , fileToModulePath
    , modulePathToFile
    , parseProjectToml
    )
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , assert
    , assertLeft
    , assertRight
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "resolve"
        ( projectTomlTests
            ++ pathDiscoveryTests
            ++ singleModuleTests
            ++ crossModuleTests
        )



-- ---------------------------------------------------------------------
-- quone.toml
-- ---------------------------------------------------------------------


projectTomlTests :: [Test]
projectTomlTests =
    [ test "resolve/toml_minimal_section_14" <|
        let
            src =
                T.unlines
                    [ "[package]"
                    , "name = \"stats\""
                    , "version = \"0.0.1\""
                    ]
        in
        case parseProjectToml "quone.toml" src of
            Prelude.Right p ->
                Prelude.pure
                    (assert
                        (metaName (projectMeta p) Prelude.== "stats"
                            Prelude.&& metaVersion (projectMeta p) Prelude.== "0.0.1"
                            Prelude.&& Prelude.null (projectDependencies p)
                        )
                        ("got " Prelude.<> T.pack (Prelude.show p)))
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "resolve/toml_with_deps_section_14" <|
        let
            src =
                T.unlines
                    [ "[package]"
                    , "name = \"stats\""
                    , "version = \"0.0.1\""
                    , ""
                    , "[dependencies]"
                    , "purrr = \">= 1.0\""
                    , "dplyr = \">= 1.1\""
                    ]
        in
        case parseProjectToml "quone.toml" src of
            Prelude.Right p ->
                Prelude.pure
                    ( Prelude.length (projectDependencies p) === 2
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "resolve/toml_authors_array_section_14" <|
        let
            src =
                T.unlines
                    [ "[package]"
                    , "name = \"stats\""
                    , "version = \"0.0.1\""
                    , "authors = [\"Andrew McNally\", \"Other\"]"
                    ]
        in
        case parseProjectToml "quone.toml" src of
            Prelude.Right p ->
                Prelude.pure
                    ( Prelude.length (metaAuthors (projectMeta p)) === 2
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "resolve/toml_missing_name_rejected_section_14" <|
        let
            src =
                T.unlines
                    [ "[package]"
                    , "version = \"0.0.1\""
                    ]
        in
        Prelude.pure (assertLeft (parseProjectToml "quone.toml" src))
    , test "resolve/toml_comments_ignored_section_14" <|
        let
            src =
                T.unlines
                    [ "# project metadata"
                    , "[package]   # the package section"
                    , "name = \"stats\"   # required"
                    , "version = \"0.0.1\""
                    ]
        in
        Prelude.pure (assertRight (parseProjectToml "quone.toml" src))
    ]



-- ---------------------------------------------------------------------
-- File path / module path
-- ---------------------------------------------------------------------


pathDiscoveryTests :: [Test]
pathDiscoveryTests =
    [ test "resolve/file_to_module_path_section_14_6" <|
        Prelude.pure
            (fileToModulePath "src/Stats/Transform.Q"
                === Just ["Stats", "Transform"])
    , test "resolve/module_path_to_file_section_14_6" <|
        Prelude.pure
            (modulePathToFile ["Stats", "Transform"]
                === "src/Stats/Transform.Q")
    , test "resolve/discover_section_14_6" <|
        Prelude.pure
            (discoverModulePath "src/Foo.Q" === Just ["Foo"])
    , test "resolve/non_src_path_returns_nothing_section_14_6" <|
        Prelude.pure
            (discoverModulePath "Foo.Q" === Nothing)
    ]



-- ---------------------------------------------------------------------
-- Single-module resolver
-- ---------------------------------------------------------------------


singleModuleTests :: [Test]
singleModuleTests =
    [ test "resolve/duplicate_decl_rejected_section_4_3" <|
        let
            src =
                T.unlines
                    [ "x <- 1"
                    , "x <- 2"
                    ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.not (Prelude.null (resolveProgram p)))
                        "expected a duplicate-binding diagnostic"
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "resolve/no_duplicates_passes_section_4_3" <|
        let
            src =
                T.unlines
                    [ "x <- 1"
                    , "y <- 2"
                    ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.null (resolveProgram p))
                        ("expected no diagnostics; got "
                            Prelude.<> T.pack (Prelude.show (resolveProgram p)))
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "resolve/collect_symbols_includes_constructors_section_6_8" <|
        let
            src =
                T.unlines
                    [ "type Maybe a"
                    , "    <- Nothing"
                    , "     | Just a"
                    ]
        in
        case desugarSource src of
            Prelude.Right p ->
                let
                    syms = collectSymbols p
                in
                Prelude.pure
                    ( assert
                        (Map.member "Maybe" (modulesymLocals syms)
                            Prelude.&& Map.member "Just" (modulesymLocals syms)
                            Prelude.&& Map.member "Nothing" (modulesymLocals syms)
                        )
                        "expected Maybe, Just, Nothing in symbol table"
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    ]


-- Tiny rebrand of the imported field accessor for readability in the
-- assertion above.
modulesymLocals :: ModuleSymbols -> Map.Map Text LocalName
modulesymLocals = modSymLocals



-- ---------------------------------------------------------------------
-- Cross-module resolver
-- ---------------------------------------------------------------------


crossModuleTests :: [Test]
crossModuleTests =
    [ test "resolve/import_visible_export_section_4_5" <|
        case (desugarSource sourceModSrc, desugarSource importerSrc) of
            (Prelude.Right sourceProg, Prelude.Right importerProg) ->
                let
                    syms = Map.singleton
                        (case programModule sourceProg of
                            Just m -> modulePath m
                            Nothing -> [])
                        (collectSymbols sourceProg)
                in
                Prelude.pure
                    ( assert
                        (Prelude.null (resolveProject syms importerProg))
                        ("expected no diagnostics; got "
                            Prelude.<> T.pack (Prelude.show (resolveProject syms importerProg)))
                    )
            _ -> Prelude.pure (Fail "could not parse fixtures")
    , test "resolve/import_invisible_name_rejected_section_4_5" <|
        let
            sourceSrc =
                T.unlines
                    [ "module Stats.Transform exporting (rmse)"
                    , ""
                    , "rmse <- 1"
                    , "internal_helper <- 2"
                    ]
            importerSrcLocal =
                T.unlines
                    [ "module Main exporting (..)"
                    , ""
                    , "import Stats.Transform.internal_helper"
                    ]
        in
        case (desugarSource sourceSrc, desugarSource importerSrcLocal) of
            (Prelude.Right sourceProg, Prelude.Right importerProg) ->
                let
                    syms = Map.singleton
                        (modulePathOf sourceProg)
                        (collectSymbols sourceProg)
                    diags = resolveProject syms importerProg
                in
                Prelude.pure
                    ( assert
                        (Prelude.length diags Prelude.== 1)
                        ("expected exactly one diagnostic; got "
                            Prelude.<> T.pack (Prelude.show diags))
                    )
            _ -> Prelude.pure (Fail "could not parse fixtures")
    , test "resolve/import_unknown_module_rejected_section_4_5" <|
        let
            importerSrcLocal =
                T.unlines
                    [ "module Main exporting (..)"
                    , ""
                    , "import Other.Module.thing"
                    ]
        in
        case desugarSource importerSrcLocal of
            Prelude.Right importerProg ->
                Prelude.pure
                    ( assert
                        (Prelude.not
                            (Prelude.null (resolveProject Map.empty importerProg))
                        )
                        "expected an unknown-module diagnostic"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]


sourceModSrc :: Text
sourceModSrc =
    T.unlines
        [ "module Stats.Transform exporting (normalize, rmse)"
        , ""
        , "normalize <- 1"
        , "rmse <- 2"
        ]


importerSrc :: Text
importerSrc =
    T.unlines
        [ "module Main exporting (..)"
        , ""
        , "import Stats.Transform.normalize"
        ]


-- | Pull a 'ModulePath' out of a parsed source module, since we can't
-- write @["Stats", "Transform"]@ as a literal (UpperName has no
-- IsString instance).
modulePathOf :: Program -> ModulePath
modulePathOf p =
    case programModule p of
        Just m -> modulePath m
        Nothing -> []
