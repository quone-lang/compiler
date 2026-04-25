module Test.ResolveTests (suite) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import Quone.Cli.Commands (compileScript)
import Quone.Diagnostic (Diagnostic (..))
import Quone.Lsp.Compile (CompileResult (..), compileText)
import Quone.Parse.Desugar (desugarSource)
import qualified Quone.Resolve.Names as Resolve
import Quone.Resolve.Project
    ( Dependency (..)
    , PackageMeta (..)
    , Project (..)
    , parseProjectToml
    )
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..), (===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "resolve"
        [ Harness.test "resolve/collects_module_symbols" <|
            case desugarSource sourceModule of
                Prelude.Left d -> Prelude.pure (Fail (showText d))
                Prelude.Right prog ->
                    let
                        syms = Resolve.collectSymbols prog
                    in
                    Prelude.pure
                        ( Map.member "normalize" (Resolve.modSymLocals syms)
                            === Prelude.True
                        )
        , Harness.test "resolve/rejects_duplicate_import_selection" <|
            expectResolveProgramFail
                ( T.unlines
                    [ "import Stats (normalize, normalize)"
                    , "x <- 1"
                    ]
                )
                "duplicate top-level binding"
        , Harness.test "resolve/project_rejects_hidden_import" <|
            expectProjectResolveFail
                sourceModule
                ( T.unlines
                    [ "module App exporting (x)"
                    , "import Stats.private"
                    , "x <- private"
                    ]
                )
                "does not export"
        , Harness.test "resolve/project_import_all_requires_defined_name" <|
            expectProjectResolveFail
                ( T.unlines
                    [ "module Stats exporting (..)"
                    , "public <- 1"
                    ]
                )
                ( T.unlines
                    [ "module App exporting (x)"
                    , "import Stats.missing"
                    , "x <- 1"
                    ]
                )
                "does not define"
        , Harness.test "resolve/compile_script_runs_resolver" <|
            case compileScript "<test>" (T.unlines ["import Stats (x, x)", "answer <- 42"]) of
                Prelude.Left d ->
                    Prelude.pure (assertMessageContains "duplicate top-level binding" d)
                Prelude.Right _ ->
                    Prelude.pure (Fail "expected resolver diagnostic")
        , Harness.test "resolve/lsp_compile_runs_resolver" <|
            case compileText "<test>" (T.unlines ["import Stats (x, x)", "answer <- 42"]) of
                CompileFailed [d] ->
                    Prelude.pure (assertMessageContains "duplicate top-level binding" d)
                CompileFailed ds ->
                    Prelude.pure (Fail ("expected one diagnostic, got " ++ showText (Prelude.length ds)))
                CompileOk _ _ ->
                    Prelude.pure (Fail "expected resolver diagnostic")
        , Harness.test "project/toml_parses_metadata_and_dependencies" <|
            case parseProjectToml "quone.toml" validToml of
                Prelude.Left d -> Prelude.pure (Fail (showText d))
                Prelude.Right project ->
                    Prelude.pure
                        ( project
                            === Project
                                { projectMeta =
                                    PackageMeta
                                        { metaName = "stats"
                                        , metaVersion = "0.0.1"
                                        , metaDescription = Just "Score normalisation."
                                        , metaAuthors = ["Andrew McNally", "Ada Lovelace"]
                                        }
                                , projectDependencies =
                                    [ Dependency {depName = "purrr", depVersion = ">= 1.0"}
                                    , Dependency {depName = "dplyr", depVersion = ">= 1.1"}
                                    ]
                                }
                        )
        , Harness.test "project/toml_rejects_unquoted_dependency" <|
            case parseProjectToml "quone.toml" invalidDependencyToml of
                Prelude.Left d ->
                    Prelude.pure (assertMessageContains "dependency value must be a quoted string" d)
                Prelude.Right _ ->
                    Prelude.pure (Fail "expected project parse failure")
        ]


sourceModule :: Text
sourceModule =
    T.unlines
        [ "module Stats exporting (normalize)"
        , "normalize <- 1"
        , "private <- 2"
        ]


validToml :: Text
validToml =
    T.unlines
        [ "[package]"
        , "name = \"stats\""
        , "version = \"0.0.1\""
        , "description = \"Score normalisation.\""
        , "authors = [\"Andrew McNally\", \"Ada Lovelace\"]"
        , ""
        , "[dependencies]"
        , "purrr = \">= 1.0\""
        , "dplyr = \">= 1.1\""
        ]


invalidDependencyToml :: Text
invalidDependencyToml =
    T.unlines
        [ "[package]"
        , "name = \"stats\""
        , "version = \"0.0.1\""
        , ""
        , "[dependencies]"
        , "purrr = >= 1.0"
        ]


expectResolveProgramFail :: Text -> Text -> Prelude.IO TestResult
expectResolveProgramFail src expected =
    case desugarSource src of
        Prelude.Left d -> Prelude.pure (Fail (showText d))
        Prelude.Right prog ->
            case Resolve.resolveProgram prog of
                [] -> Prelude.pure (Fail "expected resolver diagnostic")
                (d : _) -> Prelude.pure (assertMessageContains expected d)


expectProjectResolveFail :: Text -> Text -> Text -> Prelude.IO TestResult
expectProjectResolveFail source consumer expected =
    case (desugarSource source, desugarSource consumer) of
        (Prelude.Right sourceProg, Prelude.Right consumerProg) ->
            let
                sourceSyms = Resolve.collectSymbols sourceProg
                allSyms = Map.fromList [(Resolve.modSymPath sourceSyms, sourceSyms)]
            in
            case Resolve.resolveProject allSyms consumerProg of
                [] -> Prelude.pure (Fail "expected project resolver diagnostic")
                (d : _) -> Prelude.pure (assertMessageContains expected d)
        (Prelude.Left d, _) -> Prelude.pure (Fail (showText d))
        (_, Prelude.Left d) -> Prelude.pure (Fail (showText d))


assertMessageContains :: Text -> Diagnostic -> TestResult
assertMessageContains expected d =
    if expected `T.isInfixOf` diagMessage d
        then Pass
        else Fail ("expected diagnostic to contain " ++ expected ++ ", got " ++ diagMessage d)


showText :: Prelude.Show a => a -> Text
showText = T.pack Prelude.. Prelude.show
