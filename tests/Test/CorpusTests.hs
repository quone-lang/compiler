{-| Versioned corpus tests (LANGUAGE.md section 16.10).

Walks @tests/corpus/{valid,invalid,snapshot}/@ at runtime and:

* @valid/*.Q@: must compile (lex, parse, type-check, generate) without
  diagnostics;
* @invalid/*.Q@: must produce a diagnostic somewhere in the pipeline;
* @snapshot/*.Q@ paired with @snapshot/*.R@: the generated R must
  match the .R file (whitespace-normalised).

Tests are discovered at runtime so adding a fixture doesn't require
recompiling Haskell.

-}
module Test.CorpusTests (suite) where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Cli.Commands (compileScript)
import Quone.Diagnostic (render)
import Quone.Generate.R (generateProgram)
import Quone.Parse.Desugar (desugarSource)
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.IO.Unsafe as U
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , describe
    , test
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "corpus"
        ( validTests Prelude.++ invalidTests Prelude.++ snapshotTests
        )



-- ---------------------------------------------------------------------
-- Discovery
-- ---------------------------------------------------------------------


-- | Synchronous file-system walk via unsafePerformIO. Acceptable
-- because the test list is built once at suite-construction time and
-- the corpus directory is project-relative.
listCorpus :: Prelude.String -> [Prelude.FilePath]
listCorpus subdir = U.unsafePerformIO <| do
    let dir = "tests/corpus/" Prelude.++ subdir
    exists <- Dir.doesDirectoryExist dir
    if Prelude.not exists
        then Prelude.pure []
        else do
            entries <- Dir.listDirectory dir
            Prelude.pure
                ( List.sort
                    ( Prelude.fmap
                        (\e -> dir Prelude.++ "/" Prelude.++ e)
                        ( Prelude.filter
                            (\e -> ".Q" `List.isSuffixOf` e)
                            entries
                        )
                    )
                )


readUtf8 :: Prelude.FilePath -> Prelude.IO Text
readUtf8 = TIO.readFile



-- ---------------------------------------------------------------------
-- valid/
-- ---------------------------------------------------------------------


validTests :: [Test]
validTests =
    Prelude.fmap mkValid (listCorpus "valid")


mkValid :: Prelude.FilePath -> Test
mkValid path =
    test
        ("corpus/valid/" Prelude.<> T.pack (FP.takeBaseName path))
        ( do
            src <- readUtf8 path
            case compileScript (T.pack path) src of
                Prelude.Right _ -> Prelude.pure Pass
                Prelude.Left d ->
                    Prelude.pure
                        (Fail (render d))
        )



-- ---------------------------------------------------------------------
-- invalid/
-- ---------------------------------------------------------------------


invalidTests :: [Test]
invalidTests =
    Prelude.fmap mkInvalid (listCorpus "invalid")


mkInvalid :: Prelude.FilePath -> Test
mkInvalid path =
    test
        ("corpus/invalid/" Prelude.<> T.pack (FP.takeBaseName path))
        ( do
            src <- readUtf8 path
            case compileScript (T.pack path) src of
                Prelude.Left _ -> Prelude.pure Pass
                Prelude.Right _ ->
                    Prelude.pure
                        (Fail "expected a diagnostic, got a successful compile")
        )



-- ---------------------------------------------------------------------
-- snapshot/
-- ---------------------------------------------------------------------


snapshotTests :: [Test]
snapshotTests =
    Prelude.fmap mkSnapshot (listCorpus "snapshot")


mkSnapshot :: Prelude.FilePath -> Test
mkSnapshot qPath =
    test
        ("corpus/snapshot/" Prelude.<> T.pack (FP.takeBaseName qPath))
        ( do
            src <- readUtf8 qPath
            let rPath = FP.replaceExtension qPath "R"
            expected <- readUtf8 rPath
            case desugarSource src of
                Prelude.Left d -> Prelude.pure (Fail (render d))
                Prelude.Right p ->
                    let
                        actual = T.strip (generateProgram p)
                        wanted = T.strip expected
                    in
                    Prelude.pure
                        ( if normalise actual Prelude.== normalise wanted
                            then Pass
                            else
                                Fail
                                    ( "expected:\n"
                                        Prelude.<> wanted
                                        Prelude.<> "\nactual:\n"
                                        Prelude.<> actual
                                    )
                        )
        )


normalise :: Text -> Text
normalise = T.unwords Prelude.. T.words
