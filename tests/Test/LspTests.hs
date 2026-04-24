module Test.LspTests (suite) where

import NriPrelude
import Quone.Lsp.Compile (CompileResult (..), compileText)
import Quone.Lsp.Symbols (buildIndex)
import qualified Test.Harness as Harness
import Test.Harness ((===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "lsp"
        [ Harness.test "lsp/compile_text_success" <|
            case compileText "<test>" "answer <- 42" of
                CompileOk _ _ -> Prelude.pure (1 === (1 :: Prelude.Int))
                CompileFailed ds -> Prelude.pure (Prelude.length ds === 0)
        , Harness.test "lsp/symbol_index_includes_value" <|
            case compileText "<test>" "answer <- 42" of
                CompileOk prog typed ->
                    Prelude.pure (Prelude.length (buildIndex prog typed) === 1)
                CompileFailed ds -> Prelude.pure (Prelude.length ds === 0)
        ]

