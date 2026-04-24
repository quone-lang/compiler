module Test.LspTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Lsp.Compile (CompileResult (..), compileText)
import qualified Quone.Lsp.Handlers as Handlers
import qualified Quone.Lsp.Json as Json
import qualified Quone.Lsp.State as State
import Quone.Lsp.Symbols (buildIndex)
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..))
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
        , Harness.test "lsp/hover_uses_quone_fenced_code" <|
            Prelude.pure hoverUsesQuoneFence
        ]


hoverUsesQuoneFence :: TestResult
hoverUsesQuoneFence =
    let
        uri =
            "file:///hover.Q"

        openParams =
            Json.object
                [ ( "textDocument"
                  , Json.object
                        [ ("uri", Json.str uri)
                        , ("version", Json.int 1)
                        , ("text", Json.str "answer <- 42")
                        ]
                  )
                ]

        hoverParams =
            Json.object
                [ ( "textDocument"
                  , Json.object [("uri", Json.str uri)]
                  )
                , ( "position"
                  , Json.object
                        [ ("line", Json.int 0)
                        , ("character", Json.int 1)
                        ]
                  )
                ]

        (state, _) =
            Handlers.handleDidOpen openParams State.empty

        hover =
            Handlers.handleHover hoverParams state
    in
    case Json.lookupField "contents" hover
        Prelude.>>= Json.lookupField "value" of
        Just (Json.VString body) ->
            if "```quone" `T.isInfixOf` body && "answer : Double" `T.isInfixOf` body then
                Pass
            else
                Fail ("unexpected hover body: " ++ body)

        other ->
            Fail ("unexpected hover payload: " ++ T.pack (Prelude.show other))

