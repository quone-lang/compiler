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
        , Harness.test "lsp/hover_uses_quone_marked_string" <|
            Prelude.pure hoverUsesQuoneMarkedString
        , Harness.test "lsp/hover_shows_prelude_docs" <|
            Prelude.pure hoverShowsPreludeDocs
        , Harness.test "lsp/hover_renders_type_variables_readably" <|
            Prelude.pure hoverRendersTypeVariablesReadably
        ]


hoverUsesQuoneMarkedString :: TestResult
hoverUsesQuoneMarkedString =
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
    case Json.lookupField "contents" hover of
        Just contents ->
            let
                language =
                    Json.lookupField "language" contents
                        Prelude.>>= Json.asString

                value =
                    Json.lookupField "value" contents
                        Prelude.>>= Json.asString
            in
            if language Prelude.== Just "quone" && value Prelude.== Just "answer : Double" then
                Pass
            else
                Fail ("unexpected hover contents: " ++ T.pack (Prelude.show contents))

        other ->
            Fail ("unexpected hover payload: " ++ T.pack (Prelude.show other))


hoverShowsPreludeDocs :: TestResult
hoverShowsPreludeDocs =
    let
        uri =
            "file:///hover-prelude.Q"

        src =
            "x <- mean [1, 2, 3]"

        openParams =
            Json.object
                [ ( "textDocument"
                  , Json.object
                        [ ("uri", Json.str uri)
                        , ("version", Json.int 1)
                        , ("text", Json.str src)
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
                        , ("character", Json.int 6)
                        ]
                  )
                ]

        (state, _) =
            Handlers.handleDidOpen openParams State.empty

        hover =
            Handlers.handleHover hoverParams state
    in
    case Json.lookupField "contents" hover of
        Just (Json.VArray [Json.VString doc, signature]) ->
            let
                language =
                    Json.lookupField "language" signature
                        Prelude.>>= Json.asString

                value =
                    Json.lookupField "value" signature
                        Prelude.>>= Json.asString
            in
            if language Prelude.== Just "quone"
                && value Prelude.== Just "mean : Vector Double -> Double"
                && "Arithmetic mean." `T.isInfixOf` doc
            then
                Pass
            else
                Fail ("unexpected prelude hover: " ++ T.pack (Prelude.show hover))

        other ->
            Fail ("unexpected prelude hover payload: " ++ T.pack (Prelude.show other))


hoverRendersTypeVariablesReadably :: TestResult
hoverRendersTypeVariablesReadably =
    let
        uri =
            "file:///hover-map.Q"

        src =
            "x <- map (\\x -> x) [1, 2, 3]"

        openParams =
            Json.object
                [ ( "textDocument"
                  , Json.object
                        [ ("uri", Json.str uri)
                        , ("version", Json.int 1)
                        , ("text", Json.str src)
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
                        , ("character", Json.int 6)
                        ]
                  )
                ]

        (state, _) =
            Handlers.handleDidOpen openParams State.empty

        hover =
            Handlers.handleHover hoverParams state
    in
    case Json.lookupField "contents" hover of
        Just (Json.VArray [_, signature]) ->
            case Json.lookupField "value" signature Prelude.>>= Json.asString of
                Just value ->
                    if "TyVar" `T.isInfixOf` value then
                        Fail ("hover leaked internal TyVar: " ++ value)
                    else if value Prelude.== "map : (a -> b) -> Vector a -> Vector b" then
                        Pass
                    else
                        Fail ("unexpected map hover: " ++ value)

                Prelude.Nothing ->
                    Fail ("missing hover value: " ++ T.pack (Prelude.show hover))

        other ->
            Fail ("unexpected map hover payload: " ++ T.pack (Prelude.show other))

