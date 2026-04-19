{-| Tests for the LSP server.

Compiler track A6 (project plan): @quonec lsp@ speaks JSON-RPC 2.0
over stdin/stdout. The integration test (spawning a real subprocess
and driving a four-message conversation) is `[planned]`; this module
covers the pure handlers and the JSON / framing helpers.

-}
module Test.LspTests (suite) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import qualified Quone.Lsp.Handlers as Handlers
import qualified Quone.Lsp.Json as Json
import qualified Quone.Lsp.Protocol as Protocol
import qualified Quone.Lsp.State as State
import Test.Harness
    ( Suite
    , Test
    , TestResult (..)
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "lsp"
        ( jsonTests
            ++ protocolTests
            ++ handlerTests
            ++ hoverTests
            ++ definitionTests
            ++ completionTests
        )


jsonTests :: [Test]
jsonTests =
    [ test "lsp/json_encodes_null_section_a6"
        (Prelude.pure (Json.encode Json.VNull === "null"))
    , test "lsp/json_encodes_string_section_a6"
        ( Prelude.pure
            (Json.encode (Json.VString "hi") === "\"hi\""))
    , test "lsp/json_round_trips_object_section_a6"
        ( do
            let
                v =
                    Json.object
                        [ ("k", Json.str "v")
                        , ("n", Json.int 7)
                        ]
                encoded = Json.encode v
            case Json.decode encoded of
                Just decoded -> Prelude.pure (decoded === v)
                Prelude.Nothing -> Prelude.pure (Fail "decode failed")
        )
    , test "lsp/json_handles_nested_arrays_section_a6"
        ( do
            let
                v =
                    Json.VArray
                        [ Json.int 1
                        , Json.VArray [Json.int 2, Json.int 3]
                        ]
            case Json.decode (Json.encode v) of
                Just decoded -> Prelude.pure (decoded === v)
                Prelude.Nothing -> Prelude.pure (Fail "decode failed")
        )
    ]


protocolTests :: [Test]
protocolTests =
    [ test "lsp/protocol_parses_content_length_section_a6"
        ( Prelude.pure
            (Protocol.parseHeader
                [BSC.pack "Content-Length: 42"]
                === Just 42))
    , test "lsp/protocol_ignores_unrelated_headers_section_a6"
        ( Prelude.pure
            (Protocol.parseHeader
                [ BSC.pack "Foo: bar"
                , BSC.pack "Content-Length: 7"
                ]
                === Just 7))
    , test "lsp/protocol_returns_nothing_without_content_length_section_a6"
        ( Prelude.pure
            (Protocol.parseHeader
                [BSC.pack "Foo: bar"]
                === Prelude.Nothing))
    , test "lsp/protocol_encodes_frame_with_header_section_a6"
        ( do
            let
                frame = Protocol.encodeFrame (Json.str "hi")
                bs = BSC.unpack frame
            Prelude.pure
                ( if Prelude.elem '\r' bs
                    Prelude.&& Prelude.elem '\n' bs
                    Prelude.&& Prelude.elem '"' bs
                    then Pass
                    else Fail "frame should contain CRLF separator")
        )
    ]


handlerTests :: [Test]
handlerTests =
    [ test "lsp/initialize_advertises_capabilities_section_a6"
        ( do
            let
                encoded = Json.encode Handlers.initializeResult
            Prelude.pure
                ( if T.isInfixOf "hoverProvider" encoded
                    Prelude.&& T.isInfixOf "definitionProvider" encoded
                    Prelude.&& T.isInfixOf "documentSymbolProvider" encoded
                    Prelude.&& T.isInfixOf "completionProvider" encoded
                    Prelude.&& T.isInfixOf "documentFormattingProvider"
                        encoded
                    then Pass
                    else Fail "missing capability declarations"
                )
        )
    , test "lsp/publish_diagnostics_uses_correct_method_section_a6"
        ( do
            let
                encoded =
                    Json.encode (Handlers.publishDiagnostics "file:///x" [])
            Prelude.pure
                ( if T.isInfixOf "textDocument/publishDiagnostics" encoded
                    then Pass
                    else Fail "missing method"
                )
        )
    , test "lsp/completion_includes_keywords_section_a6"
        ( do
            let
                response = Handlers.handleCompletion Json.VNull State.empty
                encoded = Json.encode response
            Prelude.pure
                ( if T.isInfixOf "module" encoded
                    Prelude.&& T.isInfixOf "filter" encoded
                    Prelude.&& T.isInfixOf "case" encoded
                    then Pass
                    else Fail "expected core keywords in completion"
                )
        )
    ]


-- ---------------------------------------------------------------------
-- Hover (release C4)
-- ---------------------------------------------------------------------


-- | A small fixture state with one open document containing a
-- handful of top-level bindings.
fixtureState :: State.State
fixtureState =
    let
        doc =
            State.Document
                { State.docText =
                    T.intercalate "\n"
                        [ "x <- 1"
                        , "y <- 2.0"
                        , "main <- x"
                        ]
                , State.docVersion = 1
                }
    in
    State.putDocument "file:///fixture.Q" doc State.empty


hoverRequest :: Int -> Int -> Json.Value
hoverRequest line char =
    Json.object
        [ ("textDocument"
          , Json.object [("uri", Json.str "file:///fixture.Q")])
        , ("position"
          , Json.object [("line", Json.int line), ("character", Json.int char)])
        ]


hoverTests :: [Test]
hoverTests =
    [ test "lsp/hover_at_top_binding_returns_type_section_c4"
        ( do
            let
                req = hoverRequest 0 0
                response = Handlers.handleHover req fixtureState
                encoded = Json.encode response
            Prelude.pure
                ( if T.isInfixOf "x" encoded
                    Prelude.&& T.isInfixOf "Integer" encoded
                    then Pass
                    else
                        Fail
                            ("expected hover with `x : Integer`; got "
                                ++ encoded)
                )
        )
    , test "lsp/hover_outside_any_symbol_returns_null_section_c4"
        ( do
            let
                req = hoverRequest 5 100
                response = Handlers.handleHover req fixtureState
            Prelude.pure (response === Json.VNull)
        )
    , test "lsp/hover_unknown_uri_returns_null_section_c4"
        ( do
            let
                req =
                    Json.object
                        [ ("textDocument"
                          , Json.object [("uri", Json.str "file:///nope.Q")])
                        , ("position"
                          , Json.object
                                [ ("line", Json.int 0)
                                , ("character", Json.int 0)
                                ])
                        ]
                response = Handlers.handleHover req fixtureState
            Prelude.pure (response === Json.VNull)
        )
    ]


-- ---------------------------------------------------------------------
-- Definition (release C4)
-- ---------------------------------------------------------------------


definitionTests :: [Test]
definitionTests =
    [ test "lsp/definition_resolves_known_identifier_section_c4"
        ( do
            let
                -- Hovering the `x` in `main <- x` (line 2 char 8) should
                -- jump back to its definition on line 0.
                req = hoverRequest 2 8
                response = Handlers.handleDefinition req fixtureState
                encoded = Json.encode response
            Prelude.pure
                ( if T.isInfixOf "fixture.Q" encoded
                    Prelude.&& T.isInfixOf "\"line\":0" encoded
                    then Pass
                    else
                        Fail
                            ("expected definition pointing at line 0; got "
                                ++ encoded)
                )
        )
    , test "lsp/definition_unknown_identifier_returns_null_section_c4"
        ( do
            let
                req = hoverRequest 5 0
                response = Handlers.handleDefinition req fixtureState
            Prelude.pure (response === Json.VNull)
        )
    ]


-- ---------------------------------------------------------------------
-- Completion (release C4)
-- ---------------------------------------------------------------------


completionTests :: [Test]
completionTests =
    [ test "lsp/completion_includes_in_scope_bindings_section_c4"
        ( do
            let
                req = hoverRequest 2 5
                response = Handlers.handleCompletion req fixtureState
                encoded = Json.encode response
            Prelude.pure
                ( if T.isInfixOf "\"label\":\"x\"" encoded
                    Prelude.&& T.isInfixOf "\"label\":\"y\"" encoded
                    Prelude.&& T.isInfixOf "\"label\":\"main\"" encoded
                    then Pass
                    else
                        Fail
                            ("expected completion to include in-scope bindings; got "
                                ++ encoded)
                )
        )
    , test "lsp/completion_falls_back_to_keywords_when_no_doc_section_c4"
        ( do
            let
                response =
                    Handlers.handleCompletion Json.VNull State.empty
                encoded = Json.encode response
            Prelude.pure
                ( if T.isInfixOf "module" encoded
                    Prelude.&& T.isInfixOf "case" encoded
                    then Pass
                    else
                        Fail
                            ("expected keyword fallback; got " ++ encoded)
                )
        )
    ]
