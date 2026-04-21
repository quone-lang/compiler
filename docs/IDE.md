# Quone IDE Surface - Version 0.0.1

**Status:** Normative for the v0.0.1 surface where rules are stated as
MUST or SHOULD. Anything labelled `[planned]` is informative.

**Audience:** Authors of the LSP server (`quonec lsp`), the REPL
(`quonec repl`), the R companion package (`quone::repl()`,
`quone::repl_eval()`), and editor extensions (RStudio, Positron,
VS Code, Cursor, Neovim).

**Document scope:** This document specifies the two interactive
surfaces Quone exposes to editors and developers: the LSP server and
the REPL. The language itself is defined in [LANGUAGE.md](LANGUAGE.md);
the CLI flags that launch each surface in
[CLI.md section 3](CLI.md#3-subcommands); the formatter (driven by
LSP `textDocument/formatting`) in [FORMATTER.md](FORMATTER.md).

---

## Table of contents

1. [`quonec lsp` (Appendix A.6, C.4)](#1-quonec-lsp)
2. [`quonec repl` (Appendix A.5)](#2-quonec-repl)

---

## 1. `quonec lsp`

(Internally `section_a6` for the protocol/transport surface and
`section_c4` for the user-facing capabilities.)

`quonec lsp` is a Language Server Protocol implementation that speaks
LSP over stdin/stdout. It is the back-end every Quone editor extension
launches when the user opens a `.Q` file.

### 1.1 Transport (Appendix A.6)

The server reads framed messages from stdin and writes responses
(plus pushed diagnostic notifications) to stdout. Frame format is
standard LSP (`Content-Length: <n>\r\n\r\n<body>`).

The server MUST:

- accept `Content-Length`-framed JSON-RPC messages over stdin;
- emit responses on stdout in the same frame format;
- ignore unrelated headers (e.g. `Content-Type`) without erroring;
- return `Nothing` (no message) when the input has no `Content-Length`
  header rather than crashing.

`--log-file=PATH` mirrors all sent and received traffic to `PATH` for
debugging. Without it, the server runs silently.

<!-- BEGIN tests:a6 -->
**Tests** (13):

- [`cli/lsp_no_log_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L150)
- [`cli/lsp_with_log_file_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L153)
- [`lsp/json_encodes_null_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L46)
- [`lsp/json_encodes_string_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L48)
- [`lsp/json_round_trips_object_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L51)
- [`lsp/json_handles_nested_arrays_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L64)
- [`lsp/protocol_parses_content_length_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L81)
- [`lsp/protocol_ignores_unrelated_headers_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L86)
- [`lsp/protocol_returns_nothing_without_content_length_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L93)
- [`lsp/protocol_encodes_frame_with_header_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L98)
- [`lsp/initialize_advertises_capabilities_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L115)
- [`lsp/publish_diagnostics_uses_correct_method_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L130)
- [`lsp/completion_includes_keywords_section_a6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L141)

<details>
<summary>Show test source</summary>

```haskell
    test "cli/lsp_no_log_section_a6"
        ( Prelude.pure
            (parseArgs ["lsp"] === CmdLsp Prelude.Nothing))
    test "cli/lsp_with_log_file_section_a6"
        ( Prelude.pure
            (parseArgs ["lsp", "--log-file=/tmp/lsp.log"]
                === CmdLsp (Just "/tmp/lsp.log")))
    test "lsp/json_encodes_null_section_a6"
        (Prelude.pure (Json.encode Json.VNull === "null"))
    test "lsp/json_encodes_string_section_a6"
        ( Prelude.pure
            (Json.encode (Json.VString "hi") === "\"hi\""))
    test "lsp/json_round_trips_object_section_a6"
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
    test "lsp/json_handles_nested_arrays_section_a6"
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
    test "lsp/protocol_parses_content_length_section_a6"
        ( Prelude.pure
            (Protocol.parseHeader
                [BSC.pack "Content-Length: 42"]
                === Just 42))
    test "lsp/protocol_ignores_unrelated_headers_section_a6"
        ( Prelude.pure
            (Protocol.parseHeader
                [ BSC.pack "Foo: bar"
                , BSC.pack "Content-Length: 7"
                ]
                === Just 7))
    test "lsp/protocol_returns_nothing_without_content_length_section_a6"
        ( Prelude.pure
            (Protocol.parseHeader
                [BSC.pack "Foo: bar"]
                === Prelude.Nothing))
    test "lsp/protocol_encodes_frame_with_header_section_a6"
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
    test "lsp/initialize_advertises_capabilities_section_a6"
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
    test "lsp/publish_diagnostics_uses_correct_method_section_a6"
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
    test "lsp/completion_includes_keywords_section_a6"
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
```

</details>
<!-- END tests:a6 -->

### 1.2 Capabilities (Appendix C.4)

On `initialize`, the server advertises the following capabilities. The
list is the v0.0.1 minimum; richer capabilities are `[planned]`.

| Capability                 | Behaviour                                                     |
| -------------------------- | ------------------------------------------------------------- |
| `textDocumentSync.full`    | Re-parse on every change.                                     |
| `hoverProvider`            | Show inferred type on hover; see [hover rules](#121-hover). |
| `definitionProvider`       | Jump to the binding that defines a symbol.                    |
| `completionProvider`       | Suggest in-scope bindings; fall back to keywords.             |
| `documentSymbolProvider`   | List top-level declarations for the outline panel.            |
| `publishDiagnostics`       | Pushed after `didOpen` and `didChange`.                       |
| `documentFormattingProvider` | Run the formatter (see [FORMATTER.md](FORMATTER.md)).       |

#### 1.2.1 Hover

A hover request at a position MUST:

- return the inferred type of the binding at the cursor when the
  cursor is on a top-level binding name;
- return `null` when the cursor is outside any symbol;
- return `null` when the URI is unknown to the server.

The hover response is a markdown string of the form `` `name : Type` ``.

#### 1.2.2 Definition

A definition request at a position MUST:

- return the source location of the binding the symbol resolves to,
  when the symbol is bound;
- return `null` when the symbol is unbound or unknown.

#### 1.2.3 Completion

A completion request MUST include all in-scope bindings at the cursor
position. When no document is open at the requested URI, completion
MUST fall back to the reserved-keyword list from
[LANGUAGE.md section 3.4](LANGUAGE.md#34-reserved-keywords).

<!-- BEGIN tests:c4 -->
**Tests** (7):

- [`lsp/hover_at_top_binding_returns_type_section_c4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L193)
- [`lsp/hover_outside_any_symbol_returns_null_section_c4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L209)
- [`lsp/hover_unknown_uri_returns_null_section_c4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L216)
- [`lsp/definition_resolves_known_identifier_section_c4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L242)
- [`lsp/definition_unknown_identifier_returns_null_section_c4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L260)
- [`lsp/completion_includes_in_scope_bindings_section_c4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L277)
- [`lsp/completion_falls_back_to_keywords_when_no_doc_section_c4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LspTests.hs#L294)

<details>
<summary>Show test source</summary>

```haskell
    test "lsp/hover_at_top_binding_returns_type_section_c4"
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
    test "lsp/hover_outside_any_symbol_returns_null_section_c4"
        ( do
            let
                req = hoverRequest 5 100
                response = Handlers.handleHover req fixtureState
            Prelude.pure (response === Json.VNull)
        )
    test "lsp/hover_unknown_uri_returns_null_section_c4"
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
    test "lsp/definition_resolves_known_identifier_section_c4"
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
    test "lsp/definition_unknown_identifier_returns_null_section_c4"
        ( do
            let
                req = hoverRequest 5 0
                response = Handlers.handleDefinition req fixtureState
            Prelude.pure (response === Json.VNull)
        )
    test "lsp/completion_includes_in_scope_bindings_section_c4"
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
    test "lsp/completion_falls_back_to_keywords_when_no_doc_section_c4"
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
```

</details>
<!-- END tests:c4 -->

---

## 2. `quonec repl`

(Internally `section_a5`.)

`quonec repl` is an interactive Quone session. It reads a line,
type-checks it, lowers to R, and forwards the R to a long-lived
`Rscript --interactive` subprocess. The R session holds the user's
bindings; the REPL holds the typing environment.

```
quone> x <- 1 + 1
quone> mean [x, 2.0, 3.0]
[1] 2
quone> :type x
x : Integer
quone> :quit
```

### 2.1 Meta-commands

The REPL recognises the following meta-commands; everything else is
treated as a Quone fragment.

| Command              | Effect                                                    |
| -------------------- | --------------------------------------------------------- |
| `:type expr`, `:t`   | Print the inferred type of `expr`.                        |
| `:load file.Q`       | Run a file's bindings into the session.                   |
| `:reload`            | Re-run every previously `:load`ed file.                   |
| `:browse`            | List every binding currently in the session env.          |
| `:quit`, `:q`        | Exit the REPL.                                            |
| `:help`, `:?`        | Print meta-command help.                                  |

Meta-commands MUST be parsed as the entire input line; an
unrecognised command returns "unknown" rather than being forwarded
to the type checker.

### 2.2 R backend protocol

The REPL keeps a long-lived `Rscript --interactive` subprocess for
the lifetime of the session. Each evaluation:

1. parses + type-checks + lowers the Quone fragment;
2. writes the lowered R to the subprocess stdin;
3. captures stdout until a sentinel marker is seen;
4. surfaces R's printed value (or runtime error) to the user.

The backend MUST:

- return the printed value of an expression as the REPL output;
- surface an R runtime error message verbatim;
- evaluate multi-statement chunks in one round-trip (so `x <- 1; x + 1`
  does not require two prompts);
- exit cleanly when the REPL stops; `stop` MUST NOT hang waiting for
  the subprocess.

### 2.3 R companion entry points

The R companion package (`quone::repl()`, `quone::repl_eval()`)
shells out to `quonec repl` under the hood. The contract above
constrains the companion's behaviour through that delegation:

- `quone::repl()` invokes `quonec repl` interactively (terminal
  required; the wrapper warns when called from a non-terminal context
  like an IDE R console).
- `quone::repl_eval(expr)` spawns `quonec repl`, sends `expr` plus
  `:quit`, captures stdout, strips the welcome banner and `quone> `
  prompts, returns the user-visible output.

<!-- BEGIN tests:a5 -->
**Tests** (18):

- [`cli/repl_command_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L53)
- [`repl/parses_quit_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L49)
- [`repl/parses_quit_alias_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L51)
- [`repl/parses_help_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L53)
- [`repl/parses_help_alias_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L55)
- [`repl/parses_browse_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L57)
- [`repl/parses_reload_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L59)
- [`repl/parses_type_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L61)
- [`repl/parses_type_alias_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L64)
- [`repl/parses_load_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L67)
- [`repl/identifies_unknown_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L71)
- [`repl/non_meta_returns_nothing_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L75)
- [`repl/empty_session_browses_to_nothing_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L82)
- [`repl/type_lookup_for_unknown_returns_planned_message_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L85)
- [`repl/backend_returns_printed_value_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L110)
- [`repl/backend_surfaces_r_error_message_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L125)
- [`repl/backend_evaluates_multi_statement_chunk_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L143)
- [`repl/backend_stop_does_not_hang_section_a5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ReplTests.hs#L167)

<details>
<summary>Show test source</summary>

```haskell
    test "cli/repl_command_section_a5"
        (Prelude.pure (parseArgs ["repl"] === CmdRepl))
    test "repl/parses_quit_section_a5"
        (Prelude.pure (Meta.parseMeta ":quit" === Just Meta.MQuit))
    test "repl/parses_quit_alias_section_a5"
        (Prelude.pure (Meta.parseMeta ":q" === Just Meta.MQuit))
    test "repl/parses_help_section_a5"
        (Prelude.pure (Meta.parseMeta ":help" === Just Meta.MHelp))
    test "repl/parses_help_alias_section_a5"
        (Prelude.pure (Meta.parseMeta ":?" === Just Meta.MHelp))
    test "repl/parses_browse_section_a5"
        (Prelude.pure (Meta.parseMeta ":browse" === Just Meta.MBrowse))
    test "repl/parses_reload_section_a5"
        (Prelude.pure (Meta.parseMeta ":reload" === Just Meta.MReload))
    test "repl/parses_type_section_a5"
        ( Prelude.pure
            (Meta.parseMeta ":type x" === Just (Meta.MType "x")))
    test "repl/parses_type_alias_section_a5"
        ( Prelude.pure
            (Meta.parseMeta ":t x" === Just (Meta.MType "x")))
    test "repl/parses_load_section_a5"
        ( Prelude.pure
            (Meta.parseMeta ":load src/Main.Q"
                === Just (Meta.MLoad "src/Main.Q")))
    test "repl/identifies_unknown_section_a5"
        ( Prelude.pure
            (Meta.parseMeta ":wat"
                === Just (Meta.MUnknown "wat")))
    test "repl/non_meta_returns_nothing_section_a5"
        (Prelude.pure (Meta.parseMeta "x <- 1" === Prelude.Nothing))
    test "repl/empty_session_browses_to_nothing_section_a5"
        ( Prelude.pure
            (Session.browse (Session.empty Prelude.Nothing) === []))
    test "repl/type_lookup_for_unknown_returns_planned_message_section_a5"
        ( do
            let
                msg =
                    Session.inferType
                        (Session.empty Prelude.Nothing)
                        "missing"
            Prelude.pure
                ( if T.isInfixOf "[planned]" msg
                    then Pass
                    else
                        Fail
                            ("expected hint about [planned]; got: "
                                ++ msg)
                )
        )
    test "repl/backend_returns_printed_value_section_a5"
        ( whenRscriptAvailable
            ( do
                result <- withBackend (\b -> RBackend.evalChunk b "1L + 1L")
                Prelude.pure
                    ( if T.isInfixOf "[1] 2" result
                        then Pass
                        else
                            Fail
                                ( "expected '[1] 2' in output; got: "
                                    ++ T.pack (Prelude.show result)
                                )
                    )
            )
        )
    test "repl/backend_surfaces_r_error_message_section_a5"
        ( whenRscriptAvailable
            ( do
                result <-
                    withBackend
                        (\b -> RBackend.evalChunk b "stop(\"boom\")")
                Prelude.pure
                    ( if T.isInfixOf "Error" result
                        && T.isInfixOf "boom" result
                        then Pass
                        else
                            Fail
                                ( "expected an error containing 'boom'; got: "
                                    ++ T.pack (Prelude.show result)
                                )
                    )
            )
        )
    test "repl/backend_evaluates_multi_statement_chunk_section_a5"
        ( whenRscriptAvailable
            ( do
                result <-
                    withBackend
                        (\b ->
                            RBackend.evalChunk b
                                ( T.intercalate "\n"
                                    [ "x <- 41L"
                                    , "x + 1L"
                                    ]
                                )
                        )
                Prelude.pure
                    ( if T.isInfixOf "[1] 42" result
                        then Pass
                        else
                            Fail
                                ( "expected '[1] 42' in output; got: "
                                    ++ T.pack (Prelude.show result)
                                )
                    )
            )
        )
    test "repl/backend_stop_does_not_hang_section_a5"
        ( whenRscriptAvailable
            ( do
                eb <- RBackend.start RBackend.startOpts
                case eb of
                    Prelude.Left msg ->
                        Prelude.pure (Fail ("backend start failed: " ++ msg))
                    Prelude.Right b -> do
                        _ <- RBackend.evalChunk b "1L"
                        RBackend.stop b
                        Prelude.pure Pass
            )
        )
```

</details>
<!-- END tests:a5 -->

---

## See also

- [LANGUAGE.md](LANGUAGE.md) - the Quone language proper.
- [CLI.md](CLI.md) - the `quonec` command-line surface (launches lsp/repl).
- [FORMATTER.md](FORMATTER.md) - the formatter the LSP runs on `textDocument/formatting`.
- [COMPILATION.md](COMPILATION.md) - lowering rules used by both surfaces.
