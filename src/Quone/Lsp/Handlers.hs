{-| Per-method LSP handlers.

Each handler is a pure function from request payload + state to
response payload + new state, so the surrounding I/O loop in
'Quone.Lsp.Server' stays small.

The capability set matches the project plan section A6:

* @initialize@
* @textDocument/didOpen@, @textDocument/didChange@, @textDocument/didClose@
* @textDocument/publishDiagnostics@ (response side; pushed by the
  server after a compile)
* @textDocument/hover@
* @textDocument/definition@
* @textDocument/documentSymbol@
* @textDocument/completion@
* @textDocument/formatting@
* @shutdown@, @exit@

-}
module Quone.Lsp.Handlers
    ( initializeResult
    , publishDiagnostics
    , handleDidOpen
    , handleDidChange
    , handleHover
    , handleDefinition
    , handleDocumentSymbol
    , handleCompletion
    , handleFormatting
    )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import qualified Quone.Ast.Source as Ast
import Quone.Diagnostic (Diagnostic (..))
import qualified Quone.Diagnostic as Diag
import qualified Quone.Format.Format as Fmt
import qualified Quone.Lex.Token as Token
import qualified Quone.Lsp.Compile as Compile
import qualified Quone.Lsp.Json as Json
import qualified Quone.Lsp.State as State
import qualified Quone.Lsp.Symbols as Symbols
import qualified Quone.Position as Position
import qualified Quone.Prelude.Load as PreludeLoad
import qualified Quone.Type.Env as TypeEnv
import qualified Quone.Type.Infer as Infer
import qualified Prelude



-- | The capabilities object returned from @initialize@.
initializeResult :: Json.Value
initializeResult =
    Json.object
        [ ("capabilities"
          , Json.object
                [ ("textDocumentSync"
                  , Json.object
                        [ ("openClose", Json.VBool Prelude.True)
                        , ("change", Json.int 1)
                        ])
                , ("hoverProvider", Json.VBool Prelude.True)
                , ("definitionProvider", Json.VBool Prelude.True)
                , ("documentSymbolProvider", Json.VBool Prelude.True)
                , ("completionProvider"
                  , Json.object
                        [ ("triggerCharacters", Json.VArray [Json.str "."])
                        ])
                , ("documentFormattingProvider", Json.VBool Prelude.True)
                ])
        , ("serverInfo"
          , Json.object
                [ ("name", Json.str "quonec")
                , ("version", Json.str "0.0.1")
                ])
        ]


-- | Build a @textDocument/publishDiagnostics@ notification payload
-- for a given URI and diagnostic list.
publishDiagnostics :: Text -> [Diagnostic] -> Json.Value
publishDiagnostics uri ds =
    Json.object
        [ ("jsonrpc", Json.str "2.0")
        , ("method", Json.str "textDocument/publishDiagnostics")
        , ("params"
          , Json.object
                [ ("uri", Json.str uri)
                , ("diagnostics", Json.VArray (Prelude.fmap diagJson ds))
                ])
        ]


diagJson :: Diagnostic -> Json.Value
diagJson d =
    let
        Position.SourceSpan start finish = diagSpan d
    in
    Json.object
        [ ("severity", Json.int (severityCode (diagSeverity d)))
        , ("source", Json.str "quonec")
        , ("message", Json.str (diagMessage d))
        , ("code", Json.str (Diag.categoryName (diagCategory d)))
        , ("range"
          , Json.object
                [ ("start", positionJson start)
                , ("end", positionJson finish)
                ])
        ]


positionJson :: Position.SourcePos -> Json.Value
positionJson p =
    Json.object
        [ ("line", Json.int (Position.posLine p Prelude.- 1))
        , ("character", Json.int (Position.posCol p Prelude.- 1))
        ]


severityCode :: Diag.Severity -> Int
severityCode = \case
    Diag.Error -> 1
    Diag.Warning -> 2
    Diag.Info -> 3


-- ---------------------------------------------------------------------
-- Document lifecycle
-- ---------------------------------------------------------------------


handleDidOpen :: Json.Value -> State.State -> (State.State, Maybe (Text, [Diagnostic]))
handleDidOpen params state = case extractDocument params of
    Just (uri, text, ver) -> updateDocAndDiags uri text ver state
    Prelude.Nothing -> (state, Prelude.Nothing)


handleDidChange :: Json.Value -> State.State -> (State.State, Maybe (Text, [Diagnostic]))
handleDidChange params state = case extractChanged params of
    Just (uri, text, ver) -> updateDocAndDiags uri text ver state
    Prelude.Nothing -> (state, Prelude.Nothing)


-- | Common did-open / did-change body. Stores the new document text,
-- runs the compile, publishes diagnostics, and (M4.4) caches the
-- last-successful compile in @stateLastGood@ so hover / definition
-- handlers can fall back to known-good info while the user edits.
updateDocAndDiags
    :: Text
    -> Text
    -> Int
    -> State.State
    -> (State.State, Maybe (Text, [Diagnostic]))
updateDocAndDiags uri text ver state =
    let
        doc = State.Document {State.docText = text, State.docVersion = ver}
        state' = State.putDocument uri doc state
        result = Compile.compileText (uriToPath uri) text
        (state'', diags) = case result of
            Compile.CompileOk prog typed ->
                let
                    lg =
                        State.LastGood
                            { State.lastGoodVersion = ver
                            , State.lastGoodProgram = prog
                            , State.lastGoodTyped = typed
                            }
                in
                (State.putLastGood uri lg state', [])
            Compile.CompileFailed ds -> (state', ds)
    in
    (state'', Just (uri, diags))


extractDocument :: Json.Value -> Maybe (Text, Text, Int)
extractDocument params = do
    docVal <- Json.lookupField "textDocument" params
    uri <- Json.lookupField "uri" docVal Prelude.>>= Json.asString
    text <- Json.lookupField "text" docVal Prelude.>>= Json.asString
    let
        ver =
            case Json.lookupField "version" docVal Prelude.>>= Json.asInt of
                Just n -> n
                Prelude.Nothing -> 0
    Prelude.pure (uri, text, ver)


extractChanged :: Json.Value -> Maybe (Text, Text, Int)
extractChanged params = do
    docVal <- Json.lookupField "textDocument" params
    uri <- Json.lookupField "uri" docVal Prelude.>>= Json.asString
    let
        ver =
            case Json.lookupField "version" docVal Prelude.>>= Json.asInt of
                Just n -> n
                Prelude.Nothing -> 0
    changes <- Json.lookupField "contentChanges" params Prelude.>>= Json.asArray
    case changes of
        [] -> Prelude.Nothing
        (c : _) -> do
            text <- Json.lookupField "text" c Prelude.>>= Json.asString
            Prelude.pure (uri, text, ver)


uriToPath :: Text -> Text
uriToPath uri =
    case T.stripPrefix "file://" uri of
        Just rest -> rest
        Prelude.Nothing -> uri



-- ---------------------------------------------------------------------
-- Read-only handlers
-- ---------------------------------------------------------------------


handleHover :: Json.Value -> State.State -> Json.Value
handleHover params state = case lookupContext params state of
    Prelude.Nothing -> Json.VNull
    Just (uri, doc, prog, typed) ->
        case requestPosition params of
            Prelude.Nothing -> Json.VNull
            Just (line, col) ->
                let
                    symbols = allSymbols prog typed
                in
                case Symbols.symbolAt line col symbols of
                    Prelude.Nothing ->
                        case wordAt line col (State.docText doc) of
                            Prelude.Nothing -> Json.VNull
                            Just word ->
                                case Prelude.filter (\s -> Symbols.symName s Prelude.== word) symbols of
                                    (sym : _) ->
                                        Json.object
                                            [ ("contents", hoverContents sym)
                                            , ("range", spanRange (Symbols.symSpan sym))
                                            ]
                                    [] -> verbHover word
                    Just sym ->
                        Json.object
                            [ ("contents", hoverContents sym)
                            , ("range", spanRange (Symbols.symSpan sym))
                            ]


handleDefinition :: Json.Value -> State.State -> Json.Value
handleDefinition params state = case lookupContext params state of
    Prelude.Nothing -> Json.VNull
    Just (uri, doc, prog, typed) ->
        case requestPosition params of
            Prelude.Nothing -> Json.VNull
            Just (line, col) ->
                let
                    symbols = allSymbols prog typed
                    needle = wordAt line col (State.docText doc)
                    matches = case needle of
                        Prelude.Nothing -> []
                        Just n ->
                            Prelude.filter
                                (\s -> Symbols.symName s Prelude.== n)
                                symbols
                in
                case matches of
                    (sym : _) ->
                        Json.object
                            [ ("uri", Json.str uri)
                            , ("range", spanRange (Symbols.symSpan sym))
                            ]
                    [] -> Json.VNull


-- | Helper: render hover contents as LSP MarkedString / MarkedString[].
--
-- VS Code/Cursor syntax-highlight MarkedString objects with a language more
-- reliably than markdown code fences returned as MarkupContent.
hoverContents :: Symbols.Symbol -> Json.Value
hoverContents sym =
    let
        sigBlock = case Symbols.symType sym of
            Just sch ->
                Json.object
                    [ ("language", Json.str "quone")
                    , ("value"
                      , Json.str
                            (Symbols.renderSignature (Symbols.symName sym) sch)
                      )
                    ]
            Prelude.Nothing -> Json.str ("`" ++ Symbols.symName sym ++ "`")
    in
    case Symbols.symDoc sym of
        [] -> sigBlock
        doc ->
            Json.VArray
                [ Json.str (T.intercalate "\n" doc)
                , sigBlock
                ]


verbHover :: Text -> Json.Value
verbHover word =
    case Token.textKeyword word Prelude.>>= verbSignature of
        Just (doc, sig) ->
            Json.object
                [ ( "contents"
                  , Json.VArray
                        [ Json.str doc
                        , Json.object
                            [ ("language", Json.str "quone")
                            , ("value", Json.str sig)
                            ]
                        ]
                  )
                ]
        Prelude.Nothing -> Json.VNull


verbSignature :: Token.Keyword -> Maybe (Text, Text)
verbSignature = \case
    Token.KSelect ->
        Just
            ( "Keep a subset of columns from a dataframe."
            , "select : { columns } -> Dataframe a -> Dataframe selected"
            )
    Token.KFilter ->
        Just
            ( "Keep rows where the predicate is TRUE."
            , "filter : Vector Logical -> Dataframe a -> Dataframe a"
            )
    Token.KMutate ->
        Just
            ( "Add or replace columns using vectorized expressions."
            , "mutate : { new_columns } -> Dataframe a -> Dataframe (a + new_columns)"
            )
    Token.KSummarize ->
        Just
            ( "Collapse each group to summary columns."
            , "summarize : { summaries } -> GroupedDataframe keys a -> Dataframe (keys + summaries)"
            )
    Token.KGroupBy ->
        Just
            ( "Group rows by one or more columns."
            , "group_by : { keys } -> Dataframe a -> GroupedDataframe keys a"
            )
    Token.KUngroup ->
        Just
            ( "Remove grouping from a dataframe."
            , "ungroup : GroupedDataframe keys a -> Dataframe a"
            )
    Token.KArrange ->
        Just
            ( "Sort rows by one or more columns."
            , "arrange : { sort_columns } -> Dataframe a -> Dataframe a"
            )
    Token.KRename ->
        Just
            ( "Rename columns without changing their values."
            , "rename : { new = old } -> Dataframe a -> Dataframe renamed"
            )
    Token.KLeftJoin ->
        Just
            ( "Keep all left rows and attach matching right columns."
            , "left_join : Dataframe b -> { keys } -> Dataframe a -> Dataframe joined"
            )
    Token.KRightJoin ->
        Just
            ( "Keep all right rows and attach matching left columns."
            , "right_join : Dataframe b -> { keys } -> Dataframe a -> Dataframe joined"
            )
    Token.KInnerJoin ->
        Just
            ( "Keep rows whose keys match in both dataframes."
            , "inner_join : Dataframe b -> { keys } -> Dataframe a -> Dataframe joined"
            )
    _ ->
        Prelude.Nothing


-- | Best-effort identifier-at-position lookup. Used by go-to-definition
-- to scope down which symbol the user clicked. Returns 'Nothing' if
-- the line is empty or the cursor is on whitespace.
wordAt :: Int -> Int -> Text -> Maybe Text
wordAt lspLine lspCol src =
    let
        ls = T.splitOn "\n" src
        line = atIndex lspLine ls
    in
    case line of
        Prelude.Nothing -> Prelude.Nothing
        Just l ->
            let
                chars = T.unpack l
                idx = Prelude.fromIntegral lspCol :: Prelude.Int
                isWord c = Char.isAlphaNum c Prelude.|| c Prelude.== '_'
                start = takeBackWhile isWord (Prelude.take idx chars)
                end = Prelude.takeWhile isWord (Prelude.drop idx chars)
                w = start Prelude.++ end
            in
            if Prelude.null w then Prelude.Nothing else Just (T.pack w)


atIndex :: Int -> [a] -> Maybe a
atIndex i xs =
    case Prelude.drop (Prelude.fromIntegral i) xs of
        (x : _) -> Just x
        [] -> Prelude.Nothing


takeBackWhile :: (Prelude.Char -> Prelude.Bool) -> [Prelude.Char] -> [Prelude.Char]
takeBackWhile p =
    Prelude.reverse Prelude.. Prelude.takeWhile p Prelude.. Prelude.reverse


-- | Pull a typed program and the open 'State.Document' for the URI
-- in a request payload. Returns 'Nothing' when the document is not
-- open or fails to type-check (in which case hover / definition /
-- completion all silently return null, matching how other LSPs
-- behave on broken files).
lookupContext
    :: Json.Value
    -> State.State
    -> Maybe
        ( Text
        , State.Document
        , Ast.Program
        , Infer.TypedProgram
        )
lookupContext params state = do
    docVal <- Json.lookupField "textDocument" params
    uri <- Json.lookupField "uri" docVal Prelude.>>= Json.asString
    doc <- State.getDocument uri state
    case Compile.compileText (uriToPath uri) (State.docText doc) of
        Compile.CompileOk prog typed ->
            Just (uri, doc, prog, typed)
        Compile.CompileFailed _ ->
            -- Fall back to the last successful compile (M4.4) so
            -- hover / definition keep returning useful answers
            -- while the user is mid-edit.
            case State.getLastGood uri state of
                Just lg ->
                    Just
                        ( uri
                        , doc
                        , State.lastGoodProgram lg
                        , State.lastGoodTyped lg
                        )
                Prelude.Nothing -> Prelude.Nothing


-- | Pull the @position@ field from a request payload.
requestPosition :: Json.Value -> Maybe (Int, Int)
requestPosition params = do
    pos <- Json.lookupField "position" params
    line <- Json.lookupField "line" pos Prelude.>>= Json.asInt
    char <- Json.lookupField "character" pos Prelude.>>= Json.asInt
    Prelude.pure (line, char)


handleDocumentSymbol :: Json.Value -> State.State -> Json.Value
handleDocumentSymbol params state = case Json.lookupField "textDocument" params of
    Just docVal -> case Json.lookupField "uri" docVal Prelude.>>= Json.asString of
        Just uri -> case State.getDocument uri state of
            Just doc ->
                case Compile.compileText
                        (uriToPath uri)
                        (State.docText doc) of
                    Compile.CompileOk prog _ ->
                        Json.VArray
                            (Prelude.fmap valueDeclSymbol
                                (programValueDecls prog))
                    Compile.CompileFailed _ ->
                        -- Graceful fallback (M4.4): if the current
                        -- compile fails, surface the symbols from
                        -- the last successful compile so the
                        -- outline view keeps working.
                        case State.getLastGood uri state of
                            Just lg ->
                                Json.VArray
                                    (Prelude.fmap valueDeclSymbol
                                        (programValueDecls
                                            (State.lastGoodProgram lg)))
                            Prelude.Nothing -> Json.VArray []
            Prelude.Nothing -> Json.VArray []
        Prelude.Nothing -> Json.VArray []
    Prelude.Nothing -> Json.VArray []


programValueDecls :: Ast.Program -> [Ast.ValueDecl]
programValueDecls p =
    [ v | Ast.DValue v <- Ast.programDecls p ]


valueDeclSymbol :: Ast.ValueDecl -> Json.Value
valueDeclSymbol v =
    let
        sp = Ast.declSpan (Ast.DValue v)
    in
    Json.object
        [ ("name", Json.str (Ast.lowerText (Ast.valueDeclName v)))
        , ("kind", Json.int 12)
        , ("range", spanRange sp)
        , ("selectionRange", spanRange sp)
        ]


spanRange :: Position.SourceSpan -> Json.Value
spanRange (Position.SourceSpan s f) =
    Json.object
        [ ("start", positionJson s)
        , ("end", positionJson f)
        ]


handleCompletion :: Json.Value -> State.State -> Json.Value
handleCompletion params state =
    let
        symbolItems = case lookupContext params state of
            Just (_, _, prog, typed) ->
                Prelude.fmap symbolItem (allSymbols prog typed)
            Prelude.Nothing -> []
        keywordItems =
            Prelude.fmap keywordItem
                (Prelude.fmap Token.keywordText Token.allKeywords)
    in
    Json.object
        [ ("isIncomplete", Json.VBool Prelude.False)
        , ("items", Json.VArray (symbolItems Prelude.++ keywordItems))
        ]


keywordItem :: Text -> Json.Value
keywordItem kw =
    Json.object
        [ ("label", Json.str kw)
        , ("kind", Json.int 14)
        ]


-- | Build a CompletionItem for a single symbol.
--
-- Uses the LSP standard kind codes:
-- 12 = Variable, 7 = Class, 4 = Constructor, 9 = Module, 6 = Function.
symbolItem :: Symbols.Symbol -> Json.Value
symbolItem s =
    Json.object
        [ ("label", Json.str (Symbols.symName s))
        , ("kind"
          , Json.int
                (case Symbols.symKind s of
                    Symbols.SymValue -> 12
                    Symbols.SymType -> 7
                    Symbols.SymConstructor -> 4
                    Symbols.SymImport -> 9
                    Symbols.SymForeign -> 6))
        , ("detail"
          , Json.str
                (case Symbols.symType s of
                    Just sch -> Symbols.renderScheme sch
                    Prelude.Nothing -> ""))
        ]


handleFormatting :: Json.Value -> State.State -> Json.Value
handleFormatting params state = case Json.lookupField "textDocument" params of
    Just docVal -> case Json.lookupField "uri" docVal Prelude.>>= Json.asString of
        Just uri -> case State.getDocument uri state of
            Just doc ->
                case Fmt.format (uriToPath uri) (State.docText doc) of
                    Prelude.Right formatted ->
                        Json.VArray
                            [ Json.object
                                [ ("range", fullRange (State.docText doc))
                                , ("newText", Json.str formatted)
                                ]
                            ]
                    Prelude.Left _ -> Json.VArray []
            Prelude.Nothing -> Json.VArray []
        Prelude.Nothing -> Json.VArray []
    Prelude.Nothing -> Json.VArray []


allSymbols :: Ast.Program -> Infer.TypedProgram -> [Symbols.Symbol]
allSymbols prog typed =
    Symbols.buildIndex prog typed Prelude.++ preludeSymbols


preludeSymbols :: [Symbols.Symbol]
preludeSymbols =
    case PreludeLoad.loadPrelude of
        Prelude.Right loaded ->
            Symbols.buildIndex
                (PreludeLoad.preludeProgram loaded)
                ( Infer.TypedProgram
                    { Infer.typedProgram = PreludeLoad.preludeProgram loaded
                    , Infer.typedBindings = TypeEnv.envValues (PreludeLoad.preludeEnv loaded)
                    }
                )
        Prelude.Left _ -> []


fullRange :: Text -> Json.Value
fullRange src =
    let
        ls =
            T.splitOn "\n" src

        endLine :: Int
        endLine =
            Prelude.fromIntegral (Prelude.length ls Prelude.- 1)

        endCharacter :: Int
        endCharacter =
            case Prelude.reverse ls of
                lastLine : _ -> Prelude.fromIntegral (T.length lastLine)
                [] -> 0
    in
    Json.object
        [ ("start"
          , Json.object
                [ ("line", Json.int 0)
                , ("character", Json.int 0)
                ])
        , ("end"
          , Json.object
                [ ("line", Json.int endLine)
                , ("character", Json.int endCharacter)
                ])
        ]
