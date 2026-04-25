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
        , Harness.test "lsp/hover_formats_large_signatures_like_quone" <|
            Prelude.pure hoverFormatsLargeSignaturesLikeQuone
        , Harness.test "lsp/hover_shows_dplyr_verb_signature" <|
            Prelude.pure hoverShowsDplyrVerbSignature
        , Harness.test "lsp/formatting_replaces_final_line_without_trailing_newline" <|
            Prelude.pure formattingReplacesFinalLineWithoutTrailingNewline
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


hoverFormatsLargeSignaturesLikeQuone :: TestResult
hoverFormatsLargeSignaturesLikeQuone =
    let
        uri =
            "file:///hover-mtcars.Q"

        src =
            T.unlines
                [ "type alias Cars <-"
                , "    dataframe"
                , "        { model : Vector Character"
                , "        , mpg : Vector Double"
                , "        , cyl : Vector Integer"
                , "        , hp : Vector Double"
                , "        , wt : Vector Double"
                , "        }"
                , ""
                , "mtcars_demo :"
                , "    Cars ->"
                , "    dataframe"
                , "        { cyl : Vector Integer"
                , "        , n_cars : Vector Integer"
                , "        , avg_mpg : Vector Double"
                , "        , avg_hp : Vector Double"
                , "        }"
                , "mtcars_demo cars <-"
                , "    cars"
                , "        |> filter (mpg > mean mpg)"
                , "        |> mutate { power_to_weight = hp / wt }"
                , "        |> group_by { cyl }"
                , "        |> summarize"
                , "            { n_cars = count"
                , "            , avg_mpg = mean mpg"
                , "            , avg_hp = mean hp"
                , "            }"
                , "        |> arrange { desc avg_mpg }"
                ]

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
                        [ ("line", Json.int 17)
                        , ("character", Json.int 2)
                        ]
                  )
                ]

        expected =
            T.unlines
                [ "mtcars_demo :"
                , "    Cars ->"
                , "    dataframe"
                , "        { avg_hp : Vector Double"
                , "        , avg_mpg : Vector Double"
                , "        , cyl : Vector Integer"
                , "        , n_cars : Vector Integer"
                , "        }"
                ]

        (state, _) =
            Handlers.handleDidOpen openParams State.empty

        hover =
            Handlers.handleHover hoverParams state
    in
    case Json.lookupField "contents" hover of
        Just contents ->
            case Json.lookupField "value" contents Prelude.>>= Json.asString of
                Just value ->
                    value === T.dropEnd 1 expected

                Prelude.Nothing ->
                    Fail ("missing hover value: " ++ T.pack (Prelude.show hover))

        other ->
            Fail ("unexpected mtcars hover payload: " ++ T.pack (Prelude.show other))


hoverShowsDplyrVerbSignature :: TestResult
hoverShowsDplyrVerbSignature =
    let
        uri =
            "file:///hover-verb.Q"

        src =
            T.unlines
                [ "type alias Cars <- dataframe { mpg : Vector Double }"
                , ""
                , "demo : Cars -> Cars"
                , "demo cars <- cars |> filter (mpg > mean mpg)"
                ]

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
                        [ ("line", Json.int 3)
                        , ("character", Json.int 23)
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
                && value Prelude.== Just "filter : Vector Logical -> Dataframe a -> Dataframe a"
                && "Keep rows" `T.isInfixOf` doc
            then
                Pass
            else
                Fail ("unexpected verb hover: " ++ T.pack (Prelude.show hover))

        other ->
            Fail ("unexpected verb hover payload: " ++ T.pack (Prelude.show other))


formattingReplacesFinalLineWithoutTrailingNewline :: TestResult
formattingReplacesFinalLineWithoutTrailingNewline =
    let
        uri =
            "file:///format-no-newline.Q"

        src =
            "answer<-42"

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

        formatParams =
            Json.object
                [ ( "textDocument"
                  , Json.object [("uri", Json.str uri)]
                  )
                ]

        (state, _) =
            Handlers.handleDidOpen openParams State.empty

        result =
            Handlers.handleFormatting formatParams state
    in
    case result of
        Json.VArray [edit] ->
            let
                mRange =
                    Json.lookupField "range" edit

                mNewText =
                    Json.lookupField "newText" edit Prelude.>>= Json.asString

                mEnd =
                    mRange Prelude.>>= Json.lookupField "end"

                mEndLine =
                    mEnd Prelude.>>= Json.lookupField "line" Prelude.>>= Json.asInt

                mEndCharacter =
                    mEnd Prelude.>>= Json.lookupField "character" Prelude.>>= Json.asInt
            in
            if mEndLine Prelude.== Just 0
                && mEndCharacter Prelude.== Just (Prelude.fromIntegral (T.length src))
                && mNewText Prelude.== Just "answer <- 42\n"
            then
                Pass
            else
                Fail ("unexpected formatting edit: " ++ T.pack (Prelude.show edit))

        other ->
            Fail ("unexpected formatting response: " ++ T.pack (Prelude.show other))

