{-| Structured compiler diagnostics.

LANGUAGE.md section 12.1 enumerates ten error categories that a
conforming implementation MUST recognise. This module pins each
category to a constructor of 'Category' so every compiler phase can
emit diagnostics in a uniform shape and the test suite can assert on
the exact category produced.

-}
module Quone.Diagnostic
    ( Category (..)
    , Severity (..)
    , Diagnostic (..)
    , DiagnosticsFormat (..)
    , categoryName
    , render
    , renderWithSource
    )
where

import qualified Data.Text as T
import NriPrelude
import Quone.Position
    ( SourcePos (..)
    , SourceSpan (..)
    , showSourceSpan
    )
import qualified Prelude


-- | The ten error categories from LANGUAGE.md section 12.1, plus a
-- generic 'Internal' for compiler bugs.
data Category
    = Lexical
    | Parse
    | UnboundVariable
    | TypeMismatch
    | UnknownConstructor
    | RecordField
    | UnknownDataframeColumn
    | FileLoading
    | Decode
    | NonExhaustivePattern
    | Internal
    | -- Lint findings (M5.4). Advisory; severity is 'Warning'.
      UnusedImport
    | MissingExportAnnotation
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- | Diagnostic severity. initial release only emits 'Error' and 'Warning'; the
-- third level is reserved for future LSP / stylistic notices.
data Severity
    = Error
    | Warning
    | Info
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- | The output format diagnostics use on stderr. The default
-- 'HumanDiagnostics' renders one diagnostic per multi-line block via
-- 'render'; 'JsonDiagnostics' emits NDJSON via
-- 'Quone.Diagnostic.Json.encodeDiagnostic'.
--
-- Selected by the cross-cutting CLI flag
-- @--diagnostics-format=human|json@ (alias @--json@).
data DiagnosticsFormat
    = HumanDiagnostics
    | JsonDiagnostics
    deriving (Prelude.Show, Prelude.Eq)


-- | A single diagnostic message tied to a source range.
data Diagnostic = Diagnostic
    { diagSeverity :: Severity
    , diagCategory :: Category
    , diagSpan :: SourceSpan
    , diagMessage :: Text
    , diagHint :: Maybe Text
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | The short human-readable name used in CLI output and test names.
categoryName :: Category -> Text
categoryName = \case
    Lexical -> "lexical"
    Parse -> "parse"
    UnboundVariable -> "unbound-variable"
    TypeMismatch -> "type-mismatch"
    UnknownConstructor -> "unknown-constructor"
    RecordField -> "record-field"
    UnknownDataframeColumn -> "unknown-dataframe-column"
    FileLoading -> "file-loading"
    Decode -> "decode"
    NonExhaustivePattern -> "non-exhaustive-pattern"
    Internal -> "internal"
    UnusedImport -> "unused-import"
    MissingExportAnnotation -> "missing-export-annotation"


-- | Render a diagnostic without source context. CLI commands that have
-- the source text available should prefer 'renderWithSource' so users
-- see the offending line and underline.
render :: Diagnostic -> Text
render =
    renderWithSource ""


-- | Render a diagnostic as an Elm-style multi-line text block.
--
-- The diagnostic remains the compact semantic payload used by JSON and
-- LSP. This renderer is responsible for the source-level presentation:
-- title, file, source excerpt, underline, and optional explanatory hint.
--
-- @
-- -- TYPE MISMATCH ---------------------------------------- src/foo.Q
--
-- The 1st argument is not what I expect:
--
-- 3| mean 1
--         ^
-- This argument is:
--
--     Double
-- @
renderWithSource :: Text -> Diagnostic -> Text
renderWithSource source d =
    let
        headerParts =
            Prelude.filter
                (Prelude.not Prelude.. T.null)
                [ diagnosticHeader d
                , diagMessage d
                ]
        excerpt = sourceExcerpt source (diagSpan d)
        excerptAndHint = case diagHint d of
            Just hint -> excerpt ++ "\n" ++ hint
            Nothing -> excerpt
    in
    T.intercalate "\n\n" (headerParts ++ [excerptAndHint])


diagnosticHeader :: Diagnostic -> Text
diagnosticHeader d =
    let
        file = posFile (spanStart (diagSpan d))
        title = diagnosticTitle d
        width = 78
        fixed =
            3
                + T.length title
                + 1
                + 1
                + T.length file
        dashCount = Prelude.max 3 (width Prelude.- fixed)
    in
    "-- "
        ++ title
        ++ " "
        ++ T.replicate dashCount "-"
        ++ " "
        ++ file


diagnosticTitle :: Diagnostic -> Text
diagnosticTitle d =
    let
        prefix = case diagSeverity d of
            Error -> ""
            Warning -> "WARNING: "
            Info -> "INFO: "
    in
    prefix ++ categoryTitle (diagCategory d)


categoryTitle :: Category -> Text
categoryTitle = \case
    Lexical -> "LEXICAL ERROR"
    Parse -> "PARSE ERROR"
    UnboundVariable -> "UNBOUND VARIABLE"
    TypeMismatch -> "TYPE MISMATCH"
    UnknownConstructor -> "UNKNOWN CONSTRUCTOR"
    RecordField -> "RECORD FIELD"
    UnknownDataframeColumn -> "UNKNOWN DATAFRAME COLUMN"
    FileLoading -> "FILE LOADING"
    Decode -> "DECODE ERROR"
    NonExhaustivePattern -> "NON-EXHAUSTIVE PATTERN"
    Internal -> "INTERNAL ERROR"
    UnusedImport -> "UNUSED IMPORT"
    MissingExportAnnotation -> "MISSING EXPORT ANNOTATION"


sourceExcerpt :: Text -> SourceSpan -> Text
sourceExcerpt source (SourceSpan start finish) =
    case sourceLine source (posLine start) of
        Nothing -> "  --> " ++ showSourceSpan (SourceSpan start finish)
        Just lineText ->
            let
                lineNo = posLine start
                lineNoText = T.pack (Prelude.show lineNo)
                markerWidth = T.length lineNoText
                startCol = Prelude.max 1 (posCol start)
                startPadding = Prelude.fromIntegral (startCol Prelude.- 1)
                sameLine =
                    posFile start Prelude.== posFile finish
                        Prelude.&& posLine start Prelude.== posLine finish
                endCol =
                    if sameLine
                        then Prelude.max startCol (posCol finish)
                        else Prelude.max startCol (Prelude.fromIntegral (T.length lineText) Prelude.+ 1)
                caretLen = Prelude.max 1 (endCol Prelude.- startCol)
                caretCount = Prelude.fromIntegral caretLen
            in
            lineNoText
                ++ "| "
                ++ lineText
                ++ "\n"
                ++ T.replicate markerWidth " "
                ++ "| "
                ++ T.replicate startPadding " "
                ++ T.replicate caretCount "^"


sourceLine :: Text -> Int -> Maybe Text
sourceLine source lineNo
    | T.null source = Nothing
    | lineNo Prelude.<= 0 = Nothing
    | Prelude.otherwise =
        case Prelude.drop (Prelude.fromIntegral (lineNo Prelude.- 1)) (T.lines source) of
            lineText : _ -> Just lineText
            [] -> Nothing
