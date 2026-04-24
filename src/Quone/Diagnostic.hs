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
    )
where

import qualified Data.Text as T
import NriPrelude
import Quone.Position
    ( SourceSpan
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


-- | Render a diagnostic to a single multi-line text block in the form:
--
-- @
-- error[type-mismatch]: cannot unify Integer with Double
--   --> src/foo.Q:3:7-12
-- hint: insert an explicit \`to_double\` conversion
-- @
render :: Diagnostic -> Text
render d =
    let
        sev = case diagSeverity d of
            Error -> "error"
            Warning -> "warning"
            Info -> "info"

        header =
            sev
                ++ "["
                ++ categoryName (diagCategory d)
                ++ "]: "
                ++ diagMessage d

        loc =
            "  --> " ++ showSourceSpan (diagSpan d)

        hint = case diagHint d of
            Just h -> "\nhint: " ++ h
            Nothing -> ""
    in
    header ++ "\n" ++ loc ++ hint
