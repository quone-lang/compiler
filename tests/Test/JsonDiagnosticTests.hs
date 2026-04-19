{-| Tests for the NDJSON diagnostic encoder.

Per the project plan (compiler track A1) the encoder must produce a
stable schema across all ten LANGUAGE.md section 12.1 categories so
the R companion package can pattern-match without regex parsing.

-}
module Test.JsonDiagnosticTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Diagnostic
    ( Category (..)
    , Diagnostic (..)
    , Severity (..)
    )
import qualified Quone.Diagnostic.Json as Json
import Quone.Position
    ( SourcePos (..)
    , SourceSpan (..)
    )
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
        "diagnostic-json"
        ( categoryRoundTripTests
            ++ severityTests
            ++ shapeTests
            ++ encodingTests
        )


sample :: Category -> Diagnostic
sample c =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = c
        , diagSpan =
            SourceSpan
                (SourcePos "src/foo.Q" 3 7)
                (SourcePos "src/foo.Q" 3 12)
        , diagMessage = "example message"
        , diagHint = Just "an actionable hint"
        }


-- ---------------------------------------------------------------------
-- Every category must be encodable
-- ---------------------------------------------------------------------


categoryRoundTripTests :: [Test]
categoryRoundTripTests =
    [ caseTest c | c <- everyCategory ]
  where
    caseTest c =
        test
            ("diagnostic-json/category_" ++ catName c ++ "_section_12_1")
            ( do
                let
                    encoded = Json.encodeDiagnostic (sample c)
                Prelude.pure
                    (T.isInfixOf
                        ("\"category\":\"" ++ catName c ++ "\"")
                        encoded
                        === Prelude.True)
            )


everyCategory :: [Category]
everyCategory =
    [ Lexical
    , Parse
    , UnboundVariable
    , TypeMismatch
    , UnknownConstructor
    , RecordField
    , UnknownDataframeColumn
    , FileLoading
    , Decode
    , NonExhaustivePattern
    , Internal
    ]


catName :: Category -> Text
catName = \case
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


-- ---------------------------------------------------------------------
-- Severity rendering
-- ---------------------------------------------------------------------


severityTests :: [Test]
severityTests =
    [ test "diagnostic-json/severity_error_section_12_1"
        (Prelude.pure (Json.severityName Error === "error"))
    , test "diagnostic-json/severity_warning_section_12_1"
        (Prelude.pure (Json.severityName Warning === "warning"))
    , test "diagnostic-json/severity_info_section_12_1"
        (Prelude.pure (Json.severityName Info === "info"))
    ]


-- ---------------------------------------------------------------------
-- Schema shape: required keys + line/col + hint policy
-- ---------------------------------------------------------------------


shapeTests :: [Test]
shapeTests =
    [ test "diagnostic-json/contains_required_keys_section_12_1"
        ( do
            let
                encoded = Json.encodeDiagnostic (sample TypeMismatch)
                required =
                    [ "\"severity\":"
                    , "\"category\":"
                    , "\"file\":"
                    , "\"start\":"
                    , "\"end\":"
                    , "\"message\":"
                    ]
                allPresent =
                    Prelude.all (\k -> T.isInfixOf k encoded) required
            Prelude.pure (allPresent === Prelude.True)
        )
    , test "diagnostic-json/encodes_position_section_12_1"
        ( do
            let
                encoded = Json.encodeDiagnostic (sample TypeMismatch)
            Prelude.pure
                (T.isInfixOf "\"line\":3" encoded === Prelude.True)
        )
    , test "diagnostic-json/omits_hint_when_absent_section_12_1"
        ( do
            let
                d = (sample TypeMismatch) {diagHint = Prelude.Nothing}
                encoded = Json.encodeDiagnostic d
            Prelude.pure
                (T.isInfixOf "\"hint\":" encoded === Prelude.False)
        )
    , test "diagnostic-json/keeps_hint_when_present_section_12_1"
        ( do
            let
                encoded = Json.encodeDiagnostic (sample TypeMismatch)
            Prelude.pure
                (T.isInfixOf
                    "\"hint\":\"an actionable hint\""
                    encoded
                    === Prelude.True)
        )
    ]


-- ---------------------------------------------------------------------
-- Stream-level encoding
-- ---------------------------------------------------------------------


encodingTests :: [Test]
encodingTests =
    [ test "diagnostic-json/empty_list_is_empty_section_12_1"
        (Prelude.pure (Json.encodeDiagnostics [] === ""))
    , test "diagnostic-json/single_entry_has_trailing_newline_section_12_1"
        ( do
            let
                encoded = Json.encodeDiagnostics [sample Parse]
            Prelude.pure
                ( assertCond
                    "ends with newline"
                    (T.isSuffixOf "\n" encoded)
                )
        )
    , test "diagnostic-json/multiple_entries_separated_by_newline_section_12_1"
        ( do
            let
                ds = [sample Parse, sample TypeMismatch]
                encoded = Json.encodeDiagnostics ds
                lineCount =
                    Prelude.length
                        (T.unpack (T.filter (Prelude.== '\n') encoded))
            Prelude.pure
                ( assertCond
                    ("expected 2 newlines, got "
                        ++ T.pack (Prelude.show lineCount))
                    (lineCount Prelude.== 2)
                )
        )
    , test "diagnostic-json/escapes_quotes_in_message_section_12_1"
        ( do
            let
                d =
                    (sample Parse)
                        { diagMessage = "found \"oops\" in input"
                        }
                encoded = Json.encodeDiagnostic d
            Prelude.pure
                ( assertCond
                    "encoded form must escape quotes"
                    (T.isInfixOf "\\\"oops\\\"" encoded)
                )
        )
    ]


assertCond :: Text -> Prelude.Bool -> TestResult
assertCond msg cond = if cond then Pass else Fail msg
