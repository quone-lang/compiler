{-| NDJSON encoding of compiler diagnostics.

The R companion package (and any other downstream tool) consumes
diagnostics through a stable machine-readable channel. To keep the
compiler's dependency footprint small we hand-roll the JSON encoder
rather than pulling in @aeson@; the schema we need is closed and
narrow.

Schema (one JSON object per line):

@
{"severity":"error",
 "category":"type-mismatch",
 "file":"src/foo.Q",
 "start":{"line":3,"col":7},
 "end":{"line":3,"col":12},
 "message":"cannot unify Integer with Double",
 "hint":"insert an explicit `to_double` conversion"}
@

The @category@ values are the closed set returned by
'Quone.Diagnostic.categoryName' so consumers can pattern-match
on them. The @hint@ field is omitted when 'diagHint' is 'Nothing'.

-}
module Quone.Diagnostic.Json
    ( encodeDiagnostic
    , encodeDiagnostics
    , severityName
    )
where

import qualified Data.Text as T
import NriPrelude
import Quone.Diagnostic
    ( Diagnostic (..)
    , Severity (..)
    , categoryName
    )
import Quone.Position
    ( SourcePos (..)
    , SourceSpan (..)
    )
import qualified Prelude



-- | The short string used for the @severity@ field. Mirrors
-- 'categoryName' for 'Category'.
severityName :: Severity -> Text
severityName = \case
    Error -> "error"
    Warning -> "warning"
    Info -> "info"


-- | Encode a single diagnostic as one JSON object on a single line.
-- The output never contains a trailing newline; callers that emit
-- NDJSON streams add the newline themselves.
encodeDiagnostic :: Diagnostic -> Text
encodeDiagnostic d =
    let
        SourceSpan start finish = diagSpan d
        baseFields =
            [ field "severity" (jsonString (severityName (diagSeverity d)))
            , field "category" (jsonString (categoryName (diagCategory d)))
            , field "file" (jsonString (posFile start))
            , field "start" (encodePos start)
            , field "end" (encodePos finish)
            , field "message" (jsonString (diagMessage d))
            ]
        allFields = case diagHint d of
            Just h -> baseFields ++ [field "hint" (jsonString h)]
            Nothing -> baseFields
    in
    "{" ++ T.intercalate "," allFields ++ "}"


-- | Encode a list of diagnostics as NDJSON: one object per line, with
-- a trailing newline after the last entry. Empty lists produce the
-- empty string so callers can write the result directly to a stream
-- without conditional handling.
encodeDiagnostics :: [Diagnostic] -> Text
encodeDiagnostics ds =
    case ds of
        [] -> ""
        _ -> T.intercalate "\n" (Prelude.fmap encodeDiagnostic ds) ++ "\n"


-- | Encode a 'SourcePos' as @{"line":N,"col":N}@.
encodePos :: SourcePos -> Text
encodePos p =
    "{"
        ++ field "line" (jsonInt (posLine p))
        ++ ","
        ++ field "col" (jsonInt (posCol p))
        ++ "}"


-- ---------------------------------------------------------------------
-- Tiny JSON helpers
-- ---------------------------------------------------------------------


field :: Text -> Text -> Text
field key value = jsonString key ++ ":" ++ value


jsonInt :: Int -> Text
jsonInt n = T.pack (Prelude.show (Prelude.fromIntegral n :: Prelude.Int))


-- | Encode a JSON string with the standard escapes from RFC 8259.
-- Non-ASCII characters are passed through verbatim because the
-- compiler only emits UTF-8 (LANGUAGE2.md section 3.1) and consumers
-- handle UTF-8 input.
jsonString :: Text -> Text
jsonString s = "\"" ++ T.concatMap escapeChar s ++ "\""


escapeChar :: Prelude.Char -> Text
escapeChar c = case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\b' -> "\\b"
    '\f' -> "\\f"
    _ ->
        if Prelude.fromEnum c < 0x20
            then unicodeEscape c
            else T.singleton c


unicodeEscape :: Prelude.Char -> Text
unicodeEscape c =
    let
        hex = padHex (T.pack (showHex (Prelude.fromEnum c) ""))
    in
    "\\u" ++ hex


padHex :: Text -> Text
padHex t =
    let
        zeros = T.replicate (4 Prelude.- T.length t) "0"
    in
    zeros ++ t


showHex :: Prelude.Int -> Prelude.String -> Prelude.String
showHex n acc
    | n Prelude.< 16 = digit n : acc
    | Prelude.otherwise =
        showHex (n `Prelude.div` 16) (digit (n `Prelude.mod` 16) : acc)
  where
    digit d
        | d Prelude.< 10 = Prelude.toEnum (d Prelude.+ Prelude.fromEnum '0')
        | Prelude.otherwise = Prelude.toEnum (d Prelude.- 10 Prelude.+ Prelude.fromEnum 'a')
