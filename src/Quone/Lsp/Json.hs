{-| Minimal JSON encoder / parser used by the LSP server.

Hand-rolled to keep the compiler dependency-light. Covers exactly
what the LSP wire format needs: nulls, booleans, numbers, strings,
arrays, and objects. Numbers parse as @Double@s and serialise back
without exponent for integral values when possible.

This sits next to 'Quone.Diagnostic.Json' (which is a one-way
encoder for the diagnostics shape) and complements it with a
parser for the request side of LSP.

-}
module Quone.Lsp.Json
    ( Value (..)
    , encode
    , decode
    , object
    , str
    , int
    , lookupField
    , asString
    , asInt
    , asObject
    , asArray
    )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import qualified Prelude



data Value
    = VNull
    | VBool Prelude.Bool
    | VNumber Prelude.Double
    | VString Text
    | VArray [Value]
    | VObject (Map.Map Text Value)
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------


encode :: Value -> Text
encode = \case
    VNull -> "null"
    VBool Prelude.True -> "true"
    VBool Prelude.False -> "false"
    VNumber d ->
        let
            asInt_ = Prelude.truncate d :: Prelude.Integer
        in
        if Prelude.fromIntegral asInt_ Prelude.== d
            then T.pack (Prelude.show asInt_)
            else T.pack (Prelude.show d)
    VString s -> encodeString s
    VArray xs ->
        "[" ++ T.intercalate "," (Prelude.fmap encode xs) ++ "]"
    VObject m ->
        "{"
            ++ T.intercalate
                ","
                (Prelude.fmap encodePair (Map.toAscList m))
            ++ "}"


encodePair :: (Text, Value) -> Text
encodePair (k, v) = encodeString k ++ ":" ++ encode v


encodeString :: Text -> Text
encodeString s = "\"" ++ T.concatMap escapeChar s ++ "\""


escapeChar :: Prelude.Char -> Text
escapeChar c = case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ ->
        if Prelude.fromEnum c Prelude.< 0x20
            then T.pack (Prelude.replicate 1 c)
            else T.singleton c


-- ---------------------------------------------------------------------
-- Decoding
-- ---------------------------------------------------------------------


-- | Parse a single JSON value. Returns 'Just (value, rest)' on
-- success, where @rest@ is the unconsumed text after trimming
-- trailing whitespace. Returns 'Nothing' on any parse error.
decode :: Text -> Maybe Value
decode raw =
    case parseValue (skipWs raw) of
        Just (v, _) -> Just v
        Prelude.Nothing -> Prelude.Nothing


parseValue :: Text -> Maybe (Value, Text)
parseValue input = case T.uncons input of
    Prelude.Nothing -> Prelude.Nothing
    Just (c, rest) -> case c of
        'n' -> matchKeyword input "null" VNull
        't' -> matchKeyword input "true" (VBool Prelude.True)
        'f' -> matchKeyword input "false" (VBool Prelude.False)
        '"' -> parseString rest
        '[' -> parseArray (skipWs rest)
        '{' -> parseObject (skipWs rest)
        _ -> parseNumber input


matchKeyword :: Text -> Text -> Value -> Maybe (Value, Text)
matchKeyword input kw v
    | kw `T.isPrefixOf` input =
        Just (v, skipWs (T.drop (T.length kw) input))
    | Prelude.otherwise = Prelude.Nothing


parseString :: Text -> Maybe (Value, Text)
parseString = go []
  where
    go acc input = case T.uncons input of
        Prelude.Nothing -> Prelude.Nothing
        Just ('"', rest) ->
            Just
                ( VString (T.pack (Prelude.reverse acc))
                , skipWs rest
                )
        Just ('\\', rest) -> case T.uncons rest of
            Prelude.Nothing -> Prelude.Nothing
            Just (c, more) -> go (escape c : acc) more
        Just (c, rest) -> go (c : acc) rest
    escape c = case c of
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'
        '"' -> '"'
        '\\' -> '\\'
        '/' -> '/'
        other -> other


parseNumber :: Text -> Maybe (Value, Text)
parseNumber input =
    let
        isNumChar :: Prelude.Char -> Prelude.Bool
        isNumChar c =
            Char.isDigit c
                Prelude.|| c Prelude.== '.'
                Prelude.|| c Prelude.== '-'
                Prelude.|| c Prelude.== 'e'
                Prelude.|| c Prelude.== 'E'
                Prelude.|| c Prelude.== '+'
        (digits, rest) = T.span isNumChar input
    in
    if T.null digits
        then Prelude.Nothing
        else case Prelude.reads (T.unpack digits) of
            [(n, "")] -> Just (VNumber n, skipWs rest)
            _ -> Prelude.Nothing


parseArray :: Text -> Maybe (Value, Text)
parseArray input = case T.uncons input of
    Just (']', rest) -> Just (VArray [], skipWs rest)
    _ -> goItems [] input
  where
    goItems acc cursor =
        case parseValue cursor of
            Prelude.Nothing -> Prelude.Nothing
            Just (v, after) ->
                let
                    afterTrim = skipWs after
                in
                case T.uncons afterTrim of
                    Just (',', more) -> goItems (v : acc) (skipWs more)
                    Just (']', more) ->
                        Just
                            ( VArray (Prelude.reverse (v : acc))
                            , skipWs more
                            )
                    _ -> Prelude.Nothing


parseObject :: Text -> Maybe (Value, Text)
parseObject input = case T.uncons input of
    Just ('}', rest) ->
        Just (VObject Map.empty, skipWs rest)
    _ -> goEntries Map.empty input
  where
    goEntries acc cursor = case parseValue cursor of
        Just (VString k, afterKey) ->
            let
                afterColon = skipWs afterKey
            in
            case T.uncons afterColon of
                Just (':', vRaw) ->
                    case parseValue (skipWs vRaw) of
                        Prelude.Nothing -> Prelude.Nothing
                        Just (v, afterValue) ->
                            let
                                afterValueTrim = skipWs afterValue
                                acc' = Map.insert k v acc
                            in
                            case T.uncons afterValueTrim of
                                Just (',', more) ->
                                    goEntries acc' (skipWs more)
                                Just ('}', more) ->
                                    Just
                                        ( VObject acc'
                                        , skipWs more
                                        )
                                _ -> Prelude.Nothing
                _ -> Prelude.Nothing
        _ -> Prelude.Nothing


skipWs :: Text -> Text
skipWs = T.dropWhile (\c -> c Prelude.== ' ' Prelude.|| c Prelude.== '\n' Prelude.|| c Prelude.== '\r' Prelude.|| c Prelude.== '\t')


-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------


object :: [(Text, Value)] -> Value
object = VObject Prelude.. Map.fromList


str :: Text -> Value
str = VString


int :: Int -> Value
int n = VNumber (Prelude.fromIntegral n)


lookupField :: Text -> Value -> Maybe Value
lookupField k = \case
    VObject m -> Map.lookup k m
    _ -> Prelude.Nothing


asString :: Value -> Maybe Text
asString = \case
    VString s -> Just s
    _ -> Prelude.Nothing


asInt :: Value -> Maybe Int
asInt = \case
    VNumber n -> Just (Prelude.truncate n)
    _ -> Prelude.Nothing


asObject :: Value -> Maybe (Map.Map Text Value)
asObject = \case
    VObject m -> Just m
    _ -> Prelude.Nothing


asArray :: Value -> Maybe [Value]
asArray = \case
    VArray xs -> Just xs
    _ -> Prelude.Nothing
