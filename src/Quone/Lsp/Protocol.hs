{-| JSON-RPC 2.0 framing for the LSP server.

LSP messages are framed as:

@
Content-Length: 12\\r\\n
\\r\\n
{"jsonrpc"...}
@

This module exposes 'readMessage' and 'writeMessage' which handle
the framing and 'Quone.Lsp.Json'-encoded payloads. It does not
interpret method names or params.

-}
module Quone.Lsp.Protocol
    ( Message (..)
    , readMessage
    , writeMessage
    , encodeFrame
    , parseHeader
    )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import NriPrelude
import qualified Quone.Lsp.Json as Json
import qualified System.IO as IO
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
import qualified Prelude



-- | A parsed inbound or outbound LSP message.
data Message = Message
    { msgPayload :: Json.Value
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Read a single framed message from the given handle. Returns
-- 'Prelude.Nothing' when the stream is closed.
readMessage :: IO.Handle -> Prelude.IO (Maybe Message)
readMessage h = do
    headerResult <- readHeaderLines h []
    case headerResult of
        Prelude.Nothing -> Prelude.pure Prelude.Nothing
        Just headerLines ->
            case parseHeader headerLines of
                Prelude.Nothing -> Prelude.pure Prelude.Nothing
                Just len -> do
                    body <- BS.hGet h (Prelude.fromIntegral (len :: Int))
                    let
                        decoded = TE.decodeUtf8 body
                    case Json.decode decoded of
                        Just v ->
                            Prelude.pure (Just (Message v))
                        Prelude.Nothing -> Prelude.pure Prelude.Nothing


-- | Read the framing header lines up to the empty separator line.
-- Returns 'Just' the lines read (excluding the separator) on success,
-- 'Prelude.Nothing' on end-of-input.
readHeaderLines :: IO.Handle -> [BS.ByteString] -> Prelude.IO (Maybe [BS.ByteString])
readHeaderLines h acc = do
    eitherLine <-
        Exception.try (BS.hGetLine h)
            :: Prelude.IO (Prelude.Either IOError.IOError BS.ByteString)
    case eitherLine of
        Prelude.Left _ ->
            if Prelude.null acc
                then Prelude.pure Prelude.Nothing
                else Prelude.pure (Just (Prelude.reverse acc))
        Prelude.Right raw ->
            let
                trimmed = stripCr raw
            in
            if BS.null trimmed
                then Prelude.pure (Just (Prelude.reverse acc))
                else readHeaderLines h (trimmed : acc)


stripCr :: BS.ByteString -> BS.ByteString
stripCr bs
    | BS.null bs = bs
    | BS.last bs Prelude.== 13 = BS.init bs
    | Prelude.otherwise = bs


-- | Pull the @Content-Length@ value out of a header block.
parseHeader :: [BS.ByteString] -> Maybe Int
parseHeader = Prelude.foldr step Prelude.Nothing
  where
    step ln acc =
        let
            (key, val) = BSC.break (Prelude.== ':') ln
        in
        if BSC.map Char.toLower key Prelude.== BSC.pack "content-length"
            then case BSC.readInt (BSC.dropWhile (\c -> c Prelude.== ' ' Prelude.|| c Prelude.== ':') val) of
                Just (n, _) ->
                    Just (Prelude.fromIntegral (n :: Prelude.Int))
                Prelude.Nothing -> acc
            else acc


-- | Encode a JSON value as a framed LSP message ready to write to a
-- handle.
encodeFrame :: Json.Value -> BS.ByteString
encodeFrame v =
    let
        body = TE.encodeUtf8 (Json.encode v)
        header =
            BSC.pack
                ("Content-Length: "
                    Prelude.++ Prelude.show (BS.length body)
                    Prelude.++ "\r\n\r\n")
    in
    header `BS.append` body


writeMessage :: IO.Handle -> Json.Value -> Prelude.IO ()
writeMessage h v = do
    BS.hPut h (encodeFrame v)
    IO.hFlush h
