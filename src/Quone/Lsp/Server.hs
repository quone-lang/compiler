{-| Top-level loop for the @quonec lsp@ subcommand.

Reads framed messages from stdin, dispatches by method, writes
responses (and pushes diagnostic notifications after did-open /
did-change) to stdout.

This is a minimal implementation that covers the capability set
declared by 'Quone.Lsp.Handlers.initializeResult'. It is enough for
RStudio / Positron / VS Code / Neovim to connect, see diagnostics,
hover, jump to definition, browse symbols, complete keywords, and
format files.

-}
module Quone.Lsp.Server
    ( runServer
    , Opts (..)
    , defaultOpts
    )
where

import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Diagnostic (Diagnostic)
import qualified Quone.Lsp.Handlers as Handlers
import qualified Quone.Lsp.Json as Json
import qualified Quone.Lsp.Protocol as Protocol
import qualified Quone.Lsp.State as State
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Prelude



data Opts = Opts
    { optLogFile :: Maybe Prelude.FilePath
    }
    deriving (Prelude.Show, Prelude.Eq)


defaultOpts :: Opts
defaultOpts = Opts {optLogFile = Prelude.Nothing}


-- | Run the LSP server until the client sends @shutdown@ + @exit@
-- (or stdin closes).
runServer :: Opts -> Prelude.IO Exit.ExitCode
runServer opts = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetBuffering IO.stdout IO.NoBuffering
    logHandle <- openLog opts
    stateRef <- IORef.newIORef State.empty
    loop stateRef logHandle


openLog :: Opts -> Prelude.IO (Maybe IO.Handle)
openLog opts = case optLogFile opts of
    Prelude.Nothing -> Prelude.pure Prelude.Nothing
    Just path -> do
        h <- IO.openFile path IO.AppendMode
        IO.hSetBuffering h IO.LineBuffering
        Prelude.pure (Just h)


loop :: IORef.IORef State.State -> Maybe IO.Handle -> Prelude.IO Exit.ExitCode
loop stateRef logHandle = do
    msgM <- Protocol.readMessage IO.stdin
    case msgM of
        Prelude.Nothing -> Prelude.pure Exit.ExitSuccess
        Just msg -> do
            logIt logHandle ("recv: " ++ Json.encode (Protocol.msgPayload msg))
            keepGoing <-
                handle stateRef logHandle (Protocol.msgPayload msg)
            if keepGoing
                then loop stateRef logHandle
                else Prelude.pure Exit.ExitSuccess


handle
    :: IORef.IORef State.State
    -> Maybe IO.Handle
    -> Json.Value
    -> Prelude.IO Prelude.Bool
handle stateRef logHandle payload = case method payload of
    Just "initialize" -> do
        respond payload Handlers.initializeResult
        Prelude.pure Prelude.True
    Just "initialized" -> Prelude.pure Prelude.True
    Just "shutdown" -> do
        respond payload Json.VNull
        Prelude.pure Prelude.True
    Just "exit" -> Prelude.pure Prelude.False
    Just "textDocument/didOpen" -> do
        params <- requireParams payload
        state <- IORef.readIORef stateRef
        let
            (state', diagsM) = Handlers.handleDidOpen params state
        IORef.writeIORef stateRef state'
        publishMaybe logHandle diagsM
        Prelude.pure Prelude.True
    Just "textDocument/didChange" -> do
        params <- requireParams payload
        state <- IORef.readIORef stateRef
        let
            (state', diagsM) = Handlers.handleDidChange params state
        IORef.writeIORef stateRef state'
        publishMaybe logHandle diagsM
        Prelude.pure Prelude.True
    Just "textDocument/didClose" -> do
        params <- requireParams payload
        case Json.lookupField "textDocument" params Prelude.>>= Json.lookupField "uri" Prelude.>>= Json.asString of
            Just uri ->
                IORef.modifyIORef stateRef
                    (State.removeDocument uri)
            Prelude.Nothing -> Prelude.pure ()
        Prelude.pure Prelude.True
    Just "textDocument/hover" -> do
        params <- requireParams payload
        state <- IORef.readIORef stateRef
        respond payload (Handlers.handleHover params state)
        Prelude.pure Prelude.True
    Just "textDocument/definition" -> do
        params <- requireParams payload
        state <- IORef.readIORef stateRef
        respond payload (Handlers.handleDefinition params state)
        Prelude.pure Prelude.True
    Just "textDocument/documentSymbol" -> do
        params <- requireParams payload
        state <- IORef.readIORef stateRef
        respond payload (Handlers.handleDocumentSymbol params state)
        Prelude.pure Prelude.True
    Just "textDocument/completion" -> do
        params <- requireParams payload
        state <- IORef.readIORef stateRef
        respond payload (Handlers.handleCompletion params state)
        Prelude.pure Prelude.True
    Just "textDocument/formatting" -> do
        params <- requireParams payload
        state <- IORef.readIORef stateRef
        respond payload (Handlers.handleFormatting params state)
        Prelude.pure Prelude.True
    Just _ -> Prelude.pure Prelude.True
    Prelude.Nothing -> Prelude.pure Prelude.True
  where
    method v = Json.lookupField "method" v Prelude.>>= Json.asString
    requireParams v =
        Prelude.pure
            (case Json.lookupField "params" v of
                Just p -> p
                Prelude.Nothing -> Json.VNull)
    respond inbound result = case Json.lookupField "id" inbound of
        Just idVal ->
            writeOut logHandle
                (Json.object
                    [ ("jsonrpc", Json.str "2.0")
                    , ("id", idVal)
                    , ("result", result)
                    ])
        Prelude.Nothing -> Prelude.pure ()


publishMaybe
    :: Maybe IO.Handle
    -> Maybe (Text, [Diagnostic])
    -> Prelude.IO ()
publishMaybe _ Prelude.Nothing = Prelude.pure ()
publishMaybe logHandle (Just (uri, ds)) =
    writeOut logHandle (Handlers.publishDiagnostics uri ds)


writeOut :: Maybe IO.Handle -> Json.Value -> Prelude.IO ()
writeOut logHandle v = do
    Protocol.writeMessage IO.stdout v
    logIt logHandle ("send: " ++ Json.encode v)


logIt :: Maybe IO.Handle -> Text -> Prelude.IO ()
logIt Prelude.Nothing _ = Prelude.pure ()
logIt (Just h) msg = TIO.hPutStrLn h msg
