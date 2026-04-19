{-| Long-lived @Rscript@ subprocess used by the REPL.

The backend exposes 'evalChunk', which writes a chunk of R to the
subprocess stdin and reads its printed output back up to a sentinel
marker. Errors raised by R surface via 'evalChunk' as the captured
stderr text.

For v0.0.1 we deliberately keep the protocol simple: every chunk is
followed by a @cat("__quone_done__\\n")@ line so the reader knows
where the value ends. The subprocess is started with
@--no-save --no-restore --slave@ to avoid noise.

If @Rscript@ is not on @PATH@ the start operation returns
'Prelude.Left' with a human-readable message and the REPL aborts;
this lets CI without R skip the REPL gracefully.

-}
module Quone.Repl.RBackend
    ( Backend
    , StartOpts (..)
    , startOpts
    , start
    , stop
    , evalChunk
    )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import qualified System.IO as IO
import qualified System.IO.Error as IOError
import qualified Control.Exception as Exception
import qualified System.Process as Proc
import qualified Prelude



data Backend = Backend
    { backendStdin :: IO.Handle
    , backendStdout :: IO.Handle
    , backendStderr :: IO.Handle
    , backendHandle :: Proc.ProcessHandle
    }


data StartOpts = StartOpts
    { startRscript :: Maybe Prelude.FilePath
    }
    deriving (Prelude.Show, Prelude.Eq)


startOpts :: StartOpts
startOpts = StartOpts {startRscript = Prelude.Nothing}


sentinel :: Text
sentinel = "__quone_done__"


start :: StartOpts -> Prelude.IO (Prelude.Either Text Backend)
start opts = do
    let
        path = case startRscript opts of
            Just p -> p
            Prelude.Nothing -> "Rscript"
        spec =
            (Proc.proc path
                ["--no-save", "--no-restore", "--slave", "-e",
                    "while (TRUE) { invisible(eval(parse(\"stdin\"), envir = .GlobalEnv)) }"])
                { Proc.std_in = Proc.CreatePipe
                , Proc.std_out = Proc.CreatePipe
                , Proc.std_err = Proc.CreatePipe
                }
    result <-
        Exception.try (Proc.createProcess spec)
            :: Prelude.IO
                (Prelude.Either
                    IOError.IOError
                    (Maybe IO.Handle, Maybe IO.Handle, Maybe IO.Handle,
                        Proc.ProcessHandle))
    case result of
        Prelude.Left e ->
            Prelude.pure
                (Prelude.Left
                    ("could not start Rscript: " ++ T.pack (Prelude.show e)))
        Prelude.Right (Just hin, Just hout, Just herr, ph) -> do
            IO.hSetBuffering hin IO.LineBuffering
            IO.hSetBuffering hout IO.LineBuffering
            Prelude.pure
                (Prelude.Right
                    Backend
                        { backendStdin = hin
                        , backendStdout = hout
                        , backendStderr = herr
                        , backendHandle = ph
                        })
        Prelude.Right _ ->
            Prelude.pure (Prelude.Left "Rscript pipes failed to open")


stop :: Backend -> Prelude.IO ()
stop b = do
    _ <-
        Exception.try (IO.hClose (backendStdin b))
            :: Prelude.IO (Prelude.Either IOError.IOError ())
    _ <- Proc.waitForProcess (backendHandle b)
    Prelude.pure ()


-- | Send a chunk of R to the subprocess and return its captured
-- standard output up to (but not including) the sentinel line.
evalChunk :: Backend -> Text -> Prelude.IO Text
evalChunk b chunk = do
    TIO.hPutStrLn (backendStdin b) chunk
    TIO.hPutStrLn (backendStdin b)
        ("cat(\"" ++ sentinel ++ "\\n\")")
    IO.hFlush (backendStdin b)
    readUntilSentinel (backendStdout b) []


readUntilSentinel :: IO.Handle -> [Text] -> Prelude.IO Text
readUntilSentinel h acc = do
    eitherLine <-
        Exception.try (TIO.hGetLine h)
            :: Prelude.IO (Prelude.Either IOError.IOError Text)
    case eitherLine of
        Prelude.Left _ ->
            Prelude.pure (T.intercalate "\n" (Prelude.reverse acc))
        Prelude.Right line ->
            if line Prelude.== sentinel
                then
                    Prelude.pure
                        (T.intercalate "\n" (Prelude.reverse acc))
                else readUntilSentinel h (line : acc)
