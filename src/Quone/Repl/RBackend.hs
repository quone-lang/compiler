{-| Long-lived @Rscript@ subprocess used by the REPL.

The backend exposes 'evalChunk', which writes a chunk of R to the
subprocess stdin and reads its printed output back up to a sentinel
marker. Errors raised by R surface via 'evalChunk' as the captured
stderr text.

The protocol between the REPL and the R driver:

* The Haskell side writes one or more lines of R, then a single line
  containing only @__quone_eval__@. The driver collects every line
  before the marker and evaluates them as one chunk so multi-line
  programs (which is what the lowering produces) work.
* After every chunk the driver writes @__quone_done__\\n@ to stdout
  and the Haskell side reads back to that line.
* @cat()@ in R flushes after each newline when stdout is piped, so we
  do not need a pty or @stdbuf@.

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
    , doneSentinel
    , evalSentinel
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


-- | The line the driver writes after every evaluated chunk so the REPL
-- knows where one response ends.
doneSentinel :: Text
doneSentinel = "__quone_done__"


-- | The line the REPL writes after every chunk to tell the driver
-- "now evaluate what you have buffered".
evalSentinel :: Text
evalSentinel = "__quone_eval__"


-- | The R program embedded in the long-lived @Rscript@ child.
--
-- The driver loops forever, accumulating input lines into a buffer until
-- it sees the eval marker. Each chunk is parsed once, then every
-- top-level expression is evaluated with 'withVisible' so visible
-- top-level values auto-print exactly the way the interactive R console
-- does. Errors are caught per chunk and per expression and reported as
-- @Error: <msg>@ lines so the REPL caller can keep reading.
--
-- Errors during read or evaluation never crash the driver; only EOF on
-- stdin terminates the loop.
driverScript :: Text
driverScript =
    T.intercalate "\n"
        [ ".quone_done_marker <- \"" ++ doneSentinel ++ "\""
        , ".quone_eval_marker <- \"" ++ evalSentinel ++ "\""
        , ".quone_eval <- function(src) {"
        , "  exprs <- tryCatch("
        , "    parse(text = src, keep.source = FALSE),"
        , "    error = function(e) {"
        , "      cat(\"Error: \", conditionMessage(e), \"\\n\", sep = \"\")"
        , "      NULL"
        , "    }"
        , "  )"
        , "  if (is.null(exprs)) return(invisible(NULL))"
        , "  for (i in seq_along(exprs)) {"
        , "    res <- tryCatch("
        , "      withVisible(eval(exprs[[i]], envir = .GlobalEnv)),"
        , "      error = function(e) {"
        , "        cat(\"Error: \", conditionMessage(e), \"\\n\", sep = \"\")"
        , "        NULL"
        , "      }"
        , "    )"
        , "    if (!is.null(res) && isTRUE(res$visible)) {"
        , "      print(res$value)"
        , "    }"
        , "  }"
        , "  invisible(NULL)"
        , "}"
        , ".quone_con <- file(\"stdin\", open = \"r\", blocking = TRUE)"
        , "repeat {"
        , "  buf <- character(0)"
        , "  repeat {"
        , "    line <- readLines(.quone_con, n = 1L, warn = FALSE)"
        , "    if (length(line) == 0L) {"
        , "      close(.quone_con); quit(save = \"no\", status = 0L)"
        , "    }"
        , "    if (identical(line, .quone_eval_marker)) break"
        , "    buf <- c(buf, line)"
        , "  }"
        , "  if (length(buf) > 0L) {"
        , "    .quone_eval(paste(buf, collapse = \"\\n\"))"
        , "  }"
        , "  cat(.quone_done_marker, \"\\n\", sep = \"\")"
        , "}"
        ]


start :: StartOpts -> Prelude.IO (Prelude.Either Text Backend)
start opts = do
    let
        path = case startRscript opts of
            Just p -> p
            Prelude.Nothing -> "Rscript"
        spec =
            (Proc.proc path
                ["--no-save", "--no-restore", "--slave", "-e",
                    T.unpack driverScript])
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
        Prelude.Left ioe ->
            Prelude.pure
                (Prelude.Left
                    ("could not start Rscript: " ++ T.pack (Prelude.show ioe)))
        Prelude.Right (Just hin, Just hout, Just herr, ph) -> do
            IO.hSetBuffering hin IO.LineBuffering
            IO.hSetBuffering hout IO.LineBuffering
            IO.hSetBuffering herr IO.LineBuffering
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


-- | Stop the backend.
--
-- We deliberately do not 'Proc.waitForProcess' on a clean exit because
-- the driver's outer @repeat@ loop only terminates when stdin reaches
-- EOF, and a stuck or hung child must not deadlock the REPL. Instead
-- we close stdin (which causes the driver's @readLines@ to return
-- character(0) and quit gracefully), then unconditionally
-- 'Proc.terminateProcess' as a backstop, then reap the child.
stop :: Backend -> Prelude.IO ()
stop b = do
    _ <-
        Exception.try (IO.hClose (backendStdin b))
            :: Prelude.IO (Prelude.Either IOError.IOError ())
    _ <-
        Exception.try (Proc.terminateProcess (backendHandle b))
            :: Prelude.IO (Prelude.Either Exception.SomeException ())
    _ <- Proc.waitForProcess (backendHandle b)
    Prelude.pure ()


-- | Send a chunk of R to the subprocess and return its captured
-- standard output up to (but not including) the done sentinel line.
--
-- Multi-line chunks are supported: the chunk is sent verbatim, then a
-- single line containing only the eval marker tells the driver to
-- parse and evaluate everything it has buffered so far.
evalChunk :: Backend -> Text -> Prelude.IO Text
evalChunk b chunk = do
    TIO.hPutStrLn (backendStdin b) chunk
    TIO.hPutStrLn (backendStdin b) evalSentinel
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
            if line Prelude.== doneSentinel
                then
                    Prelude.pure
                        (T.intercalate "\n" (Prelude.reverse acc))
                else readUntilSentinel h (line : acc)
