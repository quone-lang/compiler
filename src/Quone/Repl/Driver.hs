{-| Top-level entry point for the @quonec repl@ subcommand.

The REPL reads a line, type-checks it, lowers to R, and forwards the
R to a long-lived @Rscript --interactive@ subprocess. The R session
holds the user's bindings; the REPL holds the typing environment.

@
quone> x <- 1 + 1
quone> mean [x, 2.0, 3.0]
[1] 2
quone> :type x
x : Integer
quone> :quit
@

Meta-commands handled by 'Quone.Repl.Meta':

* @:type@ \/ @:t expr@ -- print the inferred type
* @:load file.Q@ -- run a file's bindings into the session
* @:reload@ -- reload all loaded files
* @:browse@ -- list every binding in the session env
* @:quit@ \/ @:q@ -- exit
* @:help@ \/ @:?@ -- meta-command help
-}
module Quone.Repl.Driver
    ( runRepl
    , Opts (..)
    , defaultOpts
    )
where

import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import qualified Quone.Repl.Input as Input
import qualified Quone.Repl.Meta as Meta
import qualified Quone.Repl.RBackend as RBackend
import qualified Quone.Repl.Session as Session
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Prelude



-- | Options that influence REPL behaviour. Mirrors the @quonec repl@
-- CLI flags described in compiler track A5 of the project plan.
data Opts = Opts
    { optProject :: Maybe Prelude.FilePath
    , optAutoload :: Prelude.Bool
    , optRscriptPath :: Maybe Prelude.FilePath
    }
    deriving (Prelude.Show, Prelude.Eq)


defaultOpts :: Opts
defaultOpts =
    Opts
        { optProject = Prelude.Nothing
        , optAutoload = Prelude.True
        , optRscriptPath = Prelude.Nothing
        }


-- | Start the REPL. Returns the exit code the user picked when they
-- exited (with @:quit@ or end-of-input), or 'Exit.ExitFailure' if the
-- backend could not start.
runRepl :: Opts -> Prelude.IO Exit.ExitCode
runRepl opts = do
    backendResult <-
        RBackend.start
            RBackend.startOpts
                { RBackend.startRscript = optRscriptPath opts
                }
    case backendResult of
        Prelude.Left msg -> do
            TIO.hPutStrLn IO.stderr msg
            Prelude.pure (Exit.ExitFailure 1)
        Prelude.Right backend -> do
            sessionRef <-
                IORef.newIORef
                    (Session.empty (optProject opts))
            TIO.putStrLn welcomeBanner
            loop sessionRef backend


welcomeBanner :: Text
welcomeBanner =
    T.intercalate "\n"
        [ "quonec repl 0.0.1"
        , "Type :help for meta-commands, :quit to exit."
        ]


loop
    :: IORef.IORef Session.Session
    -> RBackend.Backend
    -> Prelude.IO Exit.ExitCode
loop sessionRef backend = do
    line <- Input.readLine
    case line of
        Input.EndOfInput -> goodbye Exit.ExitSuccess
        Input.Blank -> loop sessionRef backend
        Input.Line raw -> do
            session <- IORef.readIORef sessionRef
            case Meta.parseMeta raw of
                Just Meta.MQuit -> goodbye Exit.ExitSuccess
                Just Meta.MHelp -> do
                    TIO.putStrLn Meta.helpText
                    loop sessionRef backend
                Just (Meta.MType expr) -> do
                    TIO.putStrLn
                        (Session.inferType session expr)
                    loop sessionRef backend
                Just Meta.MBrowse -> do
                    Prelude.mapM_ TIO.putStrLn (Session.browse session)
                    loop sessionRef backend
                Just (Meta.MLoad path) -> do
                    newSession <- Session.loadFile session path
                    IORef.writeIORef sessionRef newSession
                    loop sessionRef backend
                Just Meta.MReload -> do
                    newSession <- Session.reload session
                    IORef.writeIORef sessionRef newSession
                    loop sessionRef backend
                Just (Meta.MUnknown name) -> do
                    TIO.putStrLn ("unknown meta-command: " ++ name)
                    loop sessionRef backend
                Prelude.Nothing -> do
                    (newSession, output) <-
                        Session.evaluate session backend raw
                    IORef.writeIORef sessionRef newSession
                    Prelude.mapM_ TIO.putStrLn output
                    loop sessionRef backend
  where
    goodbye code = do
        RBackend.stop backend
        Prelude.pure code
