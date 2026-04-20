{-| The persistent state of a REPL session.

The session holds the list of files loaded so far so @:reload@ can
re-load them, and an optional project root so @:browse@ can show what
the user has in scope.

The actual evaluation strategy is to compile each user input as a
miniature single-file script and forward the lowered R to the backend.
This reuses 'Quone.Cli.Commands.compileScript' unchanged so the REPL
sees exactly the same lowering as @quonec build@.

-}
module Quone.Repl.Session
    ( Session
    , empty
    , inferType
    , browse
    , loadFile
    , reload
    , evaluate
    )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import Quone.Ast.Source
    ( Decl (..)
    , Program (..)
    , ValueDecl (..)
    , LowerName (..)
    )
import Quone.Ast.Validate (validate)
import qualified Quone.Diagnostic as Diag
import Quone.Diagnostic (Diagnostic (..), render)
import Quone.Generate.R (generateProgram)
import Quone.Parse.Desugar (desugarFile)
import qualified Quone.Repl.RBackend as RBackend
import Quone.Type.Infer (TypedProgram (..), inferProgram)
import qualified Quone.Type.Types as Ty
import qualified System.Directory as Dir
import qualified Prelude



data Session = Session
    { sessionProject :: Maybe Prelude.FilePath
    , sessionLoaded :: [Prelude.FilePath]
    , sessionBindings :: Map.Map Text Ty.Scheme
    }


empty :: Maybe Prelude.FilePath -> Session
empty proj =
    Session
        { sessionProject = proj
        , sessionLoaded = []
        , sessionBindings = Map.empty
        }


-- | Rough static type lookup for @:type expr@. For v0.0.1 we only
-- recognise plain identifiers ("@:type x@"), since inferring the
-- type of an arbitrary expression in isolation requires re-running
-- the typer in the session's environment, which lands when
-- 'Quone.Type.Infer' grows an "infer expression in env" entry point.
inferType :: Session -> Text -> Text
inferType s expr =
    let
        name = T.strip expr
    in
    case Map.lookup name (sessionBindings s) of
        Just sch -> name ++ " : " ++ renderScheme sch
        Prelude.Nothing ->
            "type of arbitrary expressions is [planned]; "
                ++ "use a top-level binding first"


-- | List the bindings the session knows about.
browse :: Session -> [Text]
browse s =
    Prelude.fmap formatBinding (Map.toAscList (sessionBindings s))
  where
    formatBinding (name, sch) = name ++ " : " ++ renderScheme sch


renderScheme :: Ty.Scheme -> Text
renderScheme = T.pack Prelude.. Prelude.show


-- | Load a single .Q file, sending its lowered R to the backend.
loadFile :: Session -> Prelude.FilePath -> Prelude.IO Session
loadFile s path = do
    exists <- Dir.doesFileExist path
    if Prelude.not exists
        then do
            TIO.putStrLn (T.pack ("file not found: " Prelude.++ path))
            Prelude.pure s
        else do
            src <- TIO.readFile path
            case compileSession (T.pack path) src of
                Prelude.Left d -> do
                    TIO.putStrLn (render d)
                    Prelude.pure s
                Prelude.Right (typed, _rcode) -> do
                    let
                        next =
                            s
                                { sessionLoaded =
                                    addUnique path (sessionLoaded s)
                                , sessionBindings =
                                    Map.union
                                        (typedBindings typed)
                                        (sessionBindings s)
                                }
                    Prelude.pure next


reload :: Session -> Prelude.IO Session
reload s = do
    let
        cleared = s {sessionBindings = Map.empty}
    Prelude.foldr step (Prelude.pure cleared) (sessionLoaded s)
  where
    step path acc = do
        sNow <- acc
        loadFile sNow path


-- | Evaluate a Quone fragment.
--
-- Input is wrapped in @main <- ...@ so it parses as a single-file
-- program; the resulting R is sent to the backend whose printed
-- output is returned. The lowered R is a top-level assignment, which
-- is invisible in R, so we explicitly append @main@ as a final
-- expression to force auto-print and surface the value to the user.
-- A proper expression-only REPL chunk parser is `[planned]`.
evaluate
    :: Session
    -> RBackend.Backend
    -> Text
    -> Prelude.IO (Session, [Text])
evaluate s backend raw = do
    let
        wrapped = "main <- " ++ raw
    case compileSession "<repl>" wrapped of
        Prelude.Left d ->
            Prelude.pure (s, [render d])
        Prelude.Right (typed, rcode) -> do
            let
                rcodeWithEcho = rcode ++ "\nmain"
            output <- RBackend.evalChunk backend rcodeWithEcho
            let
                next =
                    s
                        { sessionBindings =
                            Map.union
                                (typedBindings typed)
                                (sessionBindings s)
                        }
            Prelude.pure (next, [output])


compileSession
    :: Text
    -> Text
    -> Prelude.Either Diagnostic (TypedProgram, Text)
compileSession filename src = do
    prog <- desugarFile filename src
    case validate prog of
        (d : _) -> Prelude.Left d
        [] -> Prelude.pure ()
    typed <- inferProgram prog
    Prelude.pure (typed, generateProgram prog)


addUnique :: Prelude.Eq a => a -> [a] -> [a]
addUnique x xs = if x `List.elem` xs then xs else xs Prelude.++ [x]
