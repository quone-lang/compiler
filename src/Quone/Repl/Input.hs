{-| Read a single REPL line from standard input.

A richer, history-backed line editor (via @haskeline@) lands once the
@haskeline@ dependency is added to @compiler.cabal@. For v0.0.1 this
module wraps 'TIO.getLine' with a small prompt and the @:{@ /
@:}@ multi-line block markers.

-}
module Quone.Repl.Input
    ( readLine
    , LineResult (..)
    , prompt
    )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NriPrelude
import qualified System.IO as IO
import qualified System.IO.Error as IOError
import qualified Control.Exception as Exception
import qualified Prelude



data LineResult
    = Line Text
    | Blank
    | EndOfInput
    deriving (Prelude.Show, Prelude.Eq)


prompt :: Text
prompt = "quone> "


-- | Read one logical input. Supports a multi-line block opened with
-- @:{@ and closed with @:}@, mirroring the GHCi convention.
readLine :: Prelude.IO LineResult
readLine = do
    TIO.putStr prompt
    IO.hFlush IO.stdout
    eitherLine <-
        Exception.try
            (TIO.hGetLine IO.stdin)
            :: Prelude.IO (Prelude.Either IOError.IOError Text)
    case eitherLine of
        Prelude.Left e
            | IOError.isEOFError e -> Prelude.pure EndOfInput
            | Prelude.otherwise -> Prelude.pure EndOfInput
        Prelude.Right raw ->
            let
                trimmed = T.strip raw
            in
            if T.null trimmed
                then Prelude.pure Blank
                else if trimmed Prelude.== ":{"
                    then readBlock []
                    else Prelude.pure (Line trimmed)


readBlock :: [Text] -> Prelude.IO LineResult
readBlock acc = do
    TIO.putStr "..... "
    IO.hFlush IO.stdout
    eitherLine <-
        Exception.try
            (TIO.hGetLine IO.stdin)
            :: Prelude.IO (Prelude.Either IOError.IOError Text)
    case eitherLine of
        Prelude.Left _ -> Prelude.pure EndOfInput
        Prelude.Right raw ->
            let
                trimmed = T.strip raw
            in
            if trimmed Prelude.== ":}"
                then Prelude.pure (Line (T.intercalate "\n" (Prelude.reverse acc)))
                else readBlock (raw : acc)
