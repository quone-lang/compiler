{-| Meta-command parsing for the REPL.

A meta-command is anything beginning with a colon. Everything else
is forwarded to the language pipeline by the REPL driver.

-}
module Quone.Repl.Meta
    ( Meta (..)
    , parseMeta
    , helpText
    )
where

import qualified Data.Text as T
import NriPrelude
import qualified Prelude



data Meta
    = MQuit
    | MHelp
    | MType Text
    | MBrowse
    | MLoad Prelude.FilePath
    | MReload
    | MUnknown Text
    deriving (Prelude.Show, Prelude.Eq)


-- | Parse a single line of REPL input.
--
-- Returns 'Just' for any line beginning with a colon and 'Nothing'
-- for normal Quone source. Whitespace around the command and its
-- arguments is trimmed.
parseMeta :: Text -> Maybe Meta
parseMeta raw = case T.stripPrefix ":" (T.strip raw) of
    Prelude.Nothing -> Prelude.Nothing
    Just rest ->
        let
            (cmd, argText) = T.breakOn " " rest
            arg = T.strip argText
        in
        Just (dispatch (T.toLower cmd) arg)


dispatch :: Text -> Text -> Meta
dispatch cmd arg = case cmd of
    "quit" -> MQuit
    "q" -> MQuit
    "help" -> MHelp
    "?" -> MHelp
    "type" -> MType arg
    "t" -> MType arg
    "browse" -> MBrowse
    "load" -> MLoad (T.unpack arg)
    "reload" -> MReload
    "r" -> MReload
    other -> MUnknown other


helpText :: Text
helpText =
    T.intercalate "\n"
        [ "Meta-commands:"
        , "  :type <expr>   print the inferred type of <expr>"
        , "  :t <expr>      alias for :type"
        , "  :load <file.Q> load a file's bindings into the session"
        , "  :reload        reload all loaded files"
        , "  :browse        list every binding in the session env"
        , "  :quit, :q      exit the REPL"
        , "  :help, :?      this help text"
        ]
