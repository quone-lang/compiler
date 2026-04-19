{-| Public entrypoint for the elm-format-style Quone formatter.

The formatter is deliberately opinionated: there are no options, the
output of @format@ is the spec for canonical Quone style. Callers
read the source, hand it to 'format', and write the result back.

This module is the small public face. The actual rules live in
'Quone.Format.Rules' and the layout primitives in 'Quone.Format.Doc'.

-}
module Quone.Format.Format
    ( format
    , isFormatted
    )
where

import qualified Data.Text as T
import NriPrelude
import Quone.Diagnostic (Diagnostic)
import qualified Quone.Format.Rules as Rules
import qualified Quone.Lex.Lexer as Lexer
import Quone.Parse.Parser (parseProgramFile)
import qualified Prelude



-- | Format a Quone source file.
--
-- Returns the canonical form of the program, or the first parse
-- diagnostic if the input is unparseable. Idempotent: feeding the
-- output back to 'format' returns the same string.
--
-- The whole-file replacement strategy follows elm-format: original
-- whitespace and operator alignment are not preserved beyond what
-- the rules prescribe. /Comments/ are preserved -- @#@ line
-- comments are scanned out of the original source by
-- 'Quone.Lex.Lexer.collectComments' and re-emitted between the
-- declarations they originally preceded.
format :: Text -> Text -> Prelude.Either Diagnostic Text
format filename src =
    case parseProgramFile filename src of
        Prelude.Left d -> Prelude.Left d
        Prelude.Right cst ->
            let
                comments = Lexer.collectComments filename src
            in
            Prelude.Right (Rules.formatCstWithComments comments cst)


-- | True when the input already matches the canonical form.
--
-- Equivalent to @format src == Right src@; provided for callers
-- (e.g. @quonec fmt --check@, R's @quone::fmt(check = TRUE)@) that
-- only need a yes/no answer.
isFormatted :: Text -> Text -> Prelude.Bool
isFormatted filename src =
    case format filename src of
        Prelude.Right out -> out Prelude.== src
        Prelude.Left _ -> Prelude.False
