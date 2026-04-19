{-| Lexer for Quone source files.

Implements LANGUAGE.md section 3 in full:

* identifiers (3.2): ASCII letter start, no leading @_@, @.@ reserved
  for field access;
* literals (3.3): integer, double, character, plus the @True@/@False@
  constructor names handled by the identifier path;
* keywords (3.4): framework, dataframe DSL, and modifier sets;
* operators and punctuation (3.5);
* comments (3.6): @#@ line comments are dropped, @#'@ doc comment runs
  are preserved as 'TDocBlock' tokens.

Indentation tracking from section 3.7 is applied as a layout pass on
the raw token stream. The pass converts blank or pure-whitespace lines
into 'TNewline' / 'TIndent' / 'TDedent' triples so the parser can stay
ignorant of column counting.

The lexer is total: it emits a 'Diagnostic' for any character it
cannot tokenise instead of throwing.

-}
module Quone.Lex.Lexer
    ( lexFile
    , lexInput
    , runLexer
    )
where

import Control.Applicative ((*>), (<*))
import Control.Monad (void, (=<<))
import qualified Data.Char as Char
import Data.Functor (($>), (<$), (<$>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Void as Void
import NriPrelude
import Quone.Diagnostic
    ( Category (Lexical)
    , Diagnostic (..)
    , Severity (Error)
    )
import Quone.Lex.Token
import Quone.Position (SourcePos (..), SourceSpan (..))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as PL
import qualified Prelude



-- | Convenience: lex a named file's contents.
lexFile :: Text -> Text -> Prelude.Either Diagnostic [Located Token]
lexFile filename source =
    runLexer filename source



-- | Lex a string of source text without a file association
-- (positions report the file as @\<input\>@).
lexInput :: Text -> Prelude.Either Diagnostic [Located Token]
lexInput = lexFile "<input>"



-- | Run the megaparsec-driven raw lexer, then apply the layout pass.
runLexer :: Text -> Text -> Prelude.Either Diagnostic [Located Token]
runLexer filename source =
    case P.runParser (rawTokens filename) (T.unpack filename) source of
        Prelude.Left err -> Prelude.Left (lexErrorToDiagnostic filename err)
        Prelude.Right toks -> Prelude.Right (layout toks)



-- ---------------------------------------------------------------------
-- Raw tokens
-- ---------------------------------------------------------------------


type Lexer = P.Parsec Void.Void Text


rawTokens :: Text -> Lexer [Located Token]
rawTokens filename = do
    skipPlain filename
    toks <- P.many (oneToken filename <* skipPlain filename)
    end <- located filename (TEof <$ P.eof)
    Prelude.pure (Maybe.catMaybes toks ++ [end])



-- | A single non-whitespace, non-line-comment token, or a doc block.
--
-- Returns 'Nothing' when the lexer should silently consume input
-- (this never happens at the top level today, but keeping the type
-- 'Maybe' makes it easy to add such tokens later).
oneToken :: Text -> Lexer (Maybe (Located Token))
oneToken filename =
    P.choice
        [ Just <$> docBlock filename
        , Just <$> wildcardOrIdent filename
        , Just <$> punctuation filename
        , Just <$> stringLiteral filename
        , Just <$> numericLiteral filename
        , Just <$> identOrKeyword filename
        , do
            -- Anything else is a lexical error. We attempt to consume
            -- one character so the megaparsec error points at it.
            c <- P.anySingle
            P.unexpected (P.Tokens (NE.singleton c))
        ]


-- | A lone `_` lexes as the wildcard token (used in patterns). Any
-- other identifier-like token starting with `_` falls through to
-- 'identOrKeyword', which rejects it per LANGUAGE.md section 3.2.
wildcardOrIdent :: Text -> Lexer (Located Token)
wildcardOrIdent filename =
    located filename <| do
        _ <- P.try (PC.char '_' <* P.notFollowedBy (P.satisfy isIdentCont))
        Prelude.pure TUnderscore



-- ---------------------------------------------------------------------
-- Whitespace and comments
-- ---------------------------------------------------------------------


-- | Consume whitespace and ordinary @#@ line comments. @#'@ doc
-- comments are NOT consumed here (the 'docBlock' parser sees them
-- intact).
--
-- megaparsec tracks line and column on every consumed character, so we
-- can let it eat newlines while the layout pass reconstructs
-- significant indentation from the surviving tokens' source spans.
skipPlain :: Text -> Lexer ()
skipPlain _ =
    PL.space
        (void (P.takeWhile1P Nothing isPlainSpace))
        lineComment
        P.empty
  where
    isPlainSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'


-- | A @#@ comment that is not a @#'@ doc comment.
--
-- The 'P.try' rolls back if we end up looking at a doc comment so the
-- following 'docBlock' parser sees the original @#'@.
lineComment :: Lexer ()
lineComment = P.try <| do
    _ <- PC.char '#'
    P.notFollowedBy (PC.char '\'')
    void (P.takeWhileP Nothing (\c -> c /= '\n'))



-- ---------------------------------------------------------------------
-- Doc blocks
-- ---------------------------------------------------------------------


-- | Collapse a run of @#'@ lines into a single 'TDocBlock' token. The
-- payload is the concatenation of the post-prefix line contents joined
-- by @\\n@ (with the @#'@ and at most one space stripped from each).
docBlock :: Text -> Lexer (Located Token)
docBlock filename =
    located filename <| do
        first <- docLine
        rest <- P.many (P.try (skipBlankToDoc *> docLine))
        Prelude.pure (TDocBlock (T.intercalate "\n" (first : rest)))


docLine :: Lexer Text
docLine = do
    _ <- PC.string "#'"
    -- Allow a single space immediately after the prefix to be eaten,
    -- so doc bodies don't all start with a space when authors use
    -- the conventional "#' content" spacing.
    _ <- P.optional (PC.char ' ')
    body <- P.takeWhileP Nothing (\c -> c /= '\n')
    Prelude.pure body


-- | Between two doc-comment lines we allow the previous one to end
-- with a newline, then optional indentation, before the next @#'@.
skipBlankToDoc :: Lexer ()
skipBlankToDoc = do
    _ <- PC.char '\n'
    _ <- P.takeWhileP Nothing (\c -> c == ' ' || c == '\t')
    _ <- P.lookAhead (PC.string "#'")
    Prelude.pure ()



-- ---------------------------------------------------------------------
-- Punctuation and operators
-- ---------------------------------------------------------------------


-- | Multi-character operators must be tried before their single-character
-- prefixes (e.g. @<-@ before @<@, @|>@ before @|@). The order of the
-- 'P.choice' below reflects that.
punctuation :: Text -> Lexer (Located Token)
punctuation filename =
    located filename <|
        P.choice
            [ TArrow <$ P.try (PC.string "->")
            , TBind <$ P.try (PC.string "<-")
            , TPipe <$ P.try (PC.string "|>")
            , TIntDiv <$ P.try (PC.string "//")
            , TEq <$ P.try (PC.string "==")
            , TNeq <$ P.try (PC.string "!=")
            , TGe <$ P.try (PC.string ">=")
            , TLe <$ P.try (PC.string "<=")
            , TDotDot <$ P.try (PC.string "..")
            , TPipeBar <$ PC.char '|'
            , TPlus <$ PC.char '+'
            , TMinus <$ PC.char '-'
            , TStar <$ PC.char '*'
            , TSlash <$ PC.char '/'
            , TPercent <$ PC.char '%'
            , TCaret <$ PC.char '^'
            , TGt <$ PC.char '>'
            , TLt <$ PC.char '<'
            , TAssign <$ PC.char '='
            , TColon <$ PC.char ':'
            , TDot <$ PC.char '.'
            , TComma <$ PC.char ','
            , TAt <$ PC.char '@'
            , TBackslash <$ PC.char '\\'
            , TLParen <$ PC.char '('
            , TRParen <$ PC.char ')'
            , TLBracket <$ PC.char '['
            , TRBracket <$ PC.char ']'
            , TLBrace <$ PC.char '{'
            , TRBrace <$ PC.char '}'
            ]



-- ---------------------------------------------------------------------
-- Literals
-- ---------------------------------------------------------------------


-- | Quone v0.0.1 has no escape sequences specified beyond what R itself
-- accepts; for now we accept a single-line string with no escapes.
-- (Section 18.1 leaves richer string handling deferred.)
stringLiteral :: Text -> Lexer (Located Token)
stringLiteral filename =
    located filename <| do
        _ <- PC.char '"'
        body <- P.takeWhileP (Just "string character") (\c -> c /= '"' && c /= '\n')
        _ <- PC.char '"' P.<?> "closing double quote"
        Prelude.pure (TStringLit body)


-- | Integer or double literal. A trailing @.@ followed by a digit makes
-- it a double; otherwise it is an integer.
numericLiteral :: Text -> Lexer (Located Token)
numericLiteral filename =
    located filename <| do
        intPart <- P.takeWhile1P (Just "digit") Char.isDigit
        mFrac <-
            P.optional
                ( P.try
                    ( do
                        _ <- PC.char '.'
                        frac <- P.takeWhile1P (Just "digit") Char.isDigit
                        Prelude.pure ("." Prelude.<> frac)
                    )
                )
        case mFrac of
            Just frac ->
                Prelude.pure
                    (TFloatLit (Prelude.read (T.unpack (intPart Prelude.<> frac)) :: Prelude.Double))
            Nothing ->
                Prelude.pure (TIntLit (Prelude.read (T.unpack intPart) :: Int))



-- ---------------------------------------------------------------------
-- Identifiers and keywords
-- ---------------------------------------------------------------------


-- | An identifier is a letter followed by letters, digits, and @_@.
-- Per LANGUAGE.md section 3.2 a leading @_@ is not permitted (R does
-- not allow it).
identOrKeyword :: Text -> Lexer (Located Token)
identOrKeyword filename =
    located filename <| do
        first <- P.satisfy isIdentStart
        rest <- P.takeWhileP (Just "identifier character") isIdentCont
        let ident = T.cons first rest
        case T.uncons ident of
            Just (c, _) | Char.isUpper c -> Prelude.pure (TUpperIdent ident)
            _ ->
                case textKeyword ident of
                    Just kw -> Prelude.pure (TKeyword kw)
                    Nothing -> Prelude.pure (TLowerIdent ident)


isIdentStart :: Prelude.Char -> Prelude.Bool
isIdentStart c = Char.isAlpha c


isIdentCont :: Prelude.Char -> Prelude.Bool
isIdentCont c = Char.isAlphaNum c || c == '_'



-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------


-- | Wrap a parser so the result carries the source span it consumed.
located :: Text -> Lexer a -> Lexer (Located a)
located filename inner = do
    sp <- P.getSourcePos
    a <- inner
    ep <- P.getSourcePos
    Prelude.pure
        ( Located
            { locSpan = mkSpan filename sp ep
            , locValue = a
            }
        )


mkSpan :: Text -> P.SourcePos -> P.SourcePos -> SourceSpan
mkSpan filename s e =
    SourceSpan
        { spanStart =
            SourcePos
                { posFile = filename
                , posLine = Prelude.fromIntegral (P.unPos (P.sourceLine s))
                , posCol = Prelude.fromIntegral (P.unPos (P.sourceColumn s))
                }
        , spanEnd =
            SourcePos
                { posFile = filename
                , posLine = Prelude.fromIntegral (P.unPos (P.sourceLine e))
                , posCol = Prelude.fromIntegral (P.unPos (P.sourceColumn e))
                }
        }


lexErrorToDiagnostic :: Text -> P.ParseErrorBundle Text Void.Void -> Diagnostic
lexErrorToDiagnostic filename bundle =
    let
        firstErr = NE.head (P.bundleErrors bundle)

        -- megaparsec >= 9.0 returns (Maybe String, PosState s) here.
        (_, posState) =
            P.reachOffset (P.errorOffset firstErr) (P.bundlePosState bundle)

        srcPos = P.pstateSourcePos posState

        sp =
            SourcePos
                { posFile = filename
                , posLine = Prelude.fromIntegral (P.unPos (P.sourceLine srcPos))
                , posCol = Prelude.fromIntegral (P.unPos (P.sourceColumn srcPos))
                }

        msg =
            T.pack (P.parseErrorTextPretty firstErr)
                |> T.strip
                |> T.replace "\n" "; "
    in
    Diagnostic
        { diagSeverity = Error
        , diagCategory = Lexical
        , diagSpan =
            SourceSpan
                { spanStart = sp
                , spanEnd = sp {posCol = posCol sp + 1}
                }
        , diagMessage =
            if T.null msg
                then "lexical error"
                else msg
        , diagHint = Nothing
        }



-- ---------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------


{- | Convert significant indentation into 'TNewline' / 'TIndent' /
'TDedent' tokens.

The rule is simple and intentionally conservative for v0.0.1
(LANGUAGE.md sections 3.7 and 19.1):

* Blank lines are skipped.
* When a line's indent is greater than the top of the indent stack we
  emit a 'TNewline' followed by a 'TIndent' and push the new indent.
* When a line's indent is less than the top we emit one 'TDedent' per
  layer popped, preceded by a 'TNewline'.
* When the indent matches the top we emit a single 'TNewline'.

The parser is free to ignore newlines that don't separate top-level
declarations; what it MUST consume are the indent/dedent pairs.

-}
layout :: [Located Token] -> [Located Token]
layout toks =
    go [1] Nothing toks
  where
    -- Walk the token stream tracking the current indent stack. We
    -- detect the start of a logical line by remembering the column of
    -- the previous token: if the next token starts on a different line
    -- (and is not the EOF), it begins a new logical line.
    go _ _ [] = []
    go stack lastLine (t : rest) =
        case locValue t of
            TEof ->
                -- Close every open layer, then emit EOF.
                List.replicate
                    (Prelude.fromIntegral (Prelude.length stack - 1))
                    (sameSpan t TDedent)
                    ++ [t]
            _ ->
                let
                    line = posLine (spanStart (locSpan t))
                    col = posCol (spanStart (locSpan t))
                    isLineStart = case lastLine of
                        Nothing -> Prelude.True
                        Just l -> l Prelude./= line
                in
                if isLineStart
                    then
                        let (newStack, layoutToks) = updateStack stack col t
                        in layoutToks ++ t : go newStack (Just line) rest
                    else t : go stack (Just line) rest

    updateStack stack col t =
        case stack of
            [] ->
                -- Should not happen; defensive programming.
                ([col], [sameSpan t TNewline])
            top : _
                | col Prelude.> top -> (col : stack, [sameSpan t TNewline, sameSpan t TIndent])
                | col Prelude.== top -> (stack, [sameSpan t TNewline])
                | Prelude.otherwise ->
                    let
                        (popped, restStack) = popUntil col stack 0
                    in
                    ( restStack
                    , sameSpan t TNewline
                        : List.replicate (Prelude.fromIntegral popped) (sameSpan t TDedent)
                    )

    -- Pop every layer whose indent is greater than the new indent.
    popUntil :: Int -> [Int] -> Int -> (Int, [Int])
    popUntil _ [] n = (n, [])
    popUntil col (top : rest) n
        | col Prelude.< top = popUntil col rest (n + 1)
        | Prelude.otherwise = (n, top : rest)

    -- Synthetic layout tokens carry the same span as the token that
    -- triggered them (the start of the new line).
    sameSpan t v = Located {locSpan = locSpan t, locValue = v}
