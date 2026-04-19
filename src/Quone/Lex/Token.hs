{-| The token type emitted by the lexer.

Mirrors LANGUAGE.md sections 3.3 (literals), 3.4 (keywords), and 3.5
(operators / punctuation). Doc-comment groups produced by section 3.6
are emitted as 'TDocBlock' tokens carrying their already-stripped
text.

Each 'Located' token also carries a source span; downstream phases
copy that span into the AST so diagnostics can point at the original
character range.

-}
module Quone.Lex.Token
    ( Token (..)
    , Located (..)
    , Keyword (..)
    , keywordText
    , textKeyword
    , allKeywords
    , showToken
    )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import Quone.Position (SourceSpan)
import qualified Prelude


-- | A token paired with its source span.
data Located a = Located
    { locSpan :: SourceSpan
    , locValue :: a
    }
    deriving (Prelude.Show, Prelude.Eq, Prelude.Functor)


-- | A lexical token. Most variants are spelled exactly as they appear
-- in source; the special-case ones are documented inline.
data Token
    = -- Identifiers
      TLowerIdent Text
    | TUpperIdent Text
    | TKeyword Keyword
    | -- Literals
      TIntLit Int
    | TFloatLit Prelude.Double
    | TStringLit Text
    | -- Operators
      TArrow         -- ->
    | TBind          -- <-
    | TPipe          -- |>
    | TPipeBar       -- |   (record update separator, also custom-type variant separator)
    | TPlus
    | TMinus
    | TStar
    | TSlash
    | TIntDiv        -- //
    | TPercent       -- %
    | TCaret         -- ^
    | TEq            -- ==
    | TNeq           -- !=
    | TGt
    | TLt
    | TGe            -- >=
    | TLe            -- <=
    | TAssign        -- = (record-field binding)
    | TColon         -- :
    | TDot
    | TComma
    | TUnderscore
    | TAt            -- @
    | TBackslash     -- \
    | TLParen
    | TRParen
    | TLBracket
    | TRBracket
    | TLBrace
    | TRBrace
    | TDotDot        -- .. (export-all wildcard)
    | -- Doc block: the lexer collapses a run of #' lines into a single
      -- token whose payload is the lines joined by newlines (with the
      -- "#'" prefix removed). Stage 3 ties each doc block to the
      -- following declaration.
      TDocBlock Text
    | -- Layout: not emitted by the raw lexer but inserted after the
      -- layout pass.
      TNewline
    | TIndent
    | TDedent
    | TEof
    deriving (Prelude.Show, Prelude.Eq)


-- | Reserved keywords from LANGUAGE.md section 3.4.
--
-- Categorised in source order: framework keywords, dataframe DSL
-- keywords (verbs and the @dataframe@ type former), then the modifier
-- keywords. The categorisation is informational; the lexer treats them
-- all the same way.
data Keyword
    = -- Framework
      KModule
    | KExporting
    | KType
    | KAlias
    | KImport
    | KIf
    | KThen
    | KElse
    | KCase
    | KOf
    | KLet
    | KIn
    | -- Dataframe DSL
      KDataframe
    | KSelect
    | KFilter
    | KMutate
    | KSummarize
    | KGroupBy
    | KUngroup
    | KArrange
    | KRename
    | KDistinct
    | KDistinctAll
    | KCount
    | KSlice
    | KPull
    | KRelocate
    | KTransmute
    | KMutateEach
    | KSummarizeEach
    | KLeftJoin
    | KRightJoin
    | KInnerJoin
    | KFullJoin
    | KAntiJoin
    | KSemiJoin
    | KCrossJoin
    | -- Dataframe modifiers
      KDesc
    | KAsc
    | KOn
    | KAs
    | KWhere
    | KCols
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded)


-- | The exact source spelling of a keyword.
keywordText :: Keyword -> Text
keywordText = \case
    KModule -> "module"
    KExporting -> "exporting"
    KType -> "type"
    KAlias -> "alias"
    KImport -> "import"
    KIf -> "if"
    KThen -> "then"
    KElse -> "else"
    KCase -> "case"
    KOf -> "of"
    KLet -> "let"
    KIn -> "in"
    KDataframe -> "dataframe"
    KSelect -> "select"
    KFilter -> "filter"
    KMutate -> "mutate"
    KSummarize -> "summarize"
    KGroupBy -> "group_by"
    KUngroup -> "ungroup"
    KArrange -> "arrange"
    KRename -> "rename"
    KDistinct -> "distinct"
    KDistinctAll -> "distinct_all"
    KCount -> "count"
    KSlice -> "slice"
    KPull -> "pull"
    KRelocate -> "relocate"
    KTransmute -> "transmute"
    KMutateEach -> "mutate_each"
    KSummarizeEach -> "summarize_each"
    KLeftJoin -> "left_join"
    KRightJoin -> "right_join"
    KInnerJoin -> "inner_join"
    KFullJoin -> "full_join"
    KAntiJoin -> "anti_join"
    KSemiJoin -> "semi_join"
    KCrossJoin -> "cross_join"
    KDesc -> "desc"
    KAsc -> "asc"
    KOn -> "on"
    KAs -> "as"
    KWhere -> "where"
    KCols -> "cols"


-- | Reverse lookup for the lexer.
--
-- This is the canonical reserved-word check: any 'TLowerIdent'
-- candidate is checked against this map, and a hit becomes a 'TKeyword'
-- instead. Keeping it derived from the constructor list guarantees the
-- two stay in sync.
textKeyword :: Text -> Maybe Keyword
textKeyword t = Map.lookup t keywordIndex


keywordIndex :: Map.Map Text Keyword
keywordIndex =
    Map.fromList
        [ (keywordText k, k)
        | k <- allKeywords
        ]


-- | Every reserved keyword in source order. Used by the LSP server's
-- completion handler and any other consumer that wants to enumerate
-- the keyword set.
allKeywords :: [Keyword]
allKeywords = [Prelude.minBound .. Prelude.maxBound]


-- | One-line description used in parser error messages.
showToken :: Token -> Text
showToken = \case
    TLowerIdent t -> "identifier " ++ T.pack (Prelude.show t)
    TUpperIdent t -> "type name " ++ T.pack (Prelude.show t)
    TKeyword k -> "keyword " ++ T.pack (Prelude.show (keywordText k))
    TIntLit n -> "integer literal " ++ T.pack (Prelude.show n)
    TFloatLit n -> "double literal " ++ T.pack (Prelude.show (n :: Prelude.Double))
    TStringLit s -> "string literal " ++ T.pack (Prelude.show s)
    TArrow -> "'->'"
    TBind -> "'<-'"
    TPipe -> "'|>'"
    TPipeBar -> "'|'"
    TPlus -> "'+'"
    TMinus -> "'-'"
    TStar -> "'*'"
    TSlash -> "'/'"
    TIntDiv -> "'//'"
    TPercent -> "'%'"
    TCaret -> "'^'"
    TEq -> "'=='"
    TNeq -> "'!='"
    TGt -> "'>'"
    TLt -> "'<'"
    TGe -> "'>='"
    TLe -> "'<='"
    TAssign -> "'='"
    TColon -> "':'"
    TDot -> "'.'"
    TComma -> "','"
    TUnderscore -> "'_'"
    TAt -> "'@'"
    TBackslash -> "'\\'"
    TLParen -> "'('"
    TRParen -> "')'"
    TLBracket -> "'['"
    TRBracket -> "']'"
    TLBrace -> "'{'"
    TRBrace -> "'}'"
    TDotDot -> "'..'"
    TDocBlock _ -> "doc block"
    TNewline -> "newline"
    TIndent -> "indent"
    TDedent -> "dedent"
    TEof -> "end of file"
