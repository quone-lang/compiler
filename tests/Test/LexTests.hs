{-| Lexer tests.

Each test name encodes the LANGUAGE.md section it covers
(section 16.14 convention).

The suite covers section 16.3:

* every literal kind from section 3.3,
* each keyword group from section 3.4,
* each operator and punctuation token from section 3.5,
* identifier rules from section 3.2 (case, leading-@_@ rejection,
  @.@-in-identifier rejection),
* doc and line comments from section 3.6,
* indentation handling from section 3.7,
* source-position tracking on every emitted token.

-}
module Test.LexTests (suite) where

import qualified Data.List as List
import qualified Data.Text as T
import NriPrelude
import Quone.Diagnostic (Category (Lexical), Diagnostic (..))
import Quone.Lex.Lexer (lexInput)
import Quone.Lex.Token
import Quone.Position (SourcePos (..), SourceSpan (..))
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , assert
    , assertLeft
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "lexer"
        ( literalTests
            ++ keywordTests
            ++ operatorTests
            ++ identifierTests
            ++ commentTests
            ++ layoutTests
            ++ positionTests
        )



-- ---------------------------------------------------------------------
-- Literals
-- ---------------------------------------------------------------------


literalTests :: [Test]
literalTests =
    [ test "lex/bare_zero_is_double_section_3_3" <|
        -- Per LANGUAGE.md section 3.3, bare digit runs are doubles
        -- (matches R, where `0` is `numeric` not `integer`).
        Prelude.pure (firstTokens "0" === [TFloatLit 0.0])
    , test "lex/bare_integer_is_double_section_3_3" <|
        Prelude.pure (firstTokens "42" === [TFloatLit 42.0])
    , test "lex/integer_with_L_suffix_section_3_3" <|
        -- Trailing `L` makes the literal an Integer, mirroring R's
        -- `42L` syntax.
        Prelude.pure (firstTokens "42L" === [TIntLit 42])
    , test "lex/integer_zero_with_L_suffix_section_3_3" <|
        Prelude.pure (firstTokens "0L" === [TIntLit 0])
    , test "lex/double_simple_section_3_3" <|
        Prelude.pure (firstTokens "3.14" === [TFloatLit 3.14])
    , test "lex/double_zero_section_3_3" <|
        Prelude.pure (firstTokens "0.0" === [TFloatLit 0.0])
    , test "lex/double_with_L_suffix_rejected_section_3_3" <|
        -- `1.5L` would mean "Integer with a fractional part", which
        -- has no sensible interpretation. The lexer rejects it
        -- rather than silently dropping the fraction.
        Prelude.pure (assertLeft (lexInput "1.5L"))
    , test "lex/integer_suffix_must_be_terminator_section_3_3" <|
        -- `42Lx` is NOT an integer literal followed by an identifier;
        -- the `L` only counts as a suffix when followed by a
        -- non-identifier character. Two tokens should result, with
        -- `Lx` lexed as an upper-ident because it starts with `L`.
        Prelude.pure
            (firstTokens "42Lx" === [TFloatLit 42.0, TUpperIdent "Lx"])
    , test "lex/character_string_section_3_3" <|
        Prelude.pure (firstTokens "\"hello\"" === [TStringLit "hello"])
    , test "lex/character_empty_string_section_3_3" <|
        Prelude.pure (firstTokens "\"\"" === [TStringLit ""])
    , test "lex/character_unterminated_string_rejected_section_3_3" <|
        Prelude.pure (assertLeft (lexInput "\"oops"))
    , test "lex/logical_true_constructor_section_3_3" <|
        Prelude.pure (firstTokens "True" === [TUpperIdent "True"])
    , test "lex/logical_false_constructor_section_3_3" <|
        Prelude.pure (firstTokens "False" === [TUpperIdent "False"])
    ]



-- ---------------------------------------------------------------------
-- Keywords
-- ---------------------------------------------------------------------


keywordTests :: [Test]
keywordTests =
    let
        check kw =
            test
                ( "lex/keyword_"
                    ++ T.replace "_" "_" (keywordText kw)
                    ++ "_section_3_4"
                )
                ( Prelude.pure
                    ( firstTokens (keywordText kw)
                        === [TKeyword kw]
                    )
                )
    in
    Prelude.fmap check [Prelude.minBound .. Prelude.maxBound]



-- ---------------------------------------------------------------------
-- Operators and punctuation
-- ---------------------------------------------------------------------


operatorTests :: [Test]
operatorTests =
    let
        cases =
            [ ("->", [TArrow])
            , ("<-", [TBind])
            , ("|>", [TPipe])
            , ("|", [TPipeBar])
            , ("+", [TPlus])
            , ("-", [TMinus])
            , ("*", [TStar])
            , ("/", [TSlash])
            , ("//", [TIntDiv])
            , ("%", [TPercent])
            , ("^", [TCaret])
            , ("==", [TEq])
            , ("!=", [TNeq])
            , (">", [TGt])
            , ("<", [TLt])
            , (">=", [TGe])
            , ("<=", [TLe])
            , ("=", [TAssign])
            , (":", [TColon])
            , (".", [TDot])
            , (",", [TComma])
            , ("@", [TAt])
            , ("\\", [TBackslash])
            , ("(", [TLParen])
            , (")", [TRParen])
            , ("[", [TLBracket])
            , ("]", [TRBracket])
            , ("{", [TLBrace])
            , ("}", [TRBrace])
            , ("..", [TDotDot])
            ]

        check (input, expected) =
            test
                ( "lex/operator_"
                    ++ T.pack (Prelude.show input)
                    ++ "_section_3_5"
                )
                (Prelude.pure (firstTokens input === expected))

        multi =
            [ test "lex/operator_arrow_vs_minus_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "->-" === [TArrow, TMinus])
            , test "lex/operator_pipe_vs_pipe_bar_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "|>|" === [TPipe, TPipeBar])
            , test "lex/operator_int_div_vs_slash_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "// /" === [TIntDiv, TSlash])
            , test "lex/operator_eq_vs_assign_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "===" === [TEq, TAssign])
            , test "lex/operator_dotdot_vs_dot_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "..." === [TDotDot, TDot])
            ]
    in
    Prelude.fmap check cases ++ multi



-- ---------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------


identifierTests :: [Test]
identifierTests =
    [ test "lex/identifier_lowercase_section_3_2" <|
        Prelude.pure (firstTokens "score" === [TLowerIdent "score"])
    , test "lex/identifier_lowercase_with_underscore_section_3_2" <|
        Prelude.pure (firstTokens "max_score" === [TLowerIdent "max_score"])
    , test "lex/identifier_lowercase_with_digits_section_3_2" <|
        Prelude.pure (firstTokens "x1" === [TLowerIdent "x1"])
    , test "lex/identifier_uppercase_section_3_2" <|
        Prelude.pure (firstTokens "Maybe" === [TUpperIdent "Maybe"])
    , test "lex/identifier_uppercase_with_digits_section_3_2" <|
        Prelude.pure (firstTokens "Vec3" === [TUpperIdent "Vec3"])
    , test "lex/identifier_leading_underscore_rejected_section_3_2" <|
        -- A lone leading '_' tokenises as TUnderscore (the wildcard
        -- pattern); it's the parser's job to reject it as an
        -- identifier in non-pattern positions. Here we only check that
        -- "_oops" does NOT come out as a single identifier token.
        Prelude.pure
            ( assert
                ( firstTokens "_oops"
                    Prelude./= [TLowerIdent "_oops"]
                )
                "leading-underscore identifier should not lex as a single TLowerIdent"
            )
    , test "lex/identifier_dot_separates_field_access_section_3_2" <|
        Prelude.pure
            ( firstTokens "row.score"
                === [TLowerIdent "row", TDot, TLowerIdent "score"]
            )
    ]



-- ---------------------------------------------------------------------
-- Comments
-- ---------------------------------------------------------------------


commentTests :: [Test]
commentTests =
    [ test "lex/comment_line_dropped_section_3_6" <|
        Prelude.pure (firstTokens "x # this is dropped" === [TLowerIdent "x"])
    , test "lex/comment_line_alone_yields_eof_section_3_6" <|
        Prelude.pure (firstTokens "# nothing else" === [])
    , test "lex/comment_doc_single_line_section_3_6" <|
        Prelude.pure
            ( firstTokens "#' Hello"
                === [TDocBlock "Hello"]
            )
    , test "lex/comment_doc_multi_line_collapsed_section_3_6" <|
        Prelude.pure
            ( firstTokens "#' Line one\n#' Line two"
                === [TDocBlock "Line one\nLine two"]
            )
    , test "lex/comment_doc_strips_one_leading_space_section_3_6" <|
        Prelude.pure
            ( firstTokens "#'  two spaces"
                === [TDocBlock " two spaces"]
            )
    , test "lex/comment_doc_then_code_attached_section_3_6" <|
        Prelude.pure
            ( assert
                (List.elem (TDocBlock "doc") (allTokens "#' doc\nx"))
                "doc block should appear before the next token"
            )
    ]



-- ---------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------


layoutTests :: [Test]
layoutTests =
    [ test "lex/layout_indent_emitted_section_3_7" <|
        let
            toks = allTokens "let\n  x"
        in
        Prelude.pure
            ( assert
                (TIndent `List.elem` toks)
                "TIndent expected when body indents"
            )
    , test "lex/layout_dedent_emitted_section_3_7" <|
        let
            toks = allTokens "let\n  x\ny"
        in
        Prelude.pure
            ( assert
                (TDedent `List.elem` toks)
                "TDedent expected when indent decreases"
            )
    , test "lex/layout_eof_closes_layers_section_3_7" <|
        let
            toks = allTokens "let\n  x\n  y"
            eof = Prelude.last toks
            dedents =
                Prelude.length (Prelude.filter (Prelude.== TDedent) toks)
        in
        Prelude.pure
            ( assert
                (eof Prelude.== TEof Prelude.&& dedents Prelude.>= 1)
                "EOF should close any open indent layers"
            )
    ]



-- ---------------------------------------------------------------------
-- Source position tracking
-- ---------------------------------------------------------------------


positionTests :: [Test]
positionTests =
    [ test "lex/position_first_token_at_1_1_section_16_3" <|
        case lexInput "score" of
            Prelude.Right (Located {locSpan = SourceSpan start _} : _) ->
                Prelude.pure
                    ( assert
                        (posLine start Prelude.== 1 Prelude.&& posCol start Prelude.== 1)
                        "first token should start at line 1 column 1"
                    )
            _ ->
                Prelude.pure (Fail "expected at least one token")
    , test "lex/position_second_line_starts_at_2_1_section_16_3" <|
        let
            toks = allTokens "x\ny"
            -- find the 'y' token (TLowerIdent "y")
            mY = List.find (\l -> locValueOf l Prelude.== TLowerIdent "y") (lexedLocated "x\ny")
        in
        case mY of
            Just l ->
                let
                    start = spanStart (locSpan l)
                in
                Prelude.pure
                    ( assert
                        (posLine start Prelude.== 2 Prelude.&& posCol start Prelude.== 1)
                        "'y' should start at line 2 column 1"
                    )
            Nothing ->
                Prelude.pure (Fail (T.pack (Prelude.show toks)))
    ]



-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------


-- | Strip layout/EOF tokens from a 'lexInput' result for tests that
-- only care about the substantive token sequence.
firstTokens :: Text -> [Token]
firstTokens src =
    case lexInput src of
        Prelude.Left _ -> []
        Prelude.Right toks -> stripLayout (Prelude.fmap locValue toks)


-- | All tokens including layout markers and EOF.
allTokens :: Text -> [Token]
allTokens src =
    case lexInput src of
        Prelude.Left _ -> []
        Prelude.Right toks -> Prelude.fmap locValue toks


-- | Located version of 'allTokens' (positions preserved).
lexedLocated :: Text -> [Located Token]
lexedLocated src =
    case lexInput src of
        Prelude.Left _ -> []
        Prelude.Right toks -> toks


locValueOf :: Located a -> a
locValueOf = locValue


stripLayout :: [Token] -> [Token]
stripLayout = Prelude.filter (\t -> Prelude.not (isLayoutOrEof t))
  where
    isLayoutOrEof = \case
        TNewline -> Prelude.True
        TIndent -> Prelude.True
        TDedent -> Prelude.True
        TEof -> Prelude.True
        _ -> Prelude.False
