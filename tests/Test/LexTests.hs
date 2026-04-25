module Test.LexTests (suite) where

import NriPrelude
import Quone.Lex.Lexer (lexInput)
import Quone.Lex.Token
    ( Keyword (..)
    , Located (..)
    , Token (..)
    )
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..), (===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "lexer"
        [ Harness.test "lex/initial_release_keywords" <|
            Prelude.pure
                (tokenValues "filter mutate summarize group_by ungroup arrange rename left_join right_join inner_join"
                    === [ TKeyword KFilter
                        , TKeyword KMutate
                        , TKeyword KSummarize
                        , TKeyword KGroupBy
                        , TKeyword KUngroup
                        , TKeyword KArrange
                        , TKeyword KRename
                        , TKeyword KLeftJoin
                        , TKeyword KRightJoin
                        , TKeyword KInnerJoin
                        , TEof
                        ]
                )
        , Harness.test "lex/deferred_verbs_are_identifiers" <|
            Prelude.pure
                (tokenValues "full_join distinct transmute"
                    === [ TLowerIdent "full_join"
                        , TLowerIdent "distinct"
                        , TLowerIdent "transmute"
                        , TEof
                        ]
                )
        , Harness.test "lex/doc_comments_are_preserved" <|
            Prelude.pure
                (tokenValues "#' hello\nx <- 1"
                    === [ TDocBlock "hello"
                        , TLowerIdent "x"
                        , TBind
                        , TFloatLit 1.0 "1"
                        , TEof
                        ]
                )
        ]


tokenValues :: Text -> [Token]
tokenValues src =
    case lexInput src of
        Prelude.Left _ -> []
        Prelude.Right toks ->
            Prelude.filter
                (\tok -> tok Prelude./= TNewline)
                (Prelude.fmap locValue toks)

