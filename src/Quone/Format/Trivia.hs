{-| Comments and blank-line markers that the parser currently drops.

The CST in 'Quone.Parse.Cst' tracks doc blocks (via 'CDocBlock') but
not other comments or blank-line patterns. Once the lexer in
'Quone.Lex.Lexer' is extended to attach trivia to tokens this module
gains a richer representation; for v0.0.1 we expose a small types
shell so 'Quone.Format.Rules' can reference it without depending on
the lexer extension landing first.

-}
module Quone.Format.Trivia
    ( Trivia (..)
    , noTrivia
    )
where

import NriPrelude
import qualified Prelude



-- | Trivia attached to a CST node. Every field is currently empty;
-- once the lexer tracks comments and blank-line groups, populate
-- 'leadingComments' with the @#@ comments that immediately preceded
-- the node and 'blankLinesBefore' with the count of intervening blank
-- lines.
data Trivia = Trivia
    { leadingComments :: [Text]
    , trailingComments :: [Text]
    , blankLinesBefore :: Int
    }
    deriving (Prelude.Show, Prelude.Eq)


noTrivia :: Trivia
noTrivia =
    Trivia
        { leadingComments = []
        , trailingComments = []
        , blankLinesBefore = 0
        }
