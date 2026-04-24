{-| Comments and blank-line markers that the parser currently drops.

The CST in 'Quone.Parse.Cst' tracks doc blocks (via 'CDocBlock') so
those round-trip through the formatter. Block comments and trailing
inline @#@ comments are dropped in initial release; full trivia preservation
(see LANGUAGE.md section 19.7) requires extending the lexer to
attach trivia tokens and threading them through the CST. Promoted
to future release.

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
