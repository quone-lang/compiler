{-| A tiny pretty-printing helper for the R generator.

R itself is a token-friendly language; we don't need a full Wadler-
style printer for initial release. Hand-rolling a 'Doc' with explicit indent
levels keeps generated output predictable for snapshot tests.

-}
module Quone.Generate.Pretty
    ( Doc (..)
    , empty
    , line
    , (<+>)
    , (<+|>)
    , render
    , indent
    , parens
    , braces
    , brackets
    , sepBy
    , callR
    , namedCallR
    , assign
    , comment
    )
where

import qualified Data.List as List
import qualified Data.Text as T
import NriPrelude
import qualified Prelude



-- | A 'Doc' is just lines plus an indent prefix per line.
data Doc
    = Empty
    | Line Text
    | Indent Int Doc
    | Concat [Doc]
    deriving (Prelude.Show, Prelude.Eq)


empty :: Doc
empty = Empty


line :: Text -> Doc
line = Line


(<+>) :: Doc -> Doc -> Doc
Empty <+> b = b
a <+> Empty = a
Concat xs <+> Concat ys = Concat (xs Prelude.++ ys)
Concat xs <+> y = Concat (xs Prelude.++ [y])
x <+> Concat ys = Concat (x : ys)
a <+> b = Concat [a, b]


infixr 5 <+>


-- | Same as '<+>' but inserts a blank line between the two halves.
(<+|>) :: Doc -> Doc -> Doc
Empty <+|> b = b
a <+|> Empty = a
a <+|> b = a <+> Line "" <+> b


infixr 4 <+|>


indent :: Int -> Doc -> Doc
indent _ Empty = Empty
indent n d = Indent n d


render :: Doc -> Text
render = T.intercalate "\n" Prelude.. go 0
  where
    go :: Int -> Doc -> [Text]
    go n = \case
        Empty -> []
        Line t -> [T.replicate (Prelude.fromIntegral n) " " Prelude.<> t]
        Indent k inner -> go (n Prelude.+ k) inner
        Concat ds -> Prelude.concatMap (go n) ds



-- ---------------------------------------------------------------------
-- R-shape helpers
-- ---------------------------------------------------------------------


parens :: Text -> Text
parens t = "(" Prelude.<> t Prelude.<> ")"


braces :: Text -> Text
braces t = "{" Prelude.<> t Prelude.<> "}"


brackets :: Text -> Text
brackets t = "[" Prelude.<> t Prelude.<> "]"


-- | "fn(arg1, arg2, ...)"
callR :: Text -> [Text] -> Text
callR fn args = fn Prelude.<> "(" Prelude.<> T.intercalate ", " args Prelude.<> ")"


-- | "fn(name1 = arg1, name2 = arg2, ...)"
namedCallR :: Text -> [(Text, Text)] -> Text
namedCallR fn args =
    fn
        Prelude.<> "("
        Prelude.<> T.intercalate
            ", "
            (Prelude.fmap (\(n, v) -> n Prelude.<> " = " Prelude.<> v) args)
        Prelude.<> ")"


-- | R's assignment operator (matches LANGUAGE2.md section 13.2.1).
assign :: Text -> Text -> Text
assign name body = name Prelude.<> " <- " Prelude.<> body


-- | Render a list with a separator.
sepBy :: Text -> [Text] -> Text
sepBy = T.intercalate


-- | A roxygen-style comment line.
comment :: Text -> Text
comment t = "# " Prelude.<> t
