{-| A Wadler-style pretty-printer for the formatter.

The R generator's printer in 'Quone.Generate.Pretty' is intentionally
simpler (lines + indents, no group/flatten), which is fine when the
output structure is fixed. The formatter needs to choose between a
single-line and a broken-out layout for the same input, so we use
the standard Wadler trio:

* 'group' marks a 'Doc' that may be flattened or broken;
* 'flatten' rewrites a 'Doc' to its single-line form;
* 'render' picks the broken layout when the flattened width exceeds
  the page width.

The implementation is a vendored slice of the Wadler/Leijen design,
dependency-free and small enough to live in this module.

-}
module Quone.Format.Doc
    ( Doc
    , empty
    , text
    , line
    , softline
    , space
    , (<>)
    , (<+>)
    , concatD
    , hsep
    , vsep
    , nest
    , indent
    , group
    , render
    , width
    )
where

import qualified Data.Text as T
import NriPrelude hiding ((<>), (<+>))
import qualified Prelude



-- | The page width used for line-breaking decisions.
width :: Prelude.Int
width = 80


-- | A small Wadler-style document.
data Doc
    = DEmpty
    | DText Text
    | DLine
    | DSoftline
    | DSpace
    | DConcat Doc Doc
    | DNest Prelude.Int Doc
    | DGroup Doc
    deriving (Prelude.Show)


empty :: Doc
empty = DEmpty


text :: Text -> Doc
text = DText


-- | A hard line break that is preserved even inside 'group'.
line :: Doc
line = DLine


-- | A line break that flattens to a single space inside 'group'.
softline :: Doc
softline = DSoftline


space :: Doc
space = DSpace


-- | Concatenation. Re-exported as @<>@ for convenience.
(<>) :: Doc -> Doc -> Doc
(<>) = DConcat
infixr 6 <>


-- | Concatenation with a soft space between the two parts.
(<+>) :: Doc -> Doc -> Doc
a <+> b = a <> space <> b
infixr 6 <+>


concatD :: [Doc] -> Doc
concatD = Prelude.foldr (<>) empty


hsep :: [Doc] -> Doc
hsep = Prelude.foldr (\a b -> a <+> b) empty


vsep :: [Doc] -> Doc
vsep = intersperse line


nest :: Prelude.Int -> Doc -> Doc
nest = DNest


indent :: Prelude.Int -> Doc -> Doc
indent n d = nest n (text (T.replicate n " ") <> d)


-- | Mark a sub-document for the line-break decision.
group :: Doc -> Doc
group = DGroup


-- | Render at the package-wide page width.
render :: Doc -> Text
render = renderAt width


renderAt :: Prelude.Int -> Doc -> Text
renderAt w d = layout w 0 [(0, d)]


-- | Pretty-printing core. @col@ is the column the next character
-- would land on; the stack is a list of @(indent, doc)@ continuations.
layout :: Prelude.Int -> Prelude.Int -> [(Prelude.Int, Doc)] -> Text
layout _ _ [] = ""
layout w col ((i, d) : rest) = case d of
    DEmpty -> layout w col rest
    DText t -> t Prelude.<> layout w (col Prelude.+ T.length t) rest
    DLine -> "\n" Prelude.<> pad i Prelude.<> layout w i rest
    DSpace -> " " Prelude.<> layout w (col Prelude.+ 1) rest
    DSoftline -> " " Prelude.<> layout w (col Prelude.+ 1) rest
    DConcat a b -> layout w col ((i, a) : (i, b) : rest)
    DNest n inner -> layout w col ((i Prelude.+ n, inner) : rest)
    DGroup inner ->
        let
            flat = flatten inner
            flatLen = flatWidth flat
        in
        if col Prelude.+ flatLen Prelude.<= w
            then layout w col ((i, flat) : rest)
            else layout w col ((i, inner) : rest)


flatten :: Doc -> Doc
flatten = \case
    DEmpty -> DEmpty
    DText t -> DText t
    DLine -> DText " "
    DSoftline -> DText " "
    DSpace -> DSpace
    DConcat a b -> DConcat (flatten a) (flatten b)
    DNest _ inner -> flatten inner
    DGroup inner -> flatten inner


flatWidth :: Doc -> Prelude.Int
flatWidth = \case
    DEmpty -> 0
    DText t -> T.length t
    DLine -> 1
    DSoftline -> 1
    DSpace -> 1
    DConcat a b -> flatWidth a Prelude.+ flatWidth b
    DNest _ inner -> flatWidth inner
    DGroup inner -> flatWidth inner


pad :: Prelude.Int -> Text
pad n = T.replicate n " "


intersperse :: Doc -> [Doc] -> Doc
intersperse _ [] = empty
intersperse _ [d] = d
intersperse sep (d : ds) = d <> sep <> intersperse sep ds
