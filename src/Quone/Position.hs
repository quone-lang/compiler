{-| Source positions and spans.

Every token, AST node, and diagnostic carries a 'SourceSpan' so the
compiler can point at the offending character range.

LANGUAGE.md section 16 requires every error to include the source
location of the offence; that promise lives in this module.
-}
module Quone.Position
    ( SourcePos (..)
    , SourceSpan (..)
    , spanFromPos
    , unionSpan
    , emptySpan
    , showSourcePos
    , showSourceSpan
    )
where

import qualified Data.Text as T
import NriPrelude
import qualified Prelude


-- | A point in the source.
--
-- Lines and columns are 1-indexed (so they line up with what editors
-- show in the gutter). Files use 'Text' so paths display as written.
data SourcePos = SourcePos
    { posFile :: Text
    , posLine :: Int
    , posCol :: Int
    }
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- | A half-open range @[start, end)@ in the source.
data SourceSpan = SourceSpan
    { spanStart :: SourcePos
    , spanEnd :: SourcePos
    }
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- 'SourcePos' and 'SourceSpan' derive 'Ord' so types that include them
-- (e.g. @UpperName@ in the AST) can be used as 'Map' keys for
-- cross-module symbol tables in 'Quone.Resolve.Names'.


-- | Construct a single-character span at the given position.
spanFromPos :: SourcePos -> SourceSpan
spanFromPos p =
    SourceSpan
        { spanStart = p
        , spanEnd =
            p
                { posCol = posCol p + 1
                }
        }


-- | Smallest span containing both inputs.
unionSpan :: SourceSpan -> SourceSpan -> SourceSpan
unionSpan a b =
    SourceSpan
        { spanStart = Prelude.min (spanStart a) (spanStart b)
        , spanEnd = Prelude.max (spanEnd a) (spanEnd b)
        }


-- | Useful when an error is not tied to a specific span (rare).
emptySpan :: SourceSpan
emptySpan =
    let p = SourcePos "<unknown>" 0 0
    in SourceSpan p p


-- | Render a position the way most editors do: @file:line:col@.
showSourcePos :: SourcePos -> Text
showSourcePos p =
    posFile p
        ++ ":"
        ++ T.pack (Prelude.show (posLine p))
        ++ ":"
        ++ T.pack (Prelude.show (posCol p))


-- | Render a span. If start and end share a line, collapse to a column
-- range; otherwise show both endpoints.
showSourceSpan :: SourceSpan -> Text
showSourceSpan (SourceSpan start finish) =
    if posFile start == posFile finish && posLine start == posLine finish
        then
            posFile start
                ++ ":"
                ++ T.pack (Prelude.show (posLine start))
                ++ ":"
                ++ T.pack (Prelude.show (posCol start))
                ++ "-"
                ++ T.pack (Prelude.show (posCol finish))
        else showSourcePos start ++ "-" ++ showSourcePos finish
