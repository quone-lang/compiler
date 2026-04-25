{-| Source maps from generated R back to Quone source.

A sidecar @.R.map@ file lets the R companion package rewrite R-level
runtime tracebacks back to the original @.Q@ position. Without it,
runtime failures (e.g. those produced by @Script.expect@) appear to
the user as errors in generated R they did not write.

The map is NDJSON: one entry per line, with a single header object
on the first line carrying the source/generated paths.

@
{"source":"src/foo.Q","generated":"build/R/foo.R"}
{"r":{"line":1,"col":1},"q":{"line":1,"col":1}}
{"r":{"line":2,"col":1},"q":{"line":3,"col":5}}
@

The full implementation -- threading a 'SourceSpan' anchor through
the existing 'Quone.Generate.Pretty' renderer -- is in compiler track
A2. This module defines the data types and a coarse fallback that
maps every top-level binding to its declaration span by walking the
AST. That is enough to be useful immediately and the renderer
extension can refine it later without changing the on-disk format.

-}
module Quone.Generate.SourceMap
    ( SourceMap (..)
    , Entry (..)
    , buildSourceMap
    , encodeSourceMap
    )
where

import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
    ( Decl (..)
    , Program (..)
    , ValueDecl (..)
    , declSpan
    , exprSpan
    )
import Quone.Position
    ( SourcePos (..)
    , SourceSpan (..)
    )
import qualified Prelude



data SourceMap = SourceMap
    { smGenerated :: Prelude.FilePath
    , smSource :: Prelude.FilePath
    , smEntries :: [Entry]
    }
    deriving (Prelude.Show, Prelude.Eq)


data Entry = Entry
    { entryRLine :: Int
    , entryRCol :: Int
    , entryQLine :: Int
    , entryQCol :: Int
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Coarse-grained fallback source map: one entry per top-level
-- declaration. The R-side row is approximate (each top-level binding
-- in the generated R is one or two lines and the renderer emits them
-- in declaration order), but it gives us a useful first-cut mapping
-- without having to thread anchors through the whole pretty printer.
--
-- A more precise per-statement map lands when 'Quone.Generate.Pretty'
-- grows an @Anchor 'SourceSpan' Doc@ variant.
buildSourceMap :: Program -> [Entry]
buildSourceMap prog =
    let
        spans =
            Prelude.fmap declSpan (programDecls prog)
                Prelude.++ case programFinalExpr prog of
                    Nothing -> []
                    Just expr -> [exprSpan expr]
    in
    Prelude.zipWith mkEntry [1 ..] spans
  where
    mkEntry rLine sp =
        let
            SourceSpan start _ = sp
        in
        Entry
            { entryRLine = rLine
            , entryRCol = 1
            , entryQLine = posLine start
            , entryQCol = posCol start
            }


-- | NDJSON encoder for 'SourceMap'. Header line, then one entry per
-- line, terminated with a trailing newline.
encodeSourceMap :: SourceMap -> Text
encodeSourceMap sm =
    let
        header =
            "{\"source\":"
                ++ jsonString (T.pack (smSource sm))
                ++ ",\"generated\":"
                ++ jsonString (T.pack (smGenerated sm))
                ++ "}"
        body = Prelude.fmap encodeEntry (smEntries sm)
    in
    T.unlines (header : body)


encodeEntry :: Entry -> Text
encodeEntry e =
    "{\"r\":{\"line\":"
        ++ T.pack (Prelude.show (entryRLine e))
        ++ ",\"col\":"
        ++ T.pack (Prelude.show (entryRCol e))
        ++ "},\"q\":{\"line\":"
        ++ T.pack (Prelude.show (entryQLine e))
        ++ ",\"col\":"
        ++ T.pack (Prelude.show (entryQCol e))
        ++ "}}"


jsonString :: Text -> Text
jsonString s =
    "\""
        ++ T.replace "\"" "\\\"" (T.replace "\\" "\\\\" s)
        ++ "\""
