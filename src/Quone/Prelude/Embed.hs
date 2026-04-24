{-| The embedded Quone prelude source.

The compiler ships with a single 'Prelude.Q' source file (under
@compiler/prelude/Prelude.Q@) containing every prelude declaration:
named functions ('extern'), operator overloads ('infix' / 'prefix'),
and primitive types ('extern type'). It is embedded into the binary
at build time via 'Data.FileEmbed.embedFile' so the runtime layout
needs no separate stdlib path.

The text returned by 'preludeSource' is consumed by
'Quone.Prelude.Load.loadPrelude' once at compiler startup; the
resulting typing environment seeds every user-program inference.

-}

{-# LANGUAGE TemplateHaskell #-}
module Quone.Prelude.Embed
    ( preludeSource
    , preludeFilename
    )
where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.FileEmbed (embedFile)
import NriPrelude


-- | The verbatim source of the embedded prelude module. Decoded as
-- UTF-8.
preludeSource :: Text
preludeSource = TE.decodeUtf8 preludeBytes


-- | A virtual filename used in diagnostics that fire while parsing
-- or type-checking the embedded prelude. Not a real disk path.
preludeFilename :: Text
preludeFilename = "<prelude>"


preludeBytes :: BS.ByteString
preludeBytes = $(embedFile "prelude/Prelude.Q")
