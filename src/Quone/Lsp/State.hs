{-| Per-server state for the LSP server.

Holds the open documents (URI -> source text + version) and the most
recent compilation results so handlers can answer hover and
go-to-definition without re-running the pipeline.

-}
module Quone.Lsp.State
    ( State
    , Document (..)
    , empty
    , putDocument
    , getDocument
    , removeDocument
    , listUris
    )
where

import qualified Data.Map.Strict as Map
import NriPrelude
import qualified Prelude



data Document = Document
    { docText :: Text
    , docVersion :: Int
    }
    deriving (Prelude.Show, Prelude.Eq)


newtype State = State (Map.Map Text Document)


empty :: State
empty = State Map.empty


putDocument :: Text -> Document -> State -> State
putDocument uri d (State m) = State (Map.insert uri d m)


getDocument :: Text -> State -> Maybe Document
getDocument uri (State m) = Map.lookup uri m


removeDocument :: Text -> State -> State
removeDocument uri (State m) = State (Map.delete uri m)


listUris :: State -> [Text]
listUris (State m) = Map.keys m
