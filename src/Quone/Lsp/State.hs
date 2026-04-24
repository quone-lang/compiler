{-| Per-server state for the LSP server.

Holds the open documents (URI -> source text + version) and the most
recent compilation results so handlers can answer hover and
go-to-definition without re-running the pipeline. Also caches the
LAST SUCCESSFUL compile per URI (M4.4) so hover / definition / etc.
gracefully fall back to known-good information while the user is
mid-edit and the file currently fails to typecheck.

-}
module Quone.Lsp.State
    ( State
    , Document (..)
    , empty
    , putDocument
    , getDocument
    , removeDocument
    , listUris
    , putLastGood
    , getLastGood
    , LastGood (..)
    )
where

import qualified Data.Map.Strict as Map
import NriPrelude
import Quone.Ast.Source (Program)
import Quone.Type.Infer (TypedProgram)
import qualified Prelude



data Document = Document
    { docText :: Text
    , docVersion :: Int
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | The most recent known-good compile of a document (M4.4). Used
-- by hover / definition / completion handlers to keep returning
-- useful answers while the user is mid-edit and the current text
-- doesn't yet typecheck.
data LastGood = LastGood
    { lastGoodVersion :: Int
    , lastGoodProgram :: Program
    , lastGoodTyped :: TypedProgram
    }


data State = State
    { stateDocs :: Map.Map Text Document
    , stateLastGood :: Map.Map Text LastGood
    }


empty :: State
empty = State {stateDocs = Map.empty, stateLastGood = Map.empty}


putDocument :: Text -> Document -> State -> State
putDocument uri d s = s {stateDocs = Map.insert uri d (stateDocs s)}


getDocument :: Text -> State -> Maybe Document
getDocument uri s = Map.lookup uri (stateDocs s)


removeDocument :: Text -> State -> State
removeDocument uri s =
    s
        { stateDocs = Map.delete uri (stateDocs s)
        , stateLastGood = Map.delete uri (stateLastGood s)
        }


listUris :: State -> [Text]
listUris s = Map.keys (stateDocs s)


-- | Record the latest successful compile of @uri@ at @version@.
putLastGood :: Text -> LastGood -> State -> State
putLastGood uri lg s =
    s {stateLastGood = Map.insert uri lg (stateLastGood s)}


-- | Look up the last successful compile of @uri@, regardless of
-- the current document version.
getLastGood :: Text -> State -> Maybe LastGood
getLastGood uri s = Map.lookup uri (stateLastGood s)
