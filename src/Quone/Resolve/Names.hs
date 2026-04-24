{-| Cross-module name resolution.

For each module in a project we build a 'ModuleSymbols' table, then
verify imports against the source module's exports
(LANGUAGE2.md section 4.5).

For initial release the resolver focuses on the rules that need cross-module
information; intra-module name use is left to the typer (it already
walks the AST and would otherwise duplicate work).

-}
module Quone.Resolve.Names
    ( ModuleSymbols (..)
    , collectSymbols
    , resolveProgram
    , resolveProject
    , LocalName (..)
    , localNameKind
    , LocalNameKind (..)
    )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Diagnostic
    ( Category (Parse, UnboundVariable)
    , Diagnostic (..)
    , Severity (Error)
    )
import Quone.Position (SourceSpan)
import qualified Prelude



-- ---------------------------------------------------------------------
-- Module symbols
-- ---------------------------------------------------------------------


-- | A binding-level summary of one module's surface names. Used both
-- for export-list validation (within a module) and for cross-module
-- import checking (across the project).
data ModuleSymbols = ModuleSymbols
    { modSymPath :: ModulePath
    , modSymExports :: ExportSet
    , modSymLocals :: Map.Map Text LocalName
    }
    deriving (Prelude.Show, Prelude.Eq)


data ExportSet
    = ExportEverything
    | ExportSpecific (Set.Set Text)
    deriving (Prelude.Show, Prelude.Eq)


-- | Local declarations classified for resolution. We track the kind so
-- the resolver can give specific diagnostics ("you imported a
-- constructor as a function" etc.) in later revisions.
data LocalName = LocalName
    { localKind :: LocalNameKind
    , localSpan :: SourceSpan
    }
    deriving (Prelude.Show, Prelude.Eq)


data LocalNameKind
    = LkValue
    | LkType
    | LkConstructor
    | LkAlias
    | LkImport
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


localNameKind :: LocalName -> LocalNameKind
localNameKind = localKind



-- ---------------------------------------------------------------------
-- Single-module symbol collection
-- ---------------------------------------------------------------------


-- | Build a 'ModuleSymbols' for one program.
--
-- Pure: never reads the filesystem.
collectSymbols :: Program -> ModuleSymbols
collectSymbols prog =
    let
        modPath = case programModule prog of
            Just m -> modulePath m
            Nothing -> []

        exportSet = case programModule prog of
            Just m -> case moduleExports m of
                ExportAll _ -> ExportEverything
                ExportNames _ items ->
                    ExportSpecific
                        (Set.fromList (Prelude.fmap exportItemName items))
            Nothing -> ExportEverything
    in
    ModuleSymbols
        { modSymPath = modPath
        , modSymExports = exportSet
        , modSymLocals = collectLocals (programDecls prog)
        }


collectLocals :: [Decl] -> Map.Map Text LocalName
collectLocals = List.foldl' step Map.empty
  where
    step acc decl = case decl of
        DType d ->
            let
                tName =
                    LocalName
                        { localKind = LkType
                        , localSpan = upperSpan (typeDeclName d)
                        }
                withType = Map.insert (upperText (typeDeclName d)) tName acc
                withCons = List.foldl'
                    (\m v ->
                        Map.insert
                            (upperText (variantName v))
                            ( LocalName
                                { localKind = LkConstructor
                                , localSpan = upperSpan (variantName v)
                                }
                            )
                            m
                    )
                    withType
                    (typeDeclVariants d)
            in
            withCons
        DTypeAlias d ->
            Map.insert
                (upperText (aliasDeclName d))
                ( LocalName
                    { localKind = LkAlias
                    , localSpan = upperSpan (aliasDeclName d)
                    }
                )
                acc
        DImport (QuoneImport _ _ sel) ->
            Prelude.foldr addImported acc (selectionItems sel)
        DImport (ForeignImport _ _ fname _) ->
            Map.insert
                (lowerText (foreignBindName fname))
                ( LocalName
                    { localKind = LkImport
                    , localSpan = lowerSpan (foreignBindName fname)
                    }
                )
                acc
        DValue d ->
            Map.insert
                (lowerText (valueDeclName d))
                ( LocalName
                    { localKind = LkValue
                    , localSpan = lowerSpan (valueDeclName d)
                    }
                )
                acc
        DExtern (ExternValue _ _ name _ _ _) ->
            Map.insert
                (lowerText name)
                ( LocalName
                    { localKind = LkValue
                    , localSpan = lowerSpan name
                    }
                )
                acc
        DExtern (ExternType _ name _ _) ->
            Map.insert
                (upperText name)
                ( LocalName
                    { localKind = LkType
                    , localSpan = upperSpan name
                    }
                )
                acc
        DInfix _ ->
            -- Infix overloads do not introduce a new value-level
            -- binding (the operator is parsed as 'EBinOp', not as a
            -- function reference). The dispatch table lives in the
            -- type checker.
            acc
        DPrefix _ ->
            acc

    addImported item m =
        let
            (txt, sp) = case item of
                ExportLower n -> (lowerText n, lowerSpan n)
                ExportUpper n -> (upperText n, upperSpan n)
        in
        Map.insert
            txt
            ( LocalName
                { localKind = LkImport
                , localSpan = sp
                }
            )
            m


selectionItems :: ImportSelection -> [ExportItem]
selectionItems = \case
    ImportSingle i -> [i]
    ImportNames xs -> xs
    ImportAll -> []



-- ---------------------------------------------------------------------
-- Single-module resolution (no cross-module info needed)
-- ---------------------------------------------------------------------


-- | Validate the parts of LANGUAGE2.md section 4.5 a single module can
-- check on its own. For initial release that is:
--
-- * the foreign-import path is non-empty (parser already enforces);
-- * import selections do not declare duplicate local names.
--
-- Cross-module visibility (was the imported name actually exported by
-- the source module?) lives in 'resolveProject'.
resolveProgram :: Program -> [Diagnostic]
resolveProgram prog =
    duplicateLocalNames prog


duplicateLocalNames :: Program -> [Diagnostic]
duplicateLocalNames prog =
    let
        names = List.foldl' step [] (programDecls prog)

        step acc decl = case decl of
            DType d ->
                ( upperText (typeDeclName d)
                , upperSpan (typeDeclName d)
                )
                    : Prelude.fmap
                        (\v -> (upperText (variantName v), upperSpan (variantName v)))
                        (typeDeclVariants d)
                    Prelude.++ acc
            DTypeAlias d ->
                (upperText (aliasDeclName d), upperSpan (aliasDeclName d)) : acc
            DImport (QuoneImport _ _ sel) ->
                Prelude.fmap exportItemNameSpan (selectionItems sel) Prelude.++ acc
            DImport (ForeignImport _ _ fname _) ->
                (lowerText (foreignBindName fname), lowerSpan (foreignBindName fname)) : acc
            DValue d ->
                (lowerText (valueDeclName d), lowerSpan (valueDeclName d)) : acc
            DExtern (ExternValue _ _ name _ _ _) ->
                (lowerText name, lowerSpan name) : acc
            DExtern (ExternType _ name _ _) ->
                (upperText name, upperSpan name) : acc
            DInfix _ -> acc
            DPrefix _ -> acc

        seen = Map.empty :: Map.Map Text SourceSpan

        (_, dups) =
            List.foldl'
                (\(s, ds) (txt, sp) ->
                    case Map.lookup txt s of
                        Just _ ->
                            ( s
                            , dupDiag txt sp : ds
                            )
                        Nothing -> (Map.insert txt sp s, ds)
                )
                (seen, [])
                names
    in
    Prelude.reverse dups


exportItemNameSpan :: ExportItem -> (Text, SourceSpan)
exportItemNameSpan = \case
    ExportLower n -> (lowerText n, lowerSpan n)
    ExportUpper n -> (upperText n, upperSpan n)


dupDiag :: Text -> SourceSpan -> Diagnostic
dupDiag txt sp =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = Parse
        , diagSpan = sp
        , diagMessage =
            "duplicate top-level binding "
                Prelude.<> T.pack (Prelude.show txt)
        , diagHint = Just "rename one of the bindings or remove the duplicate"
        }



-- ---------------------------------------------------------------------
-- Project-wide resolution
-- ---------------------------------------------------------------------


-- | Validate cross-module imports across a whole project.
--
-- For each Quone import, look up the source module's symbol table; the
-- imported name MUST be in the source module's exports. Foreign
-- imports (lowercase first segment) are not checked here.
resolveProject
    :: Map.Map ModulePath ModuleSymbols
    -> Program
    -> [Diagnostic]
resolveProject allSyms prog =
    Prelude.concatMap
        (checkImport allSyms)
        (programDecls prog)


checkImport
    :: Map.Map ModulePath ModuleSymbols
    -> Decl
    -> [Diagnostic]
checkImport allSyms decl =
    case decl of
        DImport (QuoneImport _ path sel) ->
            case lookupByText path allSyms of
                Nothing ->
                    [ Diagnostic
                        { diagSeverity = Error
                        , diagCategory = UnboundVariable
                        , diagSpan = importDeclSpan decl
                        , diagMessage =
                            "module "
                                Prelude.<> renderModulePath path
                                Prelude.<> " is not part of this project"
                        , diagHint = Just "check the file path under src/"
                        }
                    ]
                Just sourceSyms ->
                    Prelude.concatMap
                        (checkSelectedName sourceSyms)
                        (selectionItems sel)
        _ -> []


-- | Look up by the textual representation of the module path.
--
-- 'UpperName' carries source spans, so two paths with the same
-- segments but different spans don't compare equal. The resolver
-- always wants to compare by name text alone.
lookupByText
    :: ModulePath
    -> Map.Map ModulePath v
    -> Maybe v
lookupByText needle m =
    let
        target = pathTexts needle
    in
    case List.find (\(k, _) -> pathTexts k Prelude.== target) (Map.toList m) of
        Just (_, v) -> Just v
        Nothing -> Nothing


pathTexts :: ModulePath -> [Text]
pathTexts = Prelude.fmap upperText


checkSelectedName
    :: ModuleSymbols
    -> ExportItem
    -> [Diagnostic]
checkSelectedName sourceSyms item =
    let
        (txt, sp) = exportItemNameSpan item
    in
    case modSymExports sourceSyms of
        ExportEverything ->
            -- A wildcard exporter exposes everything that is locally
            -- declared. Verify the name exists at all.
            if Map.member txt (modSymLocals sourceSyms)
                then []
                else
                    [ Diagnostic
                        { diagSeverity = Error
                        , diagCategory = UnboundVariable
                        , diagSpan = sp
                        , diagMessage =
                            renderModulePath (modSymPath sourceSyms)
                                Prelude.<> " does not define "
                                Prelude.<> T.pack (Prelude.show txt)
                        , diagHint = Nothing
                        }
                    ]
        ExportSpecific exported ->
            if Set.member txt exported
                then []
                else
                    [ Diagnostic
                        { diagSeverity = Error
                        , diagCategory = UnboundVariable
                        , diagSpan = sp
                        , diagMessage =
                            renderModulePath (modSymPath sourceSyms)
                                Prelude.<> " does not export "
                                Prelude.<> T.pack (Prelude.show txt)
                        , diagHint = Just "add it to the source module's `exporting (..)` list"
                        }
                    ]


importDeclSpan :: Decl -> SourceSpan
importDeclSpan = \case
    DImport (QuoneImport sp _ _) -> sp
    DImport (ForeignImport sp _ _ _) -> sp
    other -> declSpan other


renderModulePath :: ModulePath -> Text
renderModulePath = T.intercalate "." Prelude.. Prelude.fmap upperText
