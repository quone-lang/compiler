{-| AST well-formedness validation.

Enforces the nine invariants from LANGUAGE.md section 6.8:

1.  Module placement (only one module declaration; no inner one).
2.  Export consistency (every name in @exporting (..)@ is defined).
3.  Quone import visibility (the imported name is exported by the
    source module). v0.0.1: deferred to the resolve stage which has
    cross-module info.
4.  R-export discipline (a binding marked @\@export@ MUST appear in
    the module's @exporting (..)@ list).
5.  Constructor arity in patterns (deferred to type checking, which
    has the constructor table).
6.  Record-update target is not a dataframe-typed expression (deferred
    to type checking).
7.  Operator typing precondition (deferred to type checking).
8.  Verb argument shape (deferred to verb typing).
9.  No 'EIf' in the AST (structurally enforced: the AST has no such
    constructor).

The two invariants that can be enforced from a single module's AST
alone (1, 2, 4) are checked here. The rest are picked up by later
passes that have the additional information they need.

The check returns a list of diagnostics; an empty list means the AST
is well-formed by the rules this pass owns.

-}
module Quone.Ast.Validate
    ( validate
    , validateProgram
    )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Diagnostic
    ( Category (Internal, Parse)
    , Diagnostic (..)
    , Severity (Error)
    )
import Quone.Position (SourceSpan)
import qualified Prelude



-- | Run every invariant check this pass is responsible for. Returns
-- the union of diagnostics; an empty list means the AST passes.
validate :: Program -> [Diagnostic]
validate prog =
    invariantExportConsistency prog
        ++ invariantRExportDiscipline prog


-- | Convenience: validate and return either the program or the first
-- failing diagnostic. Used by the CLI's pipeline driver.
validateProgram :: Program -> Prelude.Either Diagnostic Program
validateProgram prog =
    case validate prog of
        [] -> Prelude.Right prog
        (d : _) -> Prelude.Left d



-- ---------------------------------------------------------------------
-- Invariant 2: every name in `exporting (..)` is defined
-- ---------------------------------------------------------------------


invariantExportConsistency :: Program -> [Diagnostic]
invariantExportConsistency prog =
    case programModule prog of
        Nothing -> []
        Just modDecl ->
            case moduleExports modDecl of
                ExportAll _ -> []
                ExportNames _ items ->
                    let
                        defined = collectDefinedNames (programDecls prog)
                        check item =
                            if Set.member (exportItemName item) defined
                                then Nothing
                                else
                                    Just
                                        ( Diagnostic
                                            { diagSeverity = Error
                                            , diagCategory = Parse
                                            , diagSpan = exportItemSpan item
                                            , diagMessage =
                                                "exported name "
                                                    Prelude.<> T.pack (Prelude.show (exportItemName item))
                                                    Prelude.<> " is not defined in this module"
                                            , diagHint = Just "remove it from the `exporting (..)` list, or add a declaration with this name"
                                            }
                                        )
                    in
                    List.foldr
                        (\item acc -> case check item of
                            Just d -> d : acc
                            Nothing -> acc)
                        []
                        items


exportItemSpan :: ExportItem -> SourceSpan
exportItemSpan = \case
    ExportLower n -> lowerSpan n
    ExportUpper n -> upperSpan n


-- | Names defined by top-level declarations. Includes:
--
-- * value bindings (their lower name);
-- * type names and their constructors;
-- * type aliases;
-- * imported names.
collectDefinedNames :: [Decl] -> Set.Set Text
collectDefinedNames decls =
    Set.fromList (List.concatMap declNames decls)


declNames :: Decl -> [Text]
declNames = \case
    DType d ->
        upperText (typeDeclName d)
            : Prelude.fmap (\v -> upperText (variantName v)) (typeDeclVariants d)
    DTypeAlias d ->
        [upperText (aliasDeclName d)]
    DImport d ->
        case d of
            QuoneImport _ _ sel -> selectionNames sel
            ForeignImport _ fname _ -> [lowerText (foreignFn fname)]
    DValue d ->
        [lowerText (valueDeclName d)]


selectionNames :: ImportSelection -> [Text]
selectionNames = \case
    ImportSingle item -> [exportItemName item]
    ImportNames items -> Prelude.fmap exportItemName items
    -- ImportAll only contributes names once the importer's module is
    -- known; the resolver handles that case.
    ImportAll -> []



-- ---------------------------------------------------------------------
-- Invariant 4: @export tag requires Quone-level export
-- ---------------------------------------------------------------------


invariantRExportDiscipline :: Program -> [Diagnostic]
invariantRExportDiscipline prog =
    case programModule prog of
        Nothing -> []
        Just modDecl ->
            let
                exportSet = case moduleExports modDecl of
                    ExportAll _ -> Nothing  -- everything is exported
                    ExportNames _ items ->
                        Just
                            ( Set.fromList
                                (Prelude.fmap exportItemName items)
                            )
                checkValue v =
                    if hasExportTag (valueDeclDoc v)
                        then case exportSet of
                            Nothing -> Nothing
                            Just s ->
                                if Set.member (lowerText (valueDeclName v)) s
                                    then Nothing
                                    else
                                        Just
                                            ( Diagnostic
                                                { diagSeverity = Error
                                                , diagCategory = Parse
                                                , diagSpan = lowerSpan (valueDeclName v)
                                                , diagMessage =
                                                    "binding "
                                                        Prelude.<> T.pack (Prelude.show (lowerText (valueDeclName v)))
                                                        Prelude.<> " has an `@export` doc tag but is not in the module's `exporting (..)` list"
                                                , diagHint = Just "add the name to `exporting (..)` or remove the @export tag"
                                                }
                                            )
                        else Nothing
            in
            List.foldr
                (\d acc -> case d of
                    DValue v -> case checkValue v of
                        Just diag -> diag : acc
                        Nothing -> acc
                    _ -> acc)
                []
                (programDecls prog)


hasExportTag :: Maybe DocBlock -> Prelude.Bool
hasExportTag Nothing = Prelude.False
hasExportTag (Just block) =
    Prelude.any
        (\line -> "@export" `T.isInfixOf` line)
        (docLines block)
