{-| Per-module R file generation.

A Quone module @Stats.Transform@ at @src/Stats/Transform.Q@ lowers to
@R/stats-transform.R@ per LANGUAGE.md section 14.6. The file contents
are the lowered declarations preceded by their @#'@ doc blocks.

Function names are NOT mangled: the v0.0.1 design (section 14.6,
"Function-name mapping") guarantees a single flat R namespace per
package, so cross-module calls compile to bare names. The package-
wide collision check is enforced by 'Quone.Generate.Package'.

-}
module Quone.Generate.Module
    ( ModuleArtifact (..)
    , generateModule
    , moduleFileName
    , moduleExports
    , preserveDoc
    )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Generate.R (generateProgram)
import qualified Prelude



-- | The R-side artifact for a single Quone module.
data ModuleArtifact = ModuleArtifact
    { artifactPath :: Prelude.FilePath
    , artifactBody :: Text
    , artifactExports :: [Text]
    , artifactDeps :: [Text]
    }
    deriving (Prelude.Show, Prelude.Eq)


generateModule :: Program -> ModuleArtifact
generateModule prog =
    ModuleArtifact
        { artifactPath = case programModule prog of
            Just m -> moduleFileName (Prelude.fmap upperText (modulePath m))
            Nothing -> "main.R"
        , artifactBody = generateProgram prog
        , artifactExports = collectExports prog
        , artifactDeps = collectDeps prog
        }


-- | LANGUAGE.md section 14.6 file-name mapping: each segment lower-
-- cased, dots replaced by hyphens, suffix @.R@.
moduleFileName :: [Text] -> Prelude.FilePath
moduleFileName segs =
    "R/"
        Prelude.<> T.unpack
            (T.intercalate "-" (Prelude.fmap T.toLower segs))
        Prelude.<> ".R"


-- | Names that should appear in @NAMESPACE@.
--
-- A binding is exported if it has an @\@export@ tag in its doc block
-- AND appears in the module's @exporting (..)@ list (or the module
-- uses the wildcard form).
collectExports :: Program -> [Text]
collectExports prog =
    let
        wildcardExport = case programModule prog of
            Just m -> case Quone.Ast.Source.moduleExports m of
                ExportAll _ -> Prelude.True
                _ -> Prelude.False
            Nothing -> Prelude.True

        explicitExports = case programModule prog of
            Just m -> case Quone.Ast.Source.moduleExports m of
                ExportNames _ items ->
                    Prelude.fmap exportItemName items
                _ -> []
            Nothing -> []
    in
    [ lowerText (valueDeclName v)
    | DValue v <- programDecls prog
    , preserveDoc (valueDeclDoc v)
    , wildcardExport Prelude.|| (lowerText (valueDeclName v) `Prelude.elem` explicitExports)
    ]


-- | A doc block carries an @\@export@ tag if any line contains it.
preserveDoc :: Maybe DocBlock -> Prelude.Bool
preserveDoc = \case
    Nothing -> Prelude.False
    Just b -> Prelude.any (\l -> "@export" `T.isInfixOf` l) (docLines b)


-- | R packages this module needs at runtime, derived from its imports.
--
-- Per LANGUAGE.md section 13.9 the project-level dep set is the
-- union of:
--
-- * any @import pkg.fn@ (lowercase) declaration's package prefix;
-- * @dplyr@ if any dataframe verb is used;
-- * @purrr@ if any record update is used (lowers to @purrr::list_modify@).
collectDeps :: Program -> [Text]
collectDeps prog =
    let
        fromImports =
            [ T.intercalate "." (Prelude.fmap lowerText (foreignPackage f))
            | DImport (ForeignImport _ f _) <- programDecls prog
            , Prelude.not (Prelude.null (foreignPackage f))
            ]
        usesVerbs =
            Prelude.any (declUsesVerb) (programDecls prog)
        usesRecordUpdate =
            Prelude.any declUsesRecordUpdate (programDecls prog)
        verbDep = if usesVerbs then ["dplyr"] else []
        purrDep = if usesRecordUpdate then ["purrr"] else []
    in
    Prelude.foldr addUnique [] (fromImports Prelude.++ verbDep Prelude.++ purrDep)


addUnique :: Text -> [Text] -> [Text]
addUnique x xs
    | x `Prelude.elem` xs = xs
    | Prelude.otherwise = x : xs


declUsesVerb :: Decl -> Prelude.Bool
declUsesVerb = \case
    DValue v -> exprUsesVerb (valueDeclBody v)
    _ -> Prelude.False


exprUsesVerb :: Expr -> Prelude.Bool
exprUsesVerb = \case
    EVerb _ _ _ -> Prelude.True
    EApp _ a b -> exprUsesVerb a Prelude.|| exprUsesVerb b
    EBinOp _ _ a b -> exprUsesVerb a Prelude.|| exprUsesVerb b
    EUnary _ _ e -> exprUsesVerb e
    EPipe _ a b -> exprUsesVerb a Prelude.|| exprUsesVerb b
    EField _ e _ -> exprUsesVerb e
    ELambda _ _ b -> exprUsesVerb b
    ECase _ s arms ->
        exprUsesVerb s
            Prelude.|| Prelude.any (\a -> exprUsesVerb (caseArmBody a)) arms
    ELet _ binds b ->
        exprUsesVerb b
            Prelude.|| Prelude.any (\bb -> exprUsesVerb (bindingBody bb)) binds
    ERecord _ fs -> Prelude.any (\f -> exprUsesVerb (fieldBindingValue f)) fs
    ERecordUpdate _ t fs ->
        exprUsesVerb t
            Prelude.|| Prelude.any (\f -> exprUsesVerb (fieldBindingValue f)) fs
    EVector _ es -> Prelude.any exprUsesVerb es
    EDataframe _ fs -> Prelude.any (\f -> exprUsesVerb (fieldBindingValue f)) fs
    _ -> Prelude.False


declUsesRecordUpdate :: Decl -> Prelude.Bool
declUsesRecordUpdate = \case
    DValue v -> exprUsesRecordUpdate (valueDeclBody v)
    _ -> Prelude.False


exprUsesRecordUpdate :: Expr -> Prelude.Bool
exprUsesRecordUpdate = \case
    ERecordUpdate _ _ _ -> Prelude.True
    EApp _ a b -> exprUsesRecordUpdate a Prelude.|| exprUsesRecordUpdate b
    EBinOp _ _ a b -> exprUsesRecordUpdate a Prelude.|| exprUsesRecordUpdate b
    EUnary _ _ e -> exprUsesRecordUpdate e
    EPipe _ a b -> exprUsesRecordUpdate a Prelude.|| exprUsesRecordUpdate b
    EField _ e _ -> exprUsesRecordUpdate e
    ELambda _ _ b -> exprUsesRecordUpdate b
    ECase _ s arms ->
        exprUsesRecordUpdate s
            Prelude.|| Prelude.any (\a -> exprUsesRecordUpdate (caseArmBody a)) arms
    ELet _ binds b ->
        exprUsesRecordUpdate b
            Prelude.|| Prelude.any (\bb -> exprUsesRecordUpdate (bindingBody bb)) binds
    ERecord _ fs -> Prelude.any (\f -> exprUsesRecordUpdate (fieldBindingValue f)) fs
    EVector _ es -> Prelude.any exprUsesRecordUpdate es
    EDataframe _ fs -> Prelude.any (\f -> exprUsesRecordUpdate (fieldBindingValue f)) fs
    _ -> Prelude.False
