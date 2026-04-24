{-| Project model and `quone.toml` parsing.

A Quone project is:

* a directory containing a `quone.toml`;
* a `src/` subtree of @.Q@ files whose paths mirror their module
  paths (LANGUAGE.md section 14.6);
* zero or more named R-package dependencies (LANGUAGE.md section 13.9).

For initial release we keep the `quone.toml` schema small and parse it with a
hand-rolled key/value parser. A richer schema (with semver constraints,
optional sections, etc.) is `[planned]` per
[section 19.7](LANGUAGE.md#197-tooling).

The supported schema:

@
[package]
name = "stats"
version = "0.0.1"
description = "Score normalisation."
authors = ["Andrew McNally"]

[dependencies]
purrr = ">= 1.0"
dplyr = ">= 1.1"
@

Both blank lines and @#@ comments are accepted; values are parsed as
either strings (in double quotes) or arrays of strings.

-}
module Quone.Resolve.Project
    ( Project (..)
    , PackageMeta (..)
    , Dependency (..)
    , parseProjectToml
    , discoverModulePath
    , modulePathToFile
    , fileToModulePath
    )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import Quone.Diagnostic
    ( Category (Parse)
    , Diagnostic (..)
    , Severity (Error)
    )
import Quone.Position (SourcePos (..), SourceSpan (..), spanFromPos)
import qualified System.FilePath as FP
import qualified Prelude



-- ---------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------


-- | Top-level project metadata + dependency set.
data Project = Project
    { projectMeta :: PackageMeta
    , projectDependencies :: [Dependency]
    }
    deriving (Prelude.Show, Prelude.Eq)


data PackageMeta = PackageMeta
    { metaName :: Text
    , metaVersion :: Text
    , metaDescription :: Maybe Text
    , metaAuthors :: [Text]
    }
    deriving (Prelude.Show, Prelude.Eq)


data Dependency = Dependency
    { depName :: Text
    , depVersion :: Text
    }
    deriving (Prelude.Show, Prelude.Eq)



-- ---------------------------------------------------------------------
-- Module path discovery
-- ---------------------------------------------------------------------


-- | Map a module path to its expected file under @src/@.
--
-- @Stats.Transform@ ⇒ @src/Stats/Transform.Q@.
modulePathToFile :: [Text] -> Prelude.FilePath
modulePathToFile segs =
    "src" FP.</> List.foldr1 (FP.</>) (Prelude.fmap T.unpack segs) FP.<.> "Q"


-- | Recover a module path from a relative file path under @src/@.
fileToModulePath :: Prelude.FilePath -> Maybe [Text]
fileToModulePath path =
    case FP.splitDirectories (FP.dropExtension path) of
        ("src" : rest@(_ : _)) -> Just (Prelude.fmap T.pack rest)
        _ -> Nothing


-- | If the source file lives at a path of the form @src/.../Foo.Q@,
-- the discovered module path is @[..., "Foo"]@; otherwise 'Nothing'.
discoverModulePath :: Prelude.FilePath -> Maybe [Text]
discoverModulePath = fileToModulePath



-- ---------------------------------------------------------------------
-- TOML parser
-- ---------------------------------------------------------------------


-- | Parse a `quone.toml` source string.
--
-- Returns either a populated 'Project' or a diagnostic pointing at the
-- offending line. Unknown sections and unknown keys are ignored
-- (forward-compatible with future schema growth).
parseProjectToml :: Text -> Text -> Prelude.Either Diagnostic Project
parseProjectToml filename src =
    let
        ls = numbered (T.lines src)
        sections = collectSections ls
        pkgEntries = Map.findWithDefault [] "package" sections
        depEntries = Map.findWithDefault [] "dependencies" sections
    in
    case buildMeta pkgEntries of
        Prelude.Left (line, msg) -> Prelude.Left (toDiag filename line msg)
        Prelude.Right meta ->
            case buildDependencies depEntries of
                Prelude.Left (line, msg) -> Prelude.Left (toDiag filename line msg)
                Prelude.Right deps ->
                    Prelude.Right
                        (Project {projectMeta = meta, projectDependencies = deps})


toDiag :: Text -> Int -> Text -> Diagnostic
toDiag filename line msg =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = Parse
        , diagSpan = spanFromPos (SourcePos filename line 1)
        , diagMessage = msg
        , diagHint = Nothing
        }


numbered :: [Text] -> [(Int, Text)]
numbered = Prelude.zip [1 ..]



-- ---------------------------------------------------------------------
-- Section collection
-- ---------------------------------------------------------------------


type Section = Text
type Key = Text
type Value = Text


-- | Walk the line list once, building a map from section name to its
-- (line, key, raw-value) triples. Comments and blank lines are dropped.
collectSections
    :: [(Int, Text)]
    -> Map.Map Section [(Int, Key, Value)]
collectSections lns =
    Prelude.snd
        ( List.foldl'
            (\(currentSection, acc) (lineNo, raw) ->
                let
                    trimmed = T.strip (stripComment raw)
                in
                if T.null trimmed
                    then (currentSection, acc)
                    else case T.uncons trimmed of
                        Just ('[', _) -> case parseSectionHeader trimmed of
                            Just s -> (s, acc)
                            Nothing -> (currentSection, acc)
                        _ ->
                            case parseEntry trimmed of
                                Just (k, v) ->
                                    ( currentSection
                                    , Map.insertWith
                                        (Prelude.++)
                                        currentSection
                                        [(lineNo, k, v)]
                                        acc
                                    )
                                Nothing -> (currentSection, acc)
            )
            ("", Map.empty)
            lns
        )


stripComment :: Text -> Text
stripComment t =
    case T.findIndex (Prelude.== '#') t of
        Nothing -> t
        Just i -> T.take i t


-- | "[name]" (no nested tables for initial release).
parseSectionHeader :: Text -> Maybe Section
parseSectionHeader t =
    case (T.uncons t, T.unsnoc t) of
        (Just ('[', _), Just (_, ']')) ->
            Just (T.strip (T.drop 1 (T.init t)))
        _ -> Nothing


-- | "key = value" or "key=value".
parseEntry :: Text -> Maybe (Key, Value)
parseEntry t =
    case T.breakOn "=" t of
        (k, v) | Prelude.not (T.null v) ->
            Just (T.strip k, T.strip (T.drop 1 v))
        _ -> Nothing



-- ---------------------------------------------------------------------
-- Per-section interpretation
-- ---------------------------------------------------------------------


buildMeta
    :: [(Int, Key, Value)]
    -> Prelude.Either (Int, Text) PackageMeta
buildMeta entries =
    let
        get k = Prelude.fmap (\(_, _, v) -> v) (List.find (\(_, kk, _) -> kk Prelude.== k) entries)
        getLine k = Prelude.fmap (\(l, _, _) -> l) (List.find (\(_, kk, _) -> kk Prelude.== k) entries)
    in
    case (get "name", get "version") of
        (Just nameV, Just versionV) ->
            case (parseStringValue nameV, parseStringValue versionV) of
                (Just name, Just version) ->
                    Prelude.Right
                        ( PackageMeta
                            { metaName = name
                            , metaVersion = version
                            , metaDescription =
                                get "description" |> Prelude.maybe Nothing parseStringValue
                            , metaAuthors =
                                get "authors"
                                    |> Prelude.maybe [] parseArrayValue
                            }
                        )
                _ ->
                    Prelude.Left
                        ( Prelude.maybe 1 Prelude.id (getLine "name")
                        , "package.name and package.version must be quoted strings"
                        )
        (Nothing, _) ->
            Prelude.Left (1, "missing required key: package.name")
        (_, Nothing) ->
            Prelude.Left (1, "missing required key: package.version")


buildDependencies
    :: [(Int, Key, Value)]
    -> Prelude.Either (Int, Text) [Dependency]
buildDependencies entries =
    let
        toDep (lineNo, k, v) =
            case parseStringValue v of
                Just version ->
                    Prelude.Right (Dependency {depName = k, depVersion = version})
                Nothing ->
                    Prelude.Left (lineNo, "dependency value must be a quoted string")
    in
    Prelude.traverse toDep entries



-- ---------------------------------------------------------------------
-- Value parsing
-- ---------------------------------------------------------------------


-- | Strip the surrounding double quotes if both ends carry them.
parseStringValue :: Text -> Maybe Text
parseStringValue v =
    case (T.uncons v, T.unsnoc v) of
        (Just ('"', _), Just (_, '"')) ->
            Just (T.dropEnd 1 (T.drop 1 v))
        _ -> Nothing


-- | "[\"a\", \"b\"]" ⇒ ["a", "b"].
parseArrayValue :: Text -> [Text]
parseArrayValue v =
    case (T.uncons (T.strip v), T.unsnoc (T.strip v)) of
        (Just ('[', _), Just (_, ']')) ->
            let
                inner = T.dropEnd 1 (T.drop 1 (T.strip v))
                pieces = T.splitOn "," inner
            in
            Prelude.foldr
                (\p acc -> case parseStringValue (T.strip p) of
                    Just s -> s : acc
                    Nothing -> acc)
                []
                pieces
        _ -> []
