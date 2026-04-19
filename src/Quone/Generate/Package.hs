{-| Whole-package R generation.

Implements LANGUAGE.md section 14.6 in full: produces a package
directory tree containing @DESCRIPTION@, @NAMESPACE@, and
@R/<module>.R@ files.

Per the spec, @roxygen2@ is the canonical @NAMESPACE@/@man@
generator. For v0.0.1 we bundle a deterministic minimal @NAMESPACE@
ourselves (it's just a list of @export(name)@ lines derived from
@\@export@ doc tags) and rely on @roxygen2::roxygenise@ at the user's
build site for @man/*.Rd@. The CLI's @quonec build --package@ command
invokes that for them in stage 10.

The package-wide collision check from LANGUAGE.md section 14.6 lives
here: two modules may not define functions with the same
'snake_case' name.

-}
module Quone.Generate.Package
    ( PackageArtifact (..)
    , generatePackage
    , generateDescription
    , generateNamespace
    , collisionCheck
    , PackageInputs (..)
    , defaultPackageInputs
    )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Diagnostic
    ( Category (Parse)
    , Diagnostic (..)
    , Severity (Error)
    )
import Quone.Generate.Module
    ( ModuleArtifact (..)
    , generateModule
    )
import Quone.Position (SourceSpan, emptySpan)
import Quone.Resolve.Project
    ( Dependency (..)
    , PackageMeta (..)
    , Project (..)
    )
import qualified Prelude



-- | All inputs needed to produce a package.
data PackageInputs = PackageInputs
    { packageProject :: Project
    , packageModules :: [Program]   -- one per .Q file
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Convenience: build inputs from just a project + module list.
defaultPackageInputs :: Project -> [Program] -> PackageInputs
defaultPackageInputs proj progs =
    PackageInputs {packageProject = proj, packageModules = progs}


-- | The artifact set ready to be written to a directory.
data PackageArtifact = PackageArtifact
    { paDescription :: Text
    , paNamespace :: Text
    , paModules :: [ModuleArtifact]
    , paDependencies :: [Text]
    }
    deriving (Prelude.Show, Prelude.Eq)



-- ---------------------------------------------------------------------
-- Generation
-- ---------------------------------------------------------------------


-- | Generate the full package artifact set.
--
-- Returns either a 'Diagnostic' (collision detected) or the
-- 'PackageArtifact' ready for the CLI to write to disk and pass to
-- @roxygen2@.
generatePackage
    :: PackageInputs
    -> Prelude.Either Diagnostic PackageArtifact
generatePackage inputs = do
    let
        artifacts = Prelude.fmap generateModule (packageModules inputs)
    case collisionCheck artifacts of
        Just diag -> Prelude.Left diag
        Nothing ->
            let
                allDeps = unionDeps (Prelude.fmap artifactDeps artifacts)
                projDeps =
                    Prelude.fmap depName
                        (projectDependencies (packageProject inputs))
                deps = unionDeps [allDeps, projDeps]
                description = generateDescription (packageProject inputs) deps
                namespace = generateNamespace artifacts
            in
            Prelude.Right
                PackageArtifact
                    { paDescription = description
                    , paNamespace = namespace
                    , paModules = artifacts
                    , paDependencies = deps
                    }


unionDeps :: [[Text]] -> [Text]
unionDeps = List.foldl' union' []
  where
    union' acc xs = Prelude.foldr addUnique acc xs
    addUnique x xs
        | x `Prelude.elem` xs = xs
        | Prelude.otherwise = x : xs



-- ---------------------------------------------------------------------
-- DESCRIPTION
-- ---------------------------------------------------------------------


generateDescription :: Project -> [Text] -> Text
generateDescription proj deps =
    let
        m = projectMeta proj
        lines_ =
            [ "Package: " Prelude.<> metaName m
            , "Version: " Prelude.<> metaVersion m
            ]
                Prelude.++ maybeLine "Title" (metaDescription m)
                Prelude.++ authorsLines (metaAuthors m)
                Prelude.++
                    [ "Encoding: UTF-8"
                    , "Roxygen: list(markdown = TRUE)"
                    , "RoxygenNote: 7.3.0"
                    ]
                Prelude.++ importsLines deps
    in
    T.unlines lines_


maybeLine :: Text -> Maybe Text -> [Text]
maybeLine _ Nothing = []
maybeLine key (Just v) = [key Prelude.<> ": " Prelude.<> v]


authorsLines :: [Text] -> [Text]
authorsLines [] = []
authorsLines xs =
    [ "Authors@R: c("
        Prelude.<> T.intercalate ", "
            (Prelude.fmap (\a -> "person(\"" Prelude.<> a Prelude.<> "\")") xs)
        Prelude.<> ")"
    ]


importsLines :: [Text] -> [Text]
importsLines [] = []
importsLines deps =
    ["Imports:" Prelude.<> "\n    " Prelude.<> T.intercalate ",\n    " deps]



-- ---------------------------------------------------------------------
-- NAMESPACE
-- ---------------------------------------------------------------------


-- | Deterministic minimal NAMESPACE: an @export()@ line for every
-- binding marked @\@export@ in any module's doc block.
--
-- Begins with the canonical @roxygen2@ sentinel
-- @# Generated by roxygen2: do not edit by hand@ so that running
-- @roxygen2::roxygenise(build/)@ at the user's site recognises the
-- file as roxygen-owned and overwrites it. Without that sentinel,
-- @roxygen2@ refuses to update @NAMESPACE@, leaving stale exports
-- after the first compile.
--
-- LANGUAGE.md section 14.6 makes @roxygen2@ the canonical
-- @NAMESPACE@/@man@ generator; this initial @NAMESPACE@ is just a
-- bootstrap so that script-mode consumers (and CI before
-- @roxygen2@ runs) can install the package.
generateNamespace :: [ModuleArtifact] -> Text
generateNamespace artifacts =
    let
        exported =
            List.sort
                (List.nub
                    (Prelude.concatMap artifactExports artifacts))
    in
    T.unlines
        ( "# Generated by roxygen2: do not edit by hand"
            : Prelude.fmap (\n -> "export(" Prelude.<> n Prelude.<> ")") exported
        )



-- ---------------------------------------------------------------------
-- Collision check (section 14.6)
-- ---------------------------------------------------------------------


-- | Flat-namespace check: no two modules may define the same
-- snake_case name. Returns the first colliding diagnostic.
collisionCheck :: [ModuleArtifact] -> Maybe Diagnostic
collisionCheck artifacts =
    let
        named =
            Prelude.concatMap
                (\a ->
                    Prelude.fmap
                        (\n -> (n, artifactPath a))
                        (artifactExports a)
                )
                artifacts

        grouped = List.foldl' step Map.empty named

        step m (n, path) =
            Map.insertWith (Prelude.++) n [path] m

        clashes = Map.filter (\paths -> Prelude.length paths Prelude.> 1) grouped
    in
    case Map.toList clashes of
        ((name, paths) : _) -> Just (collisionDiag name paths)
        [] -> Nothing


collisionDiag :: Text -> [Prelude.FilePath] -> Diagnostic
collisionDiag name paths =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = Parse
        , diagSpan = emptySpan
        , diagMessage =
            "name clash: "
                Prelude.<> T.pack (Prelude.show name)
                Prelude.<> " is exported by "
                Prelude.<> T.pack (Prelude.show (Prelude.length paths))
                Prelude.<> " modules ("
                Prelude.<> T.intercalate ", " (Prelude.fmap T.pack paths)
                Prelude.<> ")"
        , diagHint = Just "rename one of the bindings; package mode requires a flat R namespace per LANGUAGE.md section 14.6"
        }
