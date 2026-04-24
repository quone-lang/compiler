{-| Initial typing environment.

Provides the prelude bindings listed in LANGUAGE2.md section 8.2 plus
the built-in 'Logical' constructors from section 7.5.1.

The full prelude module surface is `[planned]` per section 19.5; for
initial release we expose just enough for the type tests and small example
programs to run.

-}
module Quone.Type.Env
    ( Env (..)
    , initialEnv
    , insertValue
    , lookupValue
    , insertType
    , insertConstructor
    , lookupConstructor
      -- * Classification (LANGUAGE2.md sections 4.5, 8.7)
    , Classification (..)
    , insertClassification
    , lookupClassification
    , classificationMeet
      -- * Operator overloads (LANGUAGE2.md section 8.8)
    , OperatorOverload (..)
    , insertOperatorOverload
    , lookupOperatorOverloads
    , Fixity (..)
      -- * Unary-operator overloads
    , UnaryOverload (..)
    , insertUnaryOverload
    , lookupUnaryOverloads
    )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source (BinOp, Fixity (..), UnaryOp)
import Quone.Type.Types
import qualified Prelude



-- ---------------------------------------------------------------------
-- Environment
-- ---------------------------------------------------------------------


-- | The typing environment. Six maps:
--
--   * @envValues@: term-level bindings to their type schemes;
--   * @envTypes@: type-level names to their declared parameter count
--     (used to validate type applications);
--   * @envConstructors@: constructor name to 'ConstructorInfo';
--   * @envClassifications@: per-name R-runtime 'Classification', kept
--     parallel to @envValues@ so dataframe verbs can reject right-
--     hand sides that would silently break R's vectorisation rules
--     (LANGUAGE2.md sections 4.5, 8.7);
--   * @envOperatorOverloads@: per-'BinOp' list of typing rules read
--     from the prelude's @infix@ declarations
--     (LANGUAGE2.md section 8.8);
--   * @envUnaryOverloads@: per-'UnaryOp' list of typing rules read
--     from the prelude's @prefix@ declarations.
data Env = Env
    { envValues :: Map.Map Text Scheme
    , envTypes :: Map.Map Text Int
    , envConstructors :: Map.Map Text ConstructorInfo
    , envClassifications :: Map.Map Text Classification
    , envOperatorOverloads :: Map.Map BinOp [OperatorOverload]
    , envUnaryOverloads :: Map.Map UnaryOp [UnaryOverload]
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Static classification of a callable's R-runtime behaviour.
--
-- Used by the dataframe verb typer to decide whether a right-hand
-- side may be lowered into a verb (e.g. @mutate@ requires
-- elementwise; @summarize@ requires reducer-applied-to-column).
--
--   * 'Elementwise' — applying the function to each element of a
--     'Vector' produces a 'Vector' of results (R's natural
--     vectorisation, e.g. @sqrt@, arithmetic operators);
--   * 'Reducer'      — consumes a 'Vector' and produces a scalar
--     (e.g. @mean@, @sum@); valid in @summarize@;
--   * 'Opaque'       — unknown or non-vectorised behaviour; rejected
--     in any verb right-hand side.
data Classification
    = Opaque
    | Elementwise
    | Reducer
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- | One overload of a binary operator, declared by an @infix@
-- declaration in the prelude (LANGUAGE2.md section 8.8).
--
-- Each overload pins concrete operand and result types. The dispatch
-- algorithm in 'Quone.Type.Infer' tries the overloads in declaration
-- order and picks the first that unifies with the inferred operand
-- types.
data OperatorOverload = OperatorOverload
    { -- | Expected lhs type. May be polymorphic (in which case the
      -- type variables instantiate fresh on each use), but for initial release
      -- every operator overload pins concrete types.
      ooLhs :: Type
    , -- | Expected rhs type.
      ooRhs :: Type
    , -- | Result type.
      ooResult :: Type
    , -- | Source-spelling associativity from the prelude declaration.
      -- The parser resolves precedence and associativity statically;
      -- this field exists for future tooling (e.g. infix info in LSP
      -- hover).
      ooFixity :: Fixity
    , -- | Declared precedence. Same caveat as 'ooFixity'.
      ooPrec :: Int
    , -- | The R callable string the codegen lowers this overload to.
      -- All overloads of a given operator share the same string in
      -- initial release, but storing it per-overload keeps the door open for
      -- type-directed lowering (e.g. emitting @\`%/%\`@ vs @\`/\`@).
      ooR :: Text
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | One overload of a unary operator (currently only @-@). Same
-- shape as 'OperatorOverload' but with one operand instead of two.
data UnaryOverload = UnaryOverload
    { uoOperand :: Type
    , uoResult :: Type
    , uoPrec :: Int
    , uoR :: Text
    }
    deriving (Prelude.Show, Prelude.Eq)



-- ---------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------


-- | The initial typing environment, before the prelude is loaded.
--
-- Empty: every binding (operators, primitives, ADTs, named callables)
-- comes from @Prelude.Q@ via 'Quone.Prelude.Load.loadPrelude'. The
-- prelude loader itself starts from this empty env, then walks its
-- own declarations to populate every map.
--
-- Direct callers of 'inferProgramFrom' in tests / the REPL use
-- 'Quone.Prelude.Load.preludeEnv' as the seed instead.
initialEnv :: Env
initialEnv =
    Env
        { envValues = Map.empty
        , envTypes = Map.empty
        , envConstructors = Map.empty
        , envClassifications = Map.empty
        , envOperatorOverloads = Map.empty
        , envUnaryOverloads = Map.empty
        }



-- ---------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------


insertValue :: Text -> Scheme -> Env -> Env
insertValue name sch env =
    env {envValues = Map.insert name sch (envValues env)}


lookupValue :: Text -> Env -> Maybe Scheme
lookupValue name env = Map.lookup name (envValues env)


insertType :: Text -> Int -> Env -> Env
insertType name arity env =
    env {envTypes = Map.insert name arity (envTypes env)}


insertConstructor :: Text -> ConstructorInfo -> Env -> Env
insertConstructor name info env =
    env {envConstructors = Map.insert name info (envConstructors env)}


lookupConstructor :: Text -> Env -> Maybe ConstructorInfo
lookupConstructor name env = Map.lookup name (envConstructors env)


insertClassification :: Text -> Classification -> Env -> Env
insertClassification name c env =
    env {envClassifications = Map.insert name c (envClassifications env)}


-- | Classification for @name@; defaults to 'Opaque' if no entry has
-- been registered (per LANGUAGE2.md section 4.5: foreign imports
-- without an @elementwise@/@reducer@ modifier are opaque, and any
-- name the type checker has not classified is treated the same way).
lookupClassification :: Text -> Env -> Classification
lookupClassification name env =
    Map.findWithDefault Opaque name (envClassifications env)


-- | Most-restrictive classification. Used when combining the
-- classifications of an application's parts: @meet Elementwise
-- Reducer = Opaque@ (a reducer applied somewhere in an otherwise
-- elementwise expression is no longer purely elementwise from R's
-- vectorisation viewpoint).
classificationMeet :: Classification -> Classification -> Classification
classificationMeet a b
    | a Prelude.== b = a
    | Prelude.otherwise = Opaque


-- | Append an operator overload. New overloads land at the END of
-- the list so dispatch tries earlier-declared overloads first; this
-- matches Quone's convention that more-specific overloads (concrete
-- types) precede more-general ones (vector mixings) in the prelude
-- source.
insertOperatorOverload :: BinOp -> OperatorOverload -> Env -> Env
insertOperatorOverload op oo env =
    env
        { envOperatorOverloads =
            Map.insertWith
                (Prelude.flip (Prelude.++))
                op
                [oo]
                (envOperatorOverloads env)
        }


lookupOperatorOverloads :: BinOp -> Env -> [OperatorOverload]
lookupOperatorOverloads op env =
    Map.findWithDefault [] op (envOperatorOverloads env)


insertUnaryOverload :: UnaryOp -> UnaryOverload -> Env -> Env
insertUnaryOverload op uo env =
    env
        { envUnaryOverloads =
            Map.insertWith
                (Prelude.flip (Prelude.++))
                op
                [uo]
                (envUnaryOverloads env)
        }


lookupUnaryOverloads :: UnaryOp -> Env -> [UnaryOverload]
lookupUnaryOverloads op env =
    Map.findWithDefault [] op (envUnaryOverloads env)
