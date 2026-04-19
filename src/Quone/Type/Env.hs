{-| Initial typing environment.

Provides the prelude bindings listed in LANGUAGE.md section 8.2 plus
the built-in 'Logical' constructors from section 7.5.1.

The full prelude module surface is `[planned]` per section 19.5; for
v0.0.1 we expose just enough for the type tests and small example
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
    , builtinTypes
    , builtinConstructors
    )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import Quone.Type.Types
import qualified Prelude



-- ---------------------------------------------------------------------
-- Environment
-- ---------------------------------------------------------------------


-- | The typing environment. Three maps:
--
--   * @envValues@: term-level bindings to their type schemes;
--   * @envTypes@: type-level names to their declared parameter count
--     (used to validate type applications);
--   * @envConstructors@: constructor name to 'ConstructorInfo'.
data Env = Env
    { envValues :: Map.Map Text Scheme
    , envTypes :: Map.Map Text Int
    , envConstructors :: Map.Map Text ConstructorInfo
    }
    deriving (Prelude.Show, Prelude.Eq)



-- ---------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------


initialEnv :: Env
initialEnv =
    Env
        { envValues = builtinValues
        , envTypes = builtinTypes
        , envConstructors = builtinConstructors
        }



-- ---------------------------------------------------------------------
-- Built-in types
-- ---------------------------------------------------------------------


-- | Built-in nullary type constructors plus parameterised ones.
builtinTypes :: Map.Map Text Int
builtinTypes =
    Map.fromList
        [ ("Integer", 0)
        , ("Double", 0)
        , ("Character", 0)
        , ("Logical", 0)
        , ("Vector", 1)
        , ("Maybe", 1)
        , ("Result", 2)
        ]



-- ---------------------------------------------------------------------
-- Built-in constructors
-- ---------------------------------------------------------------------


-- | True, False, Just, Nothing, Ok, Err.
--
-- @Just :: forall a. a -> Maybe a@ etc.
builtinConstructors :: Map.Map Text ConstructorInfo
builtinConstructors =
    Map.fromList
        [ ( "True"
          , ConstructorInfo
                { ciTypeName = "Logical"
                , ciTypeParams = []
                , ciArgTypes = []
                , ciResultType = primLogical
                }
          )
        , ( "False"
          , ConstructorInfo
                { ciTypeName = "Logical"
                , ciTypeParams = []
                , ciArgTypes = []
                , ciResultType = primLogical
                }
          )
        ,
            let
                a = TyVar 0 "a"
            in
            ( "Nothing"
            , ConstructorInfo
                { ciTypeName = "Maybe"
                , ciTypeParams = [a]
                , ciArgTypes = []
                , ciResultType = TyApp (TyCon "Maybe") (TyVarT a)
                }
            )
        ,
            let
                a = TyVar 0 "a"
            in
            ( "Just"
            , ConstructorInfo
                { ciTypeName = "Maybe"
                , ciTypeParams = [a]
                , ciArgTypes = [TyVarT a]
                , ciResultType = TyApp (TyCon "Maybe") (TyVarT a)
                }
            )
        ,
            let
                e_ = TyVar 0 "e"
                v = TyVar 1 "v"
            in
            ( "Ok"
            , ConstructorInfo
                { ciTypeName = "Result"
                , ciTypeParams = [e_, v]
                , ciArgTypes = [TyVarT v]
                , ciResultType = TyApp (TyApp (TyCon "Result") (TyVarT e_)) (TyVarT v)
                }
            )
        ,
            let
                e_ = TyVar 0 "e"
                v = TyVar 1 "v"
            in
            ( "Err"
            , ConstructorInfo
                { ciTypeName = "Result"
                , ciTypeParams = [e_, v]
                , ciArgTypes = [TyVarT e_]
                , ciResultType = TyApp (TyApp (TyCon "Result") (TyVarT e_)) (TyVarT v)
                }
            )
        ]



-- ---------------------------------------------------------------------
-- Built-in values
-- ---------------------------------------------------------------------


-- | Prelude bindings from LANGUAGE.md section 8.2.
--
-- We use a tiny set sufficient for tests; richer typing rules and
-- aggregate functions land in stage 7 alongside dataframe verb typing.
builtinValues :: Map.Map Text Scheme
builtinValues =
    let
        a = TyVar 100 "a"
        b = TyVar 101 "b"
    in
    Map.fromList
        [ -- map : (a -> b) -> Vector a -> Vector b
          ( "map"
          , Scheme
                { schemeVars = [a, b]
                , schemeBody =
                    TyFun
                        (TyFun (TyVarT a) (TyVarT b))
                        (TyFun
                            (TyApp (TyCon "Vector") (TyVarT a))
                            (TyApp (TyCon "Vector") (TyVarT b))
                        )
                }
          )
        , -- length : Vector a -> Integer
            let
                aa = TyVar 102 "a"
            in
            ( "length"
            , Scheme
                { schemeVars = [aa]
                , schemeBody =
                    TyFun
                        (TyApp (TyCon "Vector") (TyVarT aa))
                        primInteger
                }
            )
        , -- to_double : Integer -> Double
          ( "to_double"
          , monoScheme (TyFun primInteger primDouble)
          )
        , -- sqrt : Double -> Double
          ( "sqrt"
          , monoScheme (TyFun primDouble primDouble)
          )
        , -- mean : Vector Double -> Double
          ( "mean"
          , monoScheme
                ( TyFun
                    (TyApp (TyCon "Vector") primDouble)
                    primDouble
                )
          )
        , -- sum : Vector Double -> Double
          ( "sum"
          , monoScheme
                ( TyFun
                    (TyApp (TyCon "Vector") primDouble)
                    primDouble
                )
          )
        ]



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
