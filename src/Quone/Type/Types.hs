{-| Type representation for the Hindley-Milner type checker.

Encodes the v0.0.1 surface from LANGUAGE.md sections 7 and 8:

* primitive types (@Integer@, @Double@, @Character@);
* the built-in 'Logical' custom type (section 7.5.1);
* function types (right-associative);
* type variables (with metavariable identity for unification);
* type applications (e.g. @Vector Double@, @Maybe Integer@);
* record types (closed for v0.0.1; openness is `[planned]` per
  section 7.6);
* dataframe types (a tagged record-like with vector fields).

Type schemes are the polymorphic generalisations stored at
top-level binding sites (and at @let@ bindings per section 8.3).

-}
module Quone.Type.Types
    ( -- * Types
      Type (..)
    , TyVar (..)
    , primInteger
    , primDouble
    , primCharacter
    , primLogical
    , isPrimComparable
    , isPrimNumeric
      -- * Type schemes
    , Scheme (..)
    , monoScheme
      -- * Constructor info
    , ConstructorInfo (..)
      -- * Substitutions
    , Subst
    , emptySubst
    , (@@)
    , applySubst
    , applySubstScheme
    , freeTypeVars
    , freeTypeVarsScheme
      -- * Pretty
    , showType
    , showScheme
    )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import NriPrelude
import qualified Prelude



-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------


-- | Type variables are identified by an 'Int' so unification can
-- compare cheaply. The original surface name (if any) is preserved for
-- diagnostics.
data TyVar = TyVar
    { tyVarId :: Int
    , tyVarName :: Text
    }
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- | The unification-friendly representation of a Quone type.
--
-- The 'TyDataframe' constructor is kept distinct from 'TyRecord' so
-- the dataframe-rejection rule from LANGUAGE.md section 8.5 (record
-- update on a dataframe) can pattern-match without inspecting fields.
data Type
    = TyVarT TyVar
    | TyCon Text                          -- nullary type constructor
    | TyApp Type Type                     -- left-associative application
    | TyFun Type Type                     -- a -> b
    | TyRecord (Map.Map Text Type)        -- closed for v0.0.1
    | TyDataframe (Map.Map Text Type)
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


primInteger, primDouble, primCharacter, primLogical :: Type
primInteger = TyCon "Integer"
primDouble = TyCon "Double"
primCharacter = TyCon "Character"
primLogical = TyCon "Logical"


-- | Per LANGUAGE.md section 8.8, comparison operators accept any of
-- the four built-in scalar types.
isPrimComparable :: Type -> Prelude.Bool
isPrimComparable t =
    t Prelude.== primInteger
        Prelude.|| t Prelude.== primDouble
        Prelude.|| t Prelude.== primCharacter
        Prelude.|| t Prelude.== primLogical


-- | Convenience for the @+@/@-@/@*@/@/@ rules.
isPrimNumeric :: Type -> Prelude.Bool
isPrimNumeric t = t Prelude.== primInteger Prelude.|| t Prelude.== primDouble



-- ---------------------------------------------------------------------
-- Schemes
-- ---------------------------------------------------------------------


-- | A type scheme is a (possibly empty) set of universally-quantified
-- variables together with a body. Used at top-level bindings and at
-- generalised let positions per LANGUAGE.md section 8.3.
data Scheme = Scheme
    { schemeVars :: [TyVar]
    , schemeBody :: Type
    }
    deriving (Prelude.Show, Prelude.Eq)


monoScheme :: Type -> Scheme
monoScheme t = Scheme {schemeVars = [], schemeBody = t}



-- ---------------------------------------------------------------------
-- Constructor info
-- ---------------------------------------------------------------------


-- | Information the type checker needs about each in-scope
-- constructor: the parent type's name (for arity checks and pattern
-- typing) and the constructor's argument types.
data ConstructorInfo = ConstructorInfo
    { ciTypeName :: Text
    , ciTypeParams :: [TyVar]
    , ciArgTypes :: [Type]
    , ciResultType :: Type
    }
    deriving (Prelude.Show, Prelude.Eq)



-- ---------------------------------------------------------------------
-- Substitutions
-- ---------------------------------------------------------------------


-- | A substitution maps type variables to types.
type Subst = Map.Map TyVar Type


emptySubst :: Subst
emptySubst = Map.empty


-- | Compose two substitutions. The right one is applied first.
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = Map.union (Map.map (applySubst s1) s2) s1


infixr 6 @@


-- | Apply a substitution to a type.
applySubst :: Subst -> Type -> Type
applySubst s = \case
    t@(TyVarT v) -> Map.findWithDefault t v s
    t@(TyCon _) -> t
    TyApp f x -> TyApp (applySubst s f) (applySubst s x)
    TyFun a b -> TyFun (applySubst s a) (applySubst s b)
    TyRecord fs -> TyRecord (Map.map (applySubst s) fs)
    TyDataframe fs -> TyDataframe (Map.map (applySubst s) fs)


-- | Apply a substitution to a scheme. Quantified variables shadow.
applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s sch =
    let
        s' = Prelude.foldr Map.delete s (schemeVars sch)
    in
    sch {schemeBody = applySubst s' (schemeBody sch)}


-- | Free type variables of a type.
freeTypeVars :: Type -> Set.Set TyVar
freeTypeVars = \case
    TyVarT v -> Set.singleton v
    TyCon _ -> Set.empty
    TyApp f x -> Set.union (freeTypeVars f) (freeTypeVars x)
    TyFun a b -> Set.union (freeTypeVars a) (freeTypeVars b)
    TyRecord fs ->
        List.foldl' Set.union Set.empty (Prelude.fmap freeTypeVars (Map.elems fs))
    TyDataframe fs ->
        List.foldl' Set.union Set.empty (Prelude.fmap freeTypeVars (Map.elems fs))


-- | Free type variables of a scheme = body's FTV minus quantified.
freeTypeVarsScheme :: Scheme -> Set.Set TyVar
freeTypeVarsScheme sch =
    Set.difference (freeTypeVars (schemeBody sch)) (Set.fromList (schemeVars sch))



-- ---------------------------------------------------------------------
-- Pretty-printing for diagnostics
-- ---------------------------------------------------------------------


showType :: Type -> Text
showType = renderType 0


renderType :: Int -> Type -> Text
renderType prec = \case
    TyVarT v -> tyVarName v
    TyCon name -> name
    TyApp f x ->
        let
            inner = renderType 1 f Prelude.<> " " Prelude.<> renderType 2 x
        in
        if prec Prelude.>= 2 then "(" Prelude.<> inner Prelude.<> ")" else inner
    TyFun a b ->
        let
            inner = renderType 1 a Prelude.<> " -> " Prelude.<> renderType 0 b
        in
        if prec Prelude.>= 1 then "(" Prelude.<> inner Prelude.<> ")" else inner
    TyRecord fs ->
        "{ "
            Prelude.<> T.intercalate ", " (Prelude.fmap renderField (Map.toList fs))
            Prelude.<> " }"
    TyDataframe fs ->
        "dataframe { "
            Prelude.<> T.intercalate ", " (Prelude.fmap renderField (Map.toList fs))
            Prelude.<> " }"


renderField :: (Text, Type) -> Text
renderField (name, t) = name Prelude.<> " : " Prelude.<> renderType 0 t


showScheme :: Scheme -> Text
showScheme sch
    | Prelude.null (schemeVars sch) = showType (schemeBody sch)
    | Prelude.otherwise =
        "forall "
            Prelude.<> T.intercalate " " (Prelude.fmap tyVarName (schemeVars sch))
            Prelude.<> ". "
            Prelude.<> showType (schemeBody sch)
