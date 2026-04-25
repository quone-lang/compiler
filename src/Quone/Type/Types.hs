{-| Type representation for the Hindley-Milner type checker.

Encodes the initial release surface from LANGUAGE.md sections 7 and 8:

* primitive types (@Integer@, @Double@, @Character@);
* the built-in 'Logical' custom type (section 7.5.1);
* function types (right-associative);
* type variables (with metavariable identity for unification);
* type applications (e.g. @Vector Double@, @Maybe Integer@);
* record types (closed: a record literal MUST list every field of
  its type, and a record pattern MUST mention every field too —
  initial release design decision, see LANGUAGE.md section 7.6);
* dataframe types (a tagged record-like with vector fields).

Type schemes are the polymorphic generalisations stored at
top-level binding sites (and at @let@ bindings per section 8.3).

-}
module Quone.Type.Types
    ( -- * Types
      Type (..)
    , TyVar (..)
    , TyVarConstraint (..)
    , mkTyVar
    , mkTyVarC
    , satisfiesConstraint
    , combineConstraints
    , DataframeShape (..)
    , ungroupedDf
    , groupedDf
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
--
-- 'tyVarConstraint' carries an Elm-style class constraint (M3.11).
-- A 'NoConstraint' var unifies with anything; a 'NumberConstraint'
-- var only unifies with 'Integer', 'Double', or another
-- 'NumberConstraint' var. The constraint is preserved through
-- substitution so that an inferred type like
-- @forall n: Number. n -> n -> n@ rejects @"oops" + "hi"@ at the
-- call site.
data TyVar = TyVar
    { tyVarId :: Int
    , tyVarName :: Text
    , tyVarConstraint :: TyVarConstraint
    }
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- | Class constraint kinds for a 'TyVar'. Only 'Number' is defined
-- in initial release; future revisions can add e.g. @Comparable@, @Equatable@.
data TyVarConstraint
    = NoConstraint
    | NumberConstraint
    -- ^ The tyvar must instantiate to 'Integer' or 'Double' (or
    -- another 'NumberConstraint' tyvar).
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- | Build an unconstrained tyvar (the initial release default).
mkTyVar :: Int -> Text -> TyVar
mkTyVar i n = TyVar {tyVarId = i, tyVarName = n, tyVarConstraint = NoConstraint}


-- | Build a tyvar with the given constraint.
mkTyVarC :: Int -> Text -> TyVarConstraint -> TyVar
mkTyVarC i n c = TyVar {tyVarId = i, tyVarName = n, tyVarConstraint = c}


-- | True iff a concrete 'Type' satisfies a 'TyVarConstraint'. Used
-- by 'unify' when binding a constrained tyvar to a non-tyvar type.
satisfiesConstraint :: TyVarConstraint -> Type -> Prelude.Bool
satisfiesConstraint NoConstraint _ = Prelude.True
satisfiesConstraint NumberConstraint t =
    t Prelude.== primInteger Prelude.|| t Prelude.== primDouble


-- | Combine two constraints when unifying two constrained tyvars.
-- The result is the more restrictive of the two; if neither is
-- 'NoConstraint' and they disagree, returns 'Nothing' (incompatible).
combineConstraints
    :: TyVarConstraint
    -> TyVarConstraint
    -> Maybe TyVarConstraint
combineConstraints NoConstraint c = Just c
combineConstraints c NoConstraint = Just c
combineConstraints NumberConstraint NumberConstraint = Just NumberConstraint


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
    | TyRecord (Map.Map Text Type)        -- closed (initial release)
    | TyDataframe DataframeShape
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- | A dataframe's static shape: column schema plus optional
-- grouping-column metadata threaded through verb chains.
--
-- @dfGroupingCols@ holds the names introduced by a preceding
-- @group_by@ (in source order); @summarize@ consumes them
-- (preserving them in its output schema and emitting an ungrouped
-- result), @ungroup@ clears them, and other verbs preserve them.
-- An ungrouped dataframe has the empty list. The list (rather than
-- a set) keeps a deterministic 'Ord' instance for use as a 'Map' key.
data DataframeShape = DataframeShape
    { dfSchema :: Map.Map Text Type
    , dfGroupingCols :: [Text]
    }
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- | Build an ungrouped dataframe shape from a column schema.
ungroupedDf :: Map.Map Text Type -> DataframeShape
ungroupedDf cols = DataframeShape {dfSchema = cols, dfGroupingCols = []}


-- | Build a dataframe shape with the given schema and grouping list.
groupedDf :: Map.Map Text Type -> [Text] -> DataframeShape
groupedDf cols gs = DataframeShape {dfSchema = cols, dfGroupingCols = gs}


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
    TyDataframe shape ->
        TyDataframe shape {dfSchema = Map.map (applySubst s) (dfSchema shape)}


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
    TyDataframe shape ->
        List.foldl' Set.union Set.empty
            (Prelude.fmap freeTypeVars (Map.elems (dfSchema shape)))


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
    TyDataframe shape ->
        let
            schemaText =
                "dataframe { "
                    Prelude.<> T.intercalate ", "
                        (Prelude.fmap renderDataframeField (Map.toList (dfSchema shape)))
                    Prelude.<> " }"
            groupingText = case dfGroupingCols shape of
                [] -> ""
                gs -> " grouped by " Prelude.<> T.intercalate ", " gs
        in
        schemaText Prelude.<> groupingText


renderField :: (Text, Type) -> Text
renderField (name, t) = name Prelude.<> " : " Prelude.<> renderType 0 t


renderDataframeField :: (Text, Type) -> Text
renderDataframeField (name, t) =
    name Prelude.<> " : Vector " Prelude.<> renderType 2 t


showScheme :: Scheme -> Text
showScheme sch
    | Prelude.null (schemeVars sch) = showType (schemeBody sch)
    | Prelude.otherwise =
        "forall "
            Prelude.<> T.intercalate " " (Prelude.fmap tyVarName (schemeVars sch))
            Prelude.<> ". "
            Prelude.<> showType (schemeBody sch)
