{-| Abstract syntax tree.

This module defines the AST exactly as specified in LANGUAGE2.md
section 6, with the desugarings from section 5.3 already applied. In
particular:

* 'Expr' has no @EIf@ constructor: @if e1 then e2 else e3@ is
  represented as 'ECase' on the built-in 'Logical' type;
* 'Expr' has no @EParen@ constructor: parentheses are absorbed into
  the surrounding expression's structure during desugaring.

Every node carries a 'SourceSpan' so diagnostics from later phases
(typing, lowering) can still point at the original source range.

The 'Verb' enum lists every reserved dataframe verb from
LANGUAGE2.md section 3.4, even those whose typing rules are still
@[planned]@; the parser already accepts them and the AST tracks the
shape so future revisions can add typing without breaking the AST.

-}
module Quone.Ast.Source
    ( -- * Programs and modules
      Program (..)
    , ModuleDecl (..)
    , ModulePath
    , ExportList (..)
    , ExportItem (..)
    , exportItemName
      -- * Declarations
    , Decl (..)
    , TypeDecl (..)
    , Variant (..)
    , TypeAliasDecl (..)
    , ImportDecl (..)
    , ImportSelection (..)
    , ForeignName (..)
    , foreignBindName
    , ForeignClassification (..)
    , ValueDecl (..)
    , ExternDecl (..)
    , ExternBody (..)
    , ExternClassification (..)
    , InfixDecl (..)
    , PrefixDecl (..)
    , Fixity (..)
      -- * Types
    , TypeSig (..)
    , TypeAtom (..)
    , RecordType (..)
    , FieldType (..)
      -- * Expressions
    , Expr (..)
    , Literal (..)
    , BinOp (..)
    , UnaryOp (..)
    , CaseArm (..)
    , Binding (..)
    , FieldBinding (..)
    , Verb (..)
    , DplyrArg (..)
    , Modifier (..)
    , JoinPair (..)
      -- * Patterns
    , Pattern (..)
    , RecordPatField (..)
      -- * Identifiers
    , LowerName (..)
    , UpperName (..)
    , DocBlock (..)
      -- * Span helpers
    , exprSpan
    , patternSpan
    , declSpan
    )
where

import NriPrelude
import Quone.Position (SourceSpan)
import qualified Prelude


-- ---------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------


data LowerName = LowerName
    { lowerSpan :: SourceSpan
    , lowerText :: Text
    }
    deriving (Prelude.Show, Prelude.Eq)


data UpperName = UpperName
    { upperSpan :: SourceSpan
    , upperText :: Text
    }
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


-- ---------------------------------------------------------------------
-- Doc blocks
-- ---------------------------------------------------------------------


data DocBlock = DocBlock
    { docSpan :: SourceSpan
    , docLines :: [Text]
    }
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Programs and modules
-- ---------------------------------------------------------------------


data Program = Program
    { programSpan :: SourceSpan
    , programModule :: Maybe ModuleDecl
    , programDecls :: [Decl]
    , -- | True for the embedded prelude module loaded by
      -- 'Quone.Prelude.Load.loadPrelude'. The implicit-import injector
      -- and the @extern@/@infix@ parser checks both consult this flag
      -- so user code cannot use prelude-only syntax.
      programIsPrelude :: Prelude.Bool
    }
    deriving (Prelude.Show, Prelude.Eq)


data ModuleDecl = ModuleDecl
    { moduleSpan :: SourceSpan
    , modulePath :: ModulePath
    , moduleExports :: ExportList
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Non-empty in valid programs (the parser guarantees at least one
-- segment). We don't reach for NonEmpty here because the rest of the
-- compiler treats it as a list.
type ModulePath = [UpperName]


data ExportList
    = ExportAll SourceSpan
    | ExportNames SourceSpan [ExportItem]
    deriving (Prelude.Show, Prelude.Eq)


data ExportItem
    = ExportLower LowerName
    | ExportUpper UpperName
    deriving (Prelude.Show, Prelude.Eq)


exportItemName :: ExportItem -> Text
exportItemName = \case
    ExportLower n -> lowerText n
    ExportUpper n -> upperText n


-- ---------------------------------------------------------------------
-- Declarations
-- ---------------------------------------------------------------------


data Decl
    = DType TypeDecl
    | DTypeAlias TypeAliasDecl
    | DImport ImportDecl
    | DValue ValueDecl
    | -- | Prelude-only @extern@ binding (value or primitive type).
      DExtern ExternDecl
    | -- | Prelude-only @infix@ operator overload.
      DInfix InfixDecl
    | -- | Prelude-only @prefix@ operator overload (unary @-@).
      DPrefix PrefixDecl
    deriving (Prelude.Show, Prelude.Eq)


declSpan :: Decl -> SourceSpan
declSpan = \case
    DType d -> typeDeclSpan d
    DTypeAlias d -> aliasDeclSpan d
    DImport d -> case d of
        QuoneImport sp _ _ -> sp
        ForeignImport sp _ _ _ -> sp
    DValue d -> valueDeclSpan d
    DExtern d -> case d of
        ExternValue sp _ _ _ _ _ -> sp
        ExternType sp _ _ _ -> sp
    DInfix d -> infixDeclSpan d
    DPrefix d -> prefixDeclSpan d


data TypeDecl = TypeDecl
    { typeDeclSpan :: SourceSpan
    , typeDeclName :: UpperName
    , typeDeclParams :: [LowerName]
    , typeDeclVariants :: [Variant]   -- non-empty in valid programs
    , typeDeclDoc :: Maybe DocBlock
    }
    deriving (Prelude.Show, Prelude.Eq)


data Variant = Variant
    { variantSpan :: SourceSpan
    , variantName :: UpperName
    , variantArgs :: [TypeAtom]
    }
    deriving (Prelude.Show, Prelude.Eq)


data TypeAliasDecl = TypeAliasDecl
    { aliasDeclSpan :: SourceSpan
    , aliasDeclName :: UpperName
    , aliasDeclParams :: [LowerName]
    , aliasDeclBody :: TypeSig
    , aliasDeclDoc :: Maybe DocBlock
    }
    deriving (Prelude.Show, Prelude.Eq)


data ImportDecl
    = QuoneImport SourceSpan ModulePath ImportSelection
    | -- | A foreign-import declaration. The 'ForeignClassification'
      -- carries the (optional) @elementwise@/@reducer@ modifier from
      -- LANGUAGE2.md section 4.5; it determines whether the imported
      -- name may appear in a dataframe verb right-hand side.
      ForeignImport SourceSpan ForeignClassification ForeignName TypeSig
    deriving (Prelude.Show, Prelude.Eq)


-- | The R-runtime classification declared for a foreign import.
-- @Opaque@ is the default when no modifier is given.
data ForeignClassification
    = FCOpaque
    | FCElementwise
    | FCReducer
    deriving (Prelude.Show, Prelude.Eq)


data ImportSelection
    = ImportSingle ExportItem
    | ImportNames [ExportItem]
    | ImportAll
    deriving (Prelude.Show, Prelude.Eq)


data ForeignName = ForeignName
    { foreignSpan :: SourceSpan
    , foreignPackage :: [LowerName]   -- empty list = base R
    , foreignFn :: LowerName
    , foreignAlias :: Maybe LowerName
    -- ^ Optional `as <newname>` rename (M3.8). When present, the
    -- import binds the alias in the local scope; the underlying
    -- @pkg::fn@ qualifier in the generated R uses 'foreignFn'.
    , foreignVia :: Maybe Text
    -- ^ Optional `via "<template>"` (M3.9). The template is the
    -- raw R call to emit at every call site, with @$1@, @$2@, ...
    -- substituted by the rendered Quone arguments. Subsumes M1's
    -- hardcoded purrr argument swap.
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | The name a foreign import binds in the local scope. Returns the
-- alias if one was given (`import pkg.fn as my_fn`), otherwise the
-- function's own name.
foreignBindName :: ForeignName -> LowerName
foreignBindName f = case foreignAlias f of
    Just alias -> alias
    Nothing -> foreignFn f


data ValueDecl = ValueDecl
    { valueDeclSpan :: SourceSpan
    , valueDeclAnnotation :: Maybe TypeSig
    , valueDeclName :: LowerName
    , valueDeclParams :: [LowerName]
    , valueDeclBody :: Expr
    , valueDeclDoc :: Maybe DocBlock
    , valueDeclClassification :: ForeignClassification
    -- ^ Optional `elementwise` / `reducer` modifier on a Quone
    -- binding (M3.10). When present, this overrides the
    -- body-inference classification used by `mutate` / `summarize`
    -- right-hand side checks. Defaults to `FCOpaque` (= "infer from
    -- body"), preserving initial release behaviour for un-annotated bindings.
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Prelude-only @extern@ declaration (LANGUAGE2.md sections 8.2 and
-- 10). Either a value binding with a compiler-supplied R callable, or
-- a primitive type with no source-language constructors.
data ExternDecl
    = -- | @extern [classification] name : sig = "rString" [{ dispatch_on = "var" }]@
      ExternValue
        SourceSpan
        ExternClassification
        LowerName
        TypeSig
        ExternBody
        (Maybe DocBlock)
    | -- | @extern type Name [a b ...]@. Wired in Phase D.
      ExternType
        SourceSpan
        UpperName
        [LowerName]
        (Maybe DocBlock)
    deriving (Prelude.Show, Prelude.Eq)


-- | The body of an @extern@ value binding.
data ExternBody
    = -- | Plain R callable: @= "<r>"@.
      ExternSimple SourceSpan Text
    | -- | @= "<r>" { dispatch_on = "<typevar>" }@. Used by @map@ /
      -- @map2@ to pick @purrr::map_*@ at codegen time.
      ExternDispatch SourceSpan Text Text
    deriving (Prelude.Show, Prelude.Eq)


-- | Classification annotation on an @extern@ value binding. Mirrors
-- 'ForeignClassification' but lives separately so the prelude is
-- explicit about what it declares.
data ExternClassification
    = ECOpaque
    | ECElementwise
    | ECReducer
    deriving (Prelude.Show, Prelude.Eq)


-- | Prelude-only @infix@ declaration: one overload of a binary
-- operator. Multiple declarations of the same operator with different
-- signatures stack into the dispatch table consulted by the type
-- checker (see Phase B).
data InfixDecl = InfixDecl
    { infixDeclSpan :: SourceSpan
    , infixDeclFixity :: Fixity
    , infixDeclPrec :: Int
    , infixDeclOp :: BinOp
    , infixDeclSig :: TypeSig
    , infixDeclConstraints :: [(LowerName, Maybe UpperName)]
    -- ^ Per-binder class constraints from a `forall n: Number, ...`
    -- prefix on this overload's signature (M3.11). Used by the type
    -- checker to instantiate the operator's TyVars with the right
    -- 'TyVarConstraint'.
    , infixDeclR :: Text
    , infixDeclDoc :: Maybe DocBlock
    }
    deriving (Prelude.Show, Prelude.Eq)


data Fixity
    = FLeft
    | FRight
    | FNon
    deriving (Prelude.Show, Prelude.Eq)


-- | Prelude-only @prefix@ declaration: one overload of a unary
-- operator (currently only unary @-@).
data PrefixDecl = PrefixDecl
    { prefixDeclSpan :: SourceSpan
    , prefixDeclPrec :: Int
    , prefixDeclOp :: UnaryOp
    , prefixDeclSig :: TypeSig
    , prefixDeclConstraints :: [(LowerName, Maybe UpperName)]
    -- ^ Per-binder class constraints (M3.11). Empty if no
    -- @forall n: Number.@ prefix was given.
    , prefixDeclR :: Text
    , prefixDeclDoc :: Maybe DocBlock
    }
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------


data TypeSig
    = TFun SourceSpan TypeSig TypeSig    -- a -> b
    | TApp SourceSpan TypeAtom [TypeAtom]
    | TAtom TypeAtom
    deriving (Prelude.Show, Prelude.Eq)


data TypeAtom
    = TName UpperName
    | TVar LowerName
    | TParen SourceSpan TypeSig
    | TRecord RecordType
    | TDataframe SourceSpan RecordType
    deriving (Prelude.Show, Prelude.Eq)


data RecordType = RecordType
    { recordTypeSpan :: SourceSpan
    , recordTypeFields :: [FieldType]
    }
    deriving (Prelude.Show, Prelude.Eq)


data FieldType = FieldType
    { fieldTypeSpan :: SourceSpan
    , fieldTypeName :: LowerName
    , fieldTypeSig :: TypeSig
    }
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------


data Expr
    = ELit SourceSpan Literal
    | EVar LowerName
    | ECon UpperName
    | ELambda SourceSpan [LowerName] Expr
    | ECase SourceSpan Expr [CaseArm]
    | ELet SourceSpan [Binding] Expr
    | EApp SourceSpan Expr Expr
    | EBinOp SourceSpan BinOp Expr Expr
    | EUnary SourceSpan UnaryOp Expr
    | EPipe SourceSpan Expr Expr
    | EField SourceSpan Expr LowerName
    | ERecord SourceSpan [FieldBinding]
    | ERecordUpdate SourceSpan Expr [FieldBinding]
    | EVector SourceSpan [Expr]
    | EDataframe SourceSpan [FieldBinding]
    | EVerb SourceSpan Verb [DplyrArg]
    deriving (Prelude.Show, Prelude.Eq)


exprSpan :: Expr -> SourceSpan
exprSpan = \case
    ELit sp _ -> sp
    EVar n -> lowerSpan n
    ECon n -> upperSpan n
    ELambda sp _ _ -> sp
    ECase sp _ _ -> sp
    ELet sp _ _ -> sp
    EApp sp _ _ -> sp
    EBinOp sp _ _ _ -> sp
    EUnary sp _ _ -> sp
    EPipe sp _ _ -> sp
    EField sp _ _ -> sp
    ERecord sp _ -> sp
    ERecordUpdate sp _ _ -> sp
    EVector sp _ -> sp
    EDataframe sp _ -> sp
    EVerb sp _ _ -> sp


data Literal
    = LInt Int
    | LDouble Prelude.Double
    | LChar Text
    deriving (Prelude.Show, Prelude.Eq)


data BinOp
    = OpAdd | OpSub | OpMul | OpDiv | OpIntDiv | OpMod | OpExp
    | OpEq | OpNeq | OpGt | OpLt | OpGe | OpLe
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


data UnaryOp
    = OpNeg
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)


data CaseArm = CaseArm
    { caseArmSpan :: SourceSpan
    , caseArmPattern :: Pattern
    , caseArmGuard :: Maybe Expr
    -- ^ Optional pattern guard (M3.3): @Just n | n > 0 -> ...@.
    -- The guard MUST type as 'Logical'; the arm only fires if the
    -- pattern matches AND the guard evaluates to 'True'. A 'Nothing'
    -- guard means the arm fires whenever the pattern matches.
    , caseArmBody :: Expr
    }
    deriving (Prelude.Show, Prelude.Eq)


data Binding = Binding
    { bindingSpan :: SourceSpan
    , bindingName :: LowerName
    , bindingBody :: Expr
    }
    deriving (Prelude.Show, Prelude.Eq)


data FieldBinding = FieldBinding
    { fieldBindingSpan :: SourceSpan
    , fieldBindingName :: LowerName
    , fieldBindingValue :: Expr
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Initial-release dataframe verbs.
data Verb
    = VSelect | VFilter | VMutate | VSummarize | VGroupBy | VUngroup
    | VArrange | VRename
    | VLeftJoin | VRightJoin | VInnerJoin
    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded)


data DplyrArg
    = DAExpr Expr
    | DARecord SourceSpan [FieldBinding]
    | DAModifier Modifier
    | DAJoinOn SourceSpan Expr [JoinPair]
    deriving (Prelude.Show, Prelude.Eq)


data Modifier
    = MDesc SourceSpan LowerName
    | MAsc SourceSpan LowerName
    | MAs SourceSpan Text
    | MWhere SourceSpan Expr
    | MCols SourceSpan [LowerName]
    deriving (Prelude.Show, Prelude.Eq)


data JoinPair = JoinPair
    { joinPairSpan :: SourceSpan
    , joinPairLeft :: LowerName
    , joinPairRight :: LowerName
    }
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------


data Pattern
    = PWildcard SourceSpan
    | PVar LowerName
    | PLit SourceSpan Literal
    | PCon SourceSpan UpperName [Pattern]
    | PRecord SourceSpan [RecordPatField]
    deriving (Prelude.Show, Prelude.Eq)


patternSpan :: Pattern -> SourceSpan
patternSpan = \case
    PWildcard sp -> sp
    PVar n -> lowerSpan n
    PLit sp _ -> sp
    PCon sp _ _ -> sp
    PRecord sp _ -> sp


data RecordPatField
    = RpfShort LowerName
    | RpfFull SourceSpan LowerName Pattern
    deriving (Prelude.Show, Prelude.Eq)
