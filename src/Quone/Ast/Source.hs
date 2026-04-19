{-| Abstract syntax tree.

This module defines the AST exactly as specified in LANGUAGE.md
section 6, with the desugarings from section 5.3 already applied. In
particular:

* 'Expr' has no @EIf@ constructor: @if e1 then e2 else e3@ is
  represented as 'ECase' on the built-in 'Logical' type;
* 'Expr' has no @EParen@ constructor: parentheses are absorbed into
  the surrounding expression's structure during desugaring.

Every node carries a 'SourceSpan' so diagnostics from later phases
(typing, lowering) can still point at the original source range.

The 'Verb' enum lists every reserved dataframe verb from
LANGUAGE.md section 3.4, even those whose typing rules are still
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
    , ValueDecl (..)
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
    deriving (Prelude.Show, Prelude.Eq)


declSpan :: Decl -> SourceSpan
declSpan = \case
    DType d -> typeDeclSpan d
    DTypeAlias d -> aliasDeclSpan d
    DImport d -> case d of
        QuoneImport sp _ _ -> sp
        ForeignImport sp _ _ -> sp
    DValue d -> valueDeclSpan d


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
    | ForeignImport SourceSpan ForeignName TypeSig
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
    }
    deriving (Prelude.Show, Prelude.Eq)


data ValueDecl = ValueDecl
    { valueDeclSpan :: SourceSpan
    , valueDeclAnnotation :: Maybe TypeSig
    , valueDeclName :: LowerName
    , valueDeclParams :: [LowerName]
    , valueDeclBody :: Expr
    , valueDeclDoc :: Maybe DocBlock
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
    deriving (Prelude.Show, Prelude.Eq)


data UnaryOp
    = OpNeg
    deriving (Prelude.Show, Prelude.Eq)


data CaseArm = CaseArm
    { caseArmSpan :: SourceSpan
    , caseArmPattern :: Pattern
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


-- | Reserved dataframe verbs (LANGUAGE.md section 3.4).
data Verb
    = VSelect | VFilter | VMutate | VSummarize | VGroupBy | VUngroup
    | VArrange | VRename | VDistinct | VDistinctAll | VCount | VSlice
    | VPull | VRelocate | VTransmute | VMutateEach | VSummarizeEach
    | VLeftJoin | VRightJoin | VInnerJoin | VFullJoin | VAntiJoin
    | VSemiJoin | VCrossJoin
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
