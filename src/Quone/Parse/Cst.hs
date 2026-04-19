{-| Concrete syntax tree.

The CST mirrors the EBNF grammar in LANGUAGE.md section 5.1 closely.
It still contains 'IfExpr' and other surface-only constructs from
section 5.3 - those are removed by 'Quone.Parse.Desugar' on the way to
the abstract syntax tree.

Every node carries a 'SourceSpan' so diagnostics can point at the
exact source range. Identifier nodes also distinguish the case at the
type level (lowercase vs uppercase) to enforce the section 3.2 split
without runtime checks.

-}
module Quone.Parse.Cst
    ( -- * Programs and modules
      CProgram (..)
    , CModuleDecl (..)
    , CExportList (..)
    , CExportItem (..)
      -- * Declarations
    , CDecl (..)
    , CTypeDecl (..)
    , CVariant (..)
    , CTypeAliasDecl (..)
    , CImportDecl (..)
    , CImportSelection (..)
    , CForeignName (..)
    , CValueDecl (..)
      -- * Types
    , CTypeSig (..)
    , CTypeAtom (..)
    , CRecordType (..)
    , CFieldType (..)
      -- * Expressions
    , CExpr (..)
    , CBinOp (..)
    , CUnaryOp (..)
    , CCaseArm (..)
    , CBinding (..)
    , CFieldBinding (..)
    , CDplyrArg (..)
    , CModifier (..)
    , CJoinPair (..)
      -- * Patterns
    , CPattern (..)
    , CRecordPatField (..)
      -- * Literals
    , CLiteral (..)
      -- * Identifiers
    , CLowerName (..)
    , CUpperName (..)
    , CDocBlock (..)
      -- * Helpers
    , cprogramSpan
    )
where

import NriPrelude
import Quone.Lex.Token (Keyword)
import Quone.Position (SourceSpan, emptySpan, unionSpan)
import qualified Prelude


-- ---------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------


data CLowerName = CLowerName
    { lowerNameSpan :: SourceSpan
    , lowerNameText :: Text
    }
    deriving (Prelude.Show, Prelude.Eq)


data CUpperName = CUpperName
    { upperNameSpan :: SourceSpan
    , upperNameText :: Text
    }
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Doc blocks
-- ---------------------------------------------------------------------


data CDocBlock = CDocBlock
    { docBlockSpan :: SourceSpan
    , docBlockLines :: [Text]
    }
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Programs and modules
-- ---------------------------------------------------------------------


data CProgram = CProgram
    { programSpan :: SourceSpan
    , programModule :: Maybe CModuleDecl
    , programDecls :: [CDecl]
    }
    deriving (Prelude.Show, Prelude.Eq)


cprogramSpan :: CProgram -> SourceSpan
cprogramSpan = programSpan


data CModuleDecl = CModuleDecl
    { moduleDeclSpan :: SourceSpan
    , moduleDeclPath :: [CUpperName]   -- non-empty in valid programs
    , moduleDeclExports :: CExportList
    }
    deriving (Prelude.Show, Prelude.Eq)


data CExportList
    = CExportAll SourceSpan          -- exporting (..)
    | CExportNames SourceSpan [CExportItem]
    deriving (Prelude.Show, Prelude.Eq)


data CExportItem
    = CExportLower CLowerName
    | CExportUpper CUpperName
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Declarations
-- ---------------------------------------------------------------------


data CDecl
    = CDType CTypeDecl
    | CDTypeAlias CTypeAliasDecl
    | CDImport CImportDecl
    | CDValue CValueDecl
    deriving (Prelude.Show, Prelude.Eq)


data CTypeDecl = CTypeDecl
    { typeDeclSpan :: SourceSpan
    , typeDeclName :: CUpperName
    , typeDeclParams :: [CLowerName]
    , typeDeclVariants :: [CVariant]   -- non-empty in valid programs
    , typeDeclDoc :: Maybe CDocBlock
    }
    deriving (Prelude.Show, Prelude.Eq)


data CVariant = CVariant
    { variantSpan :: SourceSpan
    , variantName :: CUpperName
    , variantArgs :: [CTypeAtom]
    }
    deriving (Prelude.Show, Prelude.Eq)


data CTypeAliasDecl = CTypeAliasDecl
    { aliasDeclSpan :: SourceSpan
    , aliasDeclName :: CUpperName
    , aliasDeclParams :: [CLowerName]
    , aliasDeclBody :: CTypeSig
    , aliasDeclDoc :: Maybe CDocBlock
    }
    deriving (Prelude.Show, Prelude.Eq)


data CImportDecl
    = CQuoneImport SourceSpan [CUpperName] CImportSelection
    | CForeignImport SourceSpan CForeignName CTypeSig
    deriving (Prelude.Show, Prelude.Eq)


data CImportSelection
    = CImportSingle CExportItem
    | CImportNames [CExportItem]
    | CImportAll
    deriving (Prelude.Show, Prelude.Eq)


data CForeignName = CForeignName
    { foreignNameSpan :: SourceSpan
    , foreignNamePackage :: [CLowerName]
    , foreignNameFn :: CLowerName
    }
    deriving (Prelude.Show, Prelude.Eq)


data CValueDecl = CValueDecl
    { valueDeclSpan :: SourceSpan
    , valueDeclAnnotation :: Maybe CTypeSig
    , valueDeclName :: CLowerName
    , valueDeclParams :: [CLowerName]
    , valueDeclBody :: CExpr
    , valueDeclDoc :: Maybe CDocBlock
    }
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------


data CTypeSig
    = CTFun SourceSpan CTypeSig CTypeSig
    | CTApp SourceSpan CTypeAtom [CTypeAtom]
    | CTAtom CTypeAtom
    deriving (Prelude.Show, Prelude.Eq)


data CTypeAtom
    = CTName CUpperName
    | CTVar CLowerName
    | CTParen SourceSpan CTypeSig
    | CTRecord CRecordType
    | CTDataframe SourceSpan CRecordType
    deriving (Prelude.Show, Prelude.Eq)


data CRecordType = CRecordType
    { recordTypeSpan :: SourceSpan
    , recordTypeFields :: [CFieldType]
    }
    deriving (Prelude.Show, Prelude.Eq)


data CFieldType = CFieldType
    { fieldTypeSpan :: SourceSpan
    , fieldTypeName :: CLowerName
    , fieldTypeSig :: CTypeSig
    }
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------


data CExpr
    = CELit SourceSpan CLiteral
    | CEVar CLowerName
    | CECon CUpperName
    | CELambda SourceSpan [CLowerName] CExpr
    | CEIf SourceSpan CExpr CExpr CExpr           -- desugared by Desugar
    | CECase SourceSpan CExpr [CCaseArm]
    | CELet SourceSpan [CBinding] CExpr
    | CEApp SourceSpan CExpr CExpr                -- function application
    | CEBinOp SourceSpan CBinOp CExpr CExpr
    | CEUnary SourceSpan CUnaryOp CExpr
    | CEPipe SourceSpan CExpr CExpr               -- xs |> f
    | CEField SourceSpan CExpr CLowerName
    | CEParen SourceSpan CExpr
    | CERecord SourceSpan [CFieldBinding]
    | CERecordUpdate SourceSpan CExpr [CFieldBinding]
    | CEDataframe SourceSpan [CFieldBinding]
    | CEVector SourceSpan [CExpr]
    | CEVerb SourceSpan Keyword [CDplyrArg]
    deriving (Prelude.Show, Prelude.Eq)


data CBinOp
    = COpAdd | COpSub | COpMul | COpDiv | COpIntDiv | COpMod | COpExp
    | COpEq | COpNeq | COpGt | COpLt | COpGe | COpLe
    deriving (Prelude.Show, Prelude.Eq)


data CUnaryOp
    = COpNeg
    deriving (Prelude.Show, Prelude.Eq)


data CCaseArm = CCaseArm
    { caseArmSpan :: SourceSpan
    , caseArmPattern :: CPattern
    , caseArmBody :: CExpr
    }
    deriving (Prelude.Show, Prelude.Eq)


data CBinding = CBinding
    { bindingSpan :: SourceSpan
    , bindingName :: CLowerName
    , bindingBody :: CExpr
    }
    deriving (Prelude.Show, Prelude.Eq)


data CFieldBinding = CFieldBinding
    { fieldBindingSpan :: SourceSpan
    , fieldBindingName :: CLowerName
    , fieldBindingValue :: CExpr
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Arguments to dataframe verbs.
--
-- Mirrors the EBNF in section 5.1. The 'CDAJoinOn' shape covers the
-- right-hand-side of @left_join other on { lhs = rhs, ... }@.
data CDplyrArg
    = CDAExpr CExpr
    | CDARecord SourceSpan [CFieldBinding]
    | CDAModifier CModifier
    | CDAJoinOn SourceSpan CExpr [CJoinPair]
    deriving (Prelude.Show, Prelude.Eq)


data CModifier
    = CMDesc SourceSpan CLowerName
    | CMAsc SourceSpan CLowerName
    | CMAs SourceSpan Text
    | CMWhere SourceSpan CExpr
    | CMCols SourceSpan [CLowerName]
    deriving (Prelude.Show, Prelude.Eq)


data CJoinPair = CJoinPair
    { joinPairSpan :: SourceSpan
    , joinPairLeft :: CLowerName
    , joinPairRight :: CLowerName       -- same as left for the bare-name shorthand
    }
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------


data CPattern
    = CPWildcard SourceSpan
    | CPVar CLowerName
    | CPLit SourceSpan CLiteral
    | CPCon SourceSpan CUpperName [CPattern]
    | CPRecord SourceSpan [CRecordPatField]
    | CPParen SourceSpan CPattern
    deriving (Prelude.Show, Prelude.Eq)


data CRecordPatField
    = CRpfShort CLowerName
    | CRpfFull SourceSpan CLowerName CPattern
    deriving (Prelude.Show, Prelude.Eq)


-- ---------------------------------------------------------------------
-- Literals
-- ---------------------------------------------------------------------


data CLiteral
    = CLInt Int
    | CLDouble Prelude.Double
    | CLChar Text
    deriving (Prelude.Show, Prelude.Eq)
