{-| Concrete syntax tree.

The CST mirrors the EBNF grammar in LANGUAGE2.md section 5.1 closely.
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
    , CForeignClassification (..)
    , CValueDecl (..)
    , CExternDecl (..)
    , CInfixDecl (..)
    , CPrefixDecl (..)
    , CFixity (..)
    , CExternClassification (..)
    , CExternBody (..)
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
    | -- | Prelude-only @extern@ declaration. Carries either an
      -- ordinary value binding (with classification, signature, and
      -- R-string body) or a primitive type declaration. User code is
      -- rejected for these by the parser.
      CDExtern CExternDecl
    | -- | Prelude-only @infix@ declaration.
      CDInfix CInfixDecl
    | -- | Prelude-only @prefix@ declaration (unary @-@).
      CDPrefix CPrefixDecl
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
    | -- | A foreign import @import [modifier] pkg.fn : Ty@. The modifier
      -- (LANGUAGE2.md section 4.5) declares the function's R-runtime
      -- 'CForeignClassification'; absent means 'CCOpaque'.
      CForeignImport SourceSpan CForeignClassification CForeignName CTypeSig
    deriving (Prelude.Show, Prelude.Eq)


-- | Optional CST-level classification modifier on a foreign import.
data CForeignClassification
    = CCOpaque         -- ^ no modifier; the default
    | CCElementwise    -- ^ @import elementwise pkg.fn : ...@
    | CCReducer        -- ^ @import reducer pkg.fn : ...@
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
    , foreignNameAlias :: Maybe CLowerName
    -- ^ Optional `as <newname>` rename (M3.8). When present, the
    -- import binds the alias name in the local scope; the original
    -- function name is used only for the `pkg::fn` lowering.
    , foreignNameVia :: Maybe Text
    -- ^ Optional `via "<template>"` (M3.9). The template is the R
    -- call expression to emit at every call site, with @$1@, @$2@,
    -- ... substituted by the rendered Quone arguments in
    -- left-to-right order. When 'Nothing', the codegen emits a
    -- straight @pkg::fn(args...)@ call (with the M1 hardcoded
    -- purrr swap when applicable).
    }
    deriving (Prelude.Show, Prelude.Eq)


data CValueDecl = CValueDecl
    { valueDeclSpan :: SourceSpan
    , valueDeclAnnotation :: Maybe CTypeSig
    , valueDeclName :: CLowerName
    , valueDeclParams :: [CLowerName]
    , valueDeclBody :: CExpr
    , valueDeclDoc :: Maybe CDocBlock
    , valueDeclClassification :: CForeignClassification
    -- ^ Optional `elementwise` / `reducer` modifier on a Quone
    -- binding (M3.10). When present, the binding declares its
    -- own classification rather than relying on body inference.
    -- Defaults to `CCOpaque`, which means "infer from body".
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Prelude-only @extern@ declaration.
--
-- Two shapes:
--
-- * @extern [class] name : Sig = "<r>"@ — a value binding whose body
--   is a compiler-supplied R callable string. @class@ is one of
--   @elementwise@ or @reducer@; absent means 'CECOpaque'.
-- * @extern type Name [a b ...]@ — a primitive type with no source-
--   language constructors (introduced in Phase D).
data CExternDecl
    = -- | @extern [classification] name : sig = "rString" [{ dispatch_on = "var" }]@
      CExternValue
        { externValueSpan :: SourceSpan
        , externValueClassification :: CExternClassification
        , externValueName :: CLowerName
        , externValueSig :: CTypeSig
        , externValueBody :: CExternBody
        , externValueDoc :: Maybe CDocBlock
        }
    | -- | @extern type Name [a b ...]@ (Phase D).
      CExternType
        { externTypeSpan :: SourceSpan
        , externTypeName :: CUpperName
        , externTypeParams :: [CLowerName]
        , externTypeDoc :: Maybe CDocBlock
        }
    deriving (Prelude.Show, Prelude.Eq)


-- | The body of an @extern@ value binding.
data CExternBody
    = -- | Plain R callable: @= "<r>"@.
      CExternSimple SourceSpan Text
    | -- | @= "<r>" { dispatch_on = "<typevar>" }@. Used by @map@ and
      -- @map2@ to pick @purrr::map_*@ at codegen time.
      CExternDispatch SourceSpan Text Text
    deriving (Prelude.Show, Prelude.Eq)


-- | The classification annotation on an @extern@ value binding.
-- Mirrors 'CForeignClassification' but lives on extern declarations
-- so the prelude can declare @reducer mean@ and @elementwise sqrt@
-- without going through the foreign-import path.
data CExternClassification
    = CECOpaque
    | CECElementwise
    | CECReducer
    deriving (Prelude.Show, Prelude.Eq)


-- | Prelude-only @infix@ declaration.
--
-- @infix <assoc> <prec> (<op>) : <sig> = "<r>"@
data CInfixDecl = CInfixDecl
    { infixDeclSpan :: SourceSpan
    , infixDeclFixity :: CFixity
    , infixDeclPrec :: Int
    , infixDeclOp :: CBinOp
    , infixDeclSig :: CTypeSig
    , infixDeclConstraints :: [(CLowerName, Maybe CUpperName)]
    -- ^ Class constraints from a `forall n: Number, ...` prefix
    -- on this overload's signature (M3.11). Empty when no
    -- constraints were declared.
    , infixDeclR :: Text
    , infixDeclDoc :: Maybe CDocBlock
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Associativity of an @infix@ declaration.
data CFixity
    = CFLeft
    | CFRight
    | CFNon
    deriving (Prelude.Show, Prelude.Eq)


-- | Prelude-only @prefix@ declaration. Used for unary @-@.
--
-- @prefix <prec> (<op>) : <sig> = "<r>"@
data CPrefixDecl = CPrefixDecl
    { prefixDeclSpan :: SourceSpan
    , prefixDeclPrec :: Int
    , prefixDeclOp :: CUnaryOp
    , prefixDeclSig :: CTypeSig
    , prefixDeclConstraints :: [(CLowerName, Maybe CUpperName)]
    -- ^ Class constraints from a `forall n: Number, ...` prefix
    -- (M3.11). Empty when no constraints were declared.
    , prefixDeclR :: Text
    , prefixDeclDoc :: Maybe CDocBlock
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
    , caseArmGuard :: Maybe CExpr
    -- ^ M3.3: optional pattern guard between the pattern and the
    -- arrow.
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
    | CPVector SourceSpan [CPattern]
    | CPAs SourceSpan CLowerName CPattern
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
