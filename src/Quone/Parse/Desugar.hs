{-| Desugar the CST into the AST.

Applies the surface-only desugarings from LANGUAGE.md section 5.3:

* @if e1 then e2 else e3@ ⇒ @case e1 of True -> e2; False -> e3@.

Also drops the explicit 'CEParen' wrapper (the AST records spans
directly so a paren node is redundant), and translates the parser's
'Keyword' verb identifiers to the typed 'Verb' constructors.

The desugar pass is total: any structural mismatch surfaces as a
'Diagnostic' so downstream phases never see invalid trees.

-}
module Quone.Parse.Desugar
    ( desugar
    , desugarFile
    , desugarSource
    )
where

import qualified Data.List as List
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Diagnostic
    ( Category (Internal)
    , Diagnostic (..)
    , Severity (Error)
    )
import Quone.Lex.Token (Keyword (..))
import qualified Quone.Parse.Cst as C
import Quone.Parse.Parser (parseProgram, parseProgramFile)
import Quone.Position (SourceSpan, emptySpan, unionSpan)
import qualified Prelude


-- | Lex, parse, and desugar a literal string of source.
desugarSource :: Text -> Prelude.Either Diagnostic Program
desugarSource src =
    case parseProgram src of
        Prelude.Left d -> Prelude.Left d
        Prelude.Right cst -> Prelude.Right (desugar cst)


-- | Lex, parse, and desugar a named file's contents.
desugarFile :: Text -> Text -> Prelude.Either Diagnostic Program
desugarFile filename src =
    case parseProgramFile filename src of
        Prelude.Left d -> Prelude.Left d
        Prelude.Right cst -> Prelude.Right (desugar cst)


-- ---------------------------------------------------------------------
-- Desugar CST -> AST
-- ---------------------------------------------------------------------


desugar :: C.CProgram -> Program
desugar (C.CProgram sp mModule decls) =
    Program
        { programSpan = sp
        , programModule = Prelude.fmap dModule mModule
        , programDecls = Prelude.fmap dDecl decls
        , -- The user-facing parser path always produces non-prelude
          -- programs. The prelude loader sets this flag explicitly
          -- after calling 'desugar'.
          programIsPrelude = Prelude.False
        }


dModule :: C.CModuleDecl -> ModuleDecl
dModule (C.CModuleDecl sp path exps) =
    ModuleDecl
        { moduleSpan = sp
        , modulePath = Prelude.fmap dUpper path
        , moduleExports = dExportList exps
        }


dExportList :: C.CExportList -> ExportList
dExportList = \case
    C.CExportAll sp -> ExportAll sp
    C.CExportNames sp items -> ExportNames sp (Prelude.fmap dExportItem items)


dExportItem :: C.CExportItem -> ExportItem
dExportItem = \case
    C.CExportLower n -> ExportLower (dLower n)
    C.CExportUpper n -> ExportUpper (dUpper n)


dLower :: C.CLowerName -> LowerName
dLower (C.CLowerName sp t) = LowerName sp t


dUpper :: C.CUpperName -> UpperName
dUpper (C.CUpperName sp t) = UpperName sp t


dDocBlock :: C.CDocBlock -> DocBlock
dDocBlock (C.CDocBlock sp lns) = DocBlock sp lns


-- Decls --------------------------------------------------------------


dDecl :: C.CDecl -> Decl
dDecl = \case
    C.CDType d -> DType (dTypeDecl d)
    C.CDTypeAlias d -> DTypeAlias (dTypeAliasDecl d)
    C.CDImport d -> DImport (dImportDecl d)
    C.CDValue d -> DValue (dValueDecl d)
    C.CDExtern d -> DExtern (dExternDecl d)
    C.CDInfix d -> DInfix (dInfixDecl d)
    C.CDPrefix d -> DPrefix (dPrefixDecl d)


dTypeDecl :: C.CTypeDecl -> TypeDecl
dTypeDecl d =
    TypeDecl
        { typeDeclSpan = C.typeDeclSpan d
        , typeDeclName = dUpper (C.typeDeclName d)
        , typeDeclParams = Prelude.fmap dLower (C.typeDeclParams d)
        , typeDeclVariants = Prelude.fmap dVariant (C.typeDeclVariants d)
        , typeDeclDoc = Prelude.fmap dDocBlock (C.typeDeclDoc d)
        }


dVariant :: C.CVariant -> Variant
dVariant v =
    Variant
        { variantSpan = C.variantSpan v
        , variantName = dUpper (C.variantName v)
        , variantArgs = Prelude.fmap dTypeAtom (C.variantArgs v)
        }


dTypeAliasDecl :: C.CTypeAliasDecl -> TypeAliasDecl
dTypeAliasDecl d =
    TypeAliasDecl
        { aliasDeclSpan = C.aliasDeclSpan d
        , aliasDeclName = dUpper (C.aliasDeclName d)
        , aliasDeclParams = Prelude.fmap dLower (C.aliasDeclParams d)
        , aliasDeclBody = dTypeSig (C.aliasDeclBody d)
        , aliasDeclDoc = Prelude.fmap dDocBlock (C.aliasDeclDoc d)
        }


dImportDecl :: C.CImportDecl -> ImportDecl
dImportDecl = \case
    C.CQuoneImport sp path sel ->
        QuoneImport sp (Prelude.fmap dUpper path) (dImportSelection sel)
    C.CForeignImport sp classification fname sig ->
        ForeignImport
            sp
            (dForeignClassification classification)
            (dForeignName fname)
            (dTypeSig sig)


dForeignClassification :: C.CForeignClassification -> ForeignClassification
dForeignClassification = \case
    C.CCOpaque -> FCOpaque
    C.CCElementwise -> FCElementwise
    C.CCReducer -> FCReducer


dImportSelection :: C.CImportSelection -> ImportSelection
dImportSelection = \case
    C.CImportSingle item -> ImportSingle (dExportItem item)
    C.CImportNames items -> ImportNames (Prelude.fmap dExportItem items)
    C.CImportAll -> ImportAll


dForeignName :: C.CForeignName -> ForeignName
dForeignName f =
    ForeignName
        { foreignSpan = C.foreignNameSpan f
        , foreignPackage = Prelude.fmap dLower (C.foreignNamePackage f)
        , foreignFn = dLower (C.foreignNameFn f)
        , foreignAlias = Prelude.fmap dLower (C.foreignNameAlias f)
        , foreignVia = C.foreignNameVia f
        }


dValueDecl :: C.CValueDecl -> ValueDecl
dValueDecl d =
    ValueDecl
        { valueDeclSpan = C.valueDeclSpan d
        , valueDeclAnnotation = Prelude.fmap dTypeSig (C.valueDeclAnnotation d)
        , valueDeclName = dLower (C.valueDeclName d)
        , valueDeclParams = Prelude.fmap dLower (C.valueDeclParams d)
        , valueDeclBody = dExpr (C.valueDeclBody d)
        , valueDeclDoc = Prelude.fmap dDocBlock (C.valueDeclDoc d)
        , valueDeclClassification =
            dForeignClassification (C.valueDeclClassification d)
        }


dExternDecl :: C.CExternDecl -> ExternDecl
dExternDecl = \case
    C.CExternValue sp classification name sig body mDoc ->
        ExternValue
            sp
            (dExternClassification classification)
            (dLower name)
            (dTypeSig sig)
            (dExternBody body)
            (Prelude.fmap dDocBlock mDoc)
    C.CExternType sp name params mDoc ->
        ExternType
            sp
            (dUpper name)
            (Prelude.fmap dLower params)
            (Prelude.fmap dDocBlock mDoc)


dExternBody :: C.CExternBody -> ExternBody
dExternBody = \case
    C.CExternSimple sp s -> ExternSimple sp s
    C.CExternDispatch sp s var -> ExternDispatch sp s var


dExternClassification :: C.CExternClassification -> ExternClassification
dExternClassification = \case
    C.CECOpaque -> ECOpaque
    C.CECElementwise -> ECElementwise
    C.CECReducer -> ECReducer


dInfixDecl :: C.CInfixDecl -> InfixDecl
dInfixDecl d =
    InfixDecl
        { infixDeclSpan = C.infixDeclSpan d
        , infixDeclFixity = dFixity (C.infixDeclFixity d)
        , infixDeclPrec = C.infixDeclPrec d
        , infixDeclOp = dBinOp (C.infixDeclOp d)
        , infixDeclSig = dTypeSig (C.infixDeclSig d)
        , infixDeclConstraints = dConstraints (C.infixDeclConstraints d)
        , infixDeclR = C.infixDeclR d
        , infixDeclDoc = Prelude.fmap dDocBlock (C.infixDeclDoc d)
        }


dPrefixDecl :: C.CPrefixDecl -> PrefixDecl
dPrefixDecl d =
    PrefixDecl
        { prefixDeclSpan = C.prefixDeclSpan d
        , prefixDeclPrec = C.prefixDeclPrec d
        , prefixDeclOp = dUnaryOp (C.prefixDeclOp d)
        , prefixDeclSig = dTypeSig (C.prefixDeclSig d)
        , prefixDeclConstraints = dConstraints (C.prefixDeclConstraints d)
        , prefixDeclR = C.prefixDeclR d
        , prefixDeclDoc = Prelude.fmap dDocBlock (C.prefixDeclDoc d)
        }


-- | Lower a list of @forall n: Number, ...@ binders from the CST
-- shape to the AST shape (M3.11).
dConstraints
    :: [(C.CLowerName, Maybe C.CUpperName)]
    -> [(LowerName, Maybe UpperName)]
dConstraints =
    Prelude.fmap (\(n, mc) -> (dLower n, Prelude.fmap dUpper mc))


dFixity :: C.CFixity -> Fixity
dFixity = \case
    C.CFLeft -> FLeft
    C.CFRight -> FRight
    C.CFNon -> FNon


-- Types --------------------------------------------------------------


dTypeSig :: C.CTypeSig -> TypeSig
dTypeSig = \case
    C.CTFun sp l r -> TFun sp (dTypeSig l) (dTypeSig r)
    C.CTApp sp h args -> TApp sp (dTypeAtom h) (Prelude.fmap dTypeAtom args)
    C.CTAtom a -> TAtom (dTypeAtom a)


dTypeAtom :: C.CTypeAtom -> TypeAtom
dTypeAtom = \case
    C.CTName n -> TName (dUpper n)
    C.CTVar n -> TVar (dLower n)
    C.CTParen sp inner -> TParen sp (dTypeSig inner)
    C.CTRecord r -> TRecord (dRecordType r)
    C.CTDataframe sp r -> TDataframe sp (dRecordType r)


dRecordType :: C.CRecordType -> RecordType
dRecordType (C.CRecordType sp fs) =
    RecordType
        { recordTypeSpan = sp
        , recordTypeFields = Prelude.fmap dFieldType fs
        }


dFieldType :: C.CFieldType -> FieldType
dFieldType (C.CFieldType sp n sig) =
    FieldType
        { fieldTypeSpan = sp
        , fieldTypeName = dLower n
        , fieldTypeSig = dTypeSig sig
        }


-- Expressions --------------------------------------------------------


dExpr :: C.CExpr -> Expr
dExpr = \case
    C.CELit sp lit -> ELit sp (dLiteral lit)
    C.CEVar n -> EVar (dLower n)
    C.CECon n -> ECon (dUpper n)
    C.CELambda sp ps body -> ELambda sp (Prelude.fmap dLower ps) (dExpr body)
    C.CEIf sp cond th el ->
        -- Section 5.3 desugaring: if e1 then e2 else e3
        --                        ⇒ case e1 of { True -> e2; False -> e3 }
        ECase
            sp
            (dExpr cond)
            [ CaseArm
                { caseArmSpan = exprSpan (dExpr th)
                , caseArmPattern =
                    PCon
                        (exprSpan (dExpr cond))
                        (UpperName (exprSpan (dExpr cond)) "True")
                        []
                , caseArmGuard = Prelude.Nothing
                , caseArmBody = dExpr th
                }
            , CaseArm
                { caseArmSpan = exprSpan (dExpr el)
                , caseArmPattern =
                    PCon
                        (exprSpan (dExpr cond))
                        (UpperName (exprSpan (dExpr cond)) "False")
                        []
                , caseArmGuard = Prelude.Nothing
                , caseArmBody = dExpr el
                }
            ]
    C.CECase sp scrut arms -> ECase sp (dExpr scrut) (Prelude.fmap dCaseArm arms)
    C.CELet sp binds body -> ELet sp (Prelude.fmap dBinding binds) (dExpr body)
    C.CEApp sp f x -> EApp sp (dExpr f) (dExpr x)
    C.CEBinOp sp op l r -> EBinOp sp (dBinOp op) (dExpr l) (dExpr r)
    C.CEUnary sp op e -> EUnary sp (dUnaryOp op) (dExpr e)
    C.CEPipe sp l r -> EPipe sp (dExpr l) (dExpr r)
    C.CEField sp e n -> EField sp (dExpr e) (dLower n)
    C.CEParen _ inner ->
        -- The AST drops parenthesisation: spans already preserve the
        -- source range, and operator precedence is encoded in the tree
        -- shape itself.
        dExpr inner
    C.CERecord sp fs -> ERecord sp (Prelude.fmap dFieldBinding fs)
    C.CERecordUpdate sp tgt fs ->
        ERecordUpdate sp (dExpr tgt) (Prelude.fmap dFieldBinding fs)
    C.CEDataframe sp fs -> EDataframe sp (Prelude.fmap dFieldBinding fs)
    C.CEVector sp es -> EVector sp (Prelude.fmap dExpr es)
    C.CEVerb sp kw args ->
        EVerb sp (kwToVerb kw) (Prelude.fmap dDplyrArg args)


dLiteral :: C.CLiteral -> Literal
dLiteral = \case
    C.CLInt n _ -> LInt n
    C.CLDouble d _ -> LDouble d
    C.CLChar t -> LChar t


dBinOp :: C.CBinOp -> BinOp
dBinOp = \case
    C.COpAdd -> OpAdd
    C.COpSub -> OpSub
    C.COpMul -> OpMul
    C.COpDiv -> OpDiv
    C.COpIntDiv -> OpIntDiv
    C.COpMod -> OpMod
    C.COpExp -> OpExp
    C.COpEq -> OpEq
    C.COpNeq -> OpNeq
    C.COpGt -> OpGt
    C.COpLt -> OpLt
    C.COpGe -> OpGe
    C.COpLe -> OpLe


dUnaryOp :: C.CUnaryOp -> UnaryOp
dUnaryOp = \case
    C.COpNeg -> OpNeg


dCaseArm :: C.CCaseArm -> CaseArm
dCaseArm a =
    CaseArm
        { caseArmSpan = C.caseArmSpan a
        , caseArmPattern = dPattern (C.caseArmPattern a)
        , caseArmGuard = Prelude.fmap dExpr (C.caseArmGuard a)
        , caseArmBody = dExpr (C.caseArmBody a)
        }


dBinding :: C.CBinding -> Binding
dBinding b =
    Binding
        { bindingSpan = C.bindingSpan b
        , bindingName = dLower (C.bindingName b)
        , bindingBody = dExpr (C.bindingBody b)
        }


dFieldBinding :: C.CFieldBinding -> FieldBinding
dFieldBinding f =
    FieldBinding
        { fieldBindingSpan = C.fieldBindingSpan f
        , fieldBindingName = dLower (C.fieldBindingName f)
        , fieldBindingValue = dExpr (C.fieldBindingValue f)
        }


dDplyrArg :: C.CDplyrArg -> DplyrArg
dDplyrArg = \case
    C.CDAExpr e -> DAExpr (dExpr e)
    C.CDARecord sp fs -> DARecord sp (Prelude.fmap dFieldBinding fs)
    C.CDAModifier m -> DAModifier (dModifier m)
    C.CDAJoinOn sp e ps ->
        DAJoinOn sp (dExpr e) (Prelude.fmap dJoinPair ps)


dModifier :: C.CModifier -> Modifier
dModifier = \case
    C.CMDesc sp n -> MDesc sp (dLower n)
    C.CMAsc sp n -> MAsc sp (dLower n)
    C.CMAs sp t -> MAs sp t
    C.CMWhere sp e -> MWhere sp (dExpr e)
    C.CMCols sp ns -> MCols sp (Prelude.fmap dLower ns)


dJoinPair :: C.CJoinPair -> JoinPair
dJoinPair p =
    JoinPair
        { joinPairSpan = C.joinPairSpan p
        , joinPairLeft = dLower (C.joinPairLeft p)
        , joinPairRight = dLower (C.joinPairRight p)
        }


-- Patterns -----------------------------------------------------------


dPattern :: C.CPattern -> Pattern
dPattern = \case
    C.CPWildcard sp -> PWildcard sp
    C.CPVar n -> PVar (dLower n)
    C.CPLit sp lit -> PLit sp (dLiteral lit)
    C.CPCon sp n ps -> PCon sp (dUpper n) (Prelude.fmap dPattern ps)
    C.CPRecord sp fs -> PRecord sp (Prelude.fmap dRecordPatField fs)
    C.CPParen _ inner -> dPattern inner
    C.CPVector sp _ -> PWildcard sp
    C.CPAs sp _ _ -> PWildcard sp


dRecordPatField :: C.CRecordPatField -> RecordPatField
dRecordPatField = \case
    C.CRpfShort n -> RpfShort (dLower n)
    C.CRpfFull sp n p -> RpfFull sp (dLower n) (dPattern p)


-- Verbs --------------------------------------------------------------


-- | Map the parser's 'Keyword' verb to the AST's typed 'Verb'.
--
-- Total: every keyword the parser emits as a verb is in the mapping.
-- An unknown keyword in this position is a parser bug, so we fall
-- back to a verb the rest of the compiler will refuse to type
-- ('VSelect') and rely on the validation pass to catch the
-- discrepancy. (In practice 'pVerb' only calls this for the verb
-- keyword set; this branch is dead code.)
kwToVerb :: Keyword -> Verb
kwToVerb = \case
    KSelect -> VSelect
    KFilter -> VFilter
    KMutate -> VMutate
    KSummarize -> VSummarize
    KGroupBy -> VGroupBy
    KUngroup -> VUngroup
    KArrange -> VArrange
    KRename -> VRename
    KLeftJoin -> VLeftJoin
    KRightJoin -> VRightJoin
    KInnerJoin -> VInnerJoin
    _ -> VSelect


-- | Source spelling of a verb keyword. Used by the
-- verb-as-function-call desugaring (a `count predicate xs` outside
-- a pipe context becomes `EApp (EVar "count") predicate xs`).
kwToVerbText :: Keyword -> Text
kwToVerbText = \case
    KSelect -> "select"
    KFilter -> "filter"
    KMutate -> "mutate"
    KSummarize -> "summarize"
    KGroupBy -> "group_by"
    KUngroup -> "ungroup"
    KArrange -> "arrange"
    KRename -> "rename"
    KLeftJoin -> "left_join"
    KRightJoin -> "right_join"
    KInnerJoin -> "inner_join"
    _ -> ""
