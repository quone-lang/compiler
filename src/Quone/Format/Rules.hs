{-| Canonical layout rules for the elm-format-style Quone formatter.

Every CST node has exactly one canonical rendering. The strategy is:

* Walk the CST top-down, producing 'Quone.Format.Doc.Doc' values.
* Use 'group' wherever a single-line vs broken-out choice exists; the
  printer in 'Quone.Format.Doc' picks based on the page width.
* Insert a blank line between top-level declarations (two when the
  next decl carries a doc block).

For initial release the formatter rewrites to canonical form without
preserving the user's original whitespace; comment placement beyond
'CDocBlock' lands when the lexer grows trivia tracking
('Quone.Format.Trivia').

-}
module Quone.Format.Rules
    ( formatCst
    , formatCstWithComments
    )
where

import qualified Data.Text as T
import NriPrelude
import Quone.Format.Doc
    ( Doc
    , (<+>)
    , (<>)
    , concatD
    , empty
    , group
    , hsep
    , line
    , render
    , space
    , text
    )
import qualified Quone.Format.Doc as Doc
import Quone.Lex.Lexer (Comment (..))
import Quone.Lex.Token (Keyword, keywordText)
import Quone.Parse.Cst
import Quone.Position (SourceSpan (..), SourcePos (..))
import qualified Prelude



-- | Pretty-print a parsed program with no comment preservation.
--
-- Kept for backwards compatibility; new callers should use
-- 'formatCstWithComments' so user comments survive a round-trip
-- through the formatter.
formatCst :: Text -> CProgram -> Text
formatCst _src = formatCstWithComments []


-- | Pretty-print a program, slotting @#@ comments back in between
-- the declarations they originally preceded.
--
-- initial release strategy:
--
-- * Comments whose start line is before the first declaration's
--   start line are emitted at the very top of the output (after the
--   module header if any).
-- * Comments whose start line falls between two declarations are
--   emitted before the second declaration.
-- * Comments after the last declaration are appended at the end.
-- * A comment on the same line as a token (trailing comment) is
--   approximated by emitting it on the next line. A precise per-token
--   trailing-comment model lands when the lexer attaches trivia to
--   each 'Located' token (project plan track C1, follow-up).
formatCstWithComments :: [Comment] -> CProgram -> Text
formatCstWithComments comments prog =
    let
        sortedComments = stableSortByLine comments

        decls = programDecls prog

        -- For each declaration, split the comments into "this and
        -- before" (will be emitted before this decl) vs "later".
        chunked = chunkComments sortedComments decls

        moduleDoc = case programModule prog of
            Just m -> formatModule m <> line <> line
            Prelude.Nothing -> empty

        body = renderChunks chunked
    in
    ensureTrailingNewline (render (moduleDoc <> body))


renderChunks :: [(Maybe CDecl, [Comment])] -> Doc
renderChunks = goSep
  where
    goSep [] = empty
    goSep [(d, cs)] = chunkDoc d cs
    goSep ((d, cs) : rest) =
        chunkDoc d cs
            <> separator d
            <> goSep rest

    -- Two blank lines between top-level declarations; just one when
    -- the chunk is a comment-only trailing chunk (no decl).
    separator d = case d of
        Just _ -> line <> line
        Prelude.Nothing -> line


chunkDoc :: Maybe CDecl -> [Comment] -> Doc
chunkDoc maybeDecl comments =
    let
        commentDoc = case comments of
            [] -> empty
            cs ->
                concatD
                    (Prelude.fmap renderComment cs)
                    <> case maybeDecl of
                        Just _ -> empty   -- decl follows on the next chunk position
                        Prelude.Nothing -> empty
        declDoc = case maybeDecl of
            Just d -> formatDecl d
            Prelude.Nothing -> empty
    in
    case (comments, maybeDecl) of
        ([], Just _) -> declDoc
        (_, Just _) -> commentDoc <> declDoc
        (_, Prelude.Nothing) -> commentDoc


renderComment :: Comment -> Doc
renderComment c =
    let
        body = commentBody c
        prefix =
            if T.null body
                then text "#"
                else text "# " <> text body
    in
    prefix <> line


-- | Pair each declaration with the comments that should immediately
-- precede it. A trailing tuple with 'Prelude.Nothing' carries any
-- comments that fell after the last declaration.
chunkComments :: [Comment] -> [CDecl] -> [(Maybe CDecl, [Comment])]
chunkComments cs0 decls0 =
    let
        (chunks, leftover) = go cs0 decls0
    in
    case leftover of
        [] -> chunks
        cs -> chunks ++ [(Prelude.Nothing, cs)]
  where
    go cs [] = ([], cs)
    go cs (d : rest) =
        let
            startLine = posLine (spanStart (declSpanCst d))
            (before, after) = Prelude.span (\c -> commentLine c Prelude.<= startLine) cs
            (chunks, leftover) = go after rest
        in
        ((Just d, before) : chunks, leftover)


commentLine :: Comment -> Int
commentLine = posLine Prelude.. spanStart Prelude.. commentSpan


declSpanCst :: CDecl -> SourceSpan
declSpanCst = \case
    CDValue v -> valueDeclSpan v
    CDImport (CQuoneImport sp _ _) -> sp
    CDImport (CForeignImport sp _ _ _) -> sp
    CDType td -> typeDeclSpan td
    CDTypeAlias a -> aliasDeclSpan a
    CDExtern (CExternValue sp _ _ _ _ _) -> sp
    CDExtern (CExternType sp _ _ _) -> sp
    CDInfix d -> infixDeclSpan d
    CDPrefix d -> prefixDeclSpan d


stableSortByLine :: [Comment] -> [Comment]
stableSortByLine = sortBy (\a b -> Prelude.compare (commentLine a) (commentLine b))


sortBy :: (a -> a -> Prelude.Ordering) -> [a] -> [a]
sortBy cmp = Prelude.foldr insert []
  where
    insert x [] = [x]
    insert x (y : ys) = case cmp x y of
        Prelude.GT -> y : insert x ys
        _ -> x : y : ys


ensureTrailingNewline :: Text -> Text
ensureTrailingNewline t
    | T.null t = "\n"
    | T.last t Prelude.== '\n' = t
    | Prelude.otherwise = t ++ "\n"



-- ---------------------------------------------------------------------
-- Modules and exports
-- ---------------------------------------------------------------------


formatModule :: CModuleDecl -> Doc
formatModule m =
    let
        path = T.intercalate "." (Prelude.fmap upperText (moduleDeclPath m))
    in
    text "module"
        <+> text path
        <+> formatExports (moduleDeclExports m)


formatExports :: CExportList -> Doc
formatExports = \case
    CExportAll _ -> text "exporting (..)"
    CExportNames _ items ->
        text "exporting"
            <+> text "("
            <> hsepCommas (Prelude.fmap formatExportItem items)
            <> text ")"


formatExportItem :: CExportItem -> Doc
formatExportItem = \case
    CExportLower n -> text (lowerText n)
    CExportUpper n -> text (upperText n)


-- ---------------------------------------------------------------------
-- Declarations
-- ---------------------------------------------------------------------


formatDecl :: CDecl -> Doc
formatDecl = \case
    CDValue v -> formatValue v
    CDImport i -> formatImport i
    CDType td -> formatType td
    CDTypeAlias a -> formatAlias a
    CDExtern ext -> formatExtern ext
    CDInfix d -> formatInfixDecl d
    CDPrefix d -> formatPrefixDecl d


formatImport :: CImportDecl -> Doc
formatImport = \case
    CQuoneImport _ path sel ->
        let
            name = T.intercalate "." (Prelude.fmap upperText path)
        in
        text "import"
            <+> text name
            <> formatSelection sel
    CForeignImport _ classification fname sig ->
        let
            pkg =
                T.intercalate
                    "."
                    (Prelude.fmap lowerText (foreignNamePackage fname))
            qualified =
                if T.null pkg
                    then lowerText (foreignNameFn fname)
                    else pkg ++ "." ++ lowerText (foreignNameFn fname)
            aliasDoc = case foreignNameAlias fname of
                Prelude.Nothing -> empty
                Just alias -> space <> text "as" <+> text (lowerText alias)
            viaDoc = case foreignNameVia fname of
                Prelude.Nothing -> empty
                Just template -> space <> text "via" <+> dquote <> text template <> dquote
            head_ = case classification of
                CCOpaque -> text "import"
                CCElementwise -> text "import" <+> text "elementwise"
                CCReducer -> text "import" <+> text "reducer"
            dquote = text "\""
        in
        head_
            <+> text qualified
            <> aliasDoc
            <+> text ":"
            <+> formatTypeSig sig
            <> viaDoc


formatSelection :: CImportSelection -> Doc
formatSelection = \case
    CImportAll -> empty
    CImportSingle item -> text "." <> formatExportItem item
    CImportNames items ->
        text " ("
            <> hsepCommas (Prelude.fmap formatExportItem items)
            <> text ")"


formatType :: CTypeDecl -> Doc
formatType td =
    let
        head_ =
            text "type"
                <+> text (upperText (typeDeclName td))
                <> formatParams
                    (Prelude.fmap lowerText (typeDeclParams td))
        variants = Prelude.fmap formatVariant (typeDeclVariants td)
    in
    formatDoc (typeDeclDoc td)
        <> head_
        <> line
        <> Doc.indent 4 (joinVariants variants)
  where
    -- Quone uses `<-` (not Haskell's `=`) for type-decl bodies and
    -- aliases, matching the value-binding spelling.
    joinVariants = \case
        [] -> empty
        [v] -> text "<- " <> v
        (v : rest) ->
            text "<- " <> v
                <> concatD
                    (Prelude.fmap (\x -> line <> text "| " <> x) rest)


formatVariant :: CVariant -> Doc
formatVariant v =
    let
        name = text (upperText (variantName v))
        args = Prelude.fmap formatTypeAtom (variantArgs v)
    in
    case args of
        [] -> name
        _ -> hsep (name : args)


formatAlias :: CTypeAliasDecl -> Doc
formatAlias a =
    formatDoc (aliasDeclDoc a)
        <> text "type alias"
        <+> text (upperText (aliasDeclName a))
        <> formatParams (Prelude.fmap lowerText (aliasDeclParams a))
        <+> text "<-"
        <> case aliasDeclBody a of
            CTAtom (CTDataframe _ _) ->
                line <> Doc.indent 4 (formatTypeSig (aliasDeclBody a))
            _ ->
                space <> formatTypeSig (aliasDeclBody a)


formatParams :: [Text] -> Doc
formatParams = \case
    [] -> empty
    ps -> space <> hsep (Prelude.fmap text ps)


formatValue :: CValueDecl -> Doc
formatValue v =
    let
        annotation = case valueDeclAnnotation v of
            Just sig -> formatValueAnnotation (valueDeclName v) sig
            Prelude.Nothing -> empty
        head_ =
            text (lowerText (valueDeclName v))
                <> formatParams
                    (Prelude.fmap lowerText (valueDeclParams v))
                <+> text "<-"
        body = formatExpr (valueDeclBody v)
        renderedBody =
            if exprIsMultiline (valueDeclBody v)
                then head_ <> line <> Doc.indent 4 body
                else group (head_ <+> body)
    in
    formatDoc (valueDeclDoc v)
        <> annotation
        <> renderedBody


formatValueAnnotation :: CLowerName -> CTypeSig -> Doc
formatValueAnnotation name sig =
    let
        head_ =
            text (lowerText name)
                <+> text ":"
    in
    if typeSigIsMultiline sig
        then
            head_
                <> line
                <> Doc.indent 4 (formatTypeSigMultiline sig)
                <> line
        else
            head_
                <+> formatTypeSig sig
                <> line


-- | Format a prelude-only @extern@ declaration. The value-binding
-- shape carries an optional classification modifier, a signature, and
-- an R-callable string body (with optional @{ dispatch_on = "var" }@).
formatExtern :: CExternDecl -> Doc
formatExtern = \case
    CExternValue _ classification name sig body mDoc ->
        let
            classText = case classification of
                CECOpaque -> empty
                CECElementwise -> space <> text "elementwise"
                CECReducer -> space <> text "reducer"
            head_ =
                text "extern"
                    <> classText
                    <+> text (lowerText name)
                    <+> text ":"
                    <+> formatTypeSig sig
                    <+> text "="
                    <+> formatExternBody body
        in
        formatDoc mDoc <> head_
    CExternType _ name params mDoc ->
        let
            head_ =
                text "extern type"
                    <+> text (upperText name)
                    <> formatParams (Prelude.fmap lowerText params)
        in
        formatDoc mDoc <> head_


formatExternBody :: CExternBody -> Doc
formatExternBody = \case
    CExternSimple _ s -> dquote <> text s <> dquote
    CExternDispatch _ s var ->
        dquote
            <> text s
            <> dquote
            <+> text "{ dispatch_on ="
            <+> dquote
            <> text var
            <> dquote
            <+> text "}"
  where
    dquote = text "\""


-- | Format a prelude-only @infix@ overload declaration.
formatInfixDecl :: CInfixDecl -> Doc
formatInfixDecl d =
    let
        assocText = case infixDeclFixity d of
            CFLeft -> text "left"
            CFRight -> text "right"
            CFNon -> text "non"
        opText = text "(" <> text (binOpText (infixDeclOp d)) <> text ")"
    in
    formatDoc (infixDeclDoc d)
        <> text "infix"
        <+> assocText
        <+> text (T.pack (Prelude.show (infixDeclPrec d)))
        <+> opText
        <+> text ":"
        <+> formatTypeSig (infixDeclSig d)
        <+> text "="
        <+> text "\""
        <> text (infixDeclR d)
        <> text "\""


-- | Format a prelude-only @prefix@ overload declaration (unary @-@).
formatPrefixDecl :: CPrefixDecl -> Doc
formatPrefixDecl d =
    let
        opText = text "(" <> text (unaryOpText (prefixDeclOp d)) <> text ")"
    in
    formatDoc (prefixDeclDoc d)
        <> text "prefix"
        <+> text (T.pack (Prelude.show (prefixDeclPrec d)))
        <+> opText
        <+> text ":"
        <+> formatTypeSig (prefixDeclSig d)
        <+> text "="
        <+> text "\""
        <> text (prefixDeclR d)
        <> text "\""


-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------


formatTypeSig :: CTypeSig -> Doc
formatTypeSig = \case
    CTFun _ a b ->
        formatTypeSig a <+> text "->" <+> formatTypeSig b
    CTApp _ head_ args ->
        hsep (formatTypeAtom head_ : Prelude.fmap formatTypeAtom args)
    CTAtom a -> formatTypeAtom a


formatTypeSigMultiline :: CTypeSig -> Doc
formatTypeSigMultiline = \case
    CTFun _ a b ->
        formatTypeSig a
            <+> text "->"
            <> line
            <> formatTypeSigMultiline b
    other -> formatTypeSig other


typeSigIsMultiline :: CTypeSig -> Bool
typeSigIsMultiline = \case
    CTFun _ a b -> typeSigIsMultiline a || typeSigIsMultiline b
    CTApp _ head_ args -> typeAtomIsMultiline head_ || Prelude.any typeAtomIsMultiline args
    CTAtom a -> typeAtomIsMultiline a


typeAtomIsMultiline :: CTypeAtom -> Bool
typeAtomIsMultiline = \case
    CTRecord r -> recordTypeIsMultiline r
    CTDataframe _ r -> recordTypeIsMultiline r
    CTParen _ inner -> typeSigIsMultiline inner
    _ -> False


recordTypeIsMultiline :: CRecordType -> Bool
recordTypeIsMultiline r =
    case recordTypeFields r of
        _ : _ : _ -> True
        _ -> False


formatTypeAtom :: CTypeAtom -> Doc
formatTypeAtom = \case
    CTName n -> text (upperText n)
    CTVar n -> text (lowerText n)
    CTParen _ inner -> text "(" <> formatTypeSig inner <> text ")"
    CTRecord r -> formatRecordType r
    CTDataframe _ r ->
        text "dataframe"
            <> line
            <> Doc.indent 4 (formatRecordType r)


formatRecordType :: CRecordType -> Doc
formatRecordType r =
    multilineRecordLike (Prelude.fmap formatFieldType (recordTypeFields r))


formatFieldType :: CFieldType -> Doc
formatFieldType f =
    text (lowerText (fieldTypeName f))
        <+> text ":"
        <+> formatTypeSig (fieldTypeSig f)



-- ---------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------


formatExpr :: CExpr -> Doc
formatExpr = \case
    CELit _ lit -> formatLiteral lit
    CEVar n -> text (lowerText n)
    CECon n -> text (upperText n)
    CELambda _ params body ->
        text "\\"
            <> hsep (Prelude.fmap (text Prelude.. lowerText) params)
            <+> text "->"
            <+> formatExpr body
    CEIf _ c t elseBranch ->
        text "if"
            <+> formatExpr c
            <+> text "then"
            <+> formatExpr t
            <+> text "else"
            <+> formatExpr elseBranch
    CECase _ scrut arms ->
        text "case"
            <+> formatExpr scrut
            <+> text "of"
            <> line
            <> Doc.indent 4 (vcat (Prelude.fmap formatArm arms))
    CELet _ binds body ->
        text "let"
            <> line
            <> Doc.indent 4 (vcat (Prelude.fmap formatBinding binds))
            <> line
            <> text "in"
            <+> formatExpr body
    CEApp _ a b -> formatExpr a <+> formatExprAtomic b
    CEBinOp _ op a b ->
        formatExpr a
            <+> text (binOpText op)
            <+> formatExpr b
    CEUnary _ _ expr -> text "-" <> formatExprAtomic expr
    piped@(CEPipe _ _ _) ->
        formatPipe piped
    CEField _ expr n ->
        formatExprAtomic expr <> text "." <> text (lowerText n)
    CEParen _ inner -> text "(" <> formatExpr inner <> text ")"
    CERecord _ fs -> formatRecord fs
    CERecordUpdate _ target fs ->
        group
            ( text "{ "
                <> formatExpr target
                <+> text "|"
                <+> hsepCommas (Prelude.fmap formatFieldBinding fs)
                <> text " }"
            )
    CEDataframe _ fs -> text "dataframe " <> formatRecord fs
    CEVector _ es ->
        text "["
            <> hsepCommas (Prelude.fmap formatExpr es)
            <> text "]"
    CEVerb _ kw args ->
        formatVerb kw args


exprIsMultiline :: CExpr -> Bool
exprIsMultiline = \case
    CELet _ _ _ -> True
    CECase _ _ _ -> True
    CEIf _ _ _ _ -> True
    CEPipe _ _ _ -> True
    CEVerb _ _ args -> Prelude.any dplyrArgIsMultiline args
    CEVector _ es -> Prelude.any exprIsMultiline es
    CEApp _ a b -> exprIsMultiline a || exprIsMultiline b
    CEBinOp _ _ a b -> exprIsMultiline a || exprIsMultiline b
    CEUnary _ _ expr -> exprIsMultiline expr
    CEField _ expr _ -> exprIsMultiline expr
    CEParen _ inner -> exprIsMultiline inner
    _ -> False


dplyrArgIsMultiline :: CDplyrArg -> Bool
dplyrArgIsMultiline = \case
    CDAExpr expr -> exprIsMultiline expr
    CDARecord _ fs -> Prelude.length fs Prelude.> 1
    CDAJoinOn _ target pairs -> exprIsMultiline target || Prelude.length pairs Prelude.> 1
    CDAModifier _ -> False


-- | Wrap atomic-position expressions in parentheses when the surface
-- syntax requires it.
formatExprAtomic :: CExpr -> Doc
formatExprAtomic expr = case expr of
    CEBinOp _ _ _ _ -> text "(" <> formatExpr expr <> text ")"
    CEPipe _ _ _ -> text "(" <> formatExpr expr <> text ")"
    CELambda _ _ _ -> text "(" <> formatExpr expr <> text ")"
    CEApp _ _ _ -> text "(" <> formatExpr expr <> text ")"
    CEUnary _ _ _ -> text "(" <> formatExpr expr <> text ")"
    CEIf _ _ _ _ -> text "(" <> formatExpr expr <> text ")"
    CECase _ _ _ -> text "(" <> formatExpr expr <> text ")"
    CELet _ _ _ -> text "(" <> formatExpr expr <> text ")"
    _ -> formatExpr expr


formatLiteral :: CLiteral -> Doc
formatLiteral = \case
    CLInt _ raw -> text raw
    CLDouble _ raw -> text raw
    CLChar t -> text "\"" <> text (escape t) <> text "\""
  where
    escape = T.replace "\"" "\\\"" Prelude.. T.replace "\\" "\\\\"


binOpText :: CBinOp -> Text
binOpText = \case
    COpAdd -> "+"
    COpSub -> "-"
    COpMul -> "*"
    COpDiv -> "/"
    COpIntDiv -> "//"
    COpMod -> "%"
    COpExp -> "^"
    COpEq -> "=="
    COpNeq -> "!="
    COpGt -> ">"
    COpLt -> "<"
    COpGe -> ">="
    COpLe -> "<="


unaryOpText :: CUnaryOp -> Text
unaryOpText = \case
    COpNeg -> "-"


formatArm :: CCaseArm -> Doc
formatArm a =
    let
        head_ = formatPattern (caseArmPattern a)
        guarded = case caseArmGuard a of
            Nothing -> head_
            Just g -> head_ <+> text "|" <+> formatExpr g
    in
    guarded
        <+> text "->"
        <+> formatExpr (caseArmBody a)


formatBinding :: CBinding -> Doc
formatBinding b =
    text (lowerText (bindingName b))
        <+> text "<-"
        <+> formatExpr (bindingBody b)


formatRecord :: [CFieldBinding] -> Doc
formatRecord fs = case fs of
    [] -> text "{ }"
    _ ->
        text "{ "
            <> hsepCommas (Prelude.fmap formatFieldBinding fs)
            <> text " }"


formatFieldBinding :: CFieldBinding -> Doc
formatFieldBinding f =
    let
        name = lowerText (fieldBindingName f)
        body = fieldBindingValue f
    in
    case body of
        CEVar v
            | lowerText v Prelude.== name ->
                text name
        _ ->
            text name
                <+> text "="
                <+> formatExpr body


formatDplyrArg :: CDplyrArg -> Doc
formatDplyrArg = \case
    CDAExpr expr -> formatExprAtomic expr
    CDARecord _ fs -> formatDplyrRecord fs
    CDAModifier m -> formatModifier m
    CDAJoinOn _ target pairs ->
        formatExprAtomic target
            <+> text "{ "
            <> hsepCommas (Prelude.fmap formatJoinPair pairs)
            <> text " }"


formatDplyrRecord :: [CFieldBinding] -> Doc
formatDplyrRecord fs = case fs of
    [] -> text "{ }"
    [f] -> text "{ " <> formatFieldBinding f <> text " }"
    _ -> multilineRecordLike (Prelude.fmap formatFieldBinding fs)


formatVerb :: Keyword -> [CDplyrArg] -> Doc
formatVerb kw args =
    let
        verb = text (keywordText kw)
    in
    case args of
        [] ->
            verb
        [record@(CDARecord _ (_ : _ : _))] ->
            verb <> line <> Doc.indent 4 (formatDplyrArg record)
        _ ->
            verb <> space <> hsep (Prelude.fmap formatDplyrArg args)


formatModifier :: CModifier -> Doc
formatModifier = \case
    CMDesc _ n -> text "{ desc " <> text (lowerText n) <> text " }"
    CMAsc _ n -> text "{ asc " <> text (lowerText n) <> text " }"
    CMAs _ t -> text "as \"" <> text t <> text "\""
    CMWhere _ expr -> text "where " <> formatExpr expr
    CMCols _ ns ->
        text "{ "
            <> hsepCommas (Prelude.fmap (text Prelude.. lowerText) ns)
            <> text " }"


formatJoinPair :: CJoinPair -> Doc
formatJoinPair p =
    let
        l = lowerText (joinPairLeft p)
        r = lowerText (joinPairRight p)
    in
    if l Prelude.== r
        then text l
        else text l <+> text "=" <+> text r



-- ---------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------


formatPattern :: CPattern -> Doc
formatPattern = \case
    CPWildcard _ -> text "_"
    CPVar n -> text (lowerText n)
    CPLit _ lit -> formatLiteral lit
    CPCon _ n args ->
        case args of
            [] -> text (upperText n)
            _ -> hsep (text (upperText n) : Prelude.fmap formatPattern args)
    CPRecord _ fs ->
        text "{ "
            <> hsepCommas (Prelude.fmap formatRecordPatField fs)
            <> text " }"
    CPVector _ ps ->
        text "[" <> hsepCommas (Prelude.fmap formatPattern ps) <> text "]"
    CPParen _ inner -> text "(" <> formatPattern inner <> text ")"
    CPAs _ n inner ->
        text (lowerText n) <> text "@" <> formatPattern inner


formatRecordPatField :: CRecordPatField -> Doc
formatRecordPatField = \case
    CRpfShort n -> text (lowerText n)
    CRpfFull _ n p ->
        text (lowerText n)
            <+> text "="
            <+> formatPattern p



-- ---------------------------------------------------------------------
-- Doc blocks
-- ---------------------------------------------------------------------


formatDoc :: Maybe CDocBlock -> Doc
formatDoc = \case
    Prelude.Nothing -> empty
    Just b ->
        concatD
            (Prelude.fmap (\l -> text (formatDocLine l) <> line)
                (docBlockLines b))


formatDocLine :: Text -> Text
formatDocLine docLine =
    if T.null docLine then
        "#'"
    else
        "#' " Prelude.<> docLine



-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------


hsepCommas :: [Doc] -> Doc
hsepCommas = \case
    [] -> empty
    [d] -> d
    (d : ds) ->
        d <> concatD (Prelude.fmap (\x -> text ", " <> x) ds)


vcat :: [Doc] -> Doc
vcat = \case
    [] -> empty
    [d] -> d
    (d : ds) -> d <> concatD (Prelude.fmap (\x -> line <> x) ds)


multilineRecordLike :: [Doc] -> Doc
multilineRecordLike fields = case fields of
    [] -> text "{ }"
    [field] -> text "{ " <> field <> text " }"
    first : rest ->
        text "{ "
            <> first
            <> concatD (Prelude.fmap (\field -> line <> text ", " <> field) rest)
            <> line
            <> text "}"


formatPipe :: CExpr -> Doc
formatPipe expr =
    case pipeParts expr of
        [] -> empty
        [one] -> formatExpr one
        first : rest ->
            formatExpr first
                <> concatD
                    (Prelude.fmap
                        (\part -> line <> Doc.indent 4 (text "|>" <+> formatExpr part))
                        rest)


pipeParts :: CExpr -> [CExpr]
pipeParts = \case
    CEPipe _ left right -> pipeParts left Prelude.++ [right]
    other -> [other]


lowerText :: CLowerName -> Text
lowerText = lowerNameText


upperText :: CUpperName -> Text
upperText = upperNameText
