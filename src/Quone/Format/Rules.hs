{-| Canonical layout rules for the elm-format-style Quone formatter.

Every CST node has exactly one canonical rendering. The strategy is:

* Walk the CST top-down, producing 'Quone.Format.Doc.Doc' values.
* Use 'group' wherever a single-line vs broken-out choice exists; the
  printer in 'Quone.Format.Doc' picks based on the page width.
* Insert a blank line between top-level declarations (two when the
  next decl carries a doc block).

For v0.0.1 the formatter rewrites to canonical form without
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
import NriPrelude hiding ((<>), (<+>))
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
import Quone.Lex.Token (keywordText)
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
-- v0.0.1 strategy:
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
    CDImport (CForeignImport sp _ _) -> sp
    CDType td -> typeDeclSpan td
    CDTypeAlias a -> aliasDeclSpan a


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


formatDecls :: [CDecl] -> Doc
formatDecls = goSep
  where
    goSep [] = empty
    goSep [d] = formatDecl d
    goSep (d : rest) =
        formatDecl d
            <> line
            <> line
            <> goSep rest


formatDecl :: CDecl -> Doc
formatDecl = \case
    CDValue v -> formatValue v
    CDImport i -> formatImport i
    CDType td -> formatType td
    CDTypeAlias a -> formatAlias a


formatImport :: CImportDecl -> Doc
formatImport = \case
    CQuoneImport _ path sel ->
        let
            name = T.intercalate "." (Prelude.fmap upperText path)
        in
        text "import"
            <+> text name
            <> formatSelection sel
    CForeignImport _ fname sig ->
        let
            pkg =
                T.intercalate
                    "."
                    (Prelude.fmap lowerText (foreignNamePackage fname))
            qualified =
                if T.null pkg
                    then lowerText (foreignNameFn fname)
                    else pkg ++ "." ++ lowerText (foreignNameFn fname)
        in
        text "import"
            <+> text qualified
            <+> text ":"
            <+> formatTypeSig sig


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
    joinVariants = \case
        [] -> empty
        [v] -> text "= " <> v
        (v : rest) ->
            text "= " <> v
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
        <+> text "="
        <+> formatTypeSig (aliasDeclBody a)


formatParams :: [Text] -> Doc
formatParams = \case
    [] -> empty
    ps -> space <> hsep (Prelude.fmap text ps)


formatValue :: CValueDecl -> Doc
formatValue v =
    let
        annotation = case valueDeclAnnotation v of
            Just sig ->
                text (lowerText (valueDeclName v))
                    <+> text ":"
                    <+> formatTypeSig sig
                    <> line
            Prelude.Nothing -> empty
        head_ =
            text (lowerText (valueDeclName v))
                <> formatParams
                    (Prelude.fmap lowerText (valueDeclParams v))
                <+> text "<-"
        body = formatExpr (valueDeclBody v)
    in
    formatDoc (valueDeclDoc v)
        <> annotation
        <> group (head_ <+> body)



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


formatTypeAtom :: CTypeAtom -> Doc
formatTypeAtom = \case
    CTName n -> text (upperText n)
    CTVar n -> text (lowerText n)
    CTParen _ inner -> text "(" <> formatTypeSig inner <> text ")"
    CTRecord r -> formatRecordType r
    CTDataframe _ r -> text "dataframe " <> formatRecordType r


formatRecordType :: CRecordType -> Doc
formatRecordType r =
    let
        fields = Prelude.fmap formatFieldType (recordTypeFields r)
    in
    text "{ "
        <> hsepCommas fields
        <> text " }"


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
    CEIf _ c t e ->
        text "if"
            <+> formatExpr c
            <+> text "then"
            <+> formatExpr t
            <+> text "else"
            <+> formatExpr e
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
    CEUnary _ _ e -> text "-" <> formatExprAtomic e
    CEPipe _ a b ->
        group
            ( formatExpr a
                <> Doc.softline
                <> text "|>"
                <+> formatExpr b
            )
    CEField _ e n ->
        formatExprAtomic e <> text "." <> text (lowerText n)
    CEParen _ inner -> text "(" <> formatExpr inner <> text ")"
    CERecord _ fs -> formatRecord fs
    CERecordUpdate _ target fs ->
        text "{ "
            <> formatExpr target
            <+> text "|"
            <+> hsepCommas (Prelude.fmap formatFieldBinding fs)
            <> text " }"
    CEDataframe _ fs -> text "dataframe " <> formatRecord fs
    CEVector _ es ->
        text "["
            <> hsepCommas (Prelude.fmap formatExpr es)
            <> text "]"
    CEVerb _ kw args ->
        text (keywordText kw)
            <> if Prelude.null args
                then empty
                else space <> hsep (Prelude.fmap formatDplyrArg args)


-- | Wrap atomic-position expressions in parentheses when the surface
-- syntax requires it.
formatExprAtomic :: CExpr -> Doc
formatExprAtomic e = case e of
    CEBinOp _ _ _ _ -> text "(" <> formatExpr e <> text ")"
    CEPipe _ _ _ -> text "(" <> formatExpr e <> text ")"
    CELambda _ _ _ -> text "(" <> formatExpr e <> text ")"
    CEApp _ _ _ -> text "(" <> formatExpr e <> text ")"
    CEUnary _ _ _ -> text "(" <> formatExpr e <> text ")"
    CEIf _ _ _ _ -> text "(" <> formatExpr e <> text ")"
    CECase _ _ _ -> text "(" <> formatExpr e <> text ")"
    CELet _ _ _ -> text "(" <> formatExpr e <> text ")"
    _ -> formatExpr e


formatLiteral :: CLiteral -> Doc
formatLiteral = \case
    CLInt n -> text (T.pack (Prelude.show n))
    CLDouble d -> text (T.pack (Prelude.show d))
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


formatArm :: CCaseArm -> Doc
formatArm a =
    formatPattern (caseArmPattern a)
        <+> text "->"
        <+> formatExpr (caseArmBody a)


formatBinding :: CBinding -> Doc
formatBinding b =
    text (lowerText (bindingName b))
        <+> text "<-"
        <+> formatExpr (bindingBody b)


formatRecord :: [CFieldBinding] -> Doc
formatRecord fs =
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
    CDAExpr e -> formatExprAtomic e
    CDARecord _ fs -> formatRecord fs
    CDAModifier m -> formatModifier m
    CDAJoinOn _ target pairs ->
        formatExprAtomic target
            <+> text "on"
            <+> text "{ "
            <> hsepCommas (Prelude.fmap formatJoinPair pairs)
            <> text " }"


formatModifier :: CModifier -> Doc
formatModifier = \case
    CMDesc _ n -> text "desc " <> text (lowerText n)
    CMAsc _ n -> text "asc " <> text (lowerText n)
    CMAs _ t -> text "as \"" <> text t <> text "\""
    CMWhere _ e -> text "where " <> formatExpr e
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
    CPParen _ inner -> text "(" <> formatPattern inner <> text ")"


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
            (Prelude.fmap (\l -> text "#' " <> text l <> line)
                (docBlockLines b))



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


lowerText :: CLowerName -> Text
lowerText = lowerNameText


upperText :: CUpperName -> Text
upperText = upperNameText
