{-| Parser for Quone source files.

Consumes the token stream produced by 'Quone.Lex.Lexer' and produces a
'CProgram'. The grammar implemented here is exactly the EBNF in
LANGUAGE.md section 5.1, including:

* operator precedence and associativity from section 5.2 (right-assoc
  exponent, tight unary minus, left-assoc everything else),
* surface-only @if@ / @then@ / @else@ from section 5.3 (kept as
  'CEIf' here; desugared to a 'CECase' on 'Logical' by the desugar
  pass),
* every dataframe verb from section 3.4,
* both single-name and multi-name imports from section 4.5,
* every literal kind from section 3.3.

The parser is implemented as a small token-stream combinator library
rather than megaparsec to keep dependencies minimal and error messages
explicitly under our control. It runs purely (no IO) and never
throws: every failure surfaces as a 'Diagnostic'.

-}
module Quone.Parse.Parser
    ( parseProgram
    , parseProgramFile
    , runParser
    )
where

import Control.Applicative ((*>), (<*))
import Control.Monad (when)
import qualified Data.List as List
import qualified Data.Text as T
import Data.Functor (($>), (<$>))
import NriPrelude
import Quone.Diagnostic
    ( Category (Parse)
    , Diagnostic (..)
    , Severity (Error)
    )
import Quone.Lex.Lexer (lexFile)
import Quone.Lex.Token
import Quone.Parse.Cst
import Quone.Position (SourcePos (..), SourceSpan (..), spanFromPos, unionSpan)
import qualified Prelude



-- ---------------------------------------------------------------------
-- Top-level entry points
-- ---------------------------------------------------------------------


-- | Parse a named Quone source file into a CST.
parseProgramFile :: Text -> Text -> Prelude.Either Diagnostic CProgram
parseProgramFile filename source =
    case lexFile filename source of
        Prelude.Left err -> Prelude.Left err
        Prelude.Right toks -> runParser filename toks


-- | Convenience for tests: parse from a literal string.
parseProgram :: Text -> Prelude.Either Diagnostic CProgram
parseProgram = parseProgramFile "<input>"


-- | Run the parser over an already-lexed token stream.
runParser :: Text -> [Located Token] -> Prelude.Either Diagnostic CProgram
runParser filename toks =
    case runP pProgram (initState filename toks) of
        POk prog _ -> Prelude.Right prog
        PErr d -> Prelude.Left d



-- ---------------------------------------------------------------------
-- Parser state and combinators
-- ---------------------------------------------------------------------


data State = State
    { stateFile :: Text
    , stateTokens :: [Located Token]
    }


-- | A parser is a function @State -> ParseResult a@.
newtype P a = P {runP :: State -> ParseResult a}


instance Prelude.Functor P where
    fmap f (P run) = P (\s -> case run s of
        POk a s' -> POk (f a) s'
        PErr d -> PErr d)


instance Prelude.Applicative P where
    pure x = P (\s -> POk x s)
    P pf <*> P px = P (\s -> case pf s of
        PErr d -> PErr d
        POk f s' -> case px s' of
            PErr d -> PErr d
            POk x s'' -> POk (f x) s'')


instance Prelude.Monad P where
    P run >>= k = P (\s -> case run s of
        PErr d -> PErr d
        POk a s' -> runP (k a) s')


data ParseResult a
    = POk a State
    | PErr Diagnostic


initState :: Text -> [Located Token] -> State
initState filename toks =
    State {stateFile = filename, stateTokens = toks}


-- | Consume the next token regardless of what it is.
advance :: P (Located Token)
advance = P <| \s ->
    case stateTokens s of
        [] -> PErr (eofDiag (stateFile s))
        (t : rest) -> POk t (s {stateTokens = rest})


-- | Look at the next token without consuming it.
peek :: P (Located Token)
peek = P <| \s ->
    case stateTokens s of
        [] -> PErr (eofDiag (stateFile s))
        (t : _) -> POk t s


-- | Look at the next non-layout (non-newline/indent/dedent) token.
peekSig :: P (Located Token)
peekSig = P <| \s ->
    case dropLayout (stateTokens s) of
        [] -> PErr (eofDiag (stateFile s))
        (t : _) -> POk t s


-- | Skip newlines, indents, and dedents.
--
-- The parser uses 'skipLayout' explicitly between top-level
-- declarations, after annotations, and inside structured forms so
-- newlines are mostly transparent. Within an expression, newlines act
-- as a soft separator: 'peekSig' walks past them, but 'sameLine'
-- checks whether the next significant token is still on the current
-- line, which prevents application from accidentally reaching across
-- declaration boundaries.
skipLayout :: P ()
skipLayout = P <| \s ->
    POk () (s {stateTokens = dropLayout (stateTokens s)})


dropLayout :: [Located Token] -> [Located Token]
dropLayout = Prelude.dropWhile (\lt -> isLayout (locValue lt))
  where
    isLayout = \case
        TNewline -> Prelude.True
        TIndent -> Prelude.True
        TDedent -> Prelude.True
        _ -> Prelude.False


-- | Did we cross a 'TNewline' or 'TDedent' since the last consumed
-- token? Used by application and pipe-continuation to know when an
-- expression ends.
crossedLine :: P Prelude.Bool
crossedLine = P <| \s ->
    let
        prefix =
            Prelude.takeWhile (\lt -> isLayoutTok (locValue lt)) (stateTokens s)
    in
    POk (Prelude.any (\lt -> isHardBreak (locValue lt)) prefix) s
  where
    isLayoutTok = \case
        TNewline -> Prelude.True
        TIndent -> Prelude.True
        TDedent -> Prelude.True
        _ -> Prelude.False
    isHardBreak = \case
        TNewline -> Prelude.True
        TDedent -> Prelude.True
        _ -> Prelude.False


-- | Try @left@; on failure rewind and try @right@. Both branches must
-- agree on the result type.
orElse :: P a -> P a -> P a
orElse (P l) (P r) = P (\s -> case l s of
    POk a s' -> POk a s'
    PErr _ -> r s)


-- | Backtrackable @try@: if @p@ fails, rewind to the original state.
try_ :: P a -> P a
try_ (P run) = P (\s -> case run s of
    POk a s' -> POk a s'
    PErr _ -> PErr (parseFail "" s))


-- | Greedy zero-or-more.
many_ :: P a -> P [a]
many_ p = (some_ p) `orElse` Prelude.pure []


-- | Greedy one-or-more.
some_ :: P a -> P [a]
some_ p = do
    x <- p
    xs <- many_ p
    Prelude.pure (x : xs)


-- | Optional (matches zero or one).
optional_ :: P a -> P (Maybe a)
optional_ p = (Just <$> p) `orElse` Prelude.pure Nothing


-- | Run @body@ separated by @sep@, requiring at least one item.
sepBy1 :: P a -> P b -> P [a]
sepBy1 body sep = do
    x <- body
    xs <- many_ (sep *> body)
    Prelude.pure (x : xs)


-- | Run @body@ separated by @sep@, allowing zero items.
sepBy :: P a -> P b -> P [a]
sepBy body sep = sepBy1 body sep `orElse` Prelude.pure []



-- ---------------------------------------------------------------------
-- Token expectations
-- ---------------------------------------------------------------------


-- | Consume one occurrence of an exact token, failing with a message
-- that names what was expected.
expectTok :: Token -> P SourceSpan
expectTok expected = P <| \s ->
    case dropLayout (stateTokens s) of
        (Located {locValue = t, locSpan = sp} : rest)
            | t Prelude.== expected ->
                POk sp (s {stateTokens = rest})
        toks ->
            PErr (expectedDiag (stateFile s) (showToken expected) toks)


-- | Consume one of a set of keywords.
expectKeyword :: Keyword -> P SourceSpan
expectKeyword kw = expectTok (TKeyword kw)


-- | Match a lowercase identifier.
expectLowerIdent :: P CLowerName
expectLowerIdent = P <| \s ->
    case dropLayout (stateTokens s) of
        (Located {locValue = TLowerIdent t, locSpan = sp} : rest) ->
            POk (CLowerName sp t) (s {stateTokens = rest})
        toks ->
            PErr (expectedDiag (stateFile s) "lowercase identifier" toks)


-- | Match an uppercase identifier (a type name or constructor).
expectUpperIdent :: P CUpperName
expectUpperIdent = P <| \s ->
    case dropLayout (stateTokens s) of
        (Located {locValue = TUpperIdent t, locSpan = sp} : rest) ->
            POk (CUpperName sp t) (s {stateTokens = rest})
        toks ->
            PErr (expectedDiag (stateFile s) "uppercase identifier" toks)


-- | Match a string literal.
expectStringLit :: P (SourceSpan, Text)
expectStringLit = P <| \s ->
    case dropLayout (stateTokens s) of
        (Located {locValue = TStringLit t, locSpan = sp} : rest) ->
            POk (sp, t) (s {stateTokens = rest})
        toks ->
            PErr (expectedDiag (stateFile s) "string literal" toks)


-- | Match an integer literal.
expectIntLit :: P (SourceSpan, Int)
expectIntLit = P <| \s ->
    case dropLayout (stateTokens s) of
        (Located {locValue = TIntLit n, locSpan = sp} : rest) ->
            POk (sp, n) (s {stateTokens = rest})
        toks ->
            PErr (expectedDiag (stateFile s) "integer literal" toks)


-- | Match a floating literal.
expectFloatLit :: P (SourceSpan, Prelude.Double)
expectFloatLit = P <| \s ->
    case dropLayout (stateTokens s) of
        (Located {locValue = TFloatLit n, locSpan = sp} : rest) ->
            POk (sp, n) (s {stateTokens = rest})
        toks ->
            PErr (expectedDiag (stateFile s) "double literal" toks)


-- | Match a doc block immediately preceding a declaration.
expectDocBlock :: P CDocBlock
expectDocBlock = P <| \s ->
    case dropLayout (stateTokens s) of
        (Located {locValue = TDocBlock body, locSpan = sp} : rest) ->
            POk
                ( CDocBlock
                    { docBlockSpan = sp
                    , docBlockLines = T.splitOn "\n" body
                    }
                )
                (s {stateTokens = rest})
        toks ->
            PErr (expectedDiag (stateFile s) "doc block" toks)



-- ---------------------------------------------------------------------
-- Programs and modules
-- ---------------------------------------------------------------------


pProgram :: P CProgram
pProgram = do
    skipLayout
    -- A doc block immediately preceding `module ...` documents the
    -- module itself. We accept it for ergonomics but don't yet wire
    -- it to anywhere; per LANGUAGE.md section 14.6 module-level doc
    -- is `[planned]`. Discarding for v0.0.1 keeps idiomatic R-package
    -- file headers (one-line summary above `module`) round-trippable.
    _ <- optional_ (try_ expectDocBlock)
    skipLayout
    mModule <- optional_ (try_ pModuleDecl)
    skipLayout
    decls <- many_ (pDecl <* skipLayout)
    skipLayout
    _ <- expectTok TEof
    let
        sp = case mModule of
            Just m -> moduleDeclSpan m
            Nothing -> case decls of
                (d : _) -> declSpan d
                [] -> spanFromPos (SourcePos "<input>" 1 1)
    Prelude.pure
        ( CProgram
            { programSpan = sp
            , programModule = mModule
            , programDecls = decls
            }
        )


pModuleDecl :: P CModuleDecl
pModuleDecl = do
    s <- expectKeyword KModule
    path <- pDottedUpper
    _ <- expectKeyword KExporting
    _ <- expectTok TLParen
    list <- pExportList
    e <- expectTok TRParen
    Prelude.pure
        ( CModuleDecl
            { moduleDeclSpan = unionSpan s e
            , moduleDeclPath = path
            , moduleDeclExports = list
            }
        )


pDottedUpper :: P [CUpperName]
pDottedUpper = sepBy1 expectUpperIdent (expectTok TDot)


pExportList :: P CExportList
pExportList =
    (do
        s <- expectTok TDotDot
        Prelude.pure (CExportAll s)
    )
        `orElse` (do
            items <- sepBy pExportItem (expectTok TComma)
            let
                sp = case items of
                    [] -> spanFromPos (SourcePos "<input>" 0 0)
                    [i] -> exportItemSpan i
                    (i : rest) ->
                        Prelude.foldr unionSpan (exportItemSpan i)
                            (Prelude.fmap exportItemSpan rest)
            Prelude.pure (CExportNames sp items)
        )


pExportItem :: P CExportItem
pExportItem =
    (CExportLower <$> expectLowerIdent)
        `orElse` (CExportUpper <$> expectUpperIdent)



-- ---------------------------------------------------------------------
-- Declarations
-- ---------------------------------------------------------------------


pDecl :: P CDecl
pDecl = do
    skipLayout
    mDoc <- optional_ (try_ expectDocBlock)
    skipLayout
    nextSig <- peekSig
    case locValue nextSig of
        TKeyword KType ->
            -- Could be `type` or `type alias`
            (CDTypeAlias <$> pTypeAlias mDoc)
                `orElse` (CDType <$> pTypeDecl mDoc)
        TKeyword KImport ->
            CDImport <$> pImportDecl
        TEof ->
            -- No more declarations: signal end. We use Err here so the
            -- caller's many_ stops.
            P (\s -> PErr (parseFail "no more declarations" s))
        _ ->
            CDValue <$> pValueDecl mDoc


-- | Span helper that reaches into each declaration kind.
declSpan :: CDecl -> SourceSpan
declSpan = \case
    CDType d -> typeDeclSpan d
    CDTypeAlias d -> aliasDeclSpan d
    CDImport d -> case d of
        CQuoneImport sp _ _ -> sp
        CForeignImport sp _ _ -> sp
    CDValue d -> valueDeclSpan d


pTypeDecl :: Maybe CDocBlock -> P CTypeDecl
pTypeDecl mDoc = do
    s <- expectKeyword KType
    name <- expectUpperIdent
    params <- many_ expectLowerIdent
    _ <- expectTok TBind
    first <- pVariant
    rest <- many_ (expectTok TPipeBar *> pVariant)
    let variants = first : rest
    let sp = unionSpan s (variantSpan (Prelude.last variants))
    Prelude.pure
        ( CTypeDecl
            { typeDeclSpan = sp
            , typeDeclName = name
            , typeDeclParams = params
            , typeDeclVariants = variants
            , typeDeclDoc = mDoc
            }
        )


pVariant :: P CVariant
pVariant = do
    name <- expectUpperIdent
    args <- many_ pTypeAtom
    let
        startSpan = upperNameSpan name
        endSpan = case args of
            [] -> startSpan
            xs -> typeAtomSpan (Prelude.last xs)
    Prelude.pure
        ( CVariant
            { variantSpan = unionSpan startSpan endSpan
            , variantName = name
            , variantArgs = args
            }
        )


pTypeAlias :: Maybe CDocBlock -> P CTypeAliasDecl
pTypeAlias mDoc = try_ <| do
    s <- expectKeyword KType
    _ <- expectKeyword KAlias
    name <- expectUpperIdent
    params <- many_ expectLowerIdent
    _ <- expectTok TBind
    body <- pTypeSig
    Prelude.pure
        ( CTypeAliasDecl
            { aliasDeclSpan = unionSpan s (typeSigSpan body)
            , aliasDeclName = name
            , aliasDeclParams = params
            , aliasDeclBody = body
            , aliasDeclDoc = mDoc
            }
        )


pImportDecl :: P CImportDecl
pImportDecl = do
    s <- expectKeyword KImport
    nextSig <- peekSig
    case locValue nextSig of
        TUpperIdent _ -> do
            modulePath <- pDottedUpper
            -- Two shapes: dotted name ending in lower (single import),
            -- or DottedUpper followed by '(' selection ')'.
            nextAfterPath <- peekSig
            case locValue nextAfterPath of
                TLParen -> do
                    _ <- expectTok TLParen
                    sel <- pImportSelectionList
                    e <- expectTok TRParen
                    Prelude.pure (CQuoneImport (unionSpan s e) modulePath sel)
                TDot -> do
                    -- single name: keep eating dots until we hit a
                    -- lowercase or uppercase final segment
                    _ <- expectTok TDot
                    finalE <- pExportItem
                    let
                        eSp = exportItemSpan finalE
                    Prelude.pure
                        ( CQuoneImport
                            (unionSpan s eSp)
                            modulePath
                            (CImportSingle finalE)
                        )
                _ ->
                    -- Path is just one Upper: treat the last as the
                    -- imported name itself (constructor / type), and
                    -- the prefix as the module path.
                    case modulePath of
                        [_] ->
                            -- Single Upper segment with no dot: this is
                            -- an import of a top-level type from itself,
                            -- which we don't currently support; report
                            -- a useful error.
                            P (\st -> PErr (parseFail "import requires either '.' for a single name or '(...)' for a list" st))
                        _ ->
                            let
                                lastUpper = Prelude.last modulePath
                                modPath = Prelude.init modulePath
                            in
                            Prelude.pure
                                ( CQuoneImport
                                    (unionSpan s (upperNameSpan lastUpper))
                                    modPath
                                    (CImportSingle (CExportUpper lastUpper))
                                )
        TLowerIdent _ -> do
            (fpath, fnName) <- pDottedLower
            _ <- expectTok TColon
            sig <- pTypeSig
            Prelude.pure
                ( CForeignImport
                    (unionSpan s (typeSigSpan sig))
                    ( CForeignName
                        { foreignNameSpan = unionSpan (lowerNameSpan fnName) s
                        , foreignNamePackage = fpath
                        , foreignNameFn = fnName
                        }
                    )
                    sig
                )
        _ ->
            P (\st -> PErr (parseFail "expected module path or foreign function name after 'import'" st))


pDottedLower :: P ([CLowerName], CLowerName)
pDottedLower = do
    first <- expectLowerIdent
    rest <- many_ (try_ (expectTok TDot *> expectLowerIdent))
    case rest of
        [] -> Prelude.pure ([], first)
        xs -> Prelude.pure (first : Prelude.init xs, Prelude.last xs)


pImportSelectionList :: P CImportSelection
pImportSelectionList =
    (do
        _ <- expectTok TDotDot
        Prelude.pure CImportAll
    )
        `orElse` (do
            items <- sepBy pExportItem (expectTok TComma)
            Prelude.pure (CImportNames items)
        )


pValueDecl :: Maybe CDocBlock -> P CValueDecl
pValueDecl mDoc = do
    -- Optional type annotation: `name : TypeSig` on its own line,
    -- followed by `name params <- body` on the next.
    skipLayout
    nextSig <- peekSig
    case locValue nextSig of
        TLowerIdent nm -> do
            -- Look ahead to see if this is an annotation (followed by ':')
            mAnn <- optional_ (try_ pAnnotation)
            skipLayout
            -- After an annotation, the definition's name must follow on
            -- a subsequent (de-dented) line. Skip any layout tokens.
            skipLayout
            (name, params, body, sp) <- pValueDef
            -- If both annotation and definition share a name, fine; if
            -- not, the desugar pass will catch it.
            _ <- Prelude.pure nm
            Prelude.pure
                ( CValueDecl
                    { valueDeclSpan = sp
                    , valueDeclAnnotation = mAnn
                    , valueDeclName = name
                    , valueDeclParams = params
                    , valueDeclBody = body
                    , valueDeclDoc = mDoc
                    }
                )
        _ ->
            P (\st -> PErr (parseFail "expected a value declaration" st))


pAnnotation :: P CTypeSig
pAnnotation = do
    _ <- expectLowerIdent
    _ <- expectTok TColon
    pTypeSig


pValueDef :: P (CLowerName, [CLowerName], CExpr, SourceSpan)
pValueDef = do
    name <- expectLowerIdent
    params <- many_ expectLowerIdent
    _ <- expectTok TBind
    body <- pExpr
    Prelude.pure
        ( name
        , params
        , body
        , unionSpan (lowerNameSpan name) (exprSpan body)
        )



-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------


pTypeSig :: P CTypeSig
pTypeSig = do
    left <- pTypeApp
    mArrow <- optional_ (try_ (expectTok TArrow))
    case mArrow of
        Just _ -> do
            right <- pTypeSig
            Prelude.pure (CTFun (unionSpan (typeAppSpan left) (typeSigSpan right)) left right)
        Nothing -> Prelude.pure left


pTypeApp :: P CTypeSig
pTypeApp = do
    head_ <- pTypeAtom
    args <- many_ (try_ <| do
        crossed <- crossedLine
        when crossed
            (P (\st -> PErr (parseFail "" st)))
        pTypeAtom
        )
    case args of
        [] -> Prelude.pure (CTAtom head_)
        _ -> Prelude.pure
            ( CTApp
                (unionSpan (typeAtomSpan head_) (typeAtomSpan (Prelude.last args)))
                head_
                args
            )


pTypeAtom :: P CTypeAtom
pTypeAtom = do
    nextSig <- peekSig
    case locValue nextSig of
        TUpperIdent _ -> CTName <$> expectUpperIdent
        TLowerIdent _ -> CTVar <$> expectLowerIdent
        TLParen -> do
            s <- expectTok TLParen
            inner <- pTypeSig
            e <- expectTok TRParen
            Prelude.pure (CTParen (unionSpan s e) inner)
        TLBrace -> do
            rt <- pRecordType
            Prelude.pure (CTRecord rt)
        TKeyword KDataframe -> do
            s <- expectKeyword KDataframe
            rt <- pRecordType
            Prelude.pure (CTDataframe (unionSpan s (recordTypeSpan rt)) rt)
        _ ->
            P (\st -> PErr (parseFail "expected a type atom" st))


pRecordType :: P CRecordType
pRecordType = do
    s <- expectTok TLBrace
    fields <- sepBy pFieldType (expectTok TComma)
    e <- expectTok TRBrace
    Prelude.pure
        ( CRecordType
            { recordTypeSpan = unionSpan s e
            , recordTypeFields = fields
            }
        )


pFieldType :: P CFieldType
pFieldType = do
    name <- expectLowerIdent
    _ <- expectTok TColon
    sig <- pTypeSig
    Prelude.pure
        ( CFieldType
            { fieldTypeSpan = unionSpan (lowerNameSpan name) (typeSigSpan sig)
            , fieldTypeName = name
            , fieldTypeSig = sig
            }
        )



-- ---------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------


pExpr :: P CExpr
pExpr = pPipe


-- Precedence ladder per LANGUAGE.md section 5.2 (low to high binding):
-- 1 pipe |>
-- 2 comparison
-- 3 additive + -
-- 4 multiplicative * / // %
-- 5 exponentiation ^   (right-associative)
-- 6 unary minus
-- 7 application
-- 8 field access
-- 9 primary

pPipe :: P CExpr
pPipe = do
    left <- pCmp
    -- Multi-line pipelines are common (LANGUAGE.md section 9.1's
    -- examples), so we DO allow `|>` continuations across lines.
    -- Per-iteration we still bail when the next significant token is
    -- not `|>`, which keeps the next top-level decl from being eaten.
    rest <- many_ (try_ <| do
        nextSig <- peekSig
        case locValue nextSig of
            TPipe -> do
                _ <- expectTok TPipe
                pCmp
            _ -> P (\st -> PErr (parseFail "" st)))
    Prelude.pure (Prelude.foldl
        (\acc r -> CEPipe (unionSpan (exprSpan acc) (exprSpan r)) acc r)
        left
        rest)


pCmp :: P CExpr
pCmp = do
    left <- pAdd
    rest <-
        many_
            ( try_ <| do
                op <- pCmpOp
                r <- pAdd
                Prelude.pure (op, r)
            )
    Prelude.pure (Prelude.foldl
        (\acc (op, r) -> CEBinOp (unionSpan (exprSpan acc) (exprSpan r)) op acc r)
        left
        rest)


pCmpOp :: P CBinOp
pCmpOp = do
    nextSig <- peekSig
    case locValue nextSig of
        TEq -> expectTok TEq $> COpEq
        TNeq -> expectTok TNeq $> COpNeq
        TGt -> expectTok TGt $> COpGt
        TLt -> expectTok TLt $> COpLt
        TGe -> expectTok TGe $> COpGe
        TLe -> expectTok TLe $> COpLe
        _ -> P (\st -> PErr (parseFail "expected a comparison operator" st))


pAdd :: P CExpr
pAdd = do
    left <- pMul
    rest <-
        many_
            ( try_ <| do
                op <- pAddOp
                r <- pMul
                Prelude.pure (op, r)
            )
    Prelude.pure (Prelude.foldl
        (\acc (op, r) -> CEBinOp (unionSpan (exprSpan acc) (exprSpan r)) op acc r)
        left
        rest)


pAddOp :: P CBinOp
pAddOp = do
    nextSig <- peekSig
    case locValue nextSig of
        TPlus -> expectTok TPlus $> COpAdd
        TMinus -> expectTok TMinus $> COpSub
        _ -> P (\st -> PErr (parseFail "expected '+' or '-'" st))


pMul :: P CExpr
pMul = do
    left <- pExp
    rest <-
        many_
            ( try_ <| do
                op <- pMulOp
                r <- pExp
                Prelude.pure (op, r)
            )
    Prelude.pure (Prelude.foldl
        (\acc (op, r) -> CEBinOp (unionSpan (exprSpan acc) (exprSpan r)) op acc r)
        left
        rest)


pMulOp :: P CBinOp
pMulOp = do
    nextSig <- peekSig
    case locValue nextSig of
        TStar -> expectTok TStar $> COpMul
        TSlash -> expectTok TSlash $> COpDiv
        TIntDiv -> expectTok TIntDiv $> COpIntDiv
        TPercent -> expectTok TPercent $> COpMod
        _ -> P (\st -> PErr (parseFail "expected '*', '/', '//', or '%'" st))


pExp :: P CExpr
pExp = do
    left <- pUnary
    mRest <- optional_ (try_ (expectTok TCaret *> pExp))
    case mRest of
        Just right ->
            -- Right-associative.
            Prelude.pure (CEBinOp (unionSpan (exprSpan left) (exprSpan right)) COpExp left right)
        Nothing -> Prelude.pure left


pUnary :: P CExpr
pUnary = do
    nextSig <- peekSig
    case locValue nextSig of
        TMinus -> do
            s <- expectTok TMinus
            inner <- pUnary
            Prelude.pure (CEUnary (unionSpan s (exprSpan inner)) COpNeg inner)
        _ -> pApp


pApp :: P CExpr
pApp = do
    head_ <- pAccess
    args <- many_ (try_ <| do
        -- Stop at hard line breaks so an application doesn't reach
        -- across declaration boundaries.
        crossed <- crossedLine
        when crossed
            (P (\st -> PErr (parseFail "" st)))
        pAccess
        )
    Prelude.pure (Prelude.foldl
        (\f x -> CEApp (unionSpan (exprSpan f) (exprSpan x)) f x)
        head_
        args)


pAccess :: P CExpr
pAccess = do
    base <- pPrimary
    fields <- many_ (try_ (expectTok TDot *> expectLowerIdent))
    Prelude.pure (Prelude.foldl
        (\acc f -> CEField (unionSpan (exprSpan acc) (lowerNameSpan f)) acc f)
        base
        fields)


pPrimary :: P CExpr
pPrimary = do
    nextSig <- peekSig
    case locValue nextSig of
        TIntLit n ->
            (\(sp, _) -> CELit sp (CLInt n)) <$> expectIntLit
        TFloatLit f ->
            (\(sp, _) -> CELit sp (CLDouble f)) <$> expectFloatLit
        TStringLit s ->
            (\(sp, t) -> CELit sp (CLChar t)) <$> expectStringLit
        TLowerIdent _ -> CEVar <$> expectLowerIdent
        TUpperIdent _ -> CECon <$> expectUpperIdent
        TLParen -> do
            s <- expectTok TLParen
            inner <- pExpr
            e <- expectTok TRParen
            Prelude.pure (CEParen (unionSpan s e) inner)
        TLBracket -> pVectorLit
        TLBrace -> pRecordLitOrUpdate
        TKeyword KDataframe -> pDataframeLit
        TKeyword KIf -> pIf
        TKeyword KCase -> pCase
        TKeyword KLet -> pLet
        TBackslash -> pLambda
        TKeyword kw | isVerb kw -> pVerb kw
        _ -> P (\st -> PErr (parseFail "expected expression" st))


isVerb :: Keyword -> Prelude.Bool
isVerb k = k `Prelude.elem`
    [ KSelect, KFilter, KMutate, KSummarize, KGroupBy, KUngroup
    , KArrange, KRename, KDistinct, KDistinctAll, KCount, KSlice
    , KPull, KRelocate, KTransmute, KMutateEach, KSummarizeEach
    , KLeftJoin, KRightJoin, KInnerJoin, KFullJoin, KAntiJoin
    , KSemiJoin, KCrossJoin
    ]


pLambda :: P CExpr
pLambda = do
    s <- expectTok TBackslash
    params <- some_ expectLowerIdent
    _ <- expectTok TArrow
    body <- pExpr
    Prelude.pure (CELambda (unionSpan s (exprSpan body)) params body)


pIf :: P CExpr
pIf = do
    s <- expectKeyword KIf
    cond <- pExpr
    _ <- expectKeyword KThen
    th <- pExpr
    _ <- expectKeyword KElse
    el <- pExpr
    Prelude.pure (CEIf (unionSpan s (exprSpan el)) cond th el)


pCase :: P CExpr
pCase = do
    s <- expectKeyword KCase
    scrutinee <- pExpr
    _ <- expectKeyword KOf
    arms <- some_ pCaseArm
    Prelude.pure
        ( CECase
            (unionSpan s (caseArmSpan (Prelude.last arms)))
            scrutinee
            arms
        )


pCaseArm :: P CCaseArm
pCaseArm = do
    skipLayout
    pat <- pPattern
    _ <- expectTok TArrow
    body <- pExpr
    Prelude.pure
        ( CCaseArm
            { caseArmSpan = unionSpan (patternSpan pat) (exprSpan body)
            , caseArmPattern = pat
            , caseArmBody = body
            }
        )


pLet :: P CExpr
pLet = do
    s <- expectKeyword KLet
    bindings <- some_ pBinding
    _ <- expectKeyword KIn
    body <- pExpr
    Prelude.pure (CELet (unionSpan s (exprSpan body)) bindings body)


pBinding :: P CBinding
pBinding = do
    skipLayout
    name <- expectLowerIdent
    _ <- expectTok TBind
    body <- pExpr
    Prelude.pure
        ( CBinding
            { bindingSpan = unionSpan (lowerNameSpan name) (exprSpan body)
            , bindingName = name
            , bindingBody = body
            }
        )


pVectorLit :: P CExpr
pVectorLit = do
    s <- expectTok TLBracket
    items <- sepBy pExpr (expectTok TComma)
    e <- expectTok TRBracket
    Prelude.pure (CEVector (unionSpan s e) items)


pRecordLitOrUpdate :: P CExpr
pRecordLitOrUpdate = do
    s <- expectTok TLBrace
    -- Try update form first: `expr | field = value, ...`
    mUpdate <- optional_ (try_ pRecordUpdateBody)
    case mUpdate of
        Just (target, fields) -> do
            e <- expectTok TRBrace
            Prelude.pure (CERecordUpdate (unionSpan s e) target fields)
        Nothing -> do
            fields <- sepBy pFieldBinding (expectTok TComma)
            e <- expectTok TRBrace
            Prelude.pure (CERecord (unionSpan s e) fields)


pRecordUpdateBody :: P (CExpr, [CFieldBinding])
pRecordUpdateBody = do
    target <- pExpr
    _ <- expectTok TPipeBar
    fields <- sepBy pFieldBinding (expectTok TComma)
    when (Prelude.null fields)
        (P (\st -> PErr (parseFail "record update needs at least one field" st)))
    Prelude.pure (target, fields)


pFieldBinding :: P CFieldBinding
pFieldBinding = do
    skipLayout
    name <- expectLowerIdent
    _ <- expectTok TAssign
    value <- pExpr
    Prelude.pure
        ( CFieldBinding
            { fieldBindingSpan = unionSpan (lowerNameSpan name) (exprSpan value)
            , fieldBindingName = name
            , fieldBindingValue = value
            }
        )


pDataframeLit :: P CExpr
pDataframeLit = do
    s <- expectKeyword KDataframe
    _ <- expectTok TLBrace
    fields <- sepBy pFieldBinding (expectTok TComma)
    e <- expectTok TRBrace
    Prelude.pure (CEDataframe (unionSpan s e) fields)


-- | Recognise @(desc col)@ / @(asc col)@ inside a verb argument
-- position. Returns Nothing if the parens contain anything else.
pParenthesizedModifier :: P CModifier
pParenthesizedModifier = do
    s <- expectTok TLParen
    nextSig <- peekSig
    m <- case locValue nextSig of
        TKeyword KDesc -> do
            _ <- expectKeyword KDesc
            col <- expectLowerIdent
            Prelude.pure (CMDesc (unionSpan s (lowerNameSpan col)) col)
        TKeyword KAsc -> do
            _ <- expectKeyword KAsc
            col <- expectLowerIdent
            Prelude.pure (CMAsc (unionSpan s (lowerNameSpan col)) col)
        _ -> P (\st -> PErr (parseFail "" st))
    _ <- expectTok TRParen
    Prelude.pure m


pVerb :: Keyword -> P CExpr
pVerb kw = do
    s <- expectKeyword kw
    args <- many_ (try_ <| do
        crossed <- crossedLine
        when crossed (P (\st -> PErr (parseFail "" st)))
        pDplyrArg)
    let
        sp = case args of
            [] -> s
            xs -> unionSpan s (dplyrArgSpan (Prelude.last xs))
    Prelude.pure (CEVerb sp kw args)


pDplyrArg :: P CDplyrArg
pDplyrArg = do
    nextSig <- peekSig
    case locValue nextSig of
        TKeyword KDesc -> do
            s <- expectKeyword KDesc
            col <- expectLowerIdent
            Prelude.pure (CDAModifier (CMDesc (unionSpan s (lowerNameSpan col)) col))
        TKeyword KAsc -> do
            s <- expectKeyword KAsc
            col <- expectLowerIdent
            Prelude.pure (CDAModifier (CMAsc (unionSpan s (lowerNameSpan col)) col))
        TLParen -> do
            -- Either a parenthesised modifier (e.g. `(desc score)`) or
            -- a parenthesised expression. Modifiers win when they fit.
            mMod <- optional_ (try_ pParenthesizedModifier)
            case mMod of
                Just m -> Prelude.pure (CDAModifier m)
                Nothing -> CDAExpr <$> pAccess
        TKeyword KAs -> do
            s <- expectKeyword KAs
            (sp, t) <- expectStringLit
            Prelude.pure (CDAModifier (CMAs (unionSpan s sp) t))
        TKeyword KWhere -> do
            s <- expectKeyword KWhere
            _ <- expectTok TLParen
            inner <- pExpr
            e <- expectTok TRParen
            Prelude.pure (CDAModifier (CMWhere (unionSpan s e) inner))
        TKeyword KCols -> do
            s <- expectKeyword KCols
            _ <- expectTok TLParen
            _ <- expectTok TLBrace
            cols <- sepBy expectLowerIdent (expectTok TComma)
            _ <- expectTok TRBrace
            e <- expectTok TRParen
            Prelude.pure (CDAModifier (CMCols (unionSpan s e) cols))
        TLBrace -> do
            s <- expectTok TLBrace
            -- Could be either { name } records, { col } column lists, or
            -- { lhs = rhs, ... } join targets. We accept all as record
            -- bindings or bare names; resolution decides later.
            mFirst <- optional_ (try_ pFieldBinding)
            case mFirst of
                Just first -> do
                    rest <- many_ (try_ (expectTok TComma *> pFieldBinding))
                    e <- expectTok TRBrace
                    Prelude.pure (CDARecord (unionSpan s e) (first : rest))
                Nothing -> do
                    -- bare name list: { name, score, ... }
                    names <- sepBy expectLowerIdent (expectTok TComma)
                    e <- expectTok TRBrace
                    let
                        fields =
                            Prelude.fmap
                                (\n -> CFieldBinding
                                    (lowerNameSpan n)
                                    n
                                    (CEVar n)
                                )
                                names
                    Prelude.pure (CDARecord (unionSpan s e) fields)
        _ ->
            CDAExpr <$> pAccess



-- ---------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------


pPattern :: P CPattern
pPattern = do
    nextSig <- peekSig
    case locValue nextSig of
        TUnderscore -> do
            sp <- expectTok TUnderscore
            Prelude.pure (CPWildcard sp)
        TLowerIdent _ -> CPVar <$> expectLowerIdent
        TIntLit n -> do
            (sp, _) <- expectIntLit
            Prelude.pure (CPLit sp (CLInt n))
        TFloatLit f -> do
            (sp, _) <- expectFloatLit
            Prelude.pure (CPLit sp (CLDouble f))
        TStringLit _ -> do
            (sp, t) <- expectStringLit
            Prelude.pure (CPLit sp (CLChar t))
        TUpperIdent _ -> do
            con <- expectUpperIdent
            args <- many_ (try_ pPatternAtom)
            let
                sp = case args of
                    [] -> upperNameSpan con
                    xs -> unionSpan (upperNameSpan con) (patternSpan (Prelude.last xs))
            Prelude.pure (CPCon sp con args)
        TLParen -> do
            s <- expectTok TLParen
            inner <- pPattern
            e <- expectTok TRParen
            Prelude.pure (CPParen (unionSpan s e) inner)
        TLBrace -> do
            s <- expectTok TLBrace
            fields <- sepBy pRecordPatField (expectTok TComma)
            e <- expectTok TRBrace
            Prelude.pure (CPRecord (unionSpan s e) fields)
        _ ->
            P (\st -> PErr (parseFail "expected pattern" st))


pPatternAtom :: P CPattern
pPatternAtom = do
    -- Patterns inside a constructor application don't recurse into more
    -- constructor arguments (those bind tighter via parenthesization).
    nextSig <- peekSig
    case locValue nextSig of
        TUpperIdent _ -> do
            con <- expectUpperIdent
            Prelude.pure (CPCon (upperNameSpan con) con [])
        _ -> pPattern


pRecordPatField :: P CRecordPatField
pRecordPatField = do
    name <- expectLowerIdent
    mEq <- optional_ (try_ (expectTok TAssign))
    case mEq of
        Nothing -> Prelude.pure (CRpfShort name)
        Just _ -> do
            inner <- pPattern
            Prelude.pure
                ( CRpfFull
                    (unionSpan (lowerNameSpan name) (patternSpan inner))
                    name
                    inner
                )



-- ---------------------------------------------------------------------
-- Span helpers
-- ---------------------------------------------------------------------


typeAtomSpan :: CTypeAtom -> SourceSpan
typeAtomSpan = \case
    CTName n -> upperNameSpan n
    CTVar n -> lowerNameSpan n
    CTParen sp _ -> sp
    CTRecord r -> recordTypeSpan r
    CTDataframe sp _ -> sp


typeAppSpan :: CTypeSig -> SourceSpan
typeAppSpan = typeSigSpan


typeSigSpan :: CTypeSig -> SourceSpan
typeSigSpan = \case
    CTFun sp _ _ -> sp
    CTApp sp _ _ -> sp
    CTAtom a -> typeAtomSpan a


exprSpan :: CExpr -> SourceSpan
exprSpan = \case
    CELit sp _ -> sp
    CEVar n -> lowerNameSpan n
    CECon n -> upperNameSpan n
    CELambda sp _ _ -> sp
    CEIf sp _ _ _ -> sp
    CECase sp _ _ -> sp
    CELet sp _ _ -> sp
    CEApp sp _ _ -> sp
    CEBinOp sp _ _ _ -> sp
    CEUnary sp _ _ -> sp
    CEPipe sp _ _ -> sp
    CEField sp _ _ -> sp
    CEParen sp _ -> sp
    CERecord sp _ -> sp
    CERecordUpdate sp _ _ -> sp
    CEDataframe sp _ -> sp
    CEVector sp _ -> sp
    CEVerb sp _ _ -> sp


patternSpan :: CPattern -> SourceSpan
patternSpan = \case
    CPWildcard sp -> sp
    CPVar n -> lowerNameSpan n
    CPLit sp _ -> sp
    CPCon sp _ _ -> sp
    CPRecord sp _ -> sp
    CPParen sp _ -> sp


dplyrArgSpan :: CDplyrArg -> SourceSpan
dplyrArgSpan = \case
    CDAExpr e -> exprSpan e
    CDARecord sp _ -> sp
    CDAModifier m -> modifierSpan m
    CDAJoinOn sp _ _ -> sp


modifierSpan :: CModifier -> SourceSpan
modifierSpan = \case
    CMDesc sp _ -> sp
    CMAsc sp _ -> sp
    CMAs sp _ -> sp
    CMWhere sp _ -> sp
    CMCols sp _ -> sp


exportItemSpan :: CExportItem -> SourceSpan
exportItemSpan = \case
    CExportLower n -> lowerNameSpan n
    CExportUpper n -> upperNameSpan n



-- ---------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------


eofDiag :: Text -> Diagnostic
eofDiag filename =
    Diagnostic
        { diagSeverity = Error
        , diagCategory = Parse
        , diagSpan = spanFromPos (SourcePos filename 0 0)
        , diagMessage = "unexpected end of input"
        , diagHint = Nothing
        }


parseFail :: Text -> State -> Diagnostic
parseFail msg s =
    let
        sp = case dropLayout (stateTokens s) of
            (Located {locSpan = sp_} : _) -> sp_
            _ -> spanFromPos (SourcePos (stateFile s) 0 0)
    in
    Diagnostic
        { diagSeverity = Error
        , diagCategory = Parse
        , diagSpan = sp
        , diagMessage =
            if T.null msg then "parse error" else msg
        , diagHint = Nothing
        }


expectedDiag :: Text -> Text -> [Located Token] -> Diagnostic
expectedDiag filename what toks =
    let
        sp = case toks of
            (Located {locSpan = sp_} : _) -> sp_
            _ -> spanFromPos (SourcePos filename 0 0)
        actualMsg = case toks of
            (Located {locValue = t} : _) -> showToken t
            _ -> "end of input"
    in
    Diagnostic
        { diagSeverity = Error
        , diagCategory = Parse
        , diagSpan = sp
        , diagMessage =
            "expected " ++ what ++ "; found " ++ actualMsg
        , diagHint = Nothing
        }
