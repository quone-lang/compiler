{-| Symbol index used by the LSP server's hover, definition, and
completion handlers.

Walks a typed program once and produces a flat list of every
top-level binding, type, and import the document defines, with
their declaration spans, optional doc blocks, and inferred type
schemes. The handlers in 'Quone.Lsp.Handlers' look up against this
index by 0-based LSP line/character.

-}
module Quone.Lsp.Symbols
    ( Symbol (..)
    , SymbolKind (..)
    , buildIndex
    , symbolAt
    , byPrefix
    , renderType
    , renderScheme
    , renderSignature
    )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import qualified Quone.Ast.Source as Ast
import qualified Quone.Position as Position
import qualified Quone.Type.Infer as Infer
import qualified Quone.Type.Types as Ty
import qualified Prelude



data SymbolKind
    = SymValue
    | SymType
    | SymConstructor
    | SymImport
    | SymForeign
    deriving (Prelude.Show, Prelude.Eq)


data Symbol = Symbol
    { symName :: Text
    , symKind :: SymbolKind
    , symSpan :: Position.SourceSpan
    , symDoc :: [Text]
    , symType :: Maybe Ty.Scheme
    }
    deriving (Prelude.Show, Prelude.Eq)


-- | Build the symbol index for one typed program.
buildIndex :: Ast.Program -> Infer.TypedProgram -> [Symbol]
buildIndex prog typed =
    Prelude.concatMap (declSymbols (Infer.typedBindings typed))
        (Ast.programDecls prog)


declSymbols :: Map.Map Text Ty.Scheme -> Ast.Decl -> [Symbol]
declSymbols binds = \case
    Ast.DValue v ->
        let
            name = Ast.lowerText (Ast.valueDeclName v)
        in
        [ Symbol
            { symName = name
            , symKind = SymValue
            , symSpan = Ast.lowerSpan (Ast.valueDeclName v)
            , symDoc = docLines_ (Ast.valueDeclDoc v)
            , symType = Map.lookup name binds
            }
        ]
    Ast.DType td ->
        Symbol
            { symName = Ast.upperText (Ast.typeDeclName td)
            , symKind = SymType
            , symSpan = Ast.upperSpan (Ast.typeDeclName td)
            , symDoc = docLines_ (Ast.typeDeclDoc td)
            , symType = Prelude.Nothing
            }
            : Prelude.fmap variantSymbol (Ast.typeDeclVariants td)
    Ast.DTypeAlias a ->
        [ Symbol
            { symName = Ast.upperText (Ast.aliasDeclName a)
            , symKind = SymType
            , symSpan = Ast.upperSpan (Ast.aliasDeclName a)
            , symDoc = docLines_ (Ast.aliasDeclDoc a)
            , symType = Prelude.Nothing
            }
        ]
    Ast.DImport (Ast.QuoneImport sp _ _) ->
        [ Symbol
            { symName = "<import>"
            , symKind = SymImport
            , symSpan = sp
            , symDoc = []
            , symType = Prelude.Nothing
            }
        ]
    Ast.DImport (Ast.ForeignImport sp _ f _) ->
        [ Symbol
            { symName = Ast.lowerText (Ast.foreignFn f)
            , symKind = SymForeign
            , symSpan = sp
            , symDoc = []
            , symType = Map.lookup (Ast.lowerText (Ast.foreignFn f)) binds
            }
        ]
    Ast.DExtern (Ast.ExternValue sp _ name _ _ mDoc) ->
        let
            nm = Ast.lowerText name
        in
        [ Symbol
            { symName = nm
            , symKind = SymValue
            , symSpan = sp
            , symDoc = docLines_ mDoc
            , symType = Map.lookup nm binds
            }
        ]
    Ast.DExtern (Ast.ExternType sp name _ mDoc) ->
        [ Symbol
            { symName = Ast.upperText name
            , symKind = SymType
            , symSpan = sp
            , symDoc = docLines_ mDoc
            , symType = Prelude.Nothing
            }
        ]
    Ast.DInfix _ ->
        -- Operator overloads are typed-only; the LSP surfaces the
        -- operator at use sites via the parser's 'EBinOp' span.
        []
    Ast.DPrefix _ ->
        []


variantSymbol :: Ast.Variant -> Symbol
variantSymbol v =
    Symbol
        { symName = Ast.upperText (Ast.variantName v)
        , symKind = SymConstructor
        , symSpan = Ast.upperSpan (Ast.variantName v)
        , symDoc = []
        , symType = Prelude.Nothing
        }


docLines_ :: Maybe Ast.DocBlock -> [Text]
docLines_ = \case
    Prelude.Nothing -> []
    Just b -> Ast.docLines b


-- | Look up the symbol whose span contains the given (0-based) LSP
-- position. Returns the smallest enclosing symbol, or 'Nothing' if no
-- symbol covers the position.
--
-- LSP positions are 0-based (line and character); the compiler's
-- 'Position.SourcePos' is 1-based.
symbolAt :: Int -> Int -> [Symbol] -> Maybe Symbol
symbolAt lspLine lspCol symbols =
    let
        line = lspLine Prelude.+ 1
        col = lspCol Prelude.+ 1
        contains s =
            let
                Position.SourceSpan start finish = symSpan s
            in
            Position.posLine start Prelude.<= line
                Prelude.&& line Prelude.<= Position.posLine finish
                Prelude.&& Position.posCol start Prelude.<= col
                Prelude.&& col Prelude.<= Position.posCol finish
    in
    case Prelude.filter contains symbols of
        [] -> Prelude.Nothing
        ss ->
            -- pick the symbol with the narrowest span
            Just (List.minimumBy compareSpan ss)


compareSpan :: Symbol -> Symbol -> Prelude.Ordering
compareSpan a b =
    let
        widthOf s =
            let
                Position.SourceSpan st en = symSpan s
            in
            ( Position.posLine en Prelude.- Position.posLine st
            , Position.posCol en Prelude.- Position.posCol st
            )
    in
    Prelude.compare (widthOf a) (widthOf b)


-- | Filter symbols by case-insensitive name prefix; used by the
-- completion handler.
byPrefix :: Text -> [Symbol] -> [Symbol]
byPrefix p =
    let
        needle = T.toLower p
    in
    Prelude.filter (\s -> needle `T.isPrefixOf` T.toLower (symName s))


-- ---------------------------------------------------------------------
-- Type rendering for hover
-- ---------------------------------------------------------------------


renderScheme :: Ty.Scheme -> Text
renderScheme s =
    renderType (Ty.schemeBody s)


renderSignature :: Text -> Ty.Scheme -> Text
renderSignature name scheme =
    let
        body =
            Ty.schemeBody scheme
    in
    if typeNeedsBlock body
        then
            name
                ++ " :"
                ++ "\n"
                ++ indentLines 4 (renderTypeBlock body)
        else
            name ++ " : " ++ renderType body


tyVarName :: Ty.TyVar -> Text
tyVarName = Ty.tyVarName


-- | Pretty-print a type the way the user wrote it in source.
renderType :: Ty.Type -> Text
renderType = renderTypePrec 0


renderTypePrec :: Int -> Ty.Type -> Text
renderTypePrec prec = \case
    Ty.TyVarT tv -> tyVarName tv
    Ty.TyCon n -> n
    Ty.TyApp f x ->
        let
            inner = renderTypePrec 11 f ++ " " ++ renderTypePrec 11 x
        in
        if prec Prelude.> 10 then "(" ++ inner ++ ")" else inner
    Ty.TyFun a b ->
        let
            inner = renderTypePrec 1 a ++ " -> " ++ renderTypePrec 0 b
        in
        if prec Prelude.> 0 then "(" ++ inner ++ ")" else inner
    Ty.TyRecord fs -> "{ " ++ renderRecord fs ++ " }"
    Ty.TyDataframe shape ->
        let
            base = "dataframe { " ++ renderRecord (Ty.dfSchema shape) ++ " }"
            grouping = case Ty.dfGroupingCols shape of
                [] -> ""
                gs -> " grouped by " ++ T.intercalate ", " gs
        in
        base ++ grouping


renderRecord :: Map.Map Text Ty.Type -> Text
renderRecord m =
    T.intercalate ", "
        (Prelude.fmap
            (\(k, v) -> k ++ " : " ++ renderType v)
            (Map.toAscList m))


typeNeedsBlock :: Ty.Type -> Prelude.Bool
typeNeedsBlock = \case
    Ty.TyFun a b -> typeNeedsBlock a Prelude.|| typeNeedsBlock b
    Ty.TyApp f x -> typeNeedsBlock f Prelude.|| typeNeedsBlock x
    Ty.TyRecord fs -> recordNeedsBlock fs
    Ty.TyDataframe shape -> recordNeedsBlock (Ty.dfSchema shape)
    _ -> Prelude.False


recordNeedsBlock :: Map.Map Text Ty.Type -> Prelude.Bool
recordNeedsBlock fields =
    Prelude.length (Map.toList fields) Prelude.> 1
        Prelude.|| Prelude.any typeNeedsBlock (Map.elems fields)


renderTypeBlock :: Ty.Type -> Text
renderTypeBlock ty =
    T.intercalate "\n" (renderTypeLines ty)


renderTypeLines :: Ty.Type -> [Text]
renderTypeLines ty = case ty of
    Ty.TyFun _ _ ->
        let
            (args, result) =
                collectFun [] ty
        in
        Prelude.concat
            (Prelude.fmap renderArgLine args)
            Prelude.++ renderTypeLines result
    Ty.TyRecord fs ->
        renderRecordLines fs
    Ty.TyDataframe shape ->
        let
            grouping = case Ty.dfGroupingCols shape of
                [] -> []
                gs -> ["grouped by " ++ T.intercalate ", " gs]
        in
        "dataframe"
            : indentTextLines 4 (renderRecordLines (Ty.dfSchema shape))
            Prelude.++ grouping
    other ->
        [renderType other]
  where
    collectFun :: [Ty.Type] -> Ty.Type -> ([Ty.Type], Ty.Type)
    collectFun acc = \case
        Ty.TyFun a b -> collectFun (acc Prelude.++ [a]) b
        result -> (acc, result)

    renderArgLine arg =
        case renderTypeLines arg of
            [] -> []
            [line] -> [line ++ " ->"]
            lines_ ->
                case Prelude.reverse lines_ of
                    [] -> []
                    lastLine : restRev ->
                        Prelude.reverse restRev Prelude.++ [lastLine ++ " ->"]


renderRecordLines :: Map.Map Text Ty.Type -> [Text]
renderRecordLines fields =
    case Map.toAscList fields of
        [] -> ["{ }"]
        first : rest ->
            let
                firstLines =
                    renderFieldLines "{ " first

                restLines =
                    Prelude.concatMap (renderFieldLines ", ") rest
            in
            firstLines Prelude.++ restLines Prelude.++ ["}"]


renderFieldLines :: Text -> (Text, Ty.Type) -> [Text]
renderFieldLines prefix (name, ty) =
    let
        head_ =
            prefix ++ name ++ " : "
    in
    if typeNeedsBlock ty
        then
            case renderTypeLines ty of
                [] -> [head_]
                first : rest ->
                    (head_ ++ first) : indentTextLines (Prelude.fromIntegral (T.length head_)) rest
        else
            [head_ ++ renderType ty]


indentLines :: Int -> Text -> Text
indentLines n =
    T.intercalate "\n" Prelude.. indentTextLines n Prelude.. T.splitOn "\n"


indentTextLines :: Int -> [Text] -> [Text]
indentTextLines n =
    Prelude.fmap (T.replicate (Prelude.fromIntegral n) " " ++)
