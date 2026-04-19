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
    Ast.DImport (Ast.ForeignImport sp f _) ->
        [ Symbol
            { symName = Ast.lowerText (Ast.foreignFn f)
            , symKind = SymForeign
            , symSpan = sp
            , symDoc = []
            , symType = Map.lookup (Ast.lowerText (Ast.foreignFn f)) binds
            }
        ]


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
    let
        body = renderType (Ty.schemeBody s)
        vs = Ty.schemeVars s
    in
    case vs of
        [] -> body
        _ -> "forall " ++ T.intercalate " " (Prelude.fmap tyVarName vs) ++ ". " ++ body


tyVarName :: Ty.TyVar -> Text
tyVarName tv = T.pack (Prelude.show tv)


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
            inner = renderTypePrec 11 a ++ " -> " ++ renderTypePrec 0 b
        in
        if prec Prelude.> 0 then "(" ++ inner ++ ")" else inner
    Ty.TyRecord fs -> "{ " ++ renderRecord fs ++ " }"
    Ty.TyDataframe fs -> "dataframe { " ++ renderRecord fs ++ " }"


renderRecord :: Map.Map Text Ty.Type -> Text
renderRecord m =
    T.intercalate ", "
        (Prelude.fmap
            (\(k, v) -> k ++ " : " ++ renderType v)
            (Map.toAscList m))
