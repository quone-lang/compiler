module Quone.Generate.R (
    generateProgram,
) where

import NriPrelude
import Quone.AST.Source (
    Decl (..),
    Expr (..),
    Program (..),
 )
import qualified Text

generateProgram :: Program -> Text
generateProgram (Program decls) =
    Text.join "\n" (map generateDecl decls)

generateDecl :: Decl -> Text
generateDecl decl =
    case decl of
        DeclLet name expr ->
            name ++ " <- " ++ generateExpr expr

generateExpr :: Expr -> Text
generateExpr expr =
    case expr of
        EInt n ->
            Text.fromInt n ++ "L"
        EVar name ->
            name
        EAdd left right ->
            generateInfix "+" left right
        EMul left right ->
            generateInfix "*" left right

generateInfix :: Text -> Expr -> Expr -> Text
generateInfix operator left right =
    generateExpr left
        ++ " "
        ++ operator
        ++ " "
        ++ generateExpr right