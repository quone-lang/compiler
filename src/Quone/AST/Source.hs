module Quone.AST.Source (
    Program (..),
    Decl (..),
    Expr (..),
) where

import NriPrelude

type Name = Text

newtype Program
    = Program (List Decl)
    deriving (Show, Eq)

data Decl
    = DeclLet Name Expr
    deriving (Show, Eq)

data Expr
    = EInt Int
    | EVar Name
    | EAdd Expr Expr
    | EMul Expr Expr
    deriving (Show, Eq)