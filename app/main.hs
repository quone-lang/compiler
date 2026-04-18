module Main where

import NriPrelude
import Quone.AST.Source (
    Decl (DeclLet),
    Expr (EInt, EMul),
    Program (..),
 )
import Quone.Generate.R (
    generateProgram,
 )
import qualified Prelude

exampleProgram :: Program
exampleProgram =
    Program
        [DeclLet "x" (EMul (EInt 1) (EInt 1))]

main :: Prelude.IO ()
main =
    putTextLn (generateProgram exampleProgram)
