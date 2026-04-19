{-| The @quonec@ executable.

Argument parsing and dispatch live in 'Quone.Cli.Main'; this file
just wires @System.Environment.getArgs@ to the CLI driver.

-}
module Main where

import NriPrelude
import qualified Quone.Cli.Main as Cli
import qualified System.Environment as Env
import qualified Prelude


main :: Prelude.IO ()
main = do
    argv <- Env.getArgs
    Cli.runCli argv
