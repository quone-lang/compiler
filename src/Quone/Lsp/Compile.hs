{-| Reusable compilation entrypoints for the LSP handlers.

Mirrors 'Quone.Cli.Commands.compileScript' but returns the typed
program (so hover and document-symbol can walk it) alongside any
diagnostics that fired.

-}
module Quone.Lsp.Compile
    ( compileText
    , CompileResult (..)
    )
where

import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source (Program)
import Quone.Ast.Validate (validate)
import Quone.Diagnostic (Diagnostic)
import Quone.Parse.Desugar (desugarFile)
import Quone.Type.Infer (TypedProgram, inferProgram)
import qualified Prelude



data CompileResult
    = CompileOk Program TypedProgram
    | CompileFailed [Diagnostic]
    deriving (Prelude.Show)


compileText :: Text -> Text -> CompileResult
compileText filename src =
    case desugarFile filename src of
        Prelude.Left d -> CompileFailed [d]
        Prelude.Right prog ->
            case validate prog of
                ds@(_ : _) -> CompileFailed ds
                [] -> case inferProgram prog of
                    Prelude.Left d -> CompileFailed [d]
                    Prelude.Right typed -> CompileOk prog typed
