{-| Load and type-check the embedded Quone prelude.

The pipeline is identical to a user-program compile, with one
adjustment:

  * The 'Program' is marked @'programIsPrelude' = True@ after
    desugaring so that the validator does not reject the prelude's
    own @extern@ / @infix@ / @prefix@ declarations
    (LANGUAGE2.md sections 4.5, 10).

The result of 'loadPrelude' is the typing environment that user-
program inference seeds from. Today (Phase A) this env extends the
legacy 'Quone.Type.Env.initialEnv' with whatever the prelude's
@extern@ value declarations contribute; subsequent phases will move
operator overloads, primitive types, and ADTs into the prelude as
well.

The loader is pure (the source is embedded, not read from disk). It
runs once per compiler invocation; the result is small and not worth
caching across invocations.

-}
module Quone.Prelude.Load
    ( loadPrelude
    , LoadedPrelude (..)
    , preludeNames
    )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import NriPrelude
import Quone.Ast.Source (Program (..))
import Quone.Ast.Validate (validate)
import Quone.Diagnostic (Diagnostic)
import Quone.Parse.Desugar (desugarFile)
import qualified Quone.Prelude.Embed as Embed
import Quone.Type.Env (Env (..), initialEnv)
import Quone.Type.Infer (TypedProgram (..), inferProgramFrom)
import qualified Prelude



-- | A successfully loaded prelude.
data LoadedPrelude = LoadedPrelude
    { -- | The desugared and type-checked prelude AST. Used by the
      -- code generator to look up extern lowering strings (Phase C)
      -- and by the resolver to surface declared names.
      preludeProgram :: Program
    , -- | The typing environment populated by the prelude's
      -- declarations. Subsequent inferences (user code, REPL,
      -- LSP) start from this env rather than from
      -- 'Quone.Type.Env.initialEnv'.
      preludeEnv :: Env
    }
    deriving (Prelude.Show)


-- | Parse, validate, and type-check the embedded 'Prelude.Q' source.
-- Returns the prelude AST and typing environment to seed user-program
-- inference, or the first diagnostic if the prelude itself is
-- malformed (which is a compiler-development bug, not a user-facing
-- one).
loadPrelude :: Prelude.Either Diagnostic LoadedPrelude
loadPrelude = do
    rawProg <- desugarFile Embed.preludeFilename Embed.preludeSource
    let prog = rawProg {programIsPrelude = Prelude.True}
    case validate prog of
        (d : _) -> Prelude.Left d
        [] -> do
            (typed, finalEnv) <- inferProgramFrom initialEnv prog
            Prelude.pure
                LoadedPrelude
                    { preludeProgram = typedProgram typed
                    , preludeEnv = finalEnv
                    }


-- | Every value-level name the prelude introduces. Useful for any
-- pass that builds a per-module symbol table and wants to treat
-- prelude names as implicitly-imported (LANGUAGE2.md section 10).
preludeNames :: LoadedPrelude -> Set.Set Text
preludeNames p = Map.keysSet (envValues (preludeEnv p))
