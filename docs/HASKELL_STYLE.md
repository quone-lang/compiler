# Haskell Style

The compiler uses one Haskell style by default:

- Enable `NoImplicitPrelude` through Cabal.
- Import `NriPrelude` unqualified in normal modules.
- Import `Prelude` qualified when a base name should be explicit.
- Keep collection and text modules qualified, usually as `T`, `Map`, `Set`, and `List`.

Use `Prelude.` for APIs where the base origin matters or where ambiguity would make the code harder to read:

- `Prelude.IO`, `Prelude.Either`, `Prelude.Left`, and `Prelude.Right`
- deriving classes and constraints such as `Prelude.Show`, `Prelude.Eq`, and `Prelude.Ord`
- conversions and rendering helpers such as `Prelude.fromIntegral` and `Prelude.show`
- partial or riskier helpers such as `Prelude.head`, `Prelude.last`, and `Prelude.read`

Use `NriPrelude` operators such as `<|` and `|>` when they make a small expression or pipeline easier to read. Do not force Elm-shaped code when ordinary Haskell structure is clearer, especially in parser, inference, and code generation internals.

Modules that define local operators may hide conflicting `NriPrelude` names at the import site, for example:

```haskell
import NriPrelude hiding ((<>), (<+>))
```

Compiler errors should continue to use `Either Diagnostic` and the local inference monad unless there is a broader plan to move the whole pipeline to a transformer stack.
