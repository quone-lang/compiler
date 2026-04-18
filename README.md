# compiler

A small Haskell project managed with `cabal`.

This repo uses [`nri-prelude`](https://github.com/NoRedInk/haskell-libraries/tree/trunk/nri-prelude)
to prefer Elm-style APIs and formatting conventions.

## Getting started

Build the project:

```sh
cabal build
```

Run the executable:

```sh
cabal run compiler
```

Format `.cabal` files:

```sh
cabal-fmt -i compiler.cabal
```

Format Haskell source:

```sh
fourmolu -i Compiler.hs main.hs
```

Lint the code:

```sh
hlint .
```

## Style

- Prefer `NoImplicitPrelude` and `import NriPrelude` in Haskell modules.
- Prefer `Text`, `List`, `Maybe`, and `Result` from `nri-prelude`.
- Keep formatting Elm-like through `.fourmolu.yaml`.
