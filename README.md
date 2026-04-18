# compiler

A small Haskell project managed with `cabal`.

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
fourmolu -i main.hs
```

Lint the code:

```sh
hlint .
```
