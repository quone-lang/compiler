# quonec

`quonec` is the compiler for Quone, a typed language for R dataframe workflows.
It compiles `.Q` source files to readable R.

The canonical language specification is
[`docs/LANGUAGE2.md`](docs/LANGUAGE2.md). The initial release scope is
[`docs/INITIAL_RELEASE_PLAN.md`](docs/INITIAL_RELEASE_PLAN.md).

## Build

```sh
cabal build
cabal test
```

## CLI

```sh
quonec compile <file.Q>      compile a file to .R
quonec compile-dir <dir>     compile all .Q files under a directory
quonec build <file.Q>        alias for compile
quonec check <file.Q>        typecheck without emitting
quonec fmt <file.Q>          format in place
quonec lsp                   speak LSP over stdin/stdout
quonec version               print version
```

Cross-cutting flags:

```sh
--diagnostics-format=human|json
--out=DIR
--log-file=PATH
```

## Editor Support

The first release targets VS Code through the extension in
[`editors/vscode/`](editors/vscode/). It uses `quonec lsp` for diagnostics,
hover, go to definition, document symbols, and format-on-save.

## R Package

R users should install the compiler through the minimal
[`quone`](https://github.com/quone-lang/quone) R package:

```r
pak::pak("quone-lang/quone")
quone::install_compiler()
```

