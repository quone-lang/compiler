# quonec

[![ci](https://github.com/quone-lang/compiler/actions/workflows/ci.yaml/badge.svg)](https://github.com/quone-lang/compiler/actions/workflows/ci.yaml)
[![release](https://github.com/quone-lang/compiler/actions/workflows/release.yaml/badge.svg)](https://github.com/quone-lang/compiler/actions/workflows/release.yaml)
[![license: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

The compiler for [Quone](https://quone-lang.org), a typed functional
language that compiles to readable R.

`quonec` parses, type-checks, and lowers `.Q` source to R using
[`dplyr`](https://dplyr.tidyverse.org), [`purrr`](https://purrr.tidyverse.org),
[`readr`](https://readr.tidyverse.org), and base R. The complete
language definition is in
[docs/LANGUAGE.md](docs/LANGUAGE.md); the implementation plan is in
[docs/PLAN.md](docs/PLAN.md).

## Install

Prebuilt binaries are published with every tagged release; see
[RELEASING.md](RELEASING.md) for the asset naming convention.

```sh
# macOS / Linux
curl -sSL https://github.com/quone-lang/compiler/releases/latest/download/quonec-$(uname -s | tr '[:upper:]' '[:lower:]')-$(uname -m).tar.gz | tar xz -C /usr/local/bin
quonec --help
```

R users can install the compiler from R via the companion package:

```r
install.packages("pak")
pak::pak("quone-lang/quone")
quone::install_compiler()
```

## Build from source

```sh
cabal build         # compile
cabal test          # run the test suite (currently 373 tests)
cabal run quonec    # run the CLI
```

Requires GHC 9.6 or later and `cabal-install` 3.10 or later. The
project is dependency-light by design (`megaparsec`, `text`,
`process`, `nri-prelude`).

## CLI

```
quonec new <name>             scaffold a new project
quonec build <file.Q>         compile a script to .R
quonec build --package [dir]  compile an R package into <dir>/build/
quonec run <file.Q>           compile, then print the Rscript command
quonec check <file.Q>         typecheck without emitting
quonec deps [dir]             print auto-derived runtime deps
quonec fmt <file.Q>           elm-format-style canonical formatter
quonec repl                   interactive session backed by Rscript
quonec lsp                    speak LSP over stdin/stdout
```

Cross-cutting flags include `--diagnostics-format={human,json}`,
`--out=DIR`, `--emit-sourcemap`, `--rscript`, and `--log-file=PATH`.

## Editor support

A VS Code / Cursor / Positron extension lives at
[editors/vscode/](editors/vscode/) and drives `quonec lsp`. Other
editors can wire the LSP up directly:

```sh
quonec lsp     # speaks JSON-RPC 2.0 over stdin/stdout
```

The R companion package's
[`quone::install_lsp()`](https://github.com/quone-lang/quone) sets
this up automatically for VS Code, Cursor, Positron, RStudio, Neovim,
Helix, and Zed.

## Repository layout

```
compiler/
  app/quonec.hs            # CLI entrypoint
  src/Quone/               # library: lexer, parser, type checker,
                           #   R generator, REPL, LSP, formatter
  tests/                   # nri-prelude test suite
  docs/                    # LANGUAGE.md (spec), PLAN.md (status)
  editors/vscode/          # VS Code / Positron extension
  RELEASING.md             # release procedure and asset naming
```

## Sibling repos

- **[quone-lang/quone](https://github.com/quone-lang/quone)** -- R
  package wrapping `quonec` for use from R sessions, RStudio,
  Positron, and Quarto.
- **[quone-lang/examples](https://github.com/quone-lang/examples)** --
  sample Quone programs used as documentation and as compiler
  fixtures.
- **[quone-lang/website](https://github.com/quone-lang/website)** --
  the source for [quone-lang.org](https://quone-lang.org).

## Style

- Prefer `NoImplicitPrelude` and `import NriPrelude` in modules.
- Prefer `Text`, `List`, `Maybe`, and `Result` from `nri-prelude`.
- Format Haskell source with
  [`fourmolu`](https://github.com/fourmolu/fourmolu) using the
  shipped [`.fourmolu.yaml`](.fourmolu.yaml).
- Format `.cabal` files with `cabal-fmt -i compiler.cabal`.
- Lint with `hlint .`.

## License

[MIT](LICENSE).
