# Quone Compiler

Write safer data transformation code in a typed functional language. Compile it
to readable R.

Quone is for R users who like the tidyverse workflow but want earlier feedback
than a long script can give them. The compiler checks names, types, dataframe
shapes, grouped summaries, joins, and missingness before producing ordinary R
that collaborators can inspect and run without learning Quone.

The canonical language specification is [`docs/LANGUAGE.md`](docs/LANGUAGE.md).
The initial release scope is [`docs/INITIAL_RELEASE_PLAN.md`](docs/INITIAL_RELEASE_PLAN.md).
Current maturity and contribution guidance live in [`STATUS.md`](STATUS.md) and
[`CONTRIBUTING.md`](CONTRIBUTING.md).

## Why Try It

- **Safer dataframe pipelines:** catch unknown columns, wrong types, shape
  mismatches, and non-exhaustive cases before running the R script.
- **Familiar workflow:** write dplyr-shaped transformations, grouped summaries,
  typed joins, and CSV reads through explicit decoders.
- **Readable output:** generated R uses base R, `dplyr`, `readr`, `purrr`, and
  `stringr`; there is no large runtime lock-in.
- **Editor feedback:** VS Code support provides syntax highlighting, diagnostics,
  hover, go to definition, document symbols, and format-on-save.

## Five Minute Quickstart

The easiest path for R users is the [`quone`](https://github.com/quone-lang/quone)
R package:

```r
# install.packages("pak")
pak::pak("quone-lang/quone")

quone::install_compiler()
quone::install_lsp()

quone::write_demo("mean_score.Q")
quone::check("mean_score.Q")
quone::compile("mean_score.Q")

source("mean_score.R")
mean_score(c(10, 20, 30))
```

The generated `mean_score.R` is plain R. Open it before sourcing it; readable
output is part of the product, not an implementation detail.

## Canonical Proof Example

The best end-to-end example is
[`examples/pharma-analysis`](https://github.com/quone-lang/examples/tree/main/pharma-analysis).
It mirrors a small clinical-trial workflow:

1. read CSV data,
2. derive subject-level variables,
3. filter and summarize events,
4. join summaries back to subjects,
5. emit readable R and CSV outputs.

It is intentionally small enough to review while still demonstrating the core
promise: typed data transformation that compiles to maintainable R.

## Build the Compiler

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

## Initial Release Boundaries

Quone is early. The initial release intentionally focuses on one coherent
workflow: typed CSV-to-dataframe analysis that lowers to R. REPLs, generalized
data sources, user R package generation, typeclasses, row polymorphism, and
non-equi joins are out of scope for now.

