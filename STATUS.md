# Project Status

Quone is an early release project.

The goal of the first release is not to cover all of R. It is to prove one
useful workflow well:

1. write typed Quone source,
2. read CSV data through decoders,
3. transform dataframes with dplyr-style verbs,
4. compile to readable R,
5. use VS Code for fast feedback and format-on-save.

## What Is Expected to Work

- `.Q` source parsing, typechecking, formatting, and R generation
- primitive scalars, vectors, records, custom types, `Maybe`, and `Result`
- functions, lambdas, `let`, pipes, and exhaustive `case`
- core dataframe verbs and typed equijoins
- the `quone` R package for install/check/compile/format
- VS Code syntax highlighting and LSP integration
- the examples listed in `examples/README.md`

## What Is Intentionally Deferred

- REPL workflow
- generating full R packages from user Quone projects
- non-CSV data sources
- full joins, non-equi joins, and many-to-many relationship checks
- rowwise/across-style dataframe APIs
- typeclasses, traits, open records, and tuples
- arbitrary inline R blocks
- editor targets beyond VS Code-compatible clients

## Trust Signals for Changes

Before release-facing changes are considered ready, run:

```sh
cd compiler && cabal test
cd ../quone && R CMD build . && R CMD check --no-manual --no-build-vignettes quone_*.tar.gz
cd ../website && npm run build && npm test
```

