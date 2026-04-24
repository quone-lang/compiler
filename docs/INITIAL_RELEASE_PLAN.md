# Quone Initial Release Plan

This document defines the intended scope for the first usable Quone release.
`LANGUAGE2.md` is the language specification; this file tracks release
packaging, tooling, and implementation priorities.

## Release Goal

The first release should let a technically inclined R user write typed Quone
code, read CSV data, transform dataframes with familiar dplyr-style verbs,
compile to readable R, and get useful feedback in VS Code.

## Core Language

Include:

- modules, imports, exposing lists, and indentation-sensitive source layout
- primitive types: `Integer`, `Double`, `Character`, and `Logical`
- `Vector T`, exact records, type aliases, and dataframe schemas
- custom types with payloads, including `Maybe` and `Result`
- exhaustive `case`, vectorized `case`, and `if` as sugar for `case`
- curried functions, lambdas, partial application, `let`, and pipes
- explicit numeric conversion only
- fixed operators only
- `Maybe` for missing values

Defer:

- typeclasses, traits, and constrained type variables
- open records
- tuples
- arbitrary inline R
- general named arguments
- user-defined operators

## Data Workflow

Include:

- typed dataframe schemas with `Vector` column types
- core dplyr verbs: `select`, `rename`, `filter`, `mutate`, `summarize`,
  `group_by`, `ungroup`, and `arrange`
- typed equijoins: `inner_join`, `left_join`, and `right_join`
- explicit grouped dataframe types
- reducers in `mutate` and `filter`, including grouped behavior
- CSV loading with typed decoders: `read_csv`, `Csv.column`, `Csv.column_as`,
  and `Csv.optional_column`
- `Result.expect` for script-style fail-fast use

Defer:

- `full_join`, non-equi joins, and many-to-many relationship checks
- `rowwise`
- generalized `across`-style column selection
- data sources beyond CSV

## Prelude

Keep the prelude lean but useful.

Include:

- core constructors and types: `Logical`, `Maybe`, and `Result`
- arithmetic, comparison, logical, and pipe operators
- numeric conversion helpers such as `to_double`
- common math vectorized functions such as `sqrt`, `abs`, `log`, `exp`,
  `round`, `floor`, and `ceiling`
- common stats reducers such as `mean`, `median`, `sum`, `sd`, `var`, `min`,
  `max`, `length`, and `count`
- vector helpers such as `map` and `map2`
- standard dataframe verbs
- a small set of string helpers, preferably backed by `stringr`

Avoid a large prelude. Prefer functions that lower directly to `purrr`,
`dplyr`, `readr`, `stringr`, or clear base R.

## R Package Distribution

Include a minimal R package, likely named `quone`, for installing and running
the compiler from R-oriented environments.

The package should provide:

- `quone::compile(input, output = NULL)` to compile a `.Q` file to `.R`
- `quone::compile_dir(input_dir, output_dir)` for small projects
- `quone::check(input)` for parse/type diagnostics without writing R
- a bundled or bootstrapped compiler executable
- minimal package documentation and install instructions

Generating full R packages from Quone user projects is out of scope for the
first release. The initial R package is only the compiler distribution package.

## Tooling

Include:

- CLI command to compile `.Q` files to `.R`
- parse/type diagnostics for command-line and editor use
- a VS Code extension or LSP integration

Initial VS Code support should include:

- syntax highlighting
- diagnostics
- hover for inferred types
- go to definition for local bindings
- document symbols
- basic formatting if feasible

Defer:

- other editors
- REPL
- debugger integration
- advanced refactors
- full project/package publishing workflow

## Website and Examples

Include a small public website for the first release.

The website should provide:

- a short explanation of what Quone is
- installation instructions for the R package and CLI
- a quickstart guide
- links to the language spec and release plan
- examples showing CSV loading, dataframe transformation, grouping, joins, and
  generated R output

Include one or more examples repositories for users to clone.

The examples should cover:

- a minimal "hello data" CSV workflow
- grouped summaries
- typed joins
- missing values with `Maybe`
- foreign R imports
- VS Code workflow

Examples should compile as part of release validation so they stay in sync with
the compiler.

## R Lowering Preferences

Prefer `purrr` over hand-written base-R iteration and higher-order plumbing.
Use domain-specific tidyverse packages where they are the natural target.

Lowering targets:

- `purrr` for `map`, `map2`, partial application, and higher-order vector work
- `dplyr` for dataframe verbs
- `readr` for CSV loading
- `stringr` for string helpers
- base R for primitive literals, simple arithmetic, scalar control flow, and
  direct calls where base R is clearer than forcing another dependency

Generated R should import only the packages actually used by the program.

