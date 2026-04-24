# Contributing to Quone

Quone is early, and focused contributions are especially valuable.

## Good First Contributions

- improve diagnostics for an existing parse/type error
- add coverage to the focused compiler test suites
- improve examples so they show generated readable R clearly
- tighten website copy or README wording when it overpromises
- add small prelude functions that lower directly to familiar R/tidyverse calls
- improve VS Code syntax highlighting, hover, or format-on-save behavior

Avoid starting with large language features unless there is already agreement in
`docs/LANGUAGE.md` or `docs/INITIAL_RELEASE_PLAN.md`.

## Development Loop

```sh
cd compiler
cabal build
cabal test
```

For website changes:

```sh
cd website
npm run build
npm test
```

For R package changes:

```sh
cd quone
R CMD build .
R CMD check --no-manual --no-build-vignettes quone_*.tar.gz
```

## Design Principles

- Keep generated R readable.
- Prefer clear static errors to copying permissive R behavior.
- Keep the first-release workflow small and coherent.
- Do not add compatibility shims for removed experimental behavior.
- If a public claim is not backed by docs, examples, or CI, tighten the claim.

## Spec Changes

`docs/LANGUAGE.md` is the canonical language spec. If a change affects syntax,
types, lowering, or dataframe semantics, update the spec and add tests in the
same pull request.

