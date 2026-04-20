# Scenario tests

End-to-end tests that compile a small but realistic Quone program
and check it on **two layers**:

| Layer    | Golden | What it catches                                        | Needs Rscript? |
|----------|--------|--------------------------------------------------------|----------------|
| codegen  | `.R`   | Lowering drift: the compiler now produces different R. | No             |
| runtime  | `.out` | Behavioural drift: that R now prints something different. | Yes         |

Each scenario therefore contributes two tests, e.g.
`scenarios/05_group_by_summarize/codegen` and
`scenarios/05_group_by_summarize/runtime`. A failure tells you which
dimension broke:

- **Only `codegen` fails** → the lowered R changed shape but still
  computes the same value. Usually a deliberate codegen change; just
  refresh the `.R` golden.
- **Only `runtime` fails** → the lowered R is unchanged but R itself
  now produces different output (regression in a runtime helper or a
  dependent package).
- **Both fail** → the codegen change altered runtime behaviour. Look
  at the codegen diff first.

Driven by `Test.ScenarioTests`. The point is **edge-case coverage
through feature interaction**: each scenario exercises 3+ language
features at once (records inside dataframes, `let` inside a
higher-order function, foreign imports through a pipeline, etc.).

## Adding a scenario

1. Create `tests/scenarios/NN_short_name.Q` whose `main` binding is
   the value you want to print.
2. Add a header comment that says, in plain English, what feature
   interactions the scenario is meant to stress.
3. Generate both goldens (uses the same `compileScript` path the
   harness uses, so the codegen golden matches by construction):

   ```sh
   QUONEC=$(cabal list-bin quonec)
   "$QUONEC" build tests/scenarios/NN_short_name.Q
   { cat tests/scenarios/NN_short_name.R
     printf '\nif (exists("main")) print(main)\n'; } \
       | Rscript --no-save --no-restore --vanilla - \
       > tests/scenarios/NN_short_name.out
   ```

4. `cabal test` should pass.

## Refreshing goldens

After a deliberate codegen change run the snippet above on every
affected scenario (or all 10 in a loop). Always inspect the diff in
`git diff tests/scenarios/*.R` before committing.

## Conventions

- `main` is auto-printed by the runtime harness (it appends
  `print(main)`), so scenarios don't need a print line of their own.
- `.R` goldens compare under `T.unwords . T.words` normalisation
  (matches `Test.CorpusTests`), so formatter tweaks don't break them
  but a real lowering change still fails the test.
- `.out` goldens compare line-by-line with `T.stripEnd`, so trailing
  whitespace is ignored.
- `Rscript --vanilla` is used so user-level `~/.Rprofile` changes
  can't perturb a scenario.
- The runtime layer skips silently if `Rscript` isn't on PATH, so CI
  without R stays green. The codegen layer always runs.

## Known compiler bugs we ran into while writing scenarios

See [KNOWN_FAILURES.md](./KNOWN_FAILURES.md). Each entry has a
minimal repro. When one of those is fixed, lift the workaround in
the affected scenario or add a new scenario that locks in the new
behaviour.
