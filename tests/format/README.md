# tests/format/ — the elm-format-style Quone formatter spec

Every file in this directory is half of a snapshot pair:

* `<NN>_<rule>.in.Q`  — what the user might write (often deliberately
  messy, so the test asserts the formatter normalises it).
* `<NN>_<rule>.out.Q` — the canonical elm-format-style rendering of
  that input.

The harness in `tests/Test/FormatSnapshotTests.hs` discovers each pair
at runtime, formats the `.in.Q`, byte-compares to the `.out.Q`, and
asserts both idempotence (a second format pass returns the same text)
and round-trip parsability (the formatted output still parses).

Each fixture pins one rule. To learn what canonical Quone looks like,
read this directory in numeric order — it is the human-readable spec.

## Style summary

The formatter follows [elm-format](https://github.com/avh4/elm-format)
exactly except where Quone syntax differs from Elm. The deltas from
elm are:

* Quone uses `<-` where Elm uses `=` for value bindings.
* Quone has `dataframe { … }` literals (no Elm equivalent — formatted
  as the `dataframe` keyword followed by a record literal under the
  same record-formatting rules).
* Quone has dataframe verbs (`select`, `filter`, `mutate`, …) — these
  are formatted exactly like ordinary function calls.
* Imports are tightly packed with **no blank lines between import
  lines**, then **two blank lines** before the first declaration.
  (elm-format also packs imports tightly, so this matches.)
* `let` / `in` bodies in Quone are indented one level under `in`
  (Quone is layout-sensitive in a slightly different way than Elm;
  this is the form Quone's parser canonically accepts).

Everything else is identical: 4-space indent, 100-column page width,
two blank lines between top-level declarations, leading-comma style
for multi-line records and lists, type annotations on the line
directly above the binding with no gap, doc comments (`#'`) attached
flush to the declaration they document.

## Index

### Value declarations
* `01_value_decl_oneliner` — even a one-liner like `main <- 1` wraps
  the body to a new indented line (strict elm).
* `02_value_decl_function` — function head + body wraps the same way.
* `03_value_decl_with_annotation` — type annotation sits on the line
  immediately above the binding with no blank gap.
* `04_two_blank_lines_between_decls` — exactly two blank lines
  separate top-level declarations.
* `05_doc_comment_attached` — a `#'` doc comment is flush against the
  declaration it documents (no blank line between).

### Records and lists
* `06_record_short_inline` — a record that fits stays inline
  `{ a = 1, b = 2 }`.
* `07_record_long_multiline` — a record that overflows the page width
  wraps to leading-comma form, indented under the binding.
* `08_list_short_inline` — `[ 1, 2, 3 ]` stays on one line, with
  spaces inside the brackets.
* `09_list_long_multiline` — overflow wraps to leading-comma form,
  one element per line.
* `10_field_shorthand` — in record patterns, `{ name = name }` collapses
  to `{ name }`. (Quone's parser currently only accepts shorthand in
  *patterns*, not in value record literals; if/when value-side
  shorthand is added to the parser, extend this fixture.)
* `11_record_update_inline` — short record updates stay on one line.
* `12_record_update_multiline` — long record updates put the source
  record on its own line, then `|` indented under the opening brace,
  fields leading-comma below.

### Control flow
* `13_let_single_binding` — `let` on its own line, single binding
  indented 4, `in` and body dedented to the original column.
* `14_let_multi_binding` — multiple bindings are separated by one
  blank line each (elm-format applies the same blank-line rule
  inside `let`).
* `15_case_short_arms` — `case … of`, each arm pattern on its own
  line, body indented 4 below the pattern, blank line between arms.
* `16_lambda` — a short lambda `\n -> n + 1` stays on one line.
* `17_if_then_else` — `if … then`, body indented 4, `else` dedented
  to the `if` column, body indented again.

### Imports
* `18_imports_sorted` — imports are sorted alphabetically by their
  canonical text.
* `19_imports_no_blank_lines_between` — extra blank lines between
  import lines are stripped.
* `20_imports_two_blank_lines_before_first_decl` — two blank lines
  separate the import block from the first declaration.

### Quone-specific (no Elm equivalent)
* `21_pipe_chain_long_wraps` — pipe chains that overflow break with
  one `|>` per line, indented 4 under the chain head.
* `22_pipe_short_inline` — short pipes stay on one line.
* `23_dataframe_literal_short` — short `dataframe { … }` stays
  inline.
* `24_dataframe_literal_long` — long `dataframe { … }` puts the
  record literal on its own line, indented under `dataframe`, with
  leading-comma fields.
* `25_verb_with_record_arg` — `select { a, b }` is formatted as the
  verb name followed by an inline record.
* `26_verb_with_paren_predicate` — `filter (cond)` keeps the
  parenthesised predicate.
* `27_custom_type_variants` — `type Color` wraps to one variant per
  line with leading `|`, the `=` aligned at column 4.

### Miscellaneous
* `28_module_header_explicit_exports` — `module Stats.Summary
  exporting (a, b)` followed by two blank lines.
* `29_module_header_wildcard` — `module Stats.Summary exporting (..)`
  unchanged.
* `30_comment_between_decls` — a `#` comment between two
  declarations is preserved at its original anchor.
* `31_unary_minus` — `-3.5` has no space between the minus and the
  literal.
* `32_binop_keeps_user_parens` — user-written parens are preserved
  even when not strictly required (we do not normalise away
  precedence-redundant parens; see `tests/Test/GenerateTests.hs` for
  why precedence-aware *codegen* drops them but the *formatter*
  keeps them as a clarity hint).
* `33_type_alias_record` — `type alias Point = { … }` body wraps to
  the next indented line, like any value binding.

## Adding a new fixture

1. Pick the next free number (e.g. `34_…`). Numbers are sortable so
   the directory reads in order; they have no semantic meaning beyond
   that.
2. Author the `.in.Q` to be a small program that exercises the rule.
   Include syntax that isn't already canonical (e.g. extra blanks,
   wrong indentation) so the test verifies the formatter
   *normalises* it.
3. Author the `.out.Q` to be the canonical formatted form of the
   `.in.Q`.
4. Both files must parse. The harness asserts this on every run.
5. Add an entry to the index above with a one-line description of
   what the fixture pins.

## Refreshing goldens

Once the formatter is implemented, regenerate every `.out.Q` from
its `.in.Q` after a formatter change:

```sh
cabal run quonec -- fmt tests/format/*.in.Q --output tests/format/<corresponding>.out.Q
```

Review the diff carefully — every change to a `.out.Q` is a change
to the language's canonical style. A diff in this directory should
always be discussed in the same PR that changes the formatter.
