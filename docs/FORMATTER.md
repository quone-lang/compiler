# Quone Formatter - Version 0.0.1

**Status:** Normative for the initial release surface where rules are stated as
MUST or SHOULD. Anything labelled `[planned]` is informative.

**Audience:** Authors of `quonec fmt`, IDE/format-on-save integrators,
and reviewers of formatter changes.

**Document scope:** This document specifies the canonical Quone style:
what `quonec fmt` produces, what it preserves, and the snapshot-fixture
discipline that pins each rule to a runnable test. Deltas from
[elm-format](https://github.com/avh4/elm-format) are listed
explicitly. The language itself is defined in
[LANGUAGE.md](LANGUAGE.md); the `quonec fmt` CLI flag in
[CLI.md section 3](CLI.md#3-subcommands).

---

## Table of contents

1. [Style summary](#1-style-summary)
2. [The `quonec fmt` subcommand (Appendix A.4)](#2-the-quonec-fmt-subcommand)
3. [Comment trivia (Appendix C.1)](#3-comment-trivia)
4. [Snapshot fixtures](#4-snapshot-fixtures)
5. [Adding a new fixture](#5-adding-a-new-fixture)

---

## 1. Style summary

The formatter follows [elm-format](https://github.com/avh4/elm-format)
exactly except where Quone syntax differs from Elm. The deltas from
elm are:

* Quone uses `<-` where Elm uses `=` for value bindings.
* Quone has `dataframe { ... }` literals (no Elm equivalent - formatted
  as the `dataframe` keyword followed by a record literal under the
  same record-formatting rules).
* Quone has dataframe verbs (`select`, `filter`, `mutate`, ...) - these
  are formatted exactly like ordinary function calls.
* Imports are tightly packed with **no blank lines between import
  lines**, then **two blank lines** before the first declaration.
  (elm-format also packs imports tightly, so this matches.)
* `let` / `in` bodies in Quone are indented one level under `in`
  (Quone is layout-sensitive in a slightly different way than Elm;
  this is the form Quone's parser canonically accepts).

Everything else is identical:

| Rule                                       | Value                          |
| ------------------------------------------ | ------------------------------ |
| Indent                                     | 4 spaces                       |
| Page width                                 | 100 columns                    |
| Tabs                                       | rejected at layout positions   |
| Blank lines between top-level declarations | exactly 2                      |
| Trailing newline                           | required                       |
| Multi-line records / lists                 | leading-comma                  |
| Type annotation placement                  | line directly above binding    |
| Doc-comment placement                      | flush above declaration        |

---

## 2. The `quonec fmt` subcommand

(Internally `section_a4`; corresponds to the `quonec fmt` CLI verb in
[CLI.md section 3](CLI.md#3-subcommands).)

```
quonec fmt <file.Q>
```

**Behaviour.** Reads `<file.Q>`, parses it, applies the canonical layout
rules, and writes the result back to `<file.Q>` in place. Exit code 0 on
success.

**Idempotence.** A second `quonec fmt` pass on a file that was already
formatted MUST produce no diff. Snapshot fixtures
([section 4](#4-snapshot-fixtures)) assert this.

**Round-trip parsability.** The output of `quonec fmt` MUST always
parse. A parse failure on the formatted output is a formatter bug, not
a source-style violation.

**No-op on syntax errors.** If `<file.Q>` does not parse, `quonec fmt`
emits a diagnostic and exits non-zero without modifying the file.

<!-- BEGIN tests:a4 -->
**Tests** (1):

- [`cli/fmt_command_section_a4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L55)

<details>
<summary>Show test source</summary>

```haskell
    test "cli/fmt_command_section_a4"
        ( Prelude.pure
            (parseArgs ["fmt", "src/Foo.Q"]
                === CmdFmt "src/Foo.Q"))
```

</details>
<!-- END tests:a4 -->

---

## 3. Comment trivia

(Internally `section_c1`; covers the formatter's comment-preservation
rules.)

The formatter MUST preserve every `#` and `#'` comment in the source.
Specifically:

- **Top-of-file `#` comment.** A `#` comment before the first
  declaration MUST appear at the top of the formatted output, before
  the first declaration.
- **Between-declaration `#` comment.** A `#` comment between two
  top-level declarations MUST appear in the same anchor position in
  the formatted output (between the same two declarations).
- **Trailing `#` comment.** A `#` comment after the last declaration
  MUST appear at the bottom of the formatted output.
- **Doc-block `#'` comment.** A run of `#'` lines immediately preceding
  a top-level declaration ([LANGUAGE.md section 3.6](LANGUAGE.md#36-comments))
  MUST stay flush against the declaration it documents (no blank line
  between).

Idempotence applies after every comment-preservation rule: formatting
a file that already has well-placed comments MUST produce no diff.

<!-- BEGIN tests:c1 -->
**Tests** (4):

- [`format/preserves_top_of_file_comment_section_c1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/FormatTests.hs#L99)
- [`format/preserves_between_decl_comment_section_c1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/FormatTests.hs#L110)
- [`format/preserves_trailing_comment_section_c1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/FormatTests.hs#L126)
- [`format/idempotent_with_comments_section_c1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/FormatTests.hs#L141)

<details>
<summary>Show test source</summary>

```haskell
    test "format/preserves_top_of_file_comment_section_c1"
        ( do
            let
                src = "# this is a top comment\nmain <- 1\n"
            case Fmt.format "<test>" src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "format failed")
                Prelude.Right out ->
                    Prelude.pure
                        (assertContains "this is a top comment" out)
        )
    test "format/preserves_between_decl_comment_section_c1"
        ( do
            let
                src =
                    T.unlines
                        [ "x <- 1"
                        , ""
                        , "# explain y"
                        , "y <- 2"
                        ]
            case Fmt.format "<test>" src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "format failed")
                Prelude.Right out ->
                    Prelude.pure (assertContains "explain y" out)
        )
    test "format/preserves_trailing_comment_section_c1"
        ( do
            let
                src =
                    T.unlines
                        [ "main <- 1"
                        , ""
                        , "# end-of-file note"
                        ]
            case Fmt.format "<test>" src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "format failed")
                Prelude.Right out ->
                    Prelude.pure (assertContains "end-of-file note" out)
        )
    test "format/idempotent_with_comments_section_c1"
        ( do
            let
                src =
                    T.unlines
                        [ "# top"
                        , "x <- 1"
                        , ""
                        , "# inline"
                        , "y <- 2"
                        , "# trailing"
                        ]
            case Fmt.format "<test>" src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "first format failed")
                Prelude.Right once ->
                    case Fmt.format "<test>" once of
                        Prelude.Left _ ->
                            Prelude.pure (Fail "second format failed")
                        Prelude.Right twice ->
                            Prelude.pure (twice === once)
        )
```

</details>
<!-- END tests:c1 -->

---

## 4. Snapshot fixtures

The formatter is pinned by paired-file snapshot fixtures under
`tests/format/`. Each fixture is two files:

- `<NN>_<rule>.in.Q` - what the user might write (often deliberately
  messy, so the test asserts the formatter normalises it).
- `<NN>_<rule>.out.Q` - the canonical elm-format-style rendering of
  that input.

The harness in `tests/Test/FormatSnapshotTests.hs` discovers each pair
at runtime, formats the `.in.Q`, byte-compares to the `.out.Q`, and
asserts both idempotence (a second format pass returns the same text)
and round-trip parsability (the formatted output still parses).

### 4.1 Index

#### Value declarations
* `01_value_decl_oneliner` - even a one-liner like `main <- 1` wraps
  the body to a new indented line (strict elm).
* `02_value_decl_function` - function head + body wraps the same way.
* `03_value_decl_with_annotation` - type annotation sits on the line
  immediately above the binding with no blank gap.
* `04_two_blank_lines_between_decls` - exactly two blank lines
  separate top-level declarations.
* `05_doc_comment_attached` - a `#'` doc comment is flush against the
  declaration it documents (no blank line between).

#### Records and lists
* `06_record_short_inline` - a record that fits stays inline
  `{ a = 1, b = 2 }`.
* `07_record_long_multiline` - a record that overflows the page width
  wraps to leading-comma form, indented under the binding.
* `08_list_short_inline` - `[ 1, 2, 3 ]` stays on one line, with
  spaces inside the brackets.
* `09_list_long_multiline` - overflow wraps to leading-comma form,
  one element per line.
* `10_field_shorthand` - in record patterns, `{ name = name }` collapses
  to `{ name }`. (Quone's parser currently only accepts shorthand in
  *patterns*, not in value record literals; if/when value-side
  shorthand is added to the parser, extend this fixture.)
* `11_record_update_inline` - short record updates stay on one line.
* `12_record_update_multiline` - long record updates put the source
  record on its own line, then `|` indented under the opening brace,
  fields leading-comma below.

#### Control flow
* `13_let_single_binding` - `let` on its own line, single binding
  indented 4, `in` and body dedented to the original column.
* `14_let_multi_binding` - multiple bindings are separated by one
  blank line each (elm-format applies the same blank-line rule
  inside `let`).
* `15_case_short_arms` - `case ... of`, each arm pattern on its own
  line, body indented 4 below the pattern, blank line between arms.
* `16_lambda` - a short lambda `\n -> n + 1` stays on one line.
* `17_if_then_else` - `if ... then`, body indented 4, `else` dedented
  to the `if` column, body indented again.

#### Imports
* `18_imports_sorted` - imports are sorted alphabetically by their
  canonical text.
* `19_imports_no_blank_lines_between` - extra blank lines between
  import lines are stripped.
* `20_imports_two_blank_lines_before_first_decl` - two blank lines
  separate the import block from the first declaration.

#### Quone-specific (no Elm equivalent)
* `21_pipe_chain_long_wraps` - pipe chains that overflow break with
  one `|>` per line, indented 4 under the chain head.
* `22_pipe_short_inline` - short pipes stay on one line.
* `23_dataframe_literal_short` - short `dataframe { ... }` stays
  inline.
* `24_dataframe_literal_long` - long `dataframe { ... }` puts the
  record literal on its own line, indented under `dataframe`, with
  leading-comma fields.
* `25_verb_with_record_arg` - `select { a, b }` is formatted as the
  verb name followed by an inline record.
* `26_verb_with_paren_predicate` - `filter (cond)` keeps the
  parenthesised predicate.
* `27_custom_type_variants` - `type Color` wraps to one variant per
  line with leading `|`, the `=` aligned at column 4.

#### Miscellaneous
* `28_module_header_explicit_exports` - `module Stats.Summary
  exporting (a, b)` followed by two blank lines.
* `29_module_header_wildcard` - `module Stats.Summary exporting (..)`
  unchanged.
* `30_comment_between_decls` - a `#` comment between two
  declarations is preserved at its original anchor.
* `31_unary_minus` - `-3.5` has no space between the minus and the
  literal.
* `32_binop_keeps_user_parens` - user-written parens are preserved
  even when not strictly required (we do not normalise away
  precedence-redundant parens).
* `33_type_alias_record` - `type alias Point = { ... }` body wraps to
  the next indented line, like any value binding.

<!-- BEGIN tests:format-snapshot -->
**Tests** (33):

- [`format/01_value_decl_oneliner`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/01_value_decl_oneliner.in.Q#L1)
- [`format/02_value_decl_function`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/02_value_decl_function.in.Q#L1)
- [`format/03_value_decl_with_annotation`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/03_value_decl_with_annotation.in.Q#L1)
- [`format/04_two_blank_lines_between_decls`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/04_two_blank_lines_between_decls.in.Q#L1)
- [`format/05_doc_comment_attached`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/05_doc_comment_attached.in.Q#L1)
- [`format/06_record_short_inline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/06_record_short_inline.in.Q#L1)
- [`format/07_record_long_multiline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/07_record_long_multiline.in.Q#L1)
- [`format/08_list_short_inline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/08_list_short_inline.in.Q#L1)
- [`format/09_list_long_multiline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/09_list_long_multiline.in.Q#L1)
- [`format/10_field_shorthand`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/10_field_shorthand.in.Q#L1)
- [`format/11_record_update_inline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/11_record_update_inline.in.Q#L1)
- [`format/12_record_update_multiline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/12_record_update_multiline.in.Q#L1)
- [`format/13_let_single_binding`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/13_let_single_binding.in.Q#L1)
- [`format/14_let_multi_binding`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/14_let_multi_binding.in.Q#L1)
- [`format/15_case_short_arms`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/15_case_short_arms.in.Q#L1)
- [`format/16_lambda`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/16_lambda.in.Q#L1)
- [`format/17_if_then_else`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/17_if_then_else.in.Q#L1)
- [`format/18_imports_sorted`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/18_imports_sorted.in.Q#L1)
- [`format/19_imports_no_blank_lines_between`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/19_imports_no_blank_lines_between.in.Q#L1)
- [`format/20_imports_two_blank_lines_before_first_decl`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/20_imports_two_blank_lines_before_first_decl.in.Q#L1)
- [`format/21_pipe_chain_long_wraps`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/21_pipe_chain_long_wraps.in.Q#L1)
- [`format/22_pipe_short_inline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/22_pipe_short_inline.in.Q#L1)
- [`format/23_dataframe_literal_short`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/23_dataframe_literal_short.in.Q#L1)
- [`format/24_dataframe_literal_long`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/24_dataframe_literal_long.in.Q#L1)
- [`format/25_verb_with_record_arg`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/25_verb_with_record_arg.in.Q#L1)
- [`format/26_verb_with_paren_predicate`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/26_verb_with_paren_predicate.in.Q#L1)
- [`format/27_custom_type_variants`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/27_custom_type_variants.in.Q#L1)
- [`format/28_module_header_explicit_exports`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/28_module_header_explicit_exports.in.Q#L1)
- [`format/29_module_header_wildcard`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/29_module_header_wildcard.in.Q#L1)
- [`format/30_comment_between_decls`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/30_comment_between_decls.in.Q#L1)
- [`format/31_unary_minus`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/31_unary_minus.in.Q#L1)
- [`format/32_binop_keeps_user_parens`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/32_binop_keeps_user_parens.in.Q#L1)
- [`format/33_type_alias_record`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/format/33_type_alias_record.in.Q#L1)
<!-- END tests:format-snapshot -->

---

## 5. Adding a new fixture

1. Pick the next free number (e.g. `34_...`). Numbers are sortable so
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

### 5.1 Refreshing goldens

Once the formatter is implemented, regenerate every `.out.Q` from
its `.in.Q` after a formatter change:

```sh
cabal run quonec -- fmt tests/format/*.in.Q --output tests/format/<corresponding>.out.Q
```

Review the diff carefully - every change to a `.out.Q` is a change
to the language's canonical style. A diff in this directory should
always be discussed in the same PR that changes the formatter.

---

## See also

- [LANGUAGE.md](LANGUAGE.md) - the Quone language proper (lex, syntax, types).
- [CLI.md](CLI.md) - the `quonec` command-line surface, including `fmt`.
- [COMPILATION.md](COMPILATION.md) - lowering rules.
- [LANGUAGE.md](LANGUAGE.md) - initial release language specification.
