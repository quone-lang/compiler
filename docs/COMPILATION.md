# Quone Compilation - Version 0.0.1

**Status:** Normative for the v0.0.1 surface. Anything labelled `[planned]`
or `[out of scope]` is informative and not part of the conforming surface.

**Audience:** Implementers of the Quone-to-R lowering and authors of build
tools that consume the generated R.

**Document scope:** This document specifies how a conforming Quone v0.0.1
implementation translates source text into R, and how a Quone project
maps onto an R package. The language itself is defined in
[LANGUAGE.md](LANGUAGE.md); the @quonec@ command-line surface in
[CLI.md](CLI.md); the formatter in [FORMATTER.md](FORMATTER.md); the LSP
and REPL in [IDE.md](IDE.md); the test harness and conformance tests in
[LANGUAGE.md section 16](LANGUAGE.md#16-testing-requirements).

---

## Table of contents

1. [Translation to R](#1-translation-to-r)
2. [Project model and package generation](#2-project-model-and-package-generation)

---

## 1. Translation to R

(Was section 13 of LANGUAGE.md prior to the v0.0.1 spec split.)

Quone does not define an independent runtime semantics first and then lower
later. Its practical semantics are given by translation to R. A future
revision MUST give both:

1. a source-level meaning, and
2. the required translation to R.

For v0.0.1, the translation rules below are normative.

### 1.1 Compilation target

Quone compiles to R source code.

### 1.2 Primitive mappings

| Quone        | R                                                |
| ------------ | ------------------------------------------------ |
| `Integer`    | R integer                                        |
| `Double`     | R double                                         |
| `Logical`    | R logical                                        |
| `Character`  | R character                                      |
| `Vector a`   | atomic vector when `a` is primitive; R `list` otherwise |
| record       | named list                                       |
| dataframe    | `data.frame`                                     |

`Vector a` lowers to an R atomic vector (built with `c(...)`) when `a` is
one of the primitive types `Integer`, `Double`, `Logical`, or `Character`.
For any other element type - records, custom types, nested vectors - it
lowers to an R `list` (built with `list(...)`), because R has no atomic
representation for non-atomic elements.

This split also affects how higher-order operations on `Vector a` are
emitted; see [section 1.3.2](#132-higher-order-operations-on-vector-a).

<!-- BEGIN tests:13_2 -->
**Tests** (8):

- [`generate/integer_lowers_to_L_suffix_section_13_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L61)
- [`generate/bare_digits_lower_to_R_double_section_13_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L66)
- [`generate/double_lowers_unchanged_section_13_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L71)
- [`generate/character_lowers_unchanged_section_13_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L73)
- [`generate/true_lowers_to_TRUE_section_13_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L75)
- [`generate/vector_lowers_to_c_section_13_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L79)
- [`generate/record_lowers_to_named_list_section_13_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L81)
- [`generate/dataframe_lowers_to_data_frame_section_13_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L85)

<details>
<summary>Show test source</summary>

```haskell
    test "generate/integer_lowers_to_L_suffix_section_13_2" <|
        -- An Integer literal in source (`42L`) must round-trip with
        -- the `L` suffix in the generated R, so callers of base-R
        -- functions that distinguish int/double get the right type.
        compiles "x <- 42L" "x <- 42L"
    test "generate/bare_digits_lower_to_R_double_section_13_2" <|
        -- Bare `42` in source is a Double, and R's bare numeric is
        -- also a double. The generator emits `42.0` so the value's
        -- type is unambiguous to the reader.
        compiles "x <- 42" "x <- 42.0"
    test "generate/double_lowers_unchanged_section_13_2" <|
        compiles "x <- 3.14" "x <- 3.14"
    test "generate/character_lowers_unchanged_section_13_2" <|
        compiles "x <- \"hi\"" "x <- \"hi\""
    test "generate/true_lowers_to_TRUE_section_13_2" <|
        -- Just `True` is an ECon; the case-as-if optimisation below
        -- exercises the True/False -> TRUE/FALSE path more directly.
        compiles "x <- True" "x <- True"
    test "generate/vector_lowers_to_c_section_13_2" <|
        compiles "x <- [1L, 2L, 3L]" "x <- c(1L, 2L, 3L)"
    test "generate/record_lowers_to_named_list_section_13_2" <|
        compiles
            "x <- { a = 1L, b = 2L }"
            "x <- list(a = 1L, b = 2L)"
    test "generate/dataframe_lowers_to_data_frame_section_13_2" <|
        compiles
            "x <- dataframe { a = [1L] }"
            "x <- data.frame(a = c(1L))"
```

</details>
<!-- END tests:13_2 -->


### 1.2.1 Operator mappings

The arithmetic, comparison, and structural operators lower to R as
follows. Operators not listed share the same spelling between Quone and R.

| Quone | R     | Notes                                  |
| ----- | ----- | -------------------------------------- |
| `+`   | `+`   |                                        |
| `-`   | `-`   | Both binary and unary.                 |
| `*`   | `*`   |                                        |
| `/`   | `/`   |                                        |
| `//`  | `%/%` | Integer division.                      |
| `%`   | `%%`  | Modulo (integer).                      |
| `^`   | `^`   | Exponentiation, right-associative.     |
| `==`  | `==`  |                                        |
| `!=`  | `!=`  |                                        |
| `>`   | `>`   |                                        |
| `<`   | `<`   |                                        |
| `>=`  | `>=`  |                                        |
| `<=`  | `<=`  |                                        |
| `\|>` | `\|>` | R's native pipe (R >= 4.1).            |
| `.`   | `$`   | Field access; see [section 1.7](#17-records-and-field-access). |

<!-- BEGIN tests:13_2_1 -->
**Tests** (8):

- [`generate/plus_lowers_unchanged_section_13_2_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L100)
- [`generate/intdiv_lowers_to_pct_div_pct_section_13_2_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L102)
- [`generate/mod_lowers_to_pct_pct_section_13_2_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L104)
- [`generate/exp_lowers_to_caret_section_13_2_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L106)
- [`generate/comparison_lowers_unchanged_section_13_2_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L108)
- [`generate/precedence_omits_redundant_parens_section_13_2_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L110)
- [`generate/precedence_keeps_required_parens_section_13_2_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L112)
- [`generate/negative_base_of_exponent_is_parenthesized_section_13_2_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L114)

<details>
<summary>Show test source</summary>

```haskell
    test "generate/plus_lowers_unchanged_section_13_2_1" <|
        compiles "x <- 1L + 2L" "x <- 1L + 2L"
    test "generate/intdiv_lowers_to_pct_div_pct_section_13_2_1" <|
        compiles "x <- 10L // 3L" "x <- 10L %/% 3L"
    test "generate/mod_lowers_to_pct_pct_section_13_2_1" <|
        compiles "x <- 10L % 3L" "x <- 10L %% 3L"
    test "generate/exp_lowers_to_caret_section_13_2_1" <|
        compiles "x <- 2.0 ^ 3.0" "x <- 2.0 ^ 3.0"
    test "generate/comparison_lowers_unchanged_section_13_2_1" <|
        compiles "x <- 1L == 2L" "x <- 1L == 2L"
    test "generate/precedence_omits_redundant_parens_section_13_2_1" <|
        compiles "x <- 1L + 2L * 3L" "x <- 1L + 2L * 3L"
    test "generate/precedence_keeps_required_parens_section_13_2_1" <|
        compiles "x <- (1L + 2L) * 3L" "x <- (1L + 2L) * 3L"
    test "generate/negative_base_of_exponent_is_parenthesized_section_13_2_1" <|
        -- `-2.0 ^ 3.0` (bare digits) is fine for `^` since `^`
        -- requires Double anyway. Pin both sides to Double so the
        -- test is about parenthesization, not the type-check rule.
        compiles "x <- -2.0 ^ 3.0" "x <- (-2.0) ^ 3.0"
```

</details>
<!-- END tests:13_2_1 -->


### 1.3 Functions

A Quone lambda or top-level function compiles to an R `function(...) { ... }`.

Quone is curried, but a fully-applied call MUST lower to a single
multi-argument R call rather than a chain of single-argument applications.
For example, `add 1 2` lowers to `add(1, 2)`, not `(add(1))(2)`. Partial
applications lower to R closures that capture the supplied arguments and
accept the remaining ones.

<!-- BEGIN tests:13_3 -->
**Tests** (3):

- [`generate/curried_def_to_multi_arg_R_function_section_13_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L134)
- [`generate/fully_applied_call_to_single_R_call_section_13_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L138)
- [`generate/lambda_to_anonymous_function_section_13_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L140)

<details>
<summary>Show test source</summary>

```haskell
    test "generate/curried_def_to_multi_arg_R_function_section_13_3" <|
        compiles
            "add a b <- a + b"
            "add <- function(a, b) { a + b }"
    test "generate/fully_applied_call_to_single_R_call_section_13_3" <|
        compiles "x <- add 1L 2L" "x <- add(1L, 2L)"
    test "generate/lambda_to_anonymous_function_section_13_3" <|
        compiles "x <- \\a -> a + 1L" "x <- function(a) a + 1L"
```

</details>
<!-- END tests:13_3 -->


#### 1.3.1 Argument passing

The generator selects between positional and named R arguments based on the
kind of function being called. The intent is for generated R to look the
way an idiomatic R developer would write it (design principle 7 in
[LANGUAGE.md section 1.2](LANGUAGE.md#12-design-principles)).

| Call site                         | R call style              |
| --------------------------------- | ------------------------- |
| Quone-defined function            | positional                |
| Anonymous lambda or partial call  | positional                |
| Imported R function (default)     | positional                |
| Imported R function with declared parameter names | named         |
| Dataframe verbs ([section 1.8](#18-dataframe-verbs)) | named (per dplyr API) |
| Selected prelude/source helpers (e.g. `Csv.read`) | named, per their declarations |

For v0.0.1 the simplest conforming policy is **positional everywhere
except dataframe verbs and explicitly-marked imports**. Richer named-
argument emission for general user functions is `[planned]` and depends on
a future record-style argument syntax.

#### 1.3.2 Higher-order operations on `Vector a`

The generator SHOULD prefer `purrr` for higher-order operations on
`Vector a` whenever an idiomatic equivalent exists. `purrr`'s public API
maps closely onto Quone's prelude names from
[LANGUAGE.md section 8.2](LANGUAGE.md#82-built-in-environment), so the lowering is mostly a
`prefix::` rename.

| Quone prelude / form           | `purrr` equivalent (typical)                                                              |
| ------------------------------ | ----------------------------------------------------------------------------------------- |
| `map`                          | `purrr::map` for non-atomic; `purrr::map_dbl` / `map_int` / `map_chr` / `map_lgl` for typed atomic outputs |
| `map2`                         | `purrr::map2` and its typed variants                                                      |
| `reduce`                       | `purrr::reduce`                                                                           |
| `keep`                         | `purrr::keep`                                                                             |
| `discard`                      | `purrr::discard`                                                                          |
| record update `{ r \| ... }`   | `purrr::list_modify` (see [section 1.7](#17-records-and-field-access))                  |

Two exceptions where `purrr` SHOULD NOT be used, because base R is more
idiomatic and faster:

- **Vectorised arithmetic and comparison** on atomic vectors, e.g.
  `map (\x -> x + 1) xs` for `xs : Vector Double` SHOULD lower to `xs + 1`
  (or the equivalent vectorised form) rather than
  `purrr::map_dbl(xs, ~ . + 1)`.
- **Construction** of an atomic `Vector a` from element expressions SHOULD
  lower to `c(...)`, not to a `purrr::map` over an index range.

When neither base R nor `purrr` provides a clean equivalent, the
generator MAY fall back to other idiomatic R forms; this is `[planned]`
for full normative treatment.

### 1.4 Pipes

Quone `|>` compiles to R native `|>`.

<!-- BEGIN tests:13_4 -->
**Tests** (1):

- [`generate/pipe_lowers_to_native_pipe_section_13_4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L119)

<details>
<summary>Show test source</summary>

```haskell
    test "generate/pipe_lowers_to_native_pipe_section_13_4" <|
        compiles "x <- xs |> f" "x <- xs |> f"
```

</details>
<!-- END tests:13_4 -->


### 1.5 Custom types

Constructors compile to R constructor functions that build tagged list-like
values. The exact tag representation is left to the implementation but MUST
be deterministic and stable across compilations.

### 1.6 Pattern matching

Pattern matching compiles to a local binding plus an `if` / `else if` chain
over constructor tags and literal checks.

For each pattern kind:

- **Wildcard and variable patterns** match unconditionally; a variable
  pattern additionally introduces a binding.
- **Integer / double / character literal patterns** lower to `==`
  comparisons against the literal value.
- **Constructor patterns** dispatch on the constructor tag, then bind any
  argument patterns by recursive lowering against the constructor's
  payload positions.
- **Record patterns** lower to local `$`-access bindings inside the
  arm body. `{ name, score } -> body` becomes
  `name <- scrutinee$name; score <- scrutinee$score; ...body...`. The
  longer form `{ name = n, score = s } -> body` binds to the chosen
  names instead of the field names.

As an optimisation, when a `case` matches on a `Logical` scrutinee with
exactly the two arms `True -> a` and `False -> b` (in either order), the
generator SHOULD emit R's native `if (cond) a else b` instead of a
constructor-tag chain. This shape is what
[LANGUAGE.md section 5.3](LANGUAGE.md#53-surface-only-forms)'s `if`-desugaring produces, so
ordinary Quone `if` expressions still lower to ordinary R `if`.

<!-- BEGIN tests:13_6 -->
**Tests** (2):

- [`generate/if_lowers_to_native_R_if_section_13_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L218)
- [`generate/case_on_logical_with_swapped_arms_section_13_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L224)

<details>
<summary>Show test source</summary>

```haskell
    test "generate/if_lowers_to_native_R_if_section_13_6" <|
        -- `if c then a else b` desugars to case on Logical and the
        -- generator's optimisation lowers it to R's native if.
        compiles
            "x <- if cond then 1L else 2L"
            "x <- if (cond) 1L else 2L"
    test "generate/case_on_logical_with_swapped_arms_section_13_6" <|
        compiles
            ( T.unlines
                [ "x <- case cond of"
                , "    False -> 0L"
                , "    True -> 1L"
                ]
            )
            "x <- if (cond) 1L else 0L"
```

</details>
<!-- END tests:13_6 -->


### 1.7 Records and field access

Record field access compiles to `$` access in R.

A record-update expression `{ r | f1 = v1, ..., fn = vn }`
([LANGUAGE.md section 8.5](LANGUAGE.md#85-records-and-field-access)) lowers to
`purrr::list_modify(r, f1 = v1, ..., fn = vn)`. `purrr::list_modify`
returns a new named list with the listed entries replaced and all other
entries preserved, which matches the Elm-style functional update
semantics exactly.

<!-- BEGIN tests:13_7 -->
**Tests** (3):

- [`generate/field_access_lowers_to_dollar_section_13_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L121)
- [`generate/record_update_lowers_to_list_modify_section_13_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L153)
- [`package/module_collects_purrr_dep_for_record_update_section_13_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L144)

<details>
<summary>Show test source</summary>

```haskell
    test "generate/field_access_lowers_to_dollar_section_13_7" <|
        compiles "x <- row.score" "x <- row$score"
    test "generate/record_update_lowers_to_list_modify_section_13_7" <|
        compiles
            "x <- { rec | a = 2L }"
            "x <- purrr::list_modify(rec, a = 2L)"
    test "package/module_collects_purrr_dep_for_record_update_section_13_7" <|
        case desugarSource
            ( T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "x <- { rec | a = 1 }"
                ]
            ) of
            Prelude.Right p ->
                let
                    art = generateModule p
                in
                Prelude.pure
                    ( assert
                        ("purrr" `Prelude.elem` artifactDeps art)
                        ("expected purrr in deps; got " Prelude.<> T.pack (Prelude.show (artifactDeps art)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:13_7 -->


### 1.8 Dataframe verbs

Dataframe verb nodes compile to `dplyr::verb(...)` calls.

<!-- BEGIN tests:13_8 -->
**Tests** (4):

- [`generate/filter_in_pipe_to_dplyr_filter_section_13_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L191)
- [`generate/select_in_pipe_to_dplyr_select_section_13_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L195)
- [`generate/mutate_in_pipe_to_dplyr_mutate_section_13_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L199)
- [`generate/arrange_with_desc_to_dplyr_desc_section_13_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L203)

<details>
<summary>Show test source</summary>

```haskell
    test "generate/filter_in_pipe_to_dplyr_filter_section_13_8" <|
        compiles
            "y <- xs |> filter (a > 0L)"
            "y <- xs |> dplyr::filter(a > 0L)"
    test "generate/select_in_pipe_to_dplyr_select_section_13_8" <|
        compiles
            "y <- xs |> select { a }"
            "y <- xs |> dplyr::select(a = a)"
    test "generate/mutate_in_pipe_to_dplyr_mutate_section_13_8" <|
        compiles
            "y <- xs |> mutate { b = a + 1L }"
            "y <- xs |> dplyr::mutate(b = a + 1L)"
    test "generate/arrange_with_desc_to_dplyr_desc_section_13_8" <|
        compiles
            "y <- xs |> arrange (desc score)"
            "y <- xs |> dplyr::arrange(dplyr::desc(score))"
```

</details>
<!-- END tests:13_8 -->


### 1.9 Foreign R bindings and runtime dependencies

Quone v0.0.1 has no `library` declaration. The runtime dependency set of
a compiled program is computed by the compiler from observed usage:

| Source                                            | Implied R package |
| ------------------------------------------------- | ----------------- |
| Any dataframe verb ([LANGUAGE.md section 9](LANGUAGE.md#9-dataframe-manipulation)) | `dplyr` |
| Higher-order ops on `Vector a` lowering via [section 1.3.2](#132-higher-order-operations-on-vector-a) | `purrr` |
| `Csv.*` and other source-loading prelude calls ([LANGUAGE.md section 11](LANGUAGE.md#11-file-loading-and-decoding)) | `readr` |
| `import pkg.fn : ...` declarations                | `pkg`             |

**Foreign function imports.** A foreign R import (see
[LANGUAGE.md section 4.5](LANGUAGE.md#45-imports)) carries the R namespace as the lowercase
prefix of its path; the prefix contributes to the runtime dependency
set.

```quone
import readr.read_csv : Character -> dataframe { name : Vector Character }
import data.table.fread : Character -> dataframe { name : Vector Character }
import sqrt : Double -> Double                         (* base R *)
```

Calls to a prefixed import lower to the corresponding `pkg::fn(...)` form
(`readr::read_csv(...)`, `data.table::fread(...)`). Imports with no
prefix are treated as base R and emit unqualified calls (`sqrt(...)`).

Quone module imports (Section 4.5) do not contribute to the runtime
dependency set: they reference functions defined elsewhere in the same
project's R package, which already lives in `R/` next to the calling
module.

**Emission of dependencies.** In script mode, the generator MUST ensure
every required package is reachable. Two options are conforming, and
generators MAY pick either consistently:

- emit `pkg::fn(...)` qualifications throughout and no `library(pkg)`
  calls; or
- emit `library(pkg)` calls at the top of the output and use unqualified
  names where unambiguous.

Per the "boring R" goal in [LANGUAGE.md section 1.2](LANGUAGE.md#12-design-principles), the
qualified form is preferred; it makes the source of every call visible
without a global attached-namespace state.

In package mode (see [section 2](#2-project-model-and-package-generation)),
the dependency set MUST be written into the generated `DESCRIPTION` file's
`Imports` field rather than emitted as `library(...)` calls.

<!-- BEGIN tests:13_9 -->
**Tests** (4):

- [`generate/foreign_import_qualifies_call_section_13_9`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L168)
- [`generate/foreign_import_qualifies_bare_reference_section_13_9`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/GenerateTests.hs#L177)
- [`package/module_collects_dplyr_dep_for_verb_section_13_9`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L126)
- [`package/dependency_set_includes_dplyr_section_13_9`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L201)

<details>
<summary>Show test source</summary>

```haskell
    test "generate/foreign_import_qualifies_call_section_13_9" <|
        compiles
            ( T.unlines
                [ "import readr.read_csv : Character -> Integer"
                , ""
                , "load path <- read_csv path"
                ]
            )
            "load <- function(path) { readr::read_csv(path) }"
    test "generate/foreign_import_qualifies_bare_reference_section_13_9" <|
        compiles
            ( T.unlines
                [ "import dplyr.n : Integer"
                , ""
                , "x <- n"
                ]
            )
            "x <- dplyr::n"
    test "package/module_collects_dplyr_dep_for_verb_section_13_9" <|
        case desugarSource
            ( T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "x <- xs |> filter (a > 0)"
                ]
            ) of
            Prelude.Right p ->
                let
                    art = generateModule p
                in
                Prelude.pure
                    ( assert
                        ("dplyr" `Prelude.elem` artifactDeps art)
                        ("expected dplyr in deps; got " Prelude.<> T.pack (Prelude.show (artifactDeps art)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "package/dependency_set_includes_dplyr_section_13_9" <|
        case generatePackage (oneModuleInputs simpleProject verbSrc) of
            Prelude.Right pa ->
                Prelude.pure
                    ( assert
                        ("dplyr" `Prelude.elem` paDependencies pa)
                        ("got " Prelude.<> T.pack (Prelude.show (paDependencies pa)))
                    )
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
```

</details>
<!-- END tests:13_9 -->


---

## 2. Project model and package generation

(Was section 14 of LANGUAGE.md prior to the v0.0.1 spec split.)

This section distinguishes the **language**, the **compiler output
model**, and the **project layout**.

<!-- BEGIN tests:14 -->
**Tests** (5):

- [`resolve/toml_minimal_section_14`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L66)
- [`resolve/toml_with_deps_section_14`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L86)
- [`resolve/toml_authors_array_section_14`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L106)
- [`resolve/toml_missing_name_rejected_section_14`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L123)
- [`resolve/toml_comments_ignored_section_14`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L132)

<details>
<summary>Show test source</summary>

```haskell
    test "resolve/toml_minimal_section_14" <|
        let
            src =
                T.unlines
                    [ "[package]"
                    , "name = \"stats\""
                    , "version = \"0.0.1\""
                    ]
        in
        case parseProjectToml "quone.toml" src of
            Prelude.Right p ->
                Prelude.pure
                    (assert
                        (metaName (projectMeta p) Prelude.== "stats"
                            Prelude.&& metaVersion (projectMeta p) Prelude.== "0.0.1"
                            Prelude.&& Prelude.null (projectDependencies p)
                        )
                        ("got " Prelude.<> T.pack (Prelude.show p)))
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "resolve/toml_with_deps_section_14" <|
        let
            src =
                T.unlines
                    [ "[package]"
                    , "name = \"stats\""
                    , "version = \"0.0.1\""
                    , ""
                    , "[dependencies]"
                    , "purrr = \">= 1.0\""
                    , "dplyr = \">= 1.1\""
                    ]
        in
        case parseProjectToml "quone.toml" src of
            Prelude.Right p ->
                Prelude.pure
                    ( Prelude.length (projectDependencies p) === 2
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "resolve/toml_authors_array_section_14" <|
        let
            src =
                T.unlines
                    [ "[package]"
                    , "name = \"stats\""
                    , "version = \"0.0.1\""
                    , "authors = [\"Andrew McNally\", \"Other\"]"
                    ]
        in
        case parseProjectToml "quone.toml" src of
            Prelude.Right p ->
                Prelude.pure
                    ( Prelude.length (metaAuthors (projectMeta p)) === 2
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "resolve/toml_missing_name_rejected_section_14" <|
        let
            src =
                T.unlines
                    [ "[package]"
                    , "version = \"0.0.1\""
                    ]
        in
        Prelude.pure (assertLeft (parseProjectToml "quone.toml" src))
    test "resolve/toml_comments_ignored_section_14" <|
        let
            src =
                T.unlines
                    [ "# project metadata"
                    , "[package]   # the package section"
                    , "name = \"stats\"   # required"
                    , "version = \"0.0.1\""
                    ]
        in
        Prelude.pure (assertRight (parseProjectToml "quone.toml" src))
```

</details>
<!-- END tests:14 -->


### 2.1 Package-compatible by design, not package-only

Every Quone project MUST be designed so it can compile cleanly to an R
package, but v0.0.1 MUST NOT require every project to emit a package.

The compiler MUST support two first-class output modes:

- **script mode**: compile Quone source to readable `.R`;
- **package mode**: generate an R package when requested.

This keeps the language easy to try for small examples, scripts, and
analyses while still making packaging a natural path for reusable code.

### 2.2 Why package generation matters

Package output is especially valuable for:

- reusable libraries
- exports and namespace management
- documentation generation
- tests
- multi-file projects with stable module boundaries
- distribution and installation

Therefore package generation MUST be a first-class and well-supported
compiler mode in v0.0.1.

### 2.3 Why package generation is not mandatory in v0.0.1

Mandatory package generation would add unnecessary ceremony for:

- single-file programs
- analysis scripts
- small prototypes
- REPL-driven exploration
- early language adoption

Quone's first release MUST preserve the direct experience of compiling
Quone source to readable R without forcing users into `DESCRIPTION`,
`NAMESPACE`, and package build workflows for every use case.

### 2.4 Recommended v0.0.1 policy

A conforming v0.0.1 toolchain SHOULD adopt:

- single-file and small-project workflows compile directly to `.R`;
- package generation is officially supported and documented;
- reusable library projects are strongly encouraged to use package mode;
- the internal module and dependency model is already package-compatible;
- the language definition itself does not make package generation part of
  core semantics.

### 2.5 Specification consequence

Package generation is a **compiler output and project model concern**, not a
requirement for whether a Quone program is valid. This keeps the core
language smaller and cleaner while preserving a strong long-term interop
story with R.

### 2.6 Multi-module package layout

A Quone project containing multiple modules compiles to a single R
package. The mapping is direct and uses no name mangling: every Quone
function lowers to a bare R name in `snake_case`, and the package's flat
internal namespace is shared across all modules.

**Source layout.** A Quone project has the following shape:

```
my-project/
├── quone.toml                      # project metadata and dependencies
└── src/
    ├── Stats/
    │   ├── Transform.Q             # module Stats.Transform exporting (..)
    │   └── Summary.Q               # module Stats.Summary exporting (..)
    └── Data/
        └── Loader.Q                # module Data.Loader exporting (..)
```

The directory tree under `src/` mirrors the dotted module path. Each
`.Q` file (uppercase extension, mirroring R's convention of capital
`.R`) contains exactly one module declaration whose dotted path
matches its location.

**Generated R package layout.**

```
my-project/                         # generated R package root
├── DESCRIPTION                     # from quone.toml + auto-derived deps
├── NAMESPACE                       # from `@export` tags via roxygen2
├── R/
│   ├── stats-transform.R
│   ├── stats-summary.R
│   └── data-loader.R
└── man/
    ├── normalize.Rd
    ├── mean_score.Rd
    └── load_scores.Rd
```

**File-name mapping.** A Quone module path maps to an R filename in `R/`
by lowercasing every segment and replacing dots with hyphens. The result
matches conventional R package layout (compare `dplyr/R/group_by.R`,
`tidyr/R/pivot-long.R`).

| Quone module        | R file                     |
| ------------------- | -------------------------- |
| `Stats.Transform`   | `R/stats-transform.R`      |
| `Stats.Summary`     | `R/stats-summary.R`        |
| `Data.Loader`       | `R/data-loader.R`          |

**Function-name mapping.** No mangling. Every Quone function lowers to
its bare `snake_case` name in R:

| Quone (fully qualified)             | Generated R         |
| ----------------------------------- | ------------------- |
| `Stats.Transform.normalize`         | `normalize`         |
| `Stats.Summary.mean_score`          | `mean_score`        |
| `Data.Loader.load_scores`           | `load_scores`       |

Cross-module calls within the same package use the bare R name, because
all functions share the package's flat internal namespace. The compiler
MUST reject a project in which two modules define functions (whether
exported or internal) with the same `snake_case` name. This collision
check is package-wide and runs at compile time before any R is emitted.

**R-level public API.** A binding appears in the generated `NAMESPACE`
if and only if its `#'` doc block contains an `@export` tag
([LANGUAGE.md section 3.6](LANGUAGE.md#36-comments)). A binding can therefore be:

- **Quone-private** (not in any `exporting (..)` list): unreachable
  outside its defining module.
- **Quone-exported, R-internal** (in `exporting (..)`, no `@export`):
  reachable from other Quone modules in the same project, but not in the
  R package's `NAMESPACE`. External R code can still reach it via R's
  triple-colon escape hatch (`pkg:::name`), as for any R package internal.
- **Quone-exported, R-exported** (in `exporting (..)` and `@export` in
  doc block): reachable from other Quone modules and listed in
  `NAMESPACE`. This is the package's public R API.

A binding marked `@export` MUST also appear in its module's
`exporting (..)` list; the compiler MUST reject the inverse.

**`NAMESPACE` and `man/` generation.** The compiler MUST NOT write
`NAMESPACE` or `man/*.Rd` directly. After emitting `R/`, `DESCRIPTION`,
and the source-level `#'` doc blocks, the package-mode build MUST invoke
`roxygen2::roxygenise(package_dir)` (equivalently
`devtools::document(package_dir)`) to derive `NAMESPACE` and the `.Rd`
files from the `@export`, `@param`, and other tags in the doc blocks.

This guarantees that `NAMESPACE` always reflects the `@export` tags
present in the generated R, and that `roxygen2`'s rules for namespace
imports (`@importFrom`, `@import`) and method registration (`@method`,
`@rdname`) work exactly as they do in any hand-written R package.

`roxygen2` is therefore a build-time dependency of package mode. It is
not a runtime dependency of the produced package.

**`DESCRIPTION`.** Generated by Quone (not by `roxygen2`) from
`quone.toml` plus the auto-derived runtime dependency set. The
`Imports:` field is the union of all R packages the compiled output
calls, computed per
[section 1.9](#19-foreign-r-bindings-and-runtime-dependencies). Quone
also adds a generated `Roxygen:` field declaring the markdown setting
(typically `Roxygen: list(markdown = TRUE)`) so `roxygen2` parses the
doc blocks consistently across runs.

<!-- BEGIN tests:14_6 -->
**Tests** (16):

- [`package/filename_simple_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L73)
- [`package/filename_dotted_to_kebab_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L75)
- [`package/filename_three_segments_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L77)
- [`package/module_artifact_emits_R_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L90)
- [`package/module_export_requires_at_export_tag_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L109)
- [`package/generates_description_with_metadata_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L173)
- [`package/description_includes_roxygen_marker_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L183)
- [`package/namespace_lists_at_export_bindings_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L192)
- [`package/collision_in_two_modules_rejected_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L221)
- [`package/no_collision_passes_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L234)
- [`package/e2e_compiles_multimodule_project_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L347)
- [`package/e2e_emits_export_for_at_export_only_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/PackageTests.hs#L407)
- [`resolve/file_to_module_path_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L154)
- [`resolve/module_path_to_file_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L158)
- [`resolve/discover_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L162)
- [`resolve/non_src_path_returns_nothing_section_14_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L165)

<details>
<summary>Show test source</summary>

```haskell
    test "package/filename_simple_section_14_6" <|
        Prelude.pure (moduleFileName ["Foo"] === "R/foo.R")
    test "package/filename_dotted_to_kebab_section_14_6" <|
        Prelude.pure (moduleFileName ["Stats", "Transform"] === "R/stats-transform.R")
    test "package/filename_three_segments_section_14_6" <|
        Prelude.pure (moduleFileName ["A", "B", "C"] === "R/a-b-c.R")
    test "package/module_artifact_emits_R_section_14_6" <|
        case desugarSource
            ( T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "x <- 1L"
                ]
            ) of
            Prelude.Right p ->
                let
                    art = generateModule p
                in
                Prelude.pure
                    ( assert
                        (artifactPath art Prelude.== "R/foo.R"
                            Prelude.&& T.isInfixOf "x <- 1L" (artifactBody art))
                        ("got " Prelude.<> T.pack (Prelude.show art))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "package/module_export_requires_at_export_tag_section_14_6" <|
        case desugarSource
            ( T.unlines
                [ "module Foo exporting (x, y)"
                , ""
                , "#' @export"
                , "x <- 1"
                , ""
                , "y <- 2"
                ]
            ) of
            Prelude.Right p ->
                let
                    art = generateModule p
                in
                Prelude.pure (artifactExports art === ["x"])
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "package/generates_description_with_metadata_section_14_6" <|
        case generatePackage (oneModuleInputs simpleProject simpleSrc) of
            Prelude.Right pa ->
                Prelude.pure
                    ( assert
                        (T.isInfixOf "Package: stats" (paDescription pa)
                            Prelude.&& T.isInfixOf "Version: 0.0.1" (paDescription pa))
                        ("got " Prelude.<> paDescription pa)
                    )
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "package/description_includes_roxygen_marker_section_14_6" <|
        case generatePackage (oneModuleInputs simpleProject simpleSrc) of
            Prelude.Right pa ->
                Prelude.pure
                    ( assert
                        (T.isInfixOf "Roxygen: list(markdown = TRUE)" (paDescription pa))
                        "expected Roxygen: line"
                    )
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "package/namespace_lists_at_export_bindings_section_14_6" <|
        case generatePackage (oneModuleInputs simpleProject simpleSrcExported) of
            Prelude.Right pa ->
                Prelude.pure
                    ( assert
                        (T.isInfixOf "export(x)" (paNamespace pa))
                        ("got " Prelude.<> paNamespace pa)
                    )
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "package/collision_in_two_modules_rejected_section_14_6" <|
        case (desugarSource modA, desugarSource modB) of
            (Prelude.Right pA, Prelude.Right pB) ->
                Prelude.pure
                    ( assertLeft
                        ( generatePackage
                            ( defaultPackageInputs
                                simpleProject
                                [pA, pB]
                            )
                        )
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "package/no_collision_passes_section_14_6" <|
        case (desugarSource modA, desugarSource modCdistinct) of
            (Prelude.Right pA, Prelude.Right pC) ->
                Prelude.pure
                    ( case generatePackage
                            ( defaultPackageInputs
                                simpleProject
                                [pA, pC]
                            )
                      of
                        Prelude.Right _ -> Pass
                        Prelude.Left d -> Fail (T.pack (Prelude.show d))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "package/e2e_compiles_multimodule_project_section_14_6" <|
        Temp.withSystemTempDirectory "quone-e2e-" <| \root -> do
            -- Lay out a minimal two-module project on disk.
            let
                srcDir = root FP.</> "src"
                fooDir = srcDir FP.</> "Foo"
            Dir.createDirectoryIfMissing Prelude.True fooDir
            TIO.writeFile
                (root FP.</> "quone.toml")
                ( T.unlines
                    [ "[package]"
                    , "name = \"e2e\""
                    , "version = \"0.0.1\""
                    ]
                )
            TIO.writeFile
                (srcDir FP.</> "Bar.Q")
                ( T.unlines
                    [ "module Bar exporting (greet)"
                    , ""
                    , "#' @export"
                    , "greet <- \"hi\""
                    ]
                )
            TIO.writeFile
                (fooDir FP.</> "Baz.Q")
                ( T.unlines
                    [ "module Foo.Baz exporting (n)"
                    , ""
                    , "#' @export"
                    , "n <- 7"
                    ]
                )
            -- Compile + write.
            result <- compilePackage root
            case result of
                Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
                Prelude.Right pa -> do
                    let outDir = root FP.</> "build"
                    writePackage outDir pa
                    -- All the expected files should exist.
                    descExists <- Dir.doesFileExist (outDir FP.</> "DESCRIPTION")
                    nsExists <- Dir.doesFileExist (outDir FP.</> "NAMESPACE")
                    barExists <- Dir.doesFileExist (outDir FP.</> "R" FP.</> "bar.R")
                    bazExists <- Dir.doesFileExist (outDir FP.</> "R" FP.</> "foo-baz.R")
                    Prelude.pure
                        ( assert
                            (descExists Prelude.&& nsExists Prelude.&& barExists Prelude.&& bazExists)
                            ( T.pack
                                ( "missing artifact: DESCRIPTION="
                                    Prelude.++ Prelude.show descExists
                                    Prelude.++ " NAMESPACE="
                                    Prelude.++ Prelude.show nsExists
                                    Prelude.++ " bar.R="
                                    Prelude.++ Prelude.show barExists
                                    Prelude.++ " foo-baz.R="
                                    Prelude.++ Prelude.show bazExists
                                )
                            )
                        )
    test "package/e2e_emits_export_for_at_export_only_section_14_6" <|
        Temp.withSystemTempDirectory "quone-e2e-" <| \root -> do
            let srcDir = root FP.</> "src"
            Dir.createDirectoryIfMissing Prelude.True srcDir
            TIO.writeFile
                (root FP.</> "quone.toml")
                ( T.unlines
                    [ "[package]"
                    , "name = \"e2e\""
                    , "version = \"0.0.1\""
                    ]
                )
            TIO.writeFile
                (srcDir FP.</> "Mixed.Q")
                ( T.unlines
                    [ "module Mixed exporting (a, b)"
                    , ""
                    , "#' @export"
                    , "a <- 1"
                    , ""
                    , "b <- 2"
                    ]
                )
            result <- compilePackage root
            case result of
                Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
                Prelude.Right pa ->
                    Prelude.pure
                        ( assert
                            ( T.isInfixOf "export(a)" (paNamespace pa)
                                Prelude.&& Prelude.not
                                    (T.isInfixOf "export(b)" (paNamespace pa))
                            )
                            ("got NAMESPACE: " Prelude.<> paNamespace pa)
                        )
    test "resolve/file_to_module_path_section_14_6" <|
        Prelude.pure
            (fileToModulePath "src/Stats/Transform.Q"
                === Just ["Stats", "Transform"])
    test "resolve/module_path_to_file_section_14_6" <|
        Prelude.pure
            (modulePathToFile ["Stats", "Transform"]
                === "src/Stats/Transform.Q")
    test "resolve/discover_section_14_6" <|
        Prelude.pure
            (discoverModulePath "src/Foo.Q" === Just ["Foo"])
    test "resolve/non_src_path_returns_nothing_section_14_6" <|
        Prelude.pure
            (discoverModulePath "Foo.Q" === Nothing)
```

</details>
<!-- END tests:14_6 -->


---

## See also

- [LANGUAGE.md](LANGUAGE.md) - the Quone language proper (lex, syntax, types, semantics).
- [CLI.md](CLI.md) - the `quonec` command-line surface.
- [FORMATTER.md](FORMATTER.md) - the `quonec fmt` rules.
- [IDE.md](IDE.md) - the LSP and REPL surfaces.
