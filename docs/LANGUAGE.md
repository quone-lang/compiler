# Quone Language Definition - Version 0.0.1

**Status:** Normative for the v0.0.1 surface. Anything labelled `[planned]`
or `[out of scope]` is informative and not part of the conforming surface.

**Audience:** Language implementers, tooling authors, and adopters who want
a precise picture of what Quone v0.0.1 includes, what it means, and how it
is compiled to R.

**Document scope:** This document defines the Quone language at the source
and type level for v0.0.1: lexical structure, syntax, types, static
semantics, and conformance. The translation to R and the project /
package model live in [COMPILATION.md](COMPILATION.md). The
command-line tool surface lives in [CLI.md](CLI.md). The formatter
lives in [FORMATTER.md](FORMATTER.md). The LSP server and REPL live
in [IDE.md](IDE.md).

---

## Table of contents

1. [Overview and design principles](#1-overview-and-design-principles)
2. [Document conventions and normative language](#2-document-conventions-and-normative-language)
3. [Lexical structure](#3-lexical-structure)
4. [Modules, source files, packages, and exports](#4-modules-source-files-packages-and-exports)
5. [Concrete syntax](#5-concrete-syntax)
6. [Abstract syntax](#6-abstract-syntax)
7. [Type system](#7-type-system)
8. [Static semantics](#8-static-semantics)
9. [Dataframe manipulation](#9-dataframe-manipulation)
10. [Standard environment](#10-standard-environment)
11. [File loading and decoding](#11-file-loading-and-decoding)
12. [Errors and runtime fallibility](#12-errors-and-runtime-fallibility)
13. [Translation to R](#13-translation-to-r) (moved to [COMPILATION.md section 1](COMPILATION.md#1-translation-to-r))
14. [Project model and package generation](#14-project-model-and-package-generation) (moved to [COMPILATION.md section 2](COMPILATION.md#2-project-model-and-package-generation))
15. [Conformance levels](#15-conformance-levels)
16. [Testing requirements](#16-testing-requirements)
17. [Version 0.0.1 scope](#17-version-001-scope)
18. [Glossary](#18-glossary)
19. [Open questions and future work](#19-open-questions-and-future-work)

---

## 1. Overview and design principles

### 1.1 What Quone is

Quone is a statically typed functional language that compiles to readable R
code. It is aimed at users who want:

- a lightweight functional surface syntax,
- Hindley-Milner type inference,
- custom types and pattern matching,
- records and dataframe-shaped values,
- pipe-friendly data transformation,
- and generated R that remains familiar and idiomatic.

### 1.2 Design principles

The following principles are normative for resolving ambiguity in this and
future versions of the spec.

1. **Readable output.** Generated R MUST look like code an R developer could
   maintain by hand.
2. **Minimal runtime.** Language features SHOULD compile directly to base R,
   `purrr`, `dplyr`, `readr`, or comparably familiar libraries rather than
   through a large Quone-specific runtime. Higher-order operations on
   `Vector a` SHOULD prefer `purrr` whenever an idiomatic equivalent exists
   (see [section 13.3.2](#1332-higher-order-operations-on-vector-a)).
3. **Strong static checks.** The compiler MUST catch type errors before
   execution whenever the language allows it.
4. **Small core.** The language SHOULD prefer a compact expression core plus
   library functions over special-cased syntax.
5. **Data-first ergonomics.** Records, vectors, dataframes, and dataframe
   manipulation are first-class.
6. **Semantically clean sugar.** Concise dataframe syntax is permitted, but it
   MUST desugar to explicit, principled core forms.
7. **Boring generated R.** Generated R MUST stay familiar and unsurprising to
   typical R users.
8. **Honest runtime model.** File loading and decoding are runtime-fallible
   operations and MUST be specified as such.

---

## 2. Document conventions and normative language

### 2.1 Keywords

The keywords MUST, MUST NOT, SHOULD, SHOULD NOT, MAY, and OPTIONAL in this
document are to be interpreted as in RFC 2119, restricted to the v0.0.1 surface.

### 2.2 Scope tags

Inline tags identify the maturity of each construct:

- `[v0.0.1]` - included in the v0.0.1 normative surface.
- `[planned]` - acknowledged as a target but not yet specified normatively.
- `[out of scope]` - intentionally excluded from v0.0.1.

When a tag is omitted, `[v0.0.1]` is implied.

### 2.3 Code samples

Code in fenced blocks tagged ```` ```quone ```` is Quone source. Code tagged
```` ```ebnf ```` is grammar. Code tagged ```` ```r ```` is generated R.
Examples are illustrative unless explicitly marked normative.

---

## 3. Lexical structure

### 3.1 Source character set and file extension

A Quone source file is a sequence of Unicode code points encoded as UTF-8.
Implementations MUST accept UTF-8 input. Other encodings are out of scope for
v0.0.1.

Quone source files MUST use the uppercase extension `.Q`. This mirrors R's
own `.R` convention and avoids collisions with single-letter shell aliases
that conventionally use lowercase `q`. The compiler MUST treat any other
extension as not a Quone source file when discovering project files (for
example when walking `src/` for package mode).

### 3.2 Identifiers

Identifiers are formed from ASCII letters, digits, and `_`, and MUST start
with a letter. A leading `_` is not permitted, because R does not allow
identifiers to start with `_` and Quone identifiers MUST be valid R
identifiers so that transpilation is a direct rename.

The `.` character is reserved for field access
(see [section 5.2](#52-operator-precedence)) and MUST NOT appear in a Quone
identifier, even though R itself permits `.` in identifiers.

- **Lowercase identifiers** start with a lowercase letter. They are used
  for variables, value bindings, type variables, and field names.
- **Uppercase identifiers** start with an uppercase letter. They are used
  for type names, constructors, and module path segments.

<!-- BEGIN tests:3_2 -->
**Tests** (7):

- [`lex/identifier_lowercase_section_3_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L202)
- [`lex/identifier_lowercase_with_underscore_section_3_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L204)
- [`lex/identifier_lowercase_with_digits_section_3_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L206)
- [`lex/identifier_uppercase_section_3_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L208)
- [`lex/identifier_uppercase_with_digits_section_3_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L210)
- [`lex/identifier_leading_underscore_rejected_section_3_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L212)
- [`lex/identifier_dot_separates_field_access_section_3_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L224)

<details>
<summary>Show test source</summary>

```haskell
    test "lex/identifier_lowercase_section_3_2" <|
        Prelude.pure (firstTokens "score" === [TLowerIdent "score"])
    test "lex/identifier_lowercase_with_underscore_section_3_2" <|
        Prelude.pure (firstTokens "max_score" === [TLowerIdent "max_score"])
    test "lex/identifier_lowercase_with_digits_section_3_2" <|
        Prelude.pure (firstTokens "x1" === [TLowerIdent "x1"])
    test "lex/identifier_uppercase_section_3_2" <|
        Prelude.pure (firstTokens "Maybe" === [TUpperIdent "Maybe"])
    test "lex/identifier_uppercase_with_digits_section_3_2" <|
        Prelude.pure (firstTokens "Vec3" === [TUpperIdent "Vec3"])
    test "lex/identifier_leading_underscore_rejected_section_3_2" <|
        -- A lone leading '_' tokenises as TUnderscore (the wildcard
        -- pattern); it's the parser's job to reject it as an
        -- identifier in non-pattern positions. Here we only check that
        -- "_oops" does NOT come out as a single identifier token.
        Prelude.pure
            ( assert
                ( firstTokens "_oops"
                    Prelude./= [TLowerIdent "_oops"]
                )
                "leading-underscore identifier should not lex as a single TLowerIdent"
            )
    test "lex/identifier_dot_separates_field_access_section_3_2" <|
        Prelude.pure
            ( firstTokens "row.score"
                === [TLowerIdent "row", TDot, TLowerIdent "score"]
            )
```

</details>
<!-- END tests:3_2 -->


#### 3.2.1 Naming conventions

The following conventions are normative for v0.0.1 source style. They
constrain idiomatic spelling, not the lexer: a non-conforming identifier is
still a valid token, but tools SHOULD treat it as a style violation.

- Lowercase identifiers SHOULD be `snake_case`. This applies to functions,
  value bindings, record fields, and dataframe column names.
- Uppercase identifiers SHOULD be `UpperCamelCase`. This applies to type
  names, constructors, and module path segments.
- The official formatter MUST normalize identifiers toward these
  conventions when it can do so unambiguously, and SHOULD report a
  diagnostic when it cannot.

Examples by identifier kind:

| Kind                | Convention       | Example                          |
| ------------------- | ---------------- | -------------------------------- |
| function            | `snake_case`     | `normalize`, `map_maybe`         |
| value binding       | `snake_case`     | `score`, `total_score`           |
| function parameter  | `snake_case`     | `x`, `row`, `student_score`      |
| record field        | `snake_case`     | `name`, `avg_score`              |
| dataframe column    | `snake_case`     | `subject_id`, `treatment_arm`    |
| type variable       | `snake_case`     | `a`, `key`, `error_value`        |
| type name           | `UpperCamelCase` | `Maybe`, `Vector`, `Students`    |
| type alias          | `UpperCamelCase` | `Students`, `SubjectId`          |
| constructor         | `UpperCamelCase` | `Just`, `Nothing`, `Ok`, `Err`   |
| module path segment | `UpperCamelCase` | `Stats`, `Transform` in `Stats.Transform` |

A non-normative source snippet showing every kind in context:

```quone
module Stats.Transform exporting (normalize, summarize_scores)

type Maybe a
    <- Nothing
     | Just a

type alias Students <-
    dataframe
        { subject_id : Vector Character
        , score      : Vector Double
        }

normalize : Double -> Double -> Double
normalize max_score raw <-
    raw / max_score

summarize_scores : Students -> dataframe { avg_score : Vector Double }
summarize_scores students <-
    students |> summarize { avg_score = mean score }
```

### 3.3 Literals

The following literal forms are part of v0.0.1:

Literal kind names match the underlying R type names so that source-level
vocabulary is consistent with the translation in
[section 13.2](#132-primitive-mappings).

| Kind        | Type                                       | Example                                     | Compiled R                                  |
| ----------- | ------------------------------------------ | ------------------------------------------- | ------------------------------------------- |
| integer     | `Integer`                                  | `1L`, `42L`                                 | `1L`, `42L`                                 |
| double      | `Double`                                   | `1`, `42`, `1.0`, `3.14`                    | `1.0`, `42.0`, `1.0`, `3.14`                |
| logical     | `Logical`                                  | `True`, `False` (constructors of the built-in `Logical` custom type; see [section 7.5.1](#751-built-in-types)) | `TRUE`, `FALSE` |
| character   | `Character`                                | `"hello"`                                   | `"hello"`                                   |
| vector      | `Vector Double`                            | `[1.0, 2.0, 3.0]`                           | `c(1.0, 2.0, 3.0)`                          |
| record      | `{ name : Character, score : Double }`     | `{ name = "Alice", score = 92.0 }`          | `list(name = "Alice", score = 92.0)`        |
| data.frame  | `dataframe { name : Vector Character, score : Vector Double }` | `dataframe { name = [...], score = [...] }` | `data.frame(name = c(...), score = c(...))` |

A bare digit run such as `42` is a `Double`, matching R, where the
literal `42` produces a `numeric` value (not an integer). To write an
`Integer` literal, append the trailing `L` suffix (`42L`), again
following R's syntax. A literal with both a fractional part and the
`L` suffix (`1.5L`) is a lexical error. The intent is that an author
who does not think about types reaches for `1`, `2`, `3` and produces
correct double arithmetic; an author who deliberately wants integer
arithmetic writes `1L`, `2L`, `3L` and is reminded by the suffix that
they have stepped onto the integer track.

<!-- BEGIN tests:3_3 -->
**Tests** (17):

- [`lex/bare_zero_is_double_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L63)
- [`lex/bare_integer_is_double_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L67)
- [`lex/integer_with_L_suffix_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L69)
- [`lex/integer_zero_with_L_suffix_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L73)
- [`lex/double_simple_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L75)
- [`lex/double_zero_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L77)
- [`lex/double_with_L_suffix_rejected_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L79)
- [`lex/integer_suffix_must_be_terminator_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L84)
- [`lex/character_string_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L91)
- [`lex/character_empty_string_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L93)
- [`lex/character_unterminated_string_rejected_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L95)
- [`lex/logical_true_constructor_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L97)
- [`lex/logical_false_constructor_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L99)
- [`type/literal_integer_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L55)
- [`type/literal_bare_digits_default_to_double_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L59)
- [`type/literal_double_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L61)
- [`type/literal_character_section_3_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L63)

<details>
<summary>Show test source</summary>

```haskell
    test "lex/bare_zero_is_double_section_3_3" <|
        -- Per LANGUAGE.md section 3.3, bare digit runs are doubles
        -- (matches R, where `0` is `numeric` not `integer`).
        Prelude.pure (firstTokens "0" === [TFloatLit 0.0])
    test "lex/bare_integer_is_double_section_3_3" <|
        Prelude.pure (firstTokens "42" === [TFloatLit 42.0])
    test "lex/integer_with_L_suffix_section_3_3" <|
        -- Trailing `L` makes the literal an Integer, mirroring R's
        -- `42L` syntax.
        Prelude.pure (firstTokens "42L" === [TIntLit 42])
    test "lex/integer_zero_with_L_suffix_section_3_3" <|
        Prelude.pure (firstTokens "0L" === [TIntLit 0])
    test "lex/double_simple_section_3_3" <|
        Prelude.pure (firstTokens "3.14" === [TFloatLit 3.14])
    test "lex/double_zero_section_3_3" <|
        Prelude.pure (firstTokens "0.0" === [TFloatLit 0.0])
    test "lex/double_with_L_suffix_rejected_section_3_3" <|
        -- `1.5L` would mean "Integer with a fractional part", which
        -- has no sensible interpretation. The lexer rejects it
        -- rather than silently dropping the fraction.
        Prelude.pure (assertLeft (lexInput "1.5L"))
    test "lex/integer_suffix_must_be_terminator_section_3_3" <|
        -- `42Lx` is NOT an integer literal followed by an identifier;
        -- the `L` only counts as a suffix when followed by a
        -- non-identifier character. Two tokens should result, with
        -- `Lx` lexed as an upper-ident because it starts with `L`.
        Prelude.pure
            (firstTokens "42Lx" === [TFloatLit 42.0, TUpperIdent "Lx"])
    test "lex/character_string_section_3_3" <|
        Prelude.pure (firstTokens "\"hello\"" === [TStringLit "hello"])
    test "lex/character_empty_string_section_3_3" <|
        Prelude.pure (firstTokens "\"\"" === [TStringLit ""])
    test "lex/character_unterminated_string_rejected_section_3_3" <|
        Prelude.pure (assertLeft (lexInput "\"oops"))
    test "lex/logical_true_constructor_section_3_3" <|
        Prelude.pure (firstTokens "True" === [TUpperIdent "True"])
    test "lex/logical_false_constructor_section_3_3" <|
        Prelude.pure (firstTokens "False" === [TUpperIdent "False"])
    test "type/literal_integer_section_3_3" <|
        -- Per LANGUAGE.md section 3.3, an Integer literal MUST carry
        -- the trailing `L` suffix; bare `42` is a Double.
        infersTo "x <- 42L" "x" "Integer"
    test "type/literal_bare_digits_default_to_double_section_3_3" <|
        infersTo "x <- 42" "x" "Double"
    test "type/literal_double_section_3_3" <|
        infersTo "x <- 3.14" "x" "Double"
    test "type/literal_character_section_3_3" <|
        infersTo "x <- \"hi\"" "x" "Character"
```

</details>
<!-- END tests:3_3 -->


### 3.4 Reserved keywords

The following identifiers are reserved and MUST NOT be used as variable or
type names:

**Framework keywords.**

```
module     exporting    type       alias      import
if         then         else       case       of         let
in
```

**Dataframe DSL keywords.** These are reserved because the parser must
recognise them to apply the bare-column sugar in
[section 9.2](#92-sugared-and-explicit-dataframe-scope) and the
verb-specific argument shapes in [section 9](#9-dataframe-manipulation).

```
dataframe
select       filter       mutate         summarize       group_by
ungroup      arrange      rename         distinct        distinct_all
count        slice        pull           relocate        transmute
mutate_each  summarize_each
left_join    right_join   inner_join     full_join       anti_join
semi_join    cross_join
```

**Dataframe modifier keywords.** These appear inside dataframe-verb
arguments and the parser must recognise them as modifiers rather than
ordinary identifiers (see [section 9.5](#95-modifier-keywords)).

```
desc    asc    on    as    where    cols
```

Some verbs and modifiers above are `[planned]` for normative typing rules
in a later revision; see [section 8.7](#87-dataframe-pipeline-typing).
They are reserved at the lexical level today so future additions do not
break source compatibility.

### 3.5 Operators and punctuation

| Category       | Tokens                                    |
| -------------- | ----------------------------------------- |
| binding        | `<-`                                      |
| function arrow | `->`                                      |
| pipe           | `\|>`                                     |
| arithmetic     | `+`, `-`, `*`, `/`, `//`, `%`, `^`        |
| unary          | `-` (negation)                            |
| comparison     | `==`, `!=`, `>`, `<`, `>=`, `<=`          |
| field access   | `.`                                       |
| grouping       | `(`, `)`, `[`, `]`, `{`, `}`              |
| separator      | `,`                                       |
| comment        | `#` (line), `#'` (doc); see [section 3.6](#36-comments) |

The arithmetic tokens beyond `+`/`-`/`*`/`/` mean: `//` integer
division, `%` modulo, `^` exponentiation. Their typing rules are in
[section 8.8](#88-arithmetic-and-comparison-operators) and their R
lowering is in [section 13.2](#132-primitive-mappings).

<!-- BEGIN tests:3_5 -->
**Tests** (5):

- [`lex/operator_arrow_vs_minus_disambiguation_section_3_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L179)
- [`lex/operator_pipe_vs_pipe_bar_disambiguation_section_3_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L181)
- [`lex/operator_int_div_vs_slash_disambiguation_section_3_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L183)
- [`lex/operator_eq_vs_assign_disambiguation_section_3_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L185)
- [`lex/operator_dotdot_vs_dot_disambiguation_section_3_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L187)

<details>
<summary>Show test source</summary>

```haskell
    test "lex/operator_arrow_vs_minus_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "->-" === [TArrow, TMinus])
    test "lex/operator_pipe_vs_pipe_bar_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "|>|" === [TPipe, TPipeBar])
    test "lex/operator_int_div_vs_slash_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "// /" === [TIntDiv, TSlash])
    test "lex/operator_eq_vs_assign_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "===" === [TEq, TAssign])
    test "lex/operator_dotdot_vs_dot_disambiguation_section_3_5" <|
                Prelude.pure (firstTokens "..." === [TDotDot, TDot])
```

</details>
<!-- END tests:3_5 -->


### 3.6 Comments

**Line comments.** A `#` character that is not inside a string literal
starts a comment that runs to the end of the line. The comment, including
the leading `#`, MUST be discarded by the lexer.

```quone
# This is a line comment.

normalize : Double -> Double -> Double
normalize max_score raw <-
    raw / max_score    # divide by the cap
```

`#` matches R's own comment syntax, so a Quone source file and its
generated R use the same comment character.

**Doc comments.** Lines that begin (after any leading whitespace) with the
two-character sequence `#'` are **doc comments**. They are line comments
at the lexical level - the lexer treats them exactly the same as `#` line
comments - but they carry an additional convention:

- A run of consecutive `#'` lines immediately preceding a top-level
  declaration is the **doc block** for that declaration.
- In package mode (see
  [section 14](#14-project-model-and-package-generation)), the generator
  MUST emit each doc block as a `roxygen2`-style documentation block above
  the lowered R definition, preserving the line text after the `#'`
  prefix.
- In script mode the doc block has no special effect; it lowers like any
  other comment.

The body of a doc block is interpreted by `roxygen2`. The
`@export` tag in particular controls whether the binding appears in the
generated R `NAMESPACE` file (see
[section 14.6](#146-multi-module-package-layout)). Use it to mark the
package's R-level public API:

```quone
#' Compute a normalised score in the range [0, 1].
#'
#' @param max_score The maximum possible score.
#' @param raw The raw value.
#' @export
normalize : Double -> Double -> Double
normalize max_score raw <-
    raw / max_score
```

**Block comments.** Quone v0.0.1 has no block comment syntax. Use
repeated `#` lines, as is conventional in R itself. A nestable
`{- ... -}` form is `[planned]` and MAY be added in a later revision.

<!-- BEGIN tests:3_6 -->
**Tests** (6):

- [`lex/comment_line_dropped_section_3_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L240)
- [`lex/comment_line_alone_yields_eof_section_3_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L242)
- [`lex/comment_doc_single_line_section_3_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L244)
- [`lex/comment_doc_multi_line_collapsed_section_3_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L249)
- [`lex/comment_doc_strips_one_leading_space_section_3_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L254)
- [`lex/comment_doc_then_code_attached_section_3_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L259)

<details>
<summary>Show test source</summary>

```haskell
    test "lex/comment_line_dropped_section_3_6" <|
        Prelude.pure (firstTokens "x # this is dropped" === [TLowerIdent "x"])
    test "lex/comment_line_alone_yields_eof_section_3_6" <|
        Prelude.pure (firstTokens "# nothing else" === [])
    test "lex/comment_doc_single_line_section_3_6" <|
        Prelude.pure
            ( firstTokens "#' Hello"
                === [TDocBlock "Hello"]
            )
    test "lex/comment_doc_multi_line_collapsed_section_3_6" <|
        Prelude.pure
            ( firstTokens "#' Line one\n#' Line two"
                === [TDocBlock "Line one\nLine two"]
            )
    test "lex/comment_doc_strips_one_leading_space_section_3_6" <|
        Prelude.pure
            ( firstTokens "#'  two spaces"
                === [TDocBlock " two spaces"]
            )
    test "lex/comment_doc_then_code_attached_section_3_6" <|
        Prelude.pure
            ( assert
                (List.elem (TDocBlock "doc") (allTokens "#' doc\nx"))
                "doc block should appear before the next token"
            )
```

</details>
<!-- END tests:3_6 -->


### 3.7 Indentation and layout

Quone is indentation-sensitive in practice: parsers use indentation guards to
decide when an expression body ends.

For v0.0.1, the normative rules are:

- Indentation MUST use spaces. Tabs are reserved and MUST NOT appear as
  layout-significant whitespace.
- A top-level declaration ends when a subsequent non-blank line begins at
  column 0.
- The body of a `let`, `case` arm, or function declaration MUST be indented
  more deeply than the keyword that opened it.

Finer indentation rules - exact column behaviour for nested `case`, layout
inside dataframe verbs, and continuation lines - are `[planned]`. See
[section 19](#19-open-questions-and-future-work).

<!-- BEGIN tests:3_7 -->
**Tests** (3):

- [`lex/layout_indent_emitted_section_3_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L276)
- [`lex/layout_dedent_emitted_section_3_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L285)
- [`lex/layout_eof_closes_layers_section_3_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L294)

<details>
<summary>Show test source</summary>

```haskell
    test "lex/layout_indent_emitted_section_3_7" <|
        let
            toks = allTokens "let\n  x"
        in
        Prelude.pure
            ( assert
                (TIndent `List.elem` toks)
                "TIndent expected when body indents"
            )
    test "lex/layout_dedent_emitted_section_3_7" <|
        let
            toks = allTokens "let\n  x\ny"
        in
        Prelude.pure
            ( assert
                (TDedent `List.elem` toks)
                "TDedent expected when indent decreases"
            )
    test "lex/layout_eof_closes_layers_section_3_7" <|
        let
            toks = allTokens "let\n  x\n  y"
            eof = Prelude.last toks
            dedents =
                Prelude.length (Prelude.filter (Prelude.== TDedent) toks)
        in
        Prelude.pure
            ( assert
                (eof Prelude.== TEof Prelude.&& dedents Prelude.>= 1)
                "EOF should close any open indent layers"
            )
```

</details>
<!-- END tests:3_7 -->


#### 3.7.1 Multi-variant `type` declarations

In multi-line `type` declarations, the formatter SHOULD right-align the
leading tokens by inserting one space before each `|` so that `<-` (two
characters) and `|` (one character) form a single right-aligned column.
This keeps constructor names in a single left-aligned column to the right
of those tokens.

Recommended layout:

```quone
type Maybe a
    <- Nothing
     | Just a

type Result a
    <- Ok a
     | Err Character
```

Single-line declarations such as `type Bit <- Zero | One` are unaffected.

---

## 4. Modules, source files, packages, and exports

### 4.1 Source files

A Quone source file consists of:

1. an optional module declaration, followed by
2. zero or more top-level declarations.

A file with no module declaration is a script. A file with a module
declaration is a Quone module. The Quone-level concept of a "module" is
distinct from the R-level concept of a "package": a Quone project is one
or more modules that together compile to a single R package in package
mode (see [section 14.6](#146-multi-module-package-layout)).

<!-- BEGIN tests:4_1 -->
**Tests** (2):

- [`parse/script_no_module_header_section_4_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L54)
- [`parse/multiple_top_level_decls_section_4_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L60)

<details>
<summary>Show test source</summary>

```haskell
    test "parse/script_no_module_header_section_4_1" <|
        case parseProgram "x <- 1" of
            Prelude.Right (CProgram {programModule = Nothing, programDecls = [CDValue _]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/multiple_top_level_decls_section_4_1" <|
        case parseProgram "x <- 1\ny <- 2" of
            Prelude.Right (CProgram {programDecls = decls}) ->
                Prelude.pure (Prelude.length decls === 2)
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:4_1 -->


### 4.2 Module declarations

A module declaration names the module and lists the bindings it exposes
to other Quone modules in the same project. This is the standard
functional-language module-encapsulation model: bindings not listed in
`exporting (...)` are private to the module and cannot be `import`ed
elsewhere.

R-level visibility (whether a binding appears in the generated R
`NAMESPACE` file) is controlled separately by the `@export` doc tag; see
[section 14.6](#146-multi-module-package-layout). A binding can be
Quone-exported and R-internal, or Quone-exported and R-exported, but
cannot be R-exported without first being Quone-exported.

```quone
module Stats.Transform exporting (rmse, normalize)
```

To export every top-level binding, use the wildcard form:

```quone
module Stats.Transform exporting (..)
```

The dotted name to the left of `exporting` is the module path. Module paths
are case-sensitive and MUST be composed of uppercase identifiers separated by
`.`.

<!-- BEGIN tests:4_2 -->
**Tests** (2):

- [`parse/module_decl_explicit_exports_section_4_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L71)
- [`parse/module_decl_wildcard_export_section_4_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L83)

<details>
<summary>Show test source</summary>

```haskell
    test "parse/module_decl_explicit_exports_section_4_2" <|
        case parseProgram "module Stats.Transform exporting (normalize, rmse)" of
            Prelude.Right (CProgram {programModule = Just m}) ->
                Prelude.pure
                    ( assert
                        ( Prelude.length (moduleDeclPath m) Prelude.== 2
                            Prelude.&& isExportNames (moduleDeclExports m)
                        )
                        "expected dotted path with two segments and explicit exports"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/module_decl_wildcard_export_section_4_2" <|
        case parseProgram "module Foo exporting (..)" of
            Prelude.Right (CProgram {programModule = Just m}) ->
                Prelude.pure
                    ( case moduleDeclExports m of
                        CExportAll _ -> Pass
                        _ -> Fail "expected CExportAll"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:4_2 -->


### 4.3 Top-level declaration kinds

The v0.0.1 declaration kinds are:

- custom type declarations
- type alias declarations
- value declarations
- import declarations (for both Quone module imports and foreign R
  bindings; see [section 4.5](#45-imports))

The grammar for each is given in [section 5](#5-concrete-syntax) and the
typing rules in [sections 7](#7-type-system) and
[8](#8-static-semantics).

<!-- BEGIN tests:4_3 -->
**Tests** (2):

- [`resolve/duplicate_decl_rejected_section_4_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L179)
- [`resolve/no_duplicates_passes_section_4_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L196)

<details>
<summary>Show test source</summary>

```haskell
    test "resolve/duplicate_decl_rejected_section_4_3" <|
        let
            src =
                T.unlines
                    [ "x <- 1"
                    , "x <- 2"
                    ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.not (Prelude.null (resolveProgram p)))
                        "expected a duplicate-binding diagnostic"
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "resolve/no_duplicates_passes_section_4_3" <|
        let
            src =
                T.unlines
                    [ "x <- 1"
                    , "y <- 2"
                    ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.null (resolveProgram p))
                        ("expected no diagnostics; got "
                            Prelude.<> T.pack (Prelude.show (resolveProgram p)))
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
```

</details>
<!-- END tests:4_3 -->


### 4.4 Project model

Projects can be compiled in script mode or package mode. The choice between
the two is a compiler concern, not a language concern; see
[section 14](#14-project-model-and-package-generation).

### 4.5 Imports

The `import` keyword is shared by two distinct declaration forms,
distinguished by the case of the first path segment.

**Quone module imports.** A path whose first segment is `UpperCamelCase`
is a Quone module import. No type annotation is allowed: the type comes
from the source module's declaration. The imported name MUST appear in
the source module's `exporting (..)` list.

```quone
# Single name. The last path segment is brought into local scope as
# the unqualified name. Both `Stats.Transform.normalize` and `normalize`
# are then valid call forms in this module.
import Stats.Transform.normalize

# Multiple names from the same module. Each listed name is brought
# into local scope unqualified.
import Stats.Transform (normalize, rmse)

# Every exported name from the module (parallels `exporting (..)` in
# the module declaration).
import Stats.Transform (..)
```

The single-name form may import a function (`LowerIdent`), a type
(`UpperIdent`), or a constructor (`UpperIdent`). The multi-name form
likewise mixes case as needed:

```quone
import Stats.Transform (Maybe, Just, Nothing, normalize)
```

**Foreign R imports.** A path whose first segment is lowercase is a
foreign R import. A type annotation is REQUIRED, because R has no Quone
types of its own. The package prefix (if any) names the R namespace and
contributes to the runtime dependency set per
[section 13.9](#139-foreign-r-bindings-and-runtime-dependencies).

```quone
# Function from a named R package.
import readr.read_csv : Character -> dataframe { name : Vector Character }

# Function from base R (no package prefix).
import sqrt : Double -> Double
```

The last path segment is brought into local scope as the unqualified
name. Calls lower to the corresponding `pkg::fn(...)` form (or
unqualified for base R).

**Name collisions.** Two imports producing the same local name in a
single module MUST be a compile error. The user can resolve the
conflict by removing one import and using the qualified form
(`Stats.Transform.normalize`) at the call site, or by importing only one
of the conflicting names.

**Visibility check.** For Quone module imports, the imported name MUST
appear in the source module's `exporting (..)` list (the standard FP
module-encapsulation rule; see
[section 4.2](#42-module-declarations)). For foreign R imports, no
visibility check applies - any R function the user is willing to type is
importable.

<!-- BEGIN tests:4_5 -->
**Tests** (9):

- [`parse/foreign_import_section_4_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L162)
- [`parse/quone_import_single_name_section_4_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L171)
- [`parse/quone_import_multi_name_section_4_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L177)
- [`parse/quone_import_wildcard_section_4_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L183)
- [`resolve/import_visible_export_section_4_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L255)
- [`resolve/import_invisible_name_rejected_section_4_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L272)
- [`resolve/import_unknown_module_rejected_section_4_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L303)
- [`type/foreign_import_binds_value_section_4_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L291)
- [`type/foreign_import_argument_type_enforced_section_4_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L302)

<details>
<summary>Show test source</summary>

```haskell
    test "parse/foreign_import_section_4_5" <|
        let
            src = "import readr.read_csv : Character -> dataframe { name : Vector Character }"
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDImport (CForeignImport _ _ _)]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/quone_import_single_name_section_4_5" <|
        case parseProgram "import Stats.Transform.normalize" of
            Prelude.Right (CProgram {programDecls = [CDImport (CQuoneImport _ _ (CImportSingle _))]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/quone_import_multi_name_section_4_5" <|
        case parseProgram "import Stats.Transform (normalize, rmse)" of
            Prelude.Right (CProgram {programDecls = [CDImport (CQuoneImport _ _ (CImportNames items))]}) ->
                Prelude.pure (Prelude.length items === 2)
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/quone_import_wildcard_section_4_5" <|
        case parseProgram "import Stats.Transform (..)" of
            Prelude.Right (CProgram {programDecls = [CDImport (CQuoneImport _ _ CImportAll)]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "resolve/import_visible_export_section_4_5" <|
        case (desugarSource sourceModSrc, desugarSource importerSrc) of
            (Prelude.Right sourceProg, Prelude.Right importerProg) ->
                let
                    syms = Map.singleton
                        (case programModule sourceProg of
                            Just m -> modulePath m
                            Nothing -> [])
                        (collectSymbols sourceProg)
                in
                Prelude.pure
                    ( assert
                        (Prelude.null (resolveProject syms importerProg))
                        ("expected no diagnostics; got "
                            Prelude.<> T.pack (Prelude.show (resolveProject syms importerProg)))
                    )
            _ -> Prelude.pure (Fail "could not parse fixtures")
    test "resolve/import_invisible_name_rejected_section_4_5" <|
        let
            sourceSrc =
                T.unlines
                    [ "module Stats.Transform exporting (rmse)"
                    , ""
                    , "rmse <- 1"
                    , "internal_helper <- 2"
                    ]
            importerSrcLocal =
                T.unlines
                    [ "module Main exporting (..)"
                    , ""
                    , "import Stats.Transform.internal_helper"
                    ]
        in
        case (desugarSource sourceSrc, desugarSource importerSrcLocal) of
            (Prelude.Right sourceProg, Prelude.Right importerProg) ->
                let
                    syms = Map.singleton
                        (modulePathOf sourceProg)
                        (collectSymbols sourceProg)
                    diags = resolveProject syms importerProg
                in
                Prelude.pure
                    ( assert
                        (Prelude.length diags Prelude.== 1)
                        ("expected exactly one diagnostic; got "
                            Prelude.<> T.pack (Prelude.show diags))
                    )
            _ -> Prelude.pure (Fail "could not parse fixtures")
    test "resolve/import_unknown_module_rejected_section_4_5" <|
        let
            importerSrcLocal =
                T.unlines
                    [ "module Main exporting (..)"
                    , ""
                    , "import Other.Module.thing"
                    ]
        in
        case desugarSource importerSrcLocal of
            Prelude.Right importerProg ->
                Prelude.pure
                    ( assert
                        (Prelude.not
                            (Prelude.null (resolveProject Map.empty importerProg))
                        )
                        "expected an unknown-module diagnostic"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "type/foreign_import_binds_value_section_4_5" <|
        -- A foreign-imported name can be referenced like any value.
        case infer
            ( T.unlines
                [ "import readr.read_csv : Character -> Integer"
                , ""
                , "load path <- read_csv path"
                ]
            ) of
            Prelude.Right _ -> Prelude.pure Pass
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "type/foreign_import_argument_type_enforced_section_4_5" <|
        Prelude.pure
            ( assertLeft
                ( infer
                    ( T.unlines
                        [ "import readr.read_csv : Character -> Integer"
                        , ""
                        , "x <- read_csv 42"
                        ]
                    )
                )
            )
```

</details>
<!-- END tests:4_5 -->


---

## 5. Concrete syntax

This section consolidates the v0.0.1 grammar in a single normative block. Where
the grammar is intentionally underspecified for v0.0.1, a comment marks the
gap.

### 5.1 Grammar

```ebnf
Program        ::= [ModuleDecl] Decl*

ModuleDecl     ::= "module" DottedName "exporting" "(" ExportList ")"
ExportList     ::= ".." | IdentList
IdentList      ::= LowerIdent ("," LowerIdent)*
DottedName     ::= UpperIdent ("." UpperIdent)*

Decl           ::= TypeDecl
                 | TypeAliasDecl
                 | ImportDecl
                 | ValueDecl

TypeDecl       ::= "type" UpperIdent LowerIdent* "<-" Variant ("|" Variant)*
Variant        ::= UpperIdent TypeAtom*

TypeAliasDecl  ::= "type" "alias" UpperIdent LowerIdent* "<-" TypeSig

ImportDecl     ::= QuoneImport | ForeignImport

QuoneImport    ::= "import" QualifiedName
                 | "import" DottedUpperName "(" QuoneImportList ")"
                 (* Quone module import. Distinguished from ForeignImport
                    by the first path segment being UpperCamelCase. The
                    second form imports multiple names from the same
                    module; see section 4.5. *)

ForeignImport  ::= "import" DottedLowerName ":" TypeSig
                 (* Foreign R import. The lowercase first segment names
                    the R namespace and contributes to the runtime
                    dependency set; the type annotation is required. See
                    sections 4.5 and 13.9. *)

DottedUpperName ::= UpperIdent ("." UpperIdent)*
QualifiedName   ::= DottedUpperName "." (LowerIdent | UpperIdent)
QuoneImportList ::= ".." | ImportItem ("," ImportItem)*
ImportItem      ::= LowerIdent | UpperIdent
DottedLowerName ::= LowerIdent ("." LowerIdent)*

ValueDecl      ::= [TypeAnnotation] LowerIdent LowerIdent* "<-" Expr
TypeAnnotation ::= LowerIdent ":" TypeSig

TypeSig        ::= TypeApp ("->" TypeSig)?
TypeApp        ::= TypeAtom TypeAtom*
TypeAtom       ::= UpperIdent
                 | LowerIdent
                 | "(" TypeSig ")"
                 | RecordType
                 | DataframeType
RecordType     ::= "{" FieldType ("," FieldType)* "}"
FieldType      ::= LowerIdent ":" TypeSig
DataframeType  ::= "dataframe" RecordType

Expr           ::= Lambda
                 | IfExpr
                 | CaseExpr
                 | LetExpr
                 | PipeExpr

Lambda         ::= "\\" LowerIdent+ "->" Expr
IfExpr         ::= "if" Expr "then" Expr "else" Expr
                 (* Surface-only; desugars to CaseExpr. See section 5.3. *)
CaseExpr       ::= "case" Expr "of" CaseArm+
CaseArm        ::= Pattern "->" Expr
LetExpr        ::= "let" Binding+ "in" Expr
Binding        ::= LowerIdent "<-" Expr

PipeExpr       ::= CmpExpr ("|>" CmpExpr)*
CmpExpr        ::= AddExpr (CmpOp AddExpr)*
AddExpr        ::= MulExpr (AddOp MulExpr)*
MulExpr        ::= ExpExpr (MulOp ExpExpr)*
ExpExpr        ::= UnaryExpr (ExpOp ExpExpr)?       (* right-associative *)
UnaryExpr      ::= "-" UnaryExpr | AppExpr
AppExpr        ::= AccessExpr AccessExpr*
AccessExpr     ::= Primary ("." LowerIdent)*

Primary        ::= LowerIdent
                 | UpperIdent
                 | Literal
                 | "(" Expr ")"
                 | RecordLit
                 | RecordUpdate
                 | DataframeLit
                 | VectorLit
                 | DplyrVerb

RecordLit      ::= "{" FieldBinding ("," FieldBinding)* "}"
RecordUpdate   ::= "{" Expr "|" FieldBinding ("," FieldBinding)* "}"
                 (* Distinguished from RecordLit by the "|" after the
                    initial expression (Elm-style). See section 8.5. *)
FieldBinding   ::= LowerIdent "=" Expr
DataframeLit   ::= "dataframe" RecordLit
VectorLit      ::= "[" (Expr ("," Expr)*)? "]"

DplyrVerb      ::= VerbName DplyrArg*
VerbName       ::= "select"   | "filter"   | "mutate"      | "summarize"
                 | "group_by" | "ungroup"  | "arrange"     | "rename"
                 | "distinct" | "distinct_all"             | "count"
                 | "slice"    | "pull"     | "relocate"    | "transmute"
                 | "mutate_each"           | "summarize_each"
                 | "left_join"  | "right_join"  | "inner_join"
                 | "full_join"  | "anti_join"   | "semi_join"
                 | "cross_join"
DplyrArg       ::= Expr
                 | RecordLit
                 | "(" Expr ")"
                 | Modifier
                 | JoinOn
Modifier       ::= "desc" LowerIdent
                 | "asc"  LowerIdent
                 | "as"   StringLit
                 | "where" "(" Expr ")"
                 | "cols" "{" LowerIdent ("," LowerIdent)* "}"
JoinOn         ::= Expr "on" "{" JoinPair ("," JoinPair)* "}"
JoinPair       ::= LowerIdent "=" LowerIdent | LowerIdent

Pattern        ::= "_"
                 | LowerIdent
                 | IntLit
                 | FloatLit
                 | StringLit
                 | UpperIdent Pattern*
                 | RecordPattern
                 | "(" Pattern ")"

RecordPattern  ::= "{" RecordPatField ("," RecordPatField)* "}"
RecordPatField ::= LowerIdent
                 | LowerIdent "=" Pattern

CmpOp          ::= "==" | "!=" | ">" | "<" | ">=" | "<="
AddOp          ::= "+" | "-"
MulOp          ::= "*" | "/" | "//" | "%"
ExpOp          ::= "^"

Literal        ::= IntLit | FloatLit | StringLit
                 (* Boolean values are constructors `True` / `False` of the
                    built-in `Logical` custom type; see section 7.5.1.
                    Quone v0.0.1 has no unit literal; "absence of value" is
                    expressed via `Maybe` and "fallible success" via
                    `Result`. *)
```

<!-- BEGIN tests:5_1 -->
**Tests** (20):

- [`parse/empty_program_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L52)
- [`parse/value_decl_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L110)
- [`parse/value_decl_with_annotation_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L116)
- [`parse/type_decl_single_variant_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L133)
- [`parse/type_decl_multi_variant_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L139)
- [`parse/type_alias_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L153)
- [`parse/expr_application_curried_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L200)
- [`parse/expr_lambda_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L210)
- [`parse/expr_let_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L220)
- [`parse/expr_pipe_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L233)
- [`parse/expr_record_literal_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L243)
- [`parse/expr_record_update_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L253)
- [`parse/expr_dataframe_literal_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L263)
- [`parse/expr_vector_literal_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L273)
- [`parse/expr_field_access_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L283)
- [`parse/expr_dplyr_verb_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L293)
- [`parse/literal_integer_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L389)
- [`parse/literal_bare_digits_is_double_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L400)
- [`parse/literal_double_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L409)
- [`parse/literal_string_section_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L418)

<details>
<summary>Show test source</summary>

```haskell
    test "parse/empty_program_section_5_1" <|
        Prelude.pure (assertRight (parseProgram ""))
    test "parse/value_decl_section_5_1" <|
        case parseProgram "x <- 1" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure (lowerNameText (valueDeclName v) === "x")
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/value_decl_with_annotation_section_5_1" <|
        let
            src =
                T.unlines
                    [ "x : Integer"
                    , "x <- 1"
                    ]
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( assert
                        (Prelude.maybe Prelude.False (Prelude.const Prelude.True) (valueDeclAnnotation v))
                        "expected an annotation"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/type_decl_single_variant_section_5_1" <|
        case parseProgram "type Bit <- Zero" of
            Prelude.Right (CProgram {programDecls = [CDType d]}) ->
                Prelude.pure (Prelude.length (typeDeclVariants d) === 1)
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/type_decl_multi_variant_section_5_1" <|
        let
            src =
                T.unlines
                    [ "type Maybe a"
                    , "    <- Nothing"
                    , "     | Just a"
                    ]
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDType d]}) ->
                Prelude.pure (Prelude.length (typeDeclVariants d) === 2)
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/type_alias_section_5_1" <|
        let
            src = "type alias Score <- Double"
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDTypeAlias _]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_application_curried_section_5_1" <|
        case parseProgram "x <- f 1 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( assert
                        (isApp (valueDeclBody v))
                        "expected application"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_lambda_section_5_1" <|
        case parseProgram "x <- \\a b -> a + b" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELambda _ params _ -> Prelude.length params === 2
                        _ -> Fail "expected lambda"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_let_section_5_1" <|
        let
            src = "x <- let y <- 1 in y + 2"
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELet _ bindings _ -> Prelude.length bindings === 1
                        _ -> Fail "expected let"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_pipe_section_5_1" <|
        case parseProgram "x <- xs |> f |> g" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( assert
                        (isPipe (valueDeclBody v))
                        "expected pipe expression"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_record_literal_section_5_1" <|
        case parseProgram "x <- { name = \"Alice\", score = 92.0 }" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CERecord _ fields -> Prelude.length fields === 2
                        _ -> Fail "expected record literal"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_record_update_section_5_1" <|
        case parseProgram "x <- { student | score = 95.0 }" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CERecordUpdate _ _ fields -> Prelude.length fields === 1
                        _ -> Fail (T.pack (Prelude.show (valueDeclBody v)))
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_dataframe_literal_section_5_1" <|
        case parseProgram "x <- dataframe { name = [\"a\"], score = [1.0] }" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEDataframe _ fields -> Prelude.length fields === 2
                        _ -> Fail "expected dataframe literal"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_vector_literal_section_5_1" <|
        case parseProgram "x <- [1, 2, 3]" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEVector _ items -> Prelude.length items === 3
                        _ -> Fail "expected vector literal"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_field_access_section_5_1" <|
        case parseProgram "x <- row.score" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEField _ _ _ -> Pass
                        _ -> Fail "expected field access"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/expr_dplyr_verb_section_5_1" <|
        case parseProgram "x <- students |> filter (score > 70.0)" of
            Prelude.Right (CProgram {programDecls = [CDValue _]}) -> Prelude.pure Pass
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/literal_integer_section_5_1" <|
        -- Per LANGUAGE.md section 3.3, an Integer literal MUST carry
        -- the trailing `L` suffix; a bare `42` is a Double.
        case parseProgram "x <- 42L" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELit _ (CLInt 42) -> Pass
                        _ -> Fail "expected CLInt 42"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/literal_bare_digits_is_double_section_5_1" <|
        case parseProgram "x <- 42" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELit _ (CLDouble _) -> Pass
                        _ -> Fail "expected CLDouble for bare 42"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/literal_double_section_5_1" <|
        case parseProgram "x <- 3.14" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELit _ (CLDouble _) -> Pass
                        _ -> Fail "expected CLDouble"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/literal_string_section_5_1" <|
        case parseProgram "x <- \"hello\"" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELit _ (CLChar "hello") -> Pass
                        _ -> Fail "expected CLChar"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:5_1 -->


### 5.2 Operator precedence

From lowest to highest binding strength:

| Level | Construct                                  |
| ----- | ------------------------------------------ |
| 1     | pipe (`\|>`)                               |
| 2     | equality and comparison                    |
| 3     | additive (`+`, `-`)                        |
| 4     | multiplicative (`*`, `/`, `//`, `%`)       |
| 5     | exponentiation (`^`), right-associative    |
| 6     | unary minus                                |
| 7     | function application                       |
| 8     | postfix field access (`.`)                 |
| 9     | primary expressions                        |

All binary operators at a given level are left-associative unless this
document specifies otherwise. Exponentiation (`^`) is **right-associative**:
`2 ^ 3 ^ 2` means `2 ^ (3 ^ 2)`. Unary minus binds tighter than `^`, so
`-2 ^ 2` means `(-2) ^ 2 = 4` rather than R's `-(2 ^ 2) = -4`.

<!-- BEGIN tests:5_2 -->
**Tests** (6):

- [`parse/precedence_addition_left_assoc_section_5_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L318)
- [`parse/precedence_caret_right_assoc_section_5_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L328)
- [`parse/precedence_neg_tighter_than_caret_section_5_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L338)
- [`parse/precedence_mul_tighter_than_add_section_5_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L348)
- [`parse/precedence_pipe_lowest_section_5_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L358)
- [`parse/precedence_app_tighter_than_mul_section_5_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L368)

<details>
<summary>Show test source</summary>

```haskell
    test "parse/precedence_addition_left_assoc_section_5_2" <|
        -- 1 + 2 + 3 should parse as ((1 + 2) + 3)
        case parseProgram "x <- 1 + 2 + 3" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpAdd (CEBinOp _ COpAdd _ _) _ -> Pass
                        other -> Fail ("expected ((1+2)+3); got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/precedence_caret_right_assoc_section_5_2" <|
        -- 2 ^ 3 ^ 2 should parse as (2 ^ (3 ^ 2)) = 2 ^ 9 = 512
        case parseProgram "x <- 2 ^ 3 ^ 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpExp _ (CEBinOp _ COpExp _ _) -> Pass
                        other -> Fail ("expected (2 ^ (3 ^ 2)); got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/precedence_neg_tighter_than_caret_section_5_2" <|
        -- -2 ^ 2 should parse as (-2) ^ 2 = 4 (NOT R's -(2^2) = -4)
        case parseProgram "x <- -2 ^ 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpExp (CEUnary _ COpNeg _) _ -> Pass
                        other -> Fail ("expected ((-2) ^ 2); got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/precedence_mul_tighter_than_add_section_5_2" <|
        -- 1 + 2 * 3 should parse as 1 + (2 * 3)
        case parseProgram "x <- 1 + 2 * 3" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpAdd _ (CEBinOp _ COpMul _ _) -> Pass
                        other -> Fail ("expected (1 + (2 * 3)); got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/precedence_pipe_lowest_section_5_2" <|
        -- xs |> f + 1 should parse as xs |> (f + 1)
        case parseProgram "x <- xs |> f + 1" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEPipe _ _ (CEBinOp _ COpAdd _ _) -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/precedence_app_tighter_than_mul_section_5_2" <|
        -- f x * 2 should parse as (f x) * 2
        case parseProgram "x <- f x * 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpMul (CEApp _ _ _) _ -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:5_2 -->


### 5.3 Surface-only forms

Some surface forms are accepted by the parser but desugared away before the
abstract syntax is constructed. They have no independent abstract-syntax
node in [section 6](#6-abstract-syntax) and no independent typing or
translation rule.

**`if` / `then` / `else`.**

`if e1 then e2 else e3` desugars to:

```quone
case e1 of
    True  -> e2
    False -> e3
```

The desugaring is performed during AST construction. All static-semantics
rules ([section 8.4](#84-pattern-matching)) and translation rules
([section 13.6](#136-pattern-matching)) for `if` are inherited from
`case`. Code generators MAY recognise this exact `case`-on-`Logical` shape
and emit R's native `if (cond) ... else ...` instead of an `if` / `else
if` chain over constructor tags; see
[section 13.6](#136-pattern-matching).

<!-- BEGIN tests:5_3 -->
**Tests** (3):

- [`ast/if_desugars_to_case_section_5_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/AstTests.hs#L51)
- [`parse/if_kept_in_cst_section_5_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L486)
- [`type/if_desugared_to_case_typed_section_5_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L204)

<details>
<summary>Show test source</summary>

```haskell
    test "ast/if_desugars_to_case_section_5_3" <|
        case desugarSource "x <- if c then 1 else 2" of
            Prelude.Right p -> case Prelude.head (programDecls p) of
                DValue v -> case valueDeclBody v of
                    ECase _ _ arms ->
                        Prelude.pure
                            ( assert
                                (Prelude.length arms Prelude.== 2 Prelude.&&
                                    Prelude.all (\a -> isLogicalCon (caseArmPattern a)) arms)
                                ("expected 2 case arms with True/False patterns; got " Prelude.<> T.pack (Prelude.show arms))
                            )
                    other ->
                        Prelude.pure (Fail ("expected ECase; got " Prelude.<> T.pack (Prelude.show other)))
                _ -> Prelude.pure (Fail "first decl was not DValue")
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/if_kept_in_cst_section_5_3" <|
        -- The CST keeps `if`; the desugar pass converts it to `case`.
        -- Test 16.5's "no EIf in AST" is a desugar-stage concern.
        case parseProgram "x <- if c then 1 else 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEIf _ _ _ _ -> Pass
                        other -> Fail ("expected CEIf in CST; got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "type/if_desugared_to_case_typed_section_5_3" <|
        infersTo "x <- if True then 1L else 2L" "x" "Integer"
```

</details>
<!-- END tests:5_3 -->


### 5.4 Patterns

The patterns supported in v0.0.1 are:

- wildcard `_`
- variable pattern `x`
- integer literal pattern `1`
- double literal pattern `1.0`
- character literal pattern `"hello"`
- constructor pattern `Just x` (also covers `True` and `False` as zero-arg
  constructors of `Logical`); arguments may themselves be any pattern, so
  nested forms like `Just (Just n)` are valid
- record pattern `{ name, score }` for matching and binding fields by name;
  the longer form `{ name = n, age = _ }` allows renaming and further
  destructuring
- parenthesized pattern `(Just x)`

The remaining pattern forms - vector patterns (`[a, b, c]`,
`[head | tail]`), guards (`Just n | n > 0 -> ...`), and as-patterns
(`x@(Just n) -> ...`) - are `[planned]` for a later revision.

<!-- BEGIN tests:5_4 -->
**Tests** (4):

- [`parse/pattern_wildcard_section_5_4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L438)
- [`parse/pattern_constructor_section_5_4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L447)
- [`parse/pattern_record_short_section_5_4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L456)
- [`parse/pattern_record_full_section_5_4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ParseTests.hs#L466)

<details>
<summary>Show test source</summary>

```haskell
    test "parse/pattern_wildcard_section_5_4" <|
        case parseProgram "x <- case y of\n    _ -> 1" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CECase _ _ [CCaseArm {caseArmPattern = CPWildcard _}] -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/pattern_constructor_section_5_4" <|
        case parseProgram "x <- case y of\n    Just n -> n" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CECase _ _ [CCaseArm {caseArmPattern = CPCon _ _ [_]}] -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/pattern_record_short_section_5_4" <|
        case parseProgram "x <- case y of\n    { name, score } -> name" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CECase _ _ [CCaseArm {caseArmPattern = CPRecord _ fields}] ->
                            Prelude.length fields === 2
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "parse/pattern_record_full_section_5_4" <|
        case parseProgram "x <- case y of\n    { name = n } -> n" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CECase _ _ [CCaseArm {caseArmPattern = CPRecord _ [CRpfFull _ _ _]}] -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:5_4 -->


---

## 6. Abstract syntax

This section gives the v0.0.1 abstract syntax tree (AST) - the structure
the compiler manipulates after parsing and after the desugarings in
[section 5.3](#53-surface-only-forms) have been applied. The signatures
below are normative: any conforming implementation MUST produce trees
that are isomorphic to these definitions, and the typing rules in
[section 8](#8-static-semantics) and the lowering rules in
[section 13](#13-translation-to-r) refer to these constructors by name.

The notation is Haskell-style algebraic data types. Names like
`UpperName` and `LowerName` are the case-disambiguated identifier classes
from [section 3.2](#32-identifiers).

### 6.1 Program and modules

```haskell
data Program = Program (Maybe ModuleDecl) [Decl]

data ModuleDecl = ModuleDecl
    { modulePath    :: ModulePath
    , moduleExports :: ExportList
    }

type ModulePath = NonEmpty UpperName

data ExportList
    = ExportAll                       -- exporting (..)
    | ExportNames [ExportItem]
data ExportItem
    = ExportLower LowerName           -- a value or function
    | ExportUpper UpperName           -- a type or constructor
```

A `Program` whose first component is `Nothing` is a script
([section 4.1](#41-source-files)); otherwise it is a Quone module.

### 6.2 Declarations

```haskell
data Decl
    = DType        TypeDecl
    | DTypeAlias   TypeAliasDecl
    | DImport      ImportDecl
    | DValue       ValueDecl
```

```haskell
data TypeDecl = TypeDecl
    { typeName     :: UpperName
    , typeParams   :: [LowerName]
    , typeVariants :: NonEmpty Variant
    , typeDoc      :: Maybe DocBlock
    }
data Variant = Variant
    { variantName :: UpperName
    , variantArgs :: [TypeAtom]
    }

data TypeAliasDecl = TypeAliasDecl
    { aliasName   :: UpperName
    , aliasParams :: [LowerName]
    , aliasBody   :: TypeSig
    , aliasDoc    :: Maybe DocBlock
    }

data ImportDecl
    = QuoneImport   ModulePath ImportSelection
    | ForeignImport ForeignName TypeSig

data ImportSelection
    = ImportSingle ExportItem         -- import Stats.Transform.normalize
    | ImportNames  [ExportItem]       -- import Stats.Transform (normalize, rmse)
    | ImportAll                       -- import Stats.Transform (..)

data ForeignName = ForeignName
    { foreignPackage :: [LowerName]   -- empty list means base R
    , foreignFn      :: LowerName
    }

data ValueDecl = ValueDecl
    { valueAnnotation :: Maybe TypeSig
    , valueName       :: LowerName
    , valueParams     :: [LowerName]
    , valueBody       :: Expr
    , valueDoc        :: Maybe DocBlock
    }
```

### 6.3 Types

```haskell
data TypeSig
    = TFun  TypeSig  TypeSig          -- a -> b, right-associative
    | TApp  TypeAtom [TypeAtom]       -- Vector Double, Maybe Integer
    | TAtom TypeAtom

data TypeAtom
    = TName      UpperName            -- Integer, Maybe, Logical, Vector
    | TVar       LowerName            -- a, key, error_value
    | TParen     TypeSig              -- (a -> b)
    | TRecord    RecordType           -- { name : Character }
    | TDataframe RecordType           -- dataframe { ... }

data RecordType = RecordType [(LowerName, TypeSig)]
```

### 6.4 Expressions

```haskell
data Expr
    = ELit          Literal
    | EVar          LowerName
    | ECon          UpperName         -- a constructor used as a value (Just, Nothing)
    | ELambda       (NonEmpty LowerName) Expr
    | ECase         Expr (NonEmpty CaseArm)
    | ELet          (NonEmpty Binding) Expr
    | EApp          Expr Expr         -- function application (curried)
    | EBinOp        BinOp Expr Expr
    | EUnary        UnaryOp Expr
    | EPipe         Expr Expr         -- xs |> f
    | EField        Expr LowerName    -- record.field
    | ERecord       [(LowerName, Expr)]
    | ERecordUpdate Expr [(LowerName, Expr)]
    | EVector       [Expr]
    | EDataframe    [(LowerName, Expr)]
    | EVerb         Verb [DplyrArg]

data Literal
    = LInt    Integer
    | LDouble Double
    | LChar   Text

data BinOp
    = OpAdd | OpSub | OpMul | OpDiv | OpIntDiv | OpMod | OpExp
    | OpEq  | OpNeq | OpGt  | OpLt  | OpGe     | OpLe
data UnaryOp = OpNeg

data CaseArm = CaseArm Pattern Expr
data Binding = Binding LowerName Expr
```

`Expr` deliberately has no `EIf` constructor.
[Section 5.3](#53-surface-only-forms) desugars `if e1 then e2 else e3`
into `ECase e1 [CaseArm (PCon "True" []) e2, CaseArm (PCon "False" []) e3]`
during AST construction.

<!-- BEGIN tests:6_4 -->
**Tests** (1):

- [`ast/paren_dropped_section_6_4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/AstTests.hs#L81)

<details>
<summary>Show test source</summary>

```haskell
    test "ast/paren_dropped_section_6_4" <|
        case desugarSource "x <- (1 + 2)" of
            Prelude.Right p -> case Prelude.head (programDecls p) of
                DValue v -> case valueDeclBody v of
                    EBinOp _ OpAdd _ _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                _ -> Prelude.pure (Fail "first decl was not DValue")
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:6_4 -->


### 6.5 Patterns

```haskell
data Pattern
    = PWildcard
    | PVar    LowerName
    | PLit    Literal
    | PCon    UpperName [Pattern]     -- Just x, Nothing, True, False
    | PRecord [RecordPatField]
    | PParen  Pattern

data RecordPatField
    = RpfShort LowerName              -- { name }
    | RpfFull  LowerName Pattern      -- { name = pat }
```

### 6.6 Dataframe verbs and modifiers

```haskell
data Verb
    -- Single-table verbs
    = VSelect   | VFilter    | VMutate    | VSummarize | VGroupBy
    | VUngroup  | VArrange   | VRename    | VDistinct  | VDistinctAll
    | VCount    | VSlice     | VPull      | VRelocate  | VTransmute
    | VMutateEach | VSummarizeEach
    -- Joins
    | VLeftJoin  | VRightJoin | VInnerJoin | VFullJoin
    | VAntiJoin  | VSemiJoin  | VCrossJoin

data DplyrArg
    = DAExpr     Expr
    | DARecord   [(LowerName, Expr)]
    | DAModifier Modifier
    | DAJoinOn   Expr [(LowerName, LowerName)]  -- other on { lhs = rhs, ... }

data Modifier
    = MDesc  LowerName        -- arrange { desc col }
    | MAsc   LowerName        -- arrange { asc col }
    | MAs    Text             -- summarize_each ... as "..."
    | MWhere Expr             -- mutate_each (where ...)
    | MCols  [LowerName]      -- summarize_each (cols { ... })
```

<!-- BEGIN tests:6_6 -->
**Tests** (1):

- [`ast/verb_keyword_translates_section_6_6`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/AstTests.hs#L89)

<details>
<summary>Show test source</summary>

```haskell
    test "ast/verb_keyword_translates_section_6_6" <|
        case desugarSource "x <- students |> filter (score > 70.0)" of
            Prelude.Right p -> case Prelude.head (programDecls p) of
                DValue v -> case valueDeclBody v of
                    EPipe _ _ (EVerb _ VFilter _) -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                _ -> Prelude.pure (Fail "first decl was not DValue")
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:6_6 -->


### 6.7 Doc blocks

```haskell
data DocBlock = DocBlock { docLines :: [Text] }
```

A `DocBlock` is the run of `#'` lines immediately preceding a top-level
declaration ([section 3.6](#36-comments)). The lexer captures the text
after the `#'` prefix on each line; the body is interpreted by
`roxygen2` at package-build time
([section 14.6](#146-multi-module-package-layout)).

### 6.8 Well-formedness

The following invariants MUST hold on a conforming AST. They are
enforced by the compiler before typing and lowering.

1. **Module placement.** A `Program`'s `Maybe ModuleDecl` is the only
   module declaration; if present, no other AST node MUST refer to a
   `ModuleDecl`.
2. **Export consistency.** Every name in `moduleExports` MUST be defined
   by some `Decl` in the same `Program`.
3. **Quone import visibility.** For every `QuoneImport path sel`, each
   name in `sel` MUST appear in the source module's `moduleExports`
   (the standard FP module-encapsulation rule;
   [section 4.5](#45-imports)).
4. **R-export discipline.** A `ValueDecl` whose doc block contains
   `@export` MUST have its `valueName` listed in the enclosing module's
   `moduleExports` ([section 14.6](#146-multi-module-package-layout)).
5. **Constructor arity.** A `PCon name pats` MUST satisfy
   `length pats == arity(name)` for the corresponding `Variant` in
   scope.
6. **Record-update target.** An `ERecordUpdate r fields` MUST NOT have
   `r` of a `dataframe` type; the diagnostic SHOULD point at `mutate`
   ([section 8.5](#85-records-and-field-access)).
7. **Operator typing.** `EBinOp` and `EUnary` MUST satisfy the
   monomorphic rules in
   [section 8.8](#88-arithmetic-and-comparison-operators); mixed-
   primitive operands are a type error, not an AST error.
8. **Verb arguments.** The shape of `[DplyrArg]` accepted by each
   `Verb` is verb-specific and is part of section 9's typing rules; a
   structurally valid `EVerb` may still be rejected by typing.
9. **No `EIf` constructor.** `if` expressions are desugared during AST
   construction (section 5.3) and MUST NOT appear in the AST.

<!-- BEGIN tests:6_8 -->
**Tests** (1):

- [`resolve/collect_symbols_includes_constructors_section_6_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/ResolveTests.hs#L214)

<details>
<summary>Show test source</summary>

```haskell
    test "resolve/collect_symbols_includes_constructors_section_6_8" <|
        let
            src =
                T.unlines
                    [ "type Maybe a"
                    , "    <- Nothing"
                    , "     | Just a"
                    ]
        in
        case desugarSource src of
            Prelude.Right p ->
                let
                    syms = collectSymbols p
                in
                Prelude.pure
                    ( assert
                        (Map.member "Maybe" (modulesymLocals syms)
                            Prelude.&& Map.member "Just" (modulesymLocals syms)
                            Prelude.&& Map.member "Nothing" (modulesymLocals syms)
                        )
                        "expected Maybe, Just, Nothing in symbol table"
                    )
            Prelude.Left d ->
                Prelude.pure (Fail (T.pack (Prelude.show d)))
```

</details>
<!-- END tests:6_8 -->

<!-- BEGIN tests:6_8_inv2 -->
**Tests** (3):

- [`ast/validate_export_consistency_pos_section_6_8_inv2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/AstTests.hs#L114)
- [`ast/validate_export_consistency_neg_section_6_8_inv2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/AstTests.hs#L130)
- [`ast/validate_wildcard_export_skips_consistency_check_section_6_8_inv2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/AstTests.hs#L182)

<details>
<summary>Show test source</summary>

```haskell
    test "ast/validate_export_consistency_pos_section_6_8_inv2" <|
        let
            src = T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "x <- 1"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.null (validate p))
                        ("expected no diagnostics; got " Prelude.<> T.pack (Prelude.show (validate p)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "ast/validate_export_consistency_neg_section_6_8_inv2" <|
        let
            src = T.unlines
                [ "module Foo exporting (missing)"
                , ""
                , "x <- 1"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.not (Prelude.null (validate p)))
                        "expected a diagnostic about the missing export"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "ast/validate_wildcard_export_skips_consistency_check_section_6_8_inv2" <|
        let
            src = T.unlines
                [ "module Foo exporting (..)"
                , ""
                , "x <- 1"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.null (validate p))
                        ("wildcard exports never fail consistency; got " Prelude.<> T.pack (Prelude.show (validate p)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:6_8_inv2 -->

<!-- BEGIN tests:6_8_inv4 -->
**Tests** (2):

- [`ast/validate_r_export_discipline_pos_section_6_8_inv4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/AstTests.hs#L146)
- [`ast/validate_r_export_discipline_neg_section_6_8_inv4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/AstTests.hs#L163)

<details>
<summary>Show test source</summary>

```haskell
    test "ast/validate_r_export_discipline_pos_section_6_8_inv4" <|
        let
            src = T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "#' @export"
                , "x <- 1"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.null (validate p))
                        ("expected no diagnostics; got " Prelude.<> T.pack (Prelude.show (validate p)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    test "ast/validate_r_export_discipline_neg_section_6_8_inv4" <|
        let
            src = T.unlines
                [ "module Foo exporting (y)"
                , ""
                , "#' @export"
                , "x <- 1"
                , ""
                , "y <- 2"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.not (Prelude.null (validate p)))
                        "expected a diagnostic about @export on a non-exported binding"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:6_8_inv4 -->

<!-- BEGIN tests:6_8_invariant_9 -->
**Tests** (1):

- [`ast/no_eif_constructor_section_6_8_invariant_9`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/AstTests.hs#L66)

<details>
<summary>Show test source</summary>

```haskell
    test "ast/no_eif_constructor_section_6_8_invariant_9" <|
        -- This is structurally guaranteed: the AST has no EIf
        -- constructor. We assert the desugar produces a non-EIf shape.
        case desugarSource "x <- if c then 1 else 2" of
            Prelude.Right p -> case Prelude.head (programDecls p) of
                DValue v ->
                    Prelude.pure
                        ( assert
                            (case valueDeclBody v of
                                ECase _ _ _ -> Prelude.True
                                _ -> Prelude.False)
                            "if must lower to ECase, never EIf"
                        )
                _ -> Prelude.pure (Fail "first decl was not DValue")
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
```

</details>
<!-- END tests:6_8_invariant_9 -->





---

## 7. Type system

### 7.1 Primitive types

The v0.0.1 primitive types are:

- `Integer`
- `Double`
- `Character`

`Logical` is not a primitive; it is a built-in custom type defined in
[section 7.5.1](#751-built-in-types).

### 7.2 Compound types

- function types: `a -> b`
- applied named types: `Vector Double`, `Maybe Integer`
- records: `{ name : Character, score : Double }`
- dataframe types: `dataframe { ... }`

`Vector a` is Quone's homogeneous-sequence type. Every element has the same
type `a`. R's term "vector" is broader (it covers atomic vectors and
heterogeneous lists); Quone narrows it to mean exactly the homogeneous case.
The heterogeneous "named collection" use is covered by records, and the
tabular "vector of rows with a uniform schema" use is covered by `dataframe`
types.

The lowering of `Vector a` depends on the element type and is specified in
[section 13.2](#132-primitive-mappings).

### 7.3 Type variables

Lowercase identifiers in type position are type variables. They are
universally quantified at top-level binding sites
(see [section 8.1](#81-typing-discipline)).

```quone
identity : a -> a
identity x <- x

map : (a -> b) -> Vector a -> Vector b
```

### 7.4 Type aliases

Type aliases are expanded structurally during type resolution. Aliases MAY be
parameterised over type variables.

```quone
type alias Students <-
    dataframe
        { name : Vector Character
        , score : Vector Double
        }
```

### 7.5 Custom types

A `type` declaration introduces a **custom type** with one or more named
constructors. Each constructor takes zero or more type arguments. Custom
types are Quone's mechanism for sum types ("one of these constructors")
and tagged values; pattern matching ([section 8.4](#84-pattern-matching))
is the way to consume them.

```quone
type Maybe a
    <- Nothing
     | Just a

type Result a
    <- Ok a
     | Err Character
```

<!-- BEGIN tests:7_5_1 -->
**Tests** (1):

- [`type/literal_logical_constructor_section_7_5_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L65)

<details>
<summary>Show test source</summary>

```haskell
    test "type/literal_logical_constructor_section_7_5_1" <|
        infersTo "x <- True" "x" "Logical"
```

</details>
<!-- END tests:7_5_1 -->


#### 7.5.1 Built-in types

A small number of custom types are predefined by the language and always
in scope.
For v0.0.1 the only such type is `Logical`:

```quone
type Logical
    <- True
     | False
```

`True` and `False` are ordinary uppercase constructors and follow the same
rules as any user-defined constructor (see [section 5.4](#54-patterns) and
[section 8.4](#84-pattern-matching)). Their lowering is fixed by
[section 13.2](#132-primitive-mappings): `True` lowers to R `TRUE` and
`False` lowers to R `FALSE`.

Other prelude-defined types such as `Maybe` and `Result` are described in
[section 10](#10-standard-environment) and follow the same constructor
rules; they are not language-level built-ins and MAY be redefined by a
user-supplied prelude.

### 7.6 Records

Records are unordered collections of named fields. Each field has a name and
a type. Field access is written `record.field`. A record value is constructed
with a record literal (`{ name = "Alice", score = 92.0 }`) and updated with
the Elm-style record update form (`{ student | score = 95.0 }`); the typing
rules for both are in [section 8.5](#85-records-and-field-access).

For v0.0.1, whether records are open (row polymorphic) or closed is left
intentionally underspecified; see
[section 19](#19-open-questions-and-future-work).

### 7.7 Dataframes

A dataframe type is a record-like schema whose fields normally contain
vectors. Dataframe manipulation verbs derive their type signatures from
this schema; see [section 9](#9-dataframe-manipulation).

---

## 8. Static semantics

### 8.1 Typing discipline

Quone uses Hindley-Milner type inference. That means:

- most expressions do not need type annotations;
- top-level bindings are generalised after their right-hand side is typed;
- polymorphic functions are instantiated at each use site;
- type annotations, when present, MUST be checked against the inferred type
  and the program is rejected on mismatch.

<!-- BEGIN tests:8_1 -->
**Tests** (6):

- [`type/identity_lambda_section_8_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L78)
- [`type/curried_application_section_8_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L90)
- [`type/annotation_matches_section_8_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L248)
- [`type/annotation_mismatch_rejected_section_8_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L257)
- [`type/annotation_pins_operator_operands_section_8_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L268)
- [`type/annotation_pins_param_types_in_body_section_8_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L280)

<details>
<summary>Show test source</summary>

```haskell
    test "type/identity_lambda_section_8_1" <|
        -- \x -> x : forall a. a -> a (printed as variable -> variable)
        case infer "id <- \\x -> x" of
            Prelude.Right binds -> case Map.lookup "id" binds of
                Just sch ->
                    Prelude.pure
                        ( assert
                            (T.isInfixOf "->" (showScheme sch))
                            ("expected an arrow type; got " Prelude.<> showScheme sch)
                        )
                Nothing -> Prelude.pure (Fail "no binding for id")
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "type/curried_application_section_8_1" <|
        infersTo "x <- (\\a b -> a) 1L 2L" "x" "Integer"
    test "type/annotation_matches_section_8_1" <|
        case infer
            ( T.unlines
                [ "x : Integer"
                , "x <- 1L"
                ]
            ) of
            Prelude.Right _ -> Prelude.pure Pass
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    test "type/annotation_mismatch_rejected_section_8_1" <|
        Prelude.pure
            ( assertLeft
                ( infer
                    ( T.unlines
                        [ "x : Integer"
                        , "x <- \"oops\""
                        ]
                    )
                )
            )
    test "type/annotation_pins_operator_operands_section_8_1" <|
        -- Without annotation flow this would fail (`/ : Double -> Double
        -- -> Double` can't infer the types of two unconstrained vars).
        -- The annotation makes both `max_score` and `raw` Double.
        infersTo
            ( T.unlines
                [ "normalize : Double -> Double -> Double"
                , "normalize max_score raw <- raw / max_score"
                ]
            )
            "normalize"
            "Double -> Double -> Double"
    test "type/annotation_pins_param_types_in_body_section_8_1" <|
        Prelude.pure
            ( assertLeft
                ( infer
                    ( T.unlines
                        [ "f : Integer -> Integer"
                        , "f x <- x + 1.0"
                        ]
                    )
                )
            )
```

</details>
<!-- END tests:8_1 -->


### 8.2 Built-in environment

The initial typing environment includes built-ins such as:

- list-style: `map`, `map2`, `reduce`, `keep`, `discard`
- numeric: `sqrt`, `mean`, `sum`, `length`, `to_double`

The full prelude is described in [section 10](#10-standard-environment).

### 8.3 Let-binding

A `let` expression introduces local bindings. Each non-ignored name is
generalised before being used in the body.

```quone
let
    double <- \n -> n * 2
in
    double 21
```

<!-- BEGIN tests:8_3 -->
**Tests** (1):

- [`type/let_generalisation_section_8_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L94)

<details>
<summary>Show test source</summary>

```haskell
    test "type/let_generalisation_section_8_3" <|
        -- let id <- \x -> x in (id 1, id "hi") would need tuples; we
        -- approximate by checking that a let-bound id can be applied
        -- to two different concrete types via two top-level uses.
        case infer
            ( T.unlines
                [ "first <- (let f <- \\x -> x in f 1L)"
                , "second <- (let f <- \\x -> x in f \"hi\")"
                ]
            ) of
            Prelude.Right _ -> Prelude.pure Pass
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
```

</details>
<!-- END tests:8_3 -->


### 8.4 Pattern matching

A `case` expression:

1. types the scrutinee;
2. checks each pattern against the scrutinee type;
3. extends the typing environment with bound variables in each arm;
4. requires all arm bodies to unify to a single result type.

Per-pattern checks (the rules each pattern in step 2 must satisfy):

- **Wildcard `_`**: matches any scrutinee type; binds nothing.
- **Variable `x`**: matches any scrutinee type; binds `x` with that type.
- **Literal patterns** (`IntLit`, `FloatLit`, `StringLit`): the scrutinee
  type MUST unify with `Integer`, `Double`, or `Character` respectively.
- **Constructor pattern `C p1 ... pn`**: the scrutinee type MUST unify
  with the result type of constructor `C`, the constructor MUST take
  exactly `n` arguments, and each subpattern `pᵢ` MUST typecheck against
  the corresponding argument type.
- **Record pattern `{ f1, ..., fn }` and `{ f1 = p1, ..., fn = pn }`**:
  the scrutinee MUST have a record type that contains at least the
  fields `f1 .. fn`. The short form binds each `fᵢ` with the field's
  type; the long form typechecks each `pᵢ` against the corresponding
  field type and extends the environment accordingly.
- **Parenthesised pattern**: typechecks identically to the inner
  pattern.

Exhaustiveness: v0.0.1 does not yet require a normative exhaustiveness
guarantee. Implementations SHOULD warn on non-exhaustive matches and MAY
generate a runtime trap for unmatched values; see
[section 19](#19-open-questions-and-future-work).

<!-- BEGIN tests:8_4 -->
**Tests** (3):

- [`type/case_wildcard_section_8_4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L173)
- [`type/case_constructor_section_8_4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L182)
- [`type/case_arms_must_unify_section_8_4`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L192)

<details>
<summary>Show test source</summary>

```haskell
    test "type/case_wildcard_section_8_4" <|
        infersTo
            ( T.unlines
                [ "x <- case 1L of"
                , "    _ -> 42L"
                ]
            )
            "x"
            "Integer"
    test "type/case_constructor_section_8_4" <|
        infersTo
            ( T.unlines
                [ "x <- case Just 1L of"
                , "    Just n -> n"
                , "    Nothing -> 0L"
                ]
            )
            "x"
            "Integer"
    test "type/case_arms_must_unify_section_8_4" <|
        Prelude.pure
            ( assertLeft
                ( infer
                    ( T.unlines
                        [ "x <- case 1L of"
                        , "    1L -> 1L"
                        , "    _ -> \"oops\""
                        ]
                    )
                )
            )
```

</details>
<!-- END tests:8_4 -->


### 8.5 Records and field access

Field access requires a record-typed expression and the named field MUST
exist in that record's type (see [section 7.6](#76-records) for the
record-openness question, which is `[planned]` for normative resolution).

**Record update.** A record-update expression `{ r | f1 = v1, ..., fn = vn }`
typechecks when:

1. `r` has a record type that contains at least the fields `f1 .. fn`;
2. each `vᵢ` has the same type as `fᵢ` in `r`'s type (record update MUST NOT
   change a field's type);
3. the result type equals `r`'s type.

A record-update expression where `r` has a `dataframe` type MUST be
rejected. The diagnostic SHOULD point the user at
[section 9](#9-dataframe-manipulation)'s `mutate` verb instead. The
record-update form is for record values only; column updates on dataframes
go through `mutate` so that row scope and verb-specific typing
([section 8.7](#87-dataframe-pipeline-typing)) apply.

<!-- BEGIN tests:8_5 -->
**Tests** (4):

- [`type/record_field_access_section_8_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L217)
- [`type/record_unknown_field_rejected_section_8_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L222)
- [`type/record_update_section_8_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L227)
- [`type/record_update_wrong_field_rejected_section_8_5`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L232)

<details>
<summary>Show test source</summary>

```haskell
    test "type/record_field_access_section_8_5" <|
        infersTo
            "x <- { a = 1L }.a"
            "x"
            "Integer"
    test "type/record_unknown_field_rejected_section_8_5" <|
        Prelude.pure
            ( assertLeft
                (infer "x <- { a = 1L }.b")
            )
    test "type/record_update_section_8_5" <|
        infersTo
            "x <- { { a = 1L } | a = 2L }"
            "x"
            "{ a : Integer }"
    test "type/record_update_wrong_field_rejected_section_8_5" <|
        Prelude.pure
            ( assertLeft
                (infer "x <- { { a = 1L } | b = 2L }")
            )
```

</details>
<!-- END tests:8_5 -->


### 8.6 Dataframes

Dataframe types are treated as record-like schemas whose fields usually
contain vectors. Operations on them are typed via the verb rules in
[section 9](#9-dataframe-manipulation).

### 8.7 Dataframe pipeline typing

When a dataframe-like value is piped into a dataframe verb, the compiler:

1. checks that referenced columns exist;
2. resolves names according to the verb's scope rules
   (see [section 9.3](#93-verb-specific-scope-rules));
3. computes an updated output schema.

The verbs with normative typing rules in v0.0.1 are:

- `select`
- `filter`
- `mutate`
- `summarize`
- `group_by`
- `arrange`

The remaining verbs reserved in
[section 3.4](#34-reserved-keywords) - `ungroup`, `rename`, `distinct`,
`distinct_all`, `count`, `slice`, `pull`, `relocate`, `transmute`,
`mutate_each`, `summarize_each`, and the join family (`left_join`,
`right_join`, `inner_join`, `full_join`, `anti_join`, `semi_join`,
`cross_join`) - are accepted by the parser but their typing rules are
`[planned]` for a later revision. A conforming v0.0.1 implementation MAY
implement them as ordinary lowering passthroughs to `dplyr` without
column-level type checking.

<!-- BEGIN tests:8_7 -->
**Tests** (10):

- [`verb/select_keeps_named_columns_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L59)
- [`verb/select_unknown_column_rejected_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L72)
- [`verb/filter_predicate_must_be_logical_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L98)
- [`verb/filter_unknown_column_rejected_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L105)
- [`verb/mutate_extends_schema_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L123)
- [`verb/summarize_replaces_schema_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L148)
- [`verb/group_by_keeps_schema_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L173)
- [`verb/group_by_unknown_column_rejected_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L181)
- [`verb/arrange_with_record_keeps_schema_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L199)
- [`verb/arrange_with_desc_modifier_section_8_7`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L207)

<details>
<summary>Show test source</summary>

```haskell
    test "verb/select_keeps_named_columns_section_8_7" <|
        case infer (studentsDf ++ "y <- students |> select { name }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe fs ->
                        Prelude.pure
                            ( assert
                                (Map.keysSet fs Prelude.== Map.keysSet (Map.singleton "name" ()))
                                ("expected schema {name}; got " Prelude.<> showType (TyDataframe fs))
                            )
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    test "verb/select_unknown_column_rejected_section_8_7" <|
        Prelude.pure
            ( assertLeft
                ( inferRaw
                    (studentsDf ++ "y <- students |> select { missing }")
                )
            )
    test "verb/filter_predicate_must_be_logical_section_8_7" <|
        Prelude.pure
            ( assertLeft
                ( inferRaw
                    (studentsDf ++ "y <- students |> filter (score)")
                )
            )
    test "verb/filter_unknown_column_rejected_section_8_7" <|
        Prelude.pure
            ( assertLeft
                ( inferRaw
                    (studentsDf ++ "y <- students |> filter (notACol > 70.0)")
                )
            )
    test "verb/mutate_extends_schema_section_8_7" <|
        case infer
            (studentsDf ++ "y <- students |> mutate { pct = score / 100.0 }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe fs ->
                        Prelude.pure
                            ( assert
                                (Map.member "pct" fs Prelude.&& Map.member "score" fs)
                                ("expected pct + original cols; got " Prelude.<> showType (TyDataframe fs))
                            )
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    test "verb/summarize_replaces_schema_section_8_7" <|
        case infer
            (studentsDf ++ "y <- students |> summarize { avg = mean score }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe fs ->
                        Prelude.pure
                            ( assert
                                (Map.keysSet fs Prelude.== Map.keysSet (Map.singleton "avg" ()))
                                ("expected only {avg}; got " Prelude.<> showType (TyDataframe fs))
                            )
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    test "verb/group_by_keeps_schema_section_8_7" <|
        case infer (studentsDf ++ "y <- students |> group_by { dept }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    test "verb/group_by_unknown_column_rejected_section_8_7" <|
        Prelude.pure
            ( assertLeft
                ( inferRaw
                    (studentsDf ++ "y <- students |> group_by { missing }")
                )
            )
    test "verb/arrange_with_record_keeps_schema_section_8_7" <|
        case infer (studentsDf ++ "y <- students |> arrange { score }") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
    test "verb/arrange_with_desc_modifier_section_8_7" <|
        case infer (studentsDf ++ "y <- students |> arrange (desc score)") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
```

</details>
<!-- END tests:8_7 -->


### 8.8 Arithmetic and comparison operators

The arithmetic and comparison operators listed in
[section 3.5](#35-operators-and-punctuation) are built-in to the language.
v0.0.1 does not provide user-definable operator overloading or typeclasses,
so each operator is given a small fixed set of monomorphic typing rules
that the typechecker selects between based on operand types.

**Arithmetic operators (`+`, `-`, `*`, `/`).**

The typechecker accepts exactly the same-primitive cases:

- `Integer -> Integer -> Integer`
- `Double  -> Double  -> Double`

Mixed-primitive uses such as `Integer + Double` MUST be a type error. The
user is expected to insert an explicit conversion (`to_double`).

**Integer division (`//`) and modulo (`%`).**

Both are integer-only:

- `Integer -> Integer -> Integer`

For `Double` operands, users SHOULD use `/` followed by `floor`, or call
the appropriate prelude function. Real-valued modulo is `[planned]`.

**Exponentiation (`^`).**

Typed `Double -> Double -> Double`. R's `^` always returns a double, so
matching that type avoids surprising lowering behaviour. Integer-valued
exponentiation requires explicit `to_double` on each operand. The
operator is right-associative (see [section 5.2](#52-operator-precedence)).

**Unary minus (`-`).**

Typed:

- `Integer -> Integer`
- `Double  -> Double`

Unary minus binds tighter than `^`; see
[section 5.2](#52-operator-precedence).

**Comparison operators (`==`, `!=`, `>`, `<`, `>=`, `<=`).**

Each comparison is typed `a -> a -> Logical`, where `a` is restricted to
the comparable built-in types `Integer`, `Double`, `Character`, and
`Logical` (see [section 7.5.1](#751-built-in-types)). Comparisons of
`Logical` values compare by constructor (`False < True`). Mixed-type
comparisons MUST be a type error.

**Why monomorphic.**

This rule keeps Hindley-Milner inference simple and avoids introducing
typeclasses or constrained type variables in v0.0.1. A future revision MAY
introduce an Elm-style constrained variable (e.g. `number`) to overload
arithmetic across `Integer` and `Double`; if it does, the change is
backwards-compatible because every program that typechecks today under the
monomorphic rule will continue to typecheck.

**Defaulting unconstrained numeric operands.**

Without overloading, an unannotated body such as `add a b <- a + b`
provides no information from which the typechecker can decide whether
`a` and `b` are `Integer` or `Double`. Rather than reject the program,
v0.0.1 follows R's lead and *defaults* unconstrained numeric type
variables to `Double` at the boundary of the enclosing top-level value
declaration. The rule is:

1. While inferring a value declaration's body, the typechecker tracks
   each fresh type variable that appears as an operand of an arithmetic
   or comparison operator.
2. After the body is fully inferred but before the binding's principal
   scheme is generalised, every tracked type variable that is still
   unresolved is unified with `Double`.

The defaulting decision is therefore local to one declaration and
cannot affect inference for any other binding. Combined with the
literal rule from [section 3.3](#33-literals) (bare digit runs are
`Double`), this means:

- `add a b <- a + b` infers `Double -> Double -> Double` and `add 1 2`
  works.
- `add a b <- a + b` followed by `main <- add 1L 2L` is a type error,
  because `add` was defaulted to `Double` but `1L` and `2L` are
  `Integer`. The author who intends integer arithmetic must annotate:
  `add : Integer -> Integer -> Integer`.

Defaulting only fires when an operand is *fully* unconstrained.
Concrete operands always pin via the existing operator typing rules
above, with no defaulting involved.

<!-- BEGIN tests:8_8 -->
**Tests** (17):

- [`type/op_add_int_int_int_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L117)
- [`type/op_add_bare_digits_default_to_double_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L119)
- [`type/op_add_dbl_dbl_dbl_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L123)
- [`type/op_mixed_int_dbl_rejected_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L125)
- [`type/op_intdiv_int_only_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L129)
- [`type/op_intdiv_double_rejected_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L131)
- [`type/op_mod_int_only_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L133)
- [`type/op_caret_double_only_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L135)
- [`type/op_caret_int_rejected_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L137)
- [`type/op_unary_neg_int_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L141)
- [`type/op_unary_neg_double_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L143)
- [`type/op_eq_returns_logical_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L145)
- [`type/op_lt_mixed_rejected_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L147)
- [`type/op_eq_string_returns_logical_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L149)
- [`type/op_unconstrained_add_defaults_to_double_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L156)
- [`type/op_unconstrained_compare_defaults_to_double_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L158)
- [`type/op_unconstrained_caret_defaults_to_double_section_8_8`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L160)

<details>
<summary>Show test source</summary>

```haskell
    test "type/op_add_int_int_int_section_8_8" <|
        infersTo "x <- 1L + 2L" "x" "Integer"
    test "type/op_add_bare_digits_default_to_double_section_8_8" <|
        -- Bare `1 + 2` is `Double + Double = Double` because bare
        -- digit literals are doubles.
        infersTo "x <- 1 + 2" "x" "Double"
    test "type/op_add_dbl_dbl_dbl_section_8_8" <|
        infersTo "x <- 1.0 + 2.0" "x" "Double"
    test "type/op_mixed_int_dbl_rejected_section_8_8" <|
        -- `1L` is Integer, `2.0` is Double - mixed primitives still
        -- a type error (no implicit coercion).
        Prelude.pure (assertLeft (infer "x <- 1L + 2.0"))
    test "type/op_intdiv_int_only_section_8_8" <|
        infersTo "x <- 10L // 3L" "x" "Integer"
    test "type/op_intdiv_double_rejected_section_8_8" <|
        Prelude.pure (assertLeft (infer "x <- 10.0 // 3.0"))
    test "type/op_mod_int_only_section_8_8" <|
        infersTo "x <- 10L % 3L" "x" "Integer"
    test "type/op_caret_double_only_section_8_8" <|
        infersTo "x <- 2.0 ^ 3.0" "x" "Double"
    test "type/op_caret_int_rejected_section_8_8" <|
        -- `2L ^ 3L` is Integer ^ Integer; `^` requires both sides to
        -- be Double, so this is rejected.
        Prelude.pure (assertLeft (infer "x <- 2L ^ 3L"))
    test "type/op_unary_neg_int_section_8_8" <|
        infersTo "x <- -5L" "x" "Integer"
    test "type/op_unary_neg_double_section_8_8" <|
        infersTo "x <- -5.0" "x" "Double"
    test "type/op_eq_returns_logical_section_8_8" <|
        infersTo "x <- 1L == 2L" "x" "Logical"
    test "type/op_lt_mixed_rejected_section_8_8" <|
        Prelude.pure (assertLeft (infer "x <- 1L < 2.0"))
    test "type/op_eq_string_returns_logical_section_8_8" <|
        infersTo "x <- \"a\" == \"b\"" "x" "Logical"
      -- Numeric defaulting (LANGUAGE.md section 8.8): an unannotated
      -- top-level binding whose body uses arithmetic on unconstrained
      -- operands has those operands defaulted to Double, matching R's
      -- bare-numeric default. This is what makes the natural shape
      -- @add a b <- a + b@ usable without an annotation.
    test "type/op_unconstrained_add_defaults_to_double_section_8_8" <|
        infersTo "add a b <- a + b" "add" "Double -> Double -> Double"
    test "type/op_unconstrained_compare_defaults_to_double_section_8_8" <|
        infersTo "ge a b <- a >= b" "ge" "Double -> Double -> Logical"
    test "type/op_unconstrained_caret_defaults_to_double_section_8_8" <|
        infersTo "square x <- x ^ x" "square" "Double -> Double"
```

</details>
<!-- END tests:8_8 -->


---

## 9. Dataframe manipulation

This section is the language-level specification of the dataframe-verb
surface introduced in [section 5](#5-concrete-syntax) and typed in
[section 8.7](#87-dataframe-pipeline-typing).

### 9.1 Native dataframe verbs

Quone provides native dataframe-manipulation verbs with a surface inspired by
`dplyr` but with cleaner and more uniform syntax. The recommended surface
forms include:

```quone
filter (score > 70.0)
select { name, score }
rename { student_name = name }
mutate { pct = score / 100.0, passed = score >= 50.0 }
transmute { pct = score / 100.0 }
group_by { dept }
ungroup
summarize { n = count, avg_score = mean score }
count { dept }
arrange { desc score, name }
distinct { name, dept }
distinct_all
slice 1 10
pull score
relocate { score, name }
left_join  other on { dept_id = id }
inner_join other on { id }
anti_join  other on { id }
```

These forms are part of the language-level dataframe API, not parser
tricks. The complete list of dataframe verbs reserved in v0.0.1 is given
in [section 3.4](#34-reserved-keywords). Their typing and scope rules
are normative for the verbs listed in
[section 8.7](#87-dataframe-pipeline-typing); the remaining verbs are
`[planned]`.

### 9.2 Sugared and explicit dataframe scope

Inside dataframe verbs, bare column names are syntax sugar for an explicit
row-scoped lambda.

```quone
students |> filter (score > 70.0)
```

is sugar for:

```quone
students |> filter (\row -> row.score > 70.0)
```

Both forms MUST be valid. The sugared form gives a concise dplyr-like
syntax; the explicit lambda form is the precise escape hatch for advanced
logic and is the form referenced by the typing rules.

<!-- BEGIN tests:9_2 -->
**Tests** (1):

- [`verb/filter_predicate_uses_row_scope_section_9_2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/VerbTests.hs#L90)

<details>
<summary>Show test source</summary>

```haskell
    test "verb/filter_predicate_uses_row_scope_section_9_2" <|
        case infer (studentsDf ++ "y <- students |> filter (score > 70.0)") of
            Prelude.Right binds -> case Map.lookup "y" binds of
                Just sch -> case schemeBody sch of
                    TyDataframe _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                Nothing -> Prelude.pure (Fail "no binding y")
            Prelude.Left msg -> Prelude.pure (Fail msg)
```

</details>
<!-- END tests:9_2 -->


### 9.3 Verb-specific scope rules

Different verbs resolve names in different scopes:

| Verb group                                  | Scope               |
| ------------------------------------------- | ------------------- |
| `filter`, `mutate`, `group_by`, `arrange`   | row scope           |
| `summarize`                                 | group scope         |
| join conditions                             | left and right scope|

A future revision MUST formalise the resolution rules for each verb in those
terms.

### 9.4 Column-wise operations

Quone does not directly copy `dplyr::across()`. It instead defines clearer
typed forms:

```quone
mutate_each (where numeric) (\col -> round col 2)
summarize_each (cols { ALT, AST }) as "{col}_mean" mean
```

The intent is:

- `mutate_each` for repeated column-wise transformations
- `summarize_each` for repeated grouped summaries

These forms are easier to explain and typecheck than a direct clone of
`across()`. They are `[planned]` for full normative treatment.

### 9.5 Modifier keywords

A small set of identifiers appear bareword inside dataframe-verb
arguments and the parser recognises them as **modifiers** rather than
ordinary identifiers. They are listed in the dataframe modifier section
of [section 3.4](#34-reserved-keywords).

| Modifier | Used in                  | Role                                          |
| -------- | ------------------------ | --------------------------------------------- |
| `desc`   | `arrange { desc col }`   | Sort the named column in descending order.    |
| `asc`    | `arrange { asc col }`    | Sort the named column in ascending order (default). |
| `on`     | `*_join other on { ... }`| Introduces the join condition.                |
| `as`     | `summarize_each ... as "{col}_mean" mean` | Names the output columns produced by a `*_each` form. |
| `where`  | `mutate_each (where ...)`| Selects which columns are touched by `mutate_each`. |
| `cols`   | `summarize_each (cols { ... }) ...` | Names an explicit column set for `summarize_each`. |

The exact grammar and typing for each modifier is `[planned]`; for v0.0.1
they are reserved at the lexical level and accepted by the parser in the
positions shown.

### 9.6 Compile targets for data manipulation

The primary compilation targets are:

- base R
- `purrr` (higher-order operations on `Vector a`; see
  [section 13.3.2](#1332-higher-order-operations-on-vector-a))
- `dplyr` (dataframe verbs; see [section 13.8](#138-dataframe-verbs))
- `readr` (CSV and other text loading; see
  [section 11](#11-file-loading-and-decoding))

Possible later targets include `tidyr`, `stringr`, `DBI`, `duckdb`. The
`maybe` package or similar functional helper libraries MAY be explored
experimentally but MUST NOT define the default style of generated Quone code
in v0.0.1.

---

## 10. Standard environment

A Quone implementation MUST distinguish between:

1. the **core language** (this document, sections 3-9, 12, 13);
2. the **standard prelude**;
3. the **data ecosystem integrations**;
4. the **source loading and decoding libraries**.

Recommended organisation:

| Library      | Contents                                                  |
| ------------ | --------------------------------------------------------- |
| Core prelude | arithmetic, comparisons, booleans, function combinators   |
| Vector       | `map`, `map2`, `reduce`, `keep`, `discard`                |
| Numeric      | `sum`, `mean`, `sqrt`, `length`, `to_double`              |
| Data         | dataframe constructors and dataframe verbs                |
| Source       | `Csv` and, later, generalised data sources                |
| Script       | fail-fast helpers for executable scripts                  |

The exact module names and signatures are `[planned]`. v0.0.1 only requires
that the prelude exposes the items listed in
[section 8.2](#82-built-in-environment) under the names given there.

---

## 11. File loading and decoding

Quone separates two concerns:

1. **loading** raw data from a source;
2. **decoding** and validating it into a typed shape.

This follows the same general design spirit as Elm decoders.

### 11.1 CSV as the first backend

CSV begins with dataframe decoders as the primary path:

```quone
Csv.read "adsl.csv"
    |> Csv.decode_dataframe adsl_decoder
```

A convenience form combines the two steps:

```quone
Csv.read_dataframe adsl_decoder "adsl.csv"
```

A dataframe decoder can use a pipeline style:

```quone
Csv.dataframe
    |> Csv.column "USUBJID" character
    |> Csv.column "TRTA" character
    |> Csv.optional_column "AGE" integer 0
```

### 11.2 Row decoders as a secondary path

Row decoders for vectors of records are also supported, for cases where users
want Elm-style row-by-row decoding:

```quone
Csv.succeed Subject
    |> Csv.required "USUBJID" character
    |> Csv.required "TRTA" character
    |> Csv.required "AGE" integer
```

The primary Quone path SHOULD be typed dataframe decoding rather than
row-by-row decoding.

### 11.3 Generalised sources

CSV is the first backend. The design generalises to a broader source
abstraction targeting, for example:

- CSV
- Excel
- SAS
- Parquet
- databases
- external R loader functions

The conceptual model remains: load raw data, then decode it into a typed
dataframe or typed value.

---

## 12. Errors and runtime fallibility

### 12.1 Error categories

A conforming implementation MUST recognise the following error categories:

- lexical errors
- parse errors
- unbound variable errors
- type mismatch errors
- unknown constructor errors
- record field errors
- unknown dataframe column errors
- file loading failures
- decode failures
- non-exhaustive pattern match behaviour

Each error category SHOULD include enough information for a user to locate
the source of the failure.

<!-- BEGIN tests:12_1 -->
**Tests** (1):

- [`type/unbound_variable_rejected_section_12_1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/TypeTests.hs#L92)

<details>
<summary>Show test source</summary>

```haskell
    test "type/unbound_variable_rejected_section_12_1" <|
        Prelude.pure (assertLeft (infer "x <- y"))
```

</details>
<!-- END tests:12_1 -->


### 12.2 Runtime file and decode failures

Loading a real file from disk is a runtime effect. A Quone program cannot
know at compile time whether a concrete path exists, whether permissions are
sufficient, or whether the file contents match the requested decoder.

Therefore:

- loading is a runtime fallible operation;
- decoding is a runtime fallible operation;
- downstream code is type-safe if decoding succeeds.

### 12.3 Structured failure types

The failure model uses named union types in the style of Elm. Examples:

- file errors such as missing file, bad path, or permission denied;
- decode errors such as missing column, duplicate column, parse failure, or
  invalid row length.

These failures MUST be structured values at the language level, even when
the generated R ultimately uses ordinary control flow and `stop(...)`.

### 12.4 Scripts versus libraries

For ordinary scripts, the preferred behaviour is fail-fast execution with a
clear error message:

```quone
Csv.read_dataframe adsl_decoder "adsl.csv"
    |> Script.expect
```

This means: attempt to load and decode at runtime, and if it fails, print a
clear error and stop execution.

For reusable library code, functions SHOULD return structured errors
explicitly instead of aborting.

So Quone distinguishes:

- **scripts**, which usually use `Script.expect`;
- **libraries**, which usually return `Result`-like values.

### 12.5 Compilation of `Script.expect`

`Script.expect` MUST NOT compile to an opaque Quone runtime wrapper. It is a
compile-time directive that inlines ordinary R fail-fast behaviour such as:

- `tryCatch(...)`,
- parse and problem checks,
- `stop(...)` with a readable message.

This preserves the goal that generated R remains familiar and maintainable.

---

## 13. Translation to R

Moved to [COMPILATION.md section 1: Translation to R](COMPILATION.md#1-translation-to-r).

Section 13 contained the normative lowering rules from Quone source to
R: primitive mappings, operator mappings, function-call shape,
higher-order operations on `Vector a`, pipes, custom types, pattern
matching, records, dataframe verbs, and foreign R bindings. They now
live in `COMPILATION.md` so the language proper (this file) stays
focused on lex, syntax, types, and semantics.

Subsection cross-references that used to point at `#13X` anchors here
should be redirected to the equivalent `#1X` anchor in
`COMPILATION.md`.

---

## 14. Project model and package generation

Moved to [COMPILATION.md section 2: Project model and package generation](COMPILATION.md#2-project-model-and-package-generation).

Section 14 contained the project layout, package-mode output rules, and
the cross-module collision check. Same reason as section 13 - these are
compiler-output concerns rather than language concerns.

---

## 15. Conformance levels

To keep the project manageable, conformance is layered.

### Level 1 - core language

- literals
- variables
- lambdas
- application
- `let`
- `if`
- simple custom types
- `case`

### Level 2 - data language

- records
- dataframe literals
- field access
- typed dplyr verbs (the set listed in
  [section 8.7](#87-dataframe-pipeline-typing))

### Level 3 - tooling and packaging

- module headers
- exports
- formatting
- REPL commands
- LSP hover and diagnostics
- R package generation

A conforming v0.0.1 implementation MUST support Level 1 and SHOULD support
Levels 2 and 3 as listed.

---

## 16. Testing requirements

A production-quality v0.0.1 Quone compiler MUST be backed by an
automated test suite that exercises every normative rule in this
document. The suite is part of the conforming implementation: a compiler
that ships without it is not considered production-ready, even if the
compiler itself is feature-complete.

### 16.1 Suite-wide properties

The test suite as a whole MUST satisfy:

- **Reproducible.** Every test MUST produce the same result on every
  run for a given source revision, with no dependence on machine state
  beyond the test inputs.
- **Hermetic.** Tests MUST NOT depend on network access or write
  outside their scratch directories. The R version invoked by
  end-to-end tests MUST be pinned by the test harness.
- **Fast at the unit layer.** The unit-test layers (lexer, parser,
  AST validator, type checker, code generator) SHOULD complete in under
  one minute on a current developer workstation. End-to-end and `R CMD
  check` layers MAY take longer and SHOULD be parallelised.
- **Layered.** Each compilation phase has its own dedicated suite, plus
  one or more integration suites that exercise the full pipeline.
- **Actionable on failure.** Every failure MUST report the input, the
  expected behaviour, the observed behaviour, and a stable identifier
  the developer can use to re-run the failing case in isolation.

### 16.2 Required test layers

The following layers MUST exist. Each layer is a distinct test target
that can be run independently.

| Layer                 | Inputs                  | Outputs / assertions                          |
| --------------------- | ----------------------- | --------------------------------------------- |
| Lexer                 | `.Q` source / strings   | Token list with positions                     |
| Parser                | Token streams           | AST per [section 6](#6-abstract-syntax)       |
| AST well-formedness   | AST                     | Accept / reject with diagnostic               |
| Type checker          | AST                     | Typed AST or specific type error              |
| Code generation       | Typed AST               | R source string (snapshot)                    |
| Script-mode E2E       | `.Q` files              | Compiled R + R execution output               |
| Package-mode E2E      | Quone projects          | R package directory + `R CMD check` result    |
| Property-based        | Generated random inputs | Algebraic / round-trip invariants             |

### 16.3 Lexer tests

The lexer test suite MUST cover, with positive and negative cases:

- All literal kinds in [section 3.3](#33-literals): integer (including
  `0`), double (including `0.0`, `1e10`), character (including escape
  sequences and unterminated strings).
- All keyword categories from [section 3.4](#34-reserved-keywords):
  framework, dataframe DSL, modifiers. Each reserved word MUST be
  rejected when used as a value or type identifier.
- All operator and punctuation tokens from
  [section 3.5](#35-operators-and-punctuation), including the
  multi-character forms `<-`, `->`, `|>`, `==`, `!=`, `>=`, `<=`, `//`,
  `^`, `#'`.
- Identifier rules from [section 3.2](#32-identifiers): the
  `UpperCamelCase` / `snake_case` distinction, the leading-`_`
  rejection, and the `.` rejection inside identifiers.
- Comments per [section 3.6](#36-comments): `#` line comments, `#'`
  doc comments, attaching contiguous doc blocks to the next top-level
  declaration.
- Indentation per [section 3.7](#37-indentation-and-layout): spaces
  only, tab rejection at layout-significant positions.
- Source-position tracking: every token MUST carry an accurate
  `(start_line, start_col, end_line, end_col)` range; tests MUST
  assert these positions for at least one token of each kind.

<!-- BEGIN tests:16_3 -->
**Tests** (2):

- [`lex/position_first_token_at_1_1_section_16_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L317)
- [`lex/position_second_line_starts_at_2_1_section_16_3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/LexTests.hs#L327)

<details>
<summary>Show test source</summary>

```haskell
    test "lex/position_first_token_at_1_1_section_16_3" <|
        case lexInput "score" of
            Prelude.Right (Located {locSpan = SourceSpan start _} : _) ->
                Prelude.pure
                    ( assert
                        (posLine start Prelude.== 1 Prelude.&& posCol start Prelude.== 1)
                        "first token should start at line 1 column 1"
                    )
            _ ->
                Prelude.pure (Fail "expected at least one token")
    test "lex/position_second_line_starts_at_2_1_section_16_3" <|
        let
            toks = allTokens "x\ny"
            -- find the 'y' token (TLowerIdent "y")
            mY = List.find (\l -> locValueOf l Prelude.== TLowerIdent "y") (lexedLocated "x\ny")
        in
        case mY of
            Just l ->
                let
                    start = spanStart (locSpan l)
                in
                Prelude.pure
                    ( assert
                        (posLine start Prelude.== 2 Prelude.&& posCol start Prelude.== 1)
                        "'y' should start at line 2 column 1"
                    )
            Nothing ->
                Prelude.pure (Fail (T.pack (Prelude.show toks)))
```

</details>
<!-- END tests:16_3 -->


### 16.4 Parser tests

The parser suite MUST cover every production in
[section 5.1](#51-grammar):

- Each declaration form: module declaration, type declarations
  (single- and multi-variant), type alias declarations, both import
  forms (single name, multi-name, wildcard for Quone imports;
  type-annotated for foreign imports), and value declarations with and
  without type annotations.
- Each expression form: lambdas, `if`, `case`, `let`, pipes, curried
  application, every binary and unary operator, field access, record
  literals, record updates, dataframe literals, vector literals, every
  reserved dplyr verb.
- Each pattern form from [section 5.4](#54-patterns), including
  nested constructor patterns and both record-pattern shapes.
- Operator precedence and associativity per
  [section 5.2](#52-operator-precedence). Tests MUST explicitly assert:
  - left-associativity at each binary level;
  - right-associativity of `^` (`2 ^ 3 ^ 2` parses as `2 ^ (3 ^ 2)`);
  - `-2 ^ 2` parses as `(-2) ^ 2`.
- The `if` desugaring per [section 5.3](#53-surface-only-forms): the
  parser MUST produce ASTs equivalent to the explicit `case` form and
  MUST NOT produce an `EIf` constructor.
- Parse errors: every parse error MUST produce a diagnostic with the
  source position of the offending token, classified per
  [section 12.1](#121-error-categories).

### 16.5 AST well-formedness tests

Each invariant in [section 6.8](#68-well-formedness) MUST have at
least one positive test (an AST that satisfies the invariant) and one
negative test (an AST that violates it and the diagnostic produced).
The nine invariants are: module placement, export consistency, Quone
import visibility, R-export discipline, constructor arity,
record-update target, operator typing precondition, verb argument
shape, and the absence of `EIf`.

### 16.6 Type checker tests

The type checker suite MUST cover every rule in
[section 8](#8-static-semantics):

- HM inference for unannotated bindings, polymorphic instantiation at
  use sites, and generalisation at top-level and inside `let`.
- Type-annotation checking: programs whose annotation matches inference
  (accepted) and programs whose annotation does not match (rejected
  with the type-mismatch error category).
- Pattern-match typing per [section 8.4](#84-pattern-matching) for
  every pattern kind, including both record-pattern shapes.
- Record field access and record update typing per
  [section 8.5](#85-records-and-field-access), including the
  dataframe-rejection diagnostic for `ERecordUpdate` on a dataframe
  scrutinee.
- Dataframe verb typing per
  [section 8.7](#87-dataframe-pipeline-typing) for each normatively
  typed verb (`select`, `filter`, `mutate`, `summarize`, `group_by`,
  `arrange`).
- Operator typing per
  [section 8.8](#88-arithmetic-and-comparison-operators) for each
  operator and each accepted operand type, plus negative tests for
  mixed-primitive operands.
- Foreign import type annotations per [section 4.5](#45-imports).

For every error case, the test MUST verify both the error category
from [section 12.1](#121-error-categories) and the source location of
the offending expression.

### 16.7 Code generation tests

Code generation MUST be tested by **snapshot tests**: each test
compiles a small Quone program and compares the emitted R against a
checked-in expected output file. Snapshot updates MUST require explicit
developer approval (e.g. an `--accept` flag or equivalent) so they
cannot drift silently.

The snapshot suite MUST cover:

- Each row of [section 13.2](#132-primitive-mappings).
- Each row of [section 13.2.1](#1321-operator-mappings).
- Curried function definition and fully-applied call lowering per
  [section 13.3](#133-functions), and partial-application lowering to R
  closures.
- Argument-passing rules per [section 13.3.1](#1331-argument-passing),
  including the named-argument cases for dataframe verbs and
  explicitly-marked imports.
- Higher-order vector operations per
  [section 13.3.2](#1332-higher-order-operations-on-vector-a),
  including the `purrr` preferences AND the two base-R exceptions
  (vectorised arithmetic and atomic-vector construction).
- Pattern-matching lowering per [section 13.6](#136-pattern-matching),
  including the `case`-on-`Logical` to native `if` optimisation.
- Record literal and record update lowering per
  [section 13.7](#137-records-and-field-access), including
  `purrr::list_modify`.
- Dataframe verb lowering per [section 13.8](#138-dataframe-verbs).
- Foreign-import call lowering per
  [section 13.9](#139-foreign-r-bindings-and-runtime-dependencies),
  including `pkg::fn` qualification for both single-segment package
  prefixes (`readr.read_csv`) and dotted prefixes
  (`data.table.fread`).

### 16.8 End-to-end tests

**Script mode.** The test suite MUST include programs that:

- compile, are executed by R, and produce expected stdout;
- compile, are executed by R, and exit with the expected status when
  `Script.expect` triggers a fail-fast at runtime;
- exercise the full pipeline from `.Q` source through the generated
  `.R` file and into R's interpreter.

**Package mode.** The test suite MUST include sample Quone projects
that:

- compile to a complete R package directory per
  [section 14.6](#146-multi-module-package-layout);
- pass `R CMD check` with no errors and no warnings;
- exercise multi-module compilation, the cross-module visibility rules
  from [section 4.5](#45-imports), the `@export` to `NAMESPACE`
  pipeline driven by `roxygen2`, and the package-wide collision check
  from [section 14.6](#146-multi-module-package-layout).

The R version invoked for end-to-end tests MUST be pinned in the test
harness and SHOULD be the most recent stable R release. Multiple R
versions MAY be tested in CI; if so, the matrix MUST be documented in
the test infrastructure.

<!-- BEGIN tests:16_8 -->
**Tests** (10):

- [`scenarios/01_arithmetic_pipeline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/01_arithmetic_pipeline.Q#L1)
- [`scenarios/02_higher_order_with_let`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/02_higher_order_with_let.Q#L1)
- [`scenarios/03_record_field_access`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/03_record_field_access.Q#L1)
- [`scenarios/04_dataframe_filter_arrange`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/04_dataframe_filter_arrange.Q#L1)
- [`scenarios/05_group_by_summarize`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/05_group_by_summarize.Q#L1)
- [`scenarios/06_select_then_filter`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/06_select_then_filter.Q#L1)
- [`scenarios/07_case_chain_classification`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/07_case_chain_classification.Q#L1)
- [`scenarios/08_foreign_import_dplyr`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/08_foreign_import_dplyr.Q#L1)
- [`scenarios/09_pharma_lite_pipeline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/09_pharma_lite_pipeline.Q#L1)
- [`scenarios/10_record_update`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/scenarios/10_record_update.Q#L1)
<!-- END tests:16_8 -->


### 16.9 Property-based tests

The suite MUST include property-based tests covering at least:

- **Lexer/parser/printer round-trip.** A randomly-generated AST,
  pretty-printed and re-parsed, MUST produce an equivalent AST.
- **Type-soundness invariants.** A well-typed Quone program lowered
  to R MUST produce R that, when invoked with conforming inputs,
  produces a value whose R type matches the Quone result type.
- **Operator algebra.** For randomly-generated `Integer` and `Double`
  values:
  - addition is commutative and associative;
  - subtraction is the inverse of addition (`(a + b) - b == a` modulo
    floating-point tolerance for `Double`);
  - exponentiation is right-associative.

Property-based tests MUST use a deterministic seed by default so
failures reproduce, and the harness MUST report the failing
counterexample.

### 16.10 Test corpus

The repository MUST contain a versioned corpus of test programs:

- `tests/corpus/valid/` - programs that compile and run correctly.
- `tests/corpus/invalid/` - programs that produce specific compile
  errors. Each file MUST be paired with the expected error category
  from [section 12.1](#121-error-categories) and a source location.
- `tests/corpus/snapshot/` - programs whose generated R is compared
  against a checked-in expected output.

Every change to the language or compiler that alters observable
behaviour MUST update the corpus.

<!-- BEGIN tests:16_10 -->
**Tests** (10):

- [`invalid/dataframe_unknown_column`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/invalid/dataframe_unknown_column.Q#L1)
- [`invalid/mixed_arithmetic`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/invalid/mixed_arithmetic.Q#L1)
- [`invalid/unbound_variable`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/invalid/unbound_variable.Q#L1)
- [`invalid/unknown_field`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/invalid/unknown_field.Q#L1)
- [`snapshot/hello`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/snapshot/hello.Q#L1)
- [`snapshot/pipeline`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/snapshot/pipeline.Q#L1)
- [`valid/case_and_if`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/valid/case_and_if.Q#L1)
- [`valid/dataframe`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/valid/dataframe.Q#L1)
- [`valid/literals`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/valid/literals.Q#L1)
- [`valid/operators`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/corpus/valid/operators.Q#L1)
<!-- END tests:16_10 -->


### 16.11 Regression tests

Every fixed bug MUST be accompanied by a regression test:

- the test MUST live in the test suite, not in commit messages or
  issue-tracker comments;
- the test MUST fail on the pre-fix code and pass on the post-fix code;
- the test SHOULD be named or commented to identify the bug it covers.

### 16.12 Continuous integration

The full test suite MUST run on every push to the main branch and on
every pull request, on at least one operating system. Pull requests
MUST NOT be merged with a failing test suite.

The CI configuration MUST exercise:

- the unit-test layers per
  [sections 16.3-16.7](#163-lexer-tests);
- the end-to-end layer per [section 16.8](#168-end-to-end-tests)
  against the pinned R version;
- a release-mode build of the compiler.

CI failures MUST surface the failing test name, the offending input,
and a link or path to the full failure log.

### 16.13 Coverage targets

The test suite SHOULD achieve:

- at least 90% line coverage of the compiler source;
- 100% coverage of the normative rules in this document: every MUST
  and SHOULD has at least one positive test, and every MUST NOT and
  SHOULD NOT has at least one negative test.

Coverage tooling and exact thresholds are implementation details, but
the achieved coverage MUST be visible in CI output.

### 16.14 Test naming and organisation

To keep the suite navigable as it grows:

- Each test SHOULD be named after the spec section it exercises (e.g.
  `parser/operator_precedence_section_5_2`,
  `typeck/record_update_dataframe_rejection_8_5`).
- Snapshot files SHOULD live next to the test that produced them, or
  in a parallel directory mirror.
- The test layers in [section 16.2](#162-required-test-layers) MUST
  each have their own runnable target (script, command, or build
  rule) so that a developer can iterate on one layer without rerunning
  the full suite.

---

## 17. Version 0.0.1 scope

The Quone v0.0.1 surface is the union of:

- one source file per compilation unit;
- an optional module header;
- top-level type, alias, import, and value declarations;
- multi-file projects with cross-module imports per
  [section 4.5](#45-imports);
- Hindley-Milner inference;
- vectors, records (with Elm-style update), and dataframes;
- pipes;
- arithmetic, comparison, integer-division, modulo, and exponentiation
  operators with monomorphic typing per
  [section 8.8](#88-arithmetic-and-comparison-operators);
- the core dataframe verb set with normative typing in
  [section 8.7](#87-dataframe-pipeline-typing);
- custom types and `case` expressions, including record patterns;
- CSV loading and typed dataframe decoding;
- script-style fail-fast execution through `Script.expect`;
- direct compilation to readable R;
- R package generation as a first-class compiler mode, with
  `roxygen2`-driven `NAMESPACE` and `man/`.

### 17.1 Out of scope for v0.0.1

Anything not listed above is out of scope for v0.0.1. In particular:

- exhaustiveness guarantees for pattern matching
  (see [section 8.4](#84-pattern-matching));
- record openness or row polymorphism
  (see [section 7.6](#76-records));
- dataframe verbs beyond the core set normatively typed in
  [section 8.7](#87-dataframe-pipeline-typing);
- the full normative argument grammar for column-wise verbs
  `mutate_each` and `summarize_each` (see
  [section 9.4](#94-column-wise-operations));
- generalised data sources beyond CSV
  (see [section 11.3](#113-generalised-sources));
- typeclasses, traits, or constrained type variables;
- nestable block comments;
- vector patterns, pattern guards, and as-patterns
  (see [section 5.4](#54-patterns)).

---

## 18. Glossary

- **Constructor**: a named introduction form for a custom type
  (`Just`, `Nothing`, `Ok`, `Err`, `True`, `False`).
- **Custom type**: a type introduced by a `type` declaration, built from
  one or more named constructors. The Quone term for what other
  languages call an algebraic data type, sum type, or tagged union.
- **Dataframe**: a record-like schema whose fields contain vectors;
  written `dataframe { ... }`. Distinct from a record-of-vectors because
  dataframe verbs apply only to dataframes.
- **Decoder**: a value that describes how to validate and convert
  untyped input (e.g. CSV) into a typed Quone value.
- **Doc block**: a run of `#'` lines immediately preceding a top-level
  declaration. Interpreted by `roxygen2` in package mode; see
  [section 3.6](#36-comments).
- **Doc comment**: a single `#'` line. Doc comments group into doc
  blocks.
- **`@export`**: a `roxygen2` tag inside a doc block that marks the
  associated binding as part of the generated R package's `NAMESPACE`.
  Strictly stronger than Quone's `exporting (..)`: a binding can be
  Quone-exported without being R-exported, but not vice versa.
- **Foreign import**: an `import` declaration whose first path segment
  is lowercase, naming an R function. Requires a type annotation; see
  [section 4.5](#45-imports).
- **HM**: Hindley-Milner. The type inference discipline used by Quone.
- **Module**: a Quone source file with a `module` declaration. The
  source-level naming and export unit. Distinct from an R package.
- **Modifier**: a reserved identifier (`desc`, `asc`, `on`, `as`,
  `where`, `cols`) recognised by the parser inside dataframe-verb
  arguments. See [section 9.5](#95-modifier-keywords).
- **Module path**: the dotted `UpperCamelCase` name of a module
  (`Stats.Transform`).
- **Package mode**: compiler output mode that produces an R package
  (see [section 14](#14-project-model-and-package-generation)).
- **Project**: one or more Quone modules organised under a single
  `quone.toml` and compiled together. A Quone project compiles to one R
  package in package mode.
- **Quone import**: an `import` declaration whose first path segment is
  `UpperCamelCase`, referencing a binding in another Quone module. No
  type annotation; see [section 4.5](#45-imports).
- **R package**: the distributable artifact produced in package mode,
  with `DESCRIPTION`, `NAMESPACE`, `R/`, and `man/`. See
  [section 14.6](#146-multi-module-package-layout).
- **Record**: an unordered collection of named fields with field types.
  Distinct from a dataframe.
- **Row scope**: the name resolution scope used inside row-oriented
  dataframe verbs such as `filter` and `mutate`. See
  [section 9.3](#93-verb-specific-scope-rules).
- **Script**: a Quone source file without a `module` declaration.
- **Script mode**: compiler output mode that produces a single `.R`
  file.
- **Surface form**: a syntactic shape accepted by the parser but
  desugared away before the abstract syntax is constructed (see
  [section 5.3](#53-surface-only-forms)).
- **Vector**: Quone's homogeneous-sequence type, written `Vector a`.
  Lowers to an R atomic vector when `a` is primitive and to an R `list`
  otherwise; see [section 13.2](#132-primitive-mappings).
- **Verb**: a named dataframe-manipulation operation such as `select`,
  `filter`, or `summarize`. See [section 3.4](#34-reserved-keywords) for
  the full reserved set.

---

## 19. Open questions and future work

This section tracks items intentionally deferred from v0.0.1. Each item
is informative; promotion to a normative rule belongs to a later
revision.

### 19.1 Lexical and layout

- A nestable block-comment form (e.g. `{- ... -}`); see
  [section 3.6](#36-comments).
- Exact indentation and continuation rules for nested `case`, dataframe
  verbs, and multi-line bindings; see
  [section 3.7](#37-indentation-and-layout).

### 19.2 Type system

- Whether records are open (row polymorphic) or closed; see
  [section 7.6](#76-records).
- Normative exhaustiveness guarantees for pattern matching; see
  [section 8.4](#84-pattern-matching).
- Full typing rules for the dataframe verbs reserved but not yet
  normatively typed in
  [section 8.7](#87-dataframe-pipeline-typing).
- An Elm-style constrained type variable (`number`) to overload
  arithmetic across `Integer` and `Double` without typeclasses; see
  [section 8.8](#88-arithmetic-and-comparison-operators).

### 19.3 Dataframe surface

- Formal per-verb name resolution; see
  [section 9.3](#93-verb-specific-scope-rules).
- Normative grammar and typing for `mutate_each` and `summarize_each`;
  see [section 9.4](#94-column-wise-operations).
- Real-valued (`Double`) modulo; see
  [section 8.8](#88-arithmetic-and-comparison-operators).

### 19.4 Patterns

- Vector patterns (`[a, b, c]`, `[head | tail]`); see
  [section 5.4](#54-patterns).
- Pattern guards (`Just n | n > 0 -> ...`).
- As-patterns (`x@(Just n) -> ...`).

### 19.5 Standard environment

- Final module names and exact signatures for the prelude, vector,
  numeric, data, source, and script libraries; see
  [section 10](#10-standard-environment).

### 19.6 Loading and decoding

- Generalised source abstraction beyond CSV; see
  [section 11.3](#113-generalised-sources).

### 19.7 Tooling

- LSP, REPL, and formatter behaviour beyond the high-level conformance
  bullets in [section 15](#15-conformance-levels).
- A `quone.toml` schema beyond the fields shown in
  [section 14.6](#146-multi-module-package-layout).

### 19.8 Versioning

- Compatibility rules across language versions, including how
  `[planned]` items become normative in later revisions.
