# Quone Language Definition - Initial Release

**Status:** Normative for the initial Quone language release.

**Audience:** Compiler implementers, tooling authors, and users who need a
precise description of Quone source syntax, static semantics, dataframe
behavior, CSV loading, and the intended R lowering model.

Quone is a statically typed functional language that compiles to readable R.
The language follows Elm-like defaults unless R compatibility gives a compelling
reason to do otherwise.

## Contents

1. [Design Principles](#1-design-principles)
2. [Source Files, Modules, and Layout](#2-source-files-modules-and-layout)
3. [Primitive Values and R Lowering](#3-primitive-values-and-r-lowering)
4. [Operators](#4-operators)
5. [Records](#5-records)
6. [Custom Types](#6-custom-types)
7. [Missing Values](#7-missing-values)
8. [Pattern Matching, Case, and If](#8-pattern-matching-case-and-if)
9. [Vectors and Hidden Lengths](#9-vectors-and-hidden-lengths)
10. [Functions and Expressions](#10-functions-and-expressions)
11. [Prelude and Callable Classifications](#11-prelude-and-callable-classifications)
12. [Foreign R Imports](#12-foreign-r-imports)
13. [Type Aliases and Dataframe Types](#13-type-aliases-and-dataframe-types)
14. [Dataframe Verbs](#14-dataframe-verbs)
15. [CSV Loading and Decoding](#15-csv-loading-and-decoding)
16. [Runtime Fallibility and Errors](#16-runtime-fallibility-and-errors)
17. [R Lowering and Dependencies](#17-r-lowering-and-dependencies)
18. [Initial Release Scope](#18-initial-release-scope)

## 1. Design Principles

1. **Readable generated R.** Generated R should look like code an R developer
   could maintain by hand.
2. **Static clarity over R permissiveness.** Quone should reject ambiguous or
   shape-unsafe programs rather than inherit R recycling, implicit missingness,
   or ad hoc coercions.
3. **Small functional core.** Functions, records, custom types, pattern
   matching, and pipes form the core expression language.
4. **Dataframe ergonomics.** Dataframe verbs are first-class and compile to
   familiar `dplyr` operations.
5. **Explicit fallibility.** Runtime-fallible operations such as file loading
   return `Result`.
6. **Minimal runtime.** Quone should lower to base R, `dplyr`, `readr`, and
   `purrr` where appropriate rather than require a large Quone runtime.

## 2. Source Files, Modules, and Layout

A source file may declare a module and an exposing list.

```quone
module Stats.Transform exposing (normalize)

import Csv
import Result
```

Imports bring Quone modules into scope. R package functions are imported with
foreign imports, described in [section 12](#12-foreign-r-imports).

A source file contains declarations:

- optional module declaration
- imports
- type aliases
- custom type declarations
- optional type annotations
- value bindings
- foreign R imports

Value bindings use `<-`.

```quone
answer <- 42
normalize x <- x / 100
```

Quone uses indentation-sensitive blocks. Braces are used for records, dataframe
schemas, and dataframe verb records.

Line comments start with `#`.
Documentation comments start with `#'` and may lower to roxygen comments in
generated R package output.

## 3. Primitive Values and R Lowering

Quone distinguishes primitive scalar values from vectors. `Integer`, `Double`,
and `Character` denote single values. `Logical` is a regular custom type
defined in [section 6](#6-custom-types), with constructors `True` and `False`.

`Vector T` denotes a homogeneous sequence of zero or more values of type `T`.
R represents primitive scalars as atomic vectors of length 1, but that is a
lowering detail, not a Quone type rule.

| Quone form | Quone type | Quone example | Lowered R |
| --- | --- | --- | --- |
| integer scalar | `Integer` | `1L` | `1L` |
| double scalar | `Double` | `1` | `1` |
| logical scalar | `Logical` | `True` | `TRUE` |
| character scalar | `Character` | `"a"` | `"a"` |
| integer vector | `Vector Integer` | `[1L, 2L, 3L]` | `c(1L, 2L, 3L)` |
| double vector | `Vector Double` | `[1, 2, 3]` | `c(1, 2, 3)` |
| logical vector | `Vector Logical` | `[True, False]` | `c(TRUE, FALSE)` |
| character vector | `Vector Character` | `["a", "b"]` | `c("a", "b")` |

Bare numeric literals such as `1` and `3.14` are `Double`. Integer literals use
the `L` suffix, such as `1L`.

Quone does not implicitly promote `Integer` to `Double`. Mixed numeric
operations are type errors unless the conversion is explicit.

```quone
to_double 1L + 2
```

## 4. Operators

Quone has a small fixed operator set. User-defined operators are not supported.

The initial release includes arithmetic, comparison, logical operators, unary
negation, and pipe.

| Category | Operators |
| --- | --- |
| arithmetic | `+`, `-`, `*`, `/`, `//`, `%`, `^` |
| comparison | `==`, `!=`, `<`, `<=`, `>`, `>=` |
| logical | `&&`, `||`, `not` |
| pipe | `|>` |

Arithmetic and comparison operators use fixed built-in overloads. Quone has no
typeclasses, traits, or constrained type variables.

Logical operators are `elementwise` and follow the same strict vector length and
scalar broadcast rules as arithmetic operators.

## 5. Records

A record is a named collection of fields. Record types may be written directly
in annotations.

```quone
student : { name : Character, score : Double }
student <- { name = "Ada", score = 95 }
```

Record literals require explicit `field = value` bindings. Field punning is not
part of the initial release.

Records lower to named R lists.

```r
student <- list(name = "Ada", score = 95)
```

Fields are accessed with dot syntax.

```quone
student.name
student.score
```

Records can be updated by copying an existing record and replacing selected
fields.

```quone
updated <- { student | score = 100 }
```

Record types are exact. A value of type
`{ name : Character, score : Double }` is not the same type as
`{ name : Character }`, even though both have a `name` field. Open records are
out of scope for the initial release.

Quone does not have tuples. Use records for heterogeneous grouped values.

## 6. Custom Types

A custom type defines one or more named constructors. Constructors may carry
payloads.

```quone
type Logical
    <- True
     | False

type Maybe a
    <- Nothing
     | Just a

type Result e a
    <- Ok a
     | Err e
```

Constructors are ordinary values and may be used in expressions. Custom types
are consumed with `case` expressions.

General custom types lower to tagged R lists because constructors may carry
payloads.

```r
list(tag = "Just", value = 1)
list(tag = "Err", value = error)
```

`Logical` is a regular custom type in Quone, but `True` and `False` lower
directly to R `TRUE` and `FALSE`.

Enum-like custom types with no payload may lower to character strings.

```quone
type Arm
    <- Placebo
     | Treatment
```

```r
"Placebo"
"Treatment"
```

Custom types do not lower to R factors by default. Factors are a dataframe
column representation for categorical data, not a general representation for
tagged unions. If Quone supports factors, they must be explicit in decoding or
column types.

```quone
Csv.column "arm" (factor Arm)
```

## 7. Missing Values

Quone represents missing values with `Maybe a`. Primitive types such as
`Integer`, `Double`, `Logical`, and `Character` do not implicitly contain
missing values.

```quone
age : Vector (Maybe Integer)
age <- [Just 20L, Nothing, Just 31L]
```

A scalar `Maybe a` lowers like an ordinary custom type. In vectors or dataframe
columns with primitive element types, `Nothing` lowers to the appropriate R
missing value such as `NA_integer_`, `NA_real_`, `NA`, or `NA_character_`.

CSV decoding must make missingness explicit. A missing CSV value decodes to
`Nothing` only when the decoder asks for `Maybe a`; otherwise it is a decode
error.

## 8. Pattern Matching, Case, and If

The initial release supports variable patterns, wildcard patterns, constructor
patterns, record patterns, and primitive literal patterns.

```quone
case result of
    Ok value -> value
    Err error -> fallback
```

`case` expressions must be exhaustive. For a custom type, every constructor
must be covered unless a wildcard branch is present.

`if` is syntax sugar for a `case` on `Logical`.

```quone
if condition then yes else no
```

desugars to:

```quone
case condition of
    True -> yes
    False -> no
```

When the matched value is scalar, `case` is scalar control flow. When the
matched value is `Vector n T`, `case` is vectorized and produces
`Vector n U`. Each branch must produce the same result type `U`; scalar branch
results may broadcast to length `n`.

Vectorized `case` supports payload-binding patterns. When a branch pattern
binds a payload, the binding is available in that branch as a `Vector n` value
masked to the rows matching that constructor. Lowering must not allow branch
bodies to observe invalid payload values for rows that do not match the branch.

```quone
students
    |> mutate
        { age_text =
            case age of
                Just n -> to_character n
                Nothing -> "missing"
        }
```

If `age` has type `Vector n (Maybe Integer)`, then the `Just n` branch binds
`n` as a `Vector n Integer` for the matching rows.

Vectorized `if` lowers to `dplyr::if_else`. Multi-arm vectorized `case` lowers
to `dplyr::case_when` when its branches are safe to evaluate pointwise. Payload
patterns lower by generating constructor masks and extracted payload vectors
before building the final vectorized result. Scalar `if` and scalar `case`
lower to ordinary R control flow.

Function parameters are names, not patterns. Use `case` to destructure records
or custom types.

## 9. Vectors and Hidden Lengths

Every vector carries a hidden length parameter for typechecking. Surface Quone
types are written `Vector T`, but the typechecker reasons as if they were
`Vector n T`.

The length parameter is not normally written in Quone source and is not part of
the ordinary displayed type. It exists so the typechecker can distinguish
"these two vectors have the same length" from "these vectors may have unrelated
lengths."

```text
[1, 2, 3]      : Vector 3 Double
["a", "b"]     : Vector 2 Character
external_value : Vector n Double
```

Vector literals are homogeneous. Every element must have the same type.

An empty vector literal needs an expected element type from an annotation or
surrounding context.

```quone
xs : Vector Double
xs <- []
```

An external vector may have an unknown but fixed length `n`. Operations that
preserve length keep the same `n`. Operations that combine multiple vectors
require the relevant vectors to have the same `n`.

Quone does not implement R's general vector recycling. Scalar broadcast is
allowed only through rules stated in this specification.

## 10. Functions and Expressions

Top-level type annotations are optional. When an annotation is omitted, Quone
infers the binding's type.

Functions are curried by default. A function with multiple arguments is written
as a chain of arrows.

```quone
add : Double -> Double -> Double
add x y <- x + y

total <- add 1 2
```

Function application is whitespace-separated. Parentheses are used for grouping,
not for ordinary function calls. Arguments are positional; Quone does not have
general named arguments.

```quone
sqrt 9
sqrt (score + bonus)
```

Partial application is allowed. Applying a curried function to fewer than all
of its arguments produces a new function.

```quone
add_one : Double -> Double
add_one <- add 1
```

Fully applied calls lower to ordinary R calls. Partial application lowers to
`purrr::partial` or an equivalent generated closure. The generated R dependency
on `purrr` is required only when a program uses features that lower to `purrr`.

Anonymous functions are written with `\`.

```quone
\x -> x + 1
\x y -> x + y
```

`let` expressions name intermediate values inside another expression.

```quone
let
    avg <- mean score
in
    score - avg
```

The pipe operator passes the value on its left as the first argument to the
function or verb on its right.

```quone
students
    |> filter (score > 70)
    |> mutate { centered = score - mean score }
```

Quone supports ordinary polymorphic functions, such as `map`. It does not have
typeclasses, traits, or constrained type variables. Operators use fixed
built-in overloads instead.

## 11. Prelude and Callable Classifications

Every source file imports the standard prelude implicitly. The prelude contains
primitive types, `Logical`, `Maybe`, `Result`, standard operators, common vector
functions such as `map`, numeric conversion functions such as `to_double`,
common reducers such as `mean`, and `Result.expect`.

Quone callables may carry a classification describing how they interact with
vectors. The main classifications are `elementwise`, `reducer`, and `opaque`.

An `elementwise` callable preserves vector length. Its displayed type stays the
ordinary scalar or vector type, with `elementwise` shown as a separate
classification.

```text
sqrt : Double -> Double [elementwise]
(+)  : Double -> Double -> Double [elementwise]
map  : (a -> b) -> Vector a -> Vector b [elementwise]
```

If an `elementwise` callable is applied to `Vector n` inputs, the result has
length `n`. For multi-argument `elementwise` callables such as `+`, all vector
arguments must have the same length `n`; scalar arguments may broadcast.

A `reducer` callable consumes vectors of some length `n` and returns a scalar.
Its result does not carry the input length.

```text
mean : Vector Double -> Double [reducer]
```

An `opaque` callable has no declared vector behavior. The typechecker must not
assume that it preserves length or reduces a vector to a scalar. Opaque
callables may be used where their explicit type is sufficient, but not where
the compiler needs vector behavior.

## 12. Foreign R Imports

Foreign imports expose R functions to Quone with an explicit Quone type.
Imports may also declare a callable classification.

```quone
import elementwise stringr.str_to_upper : Character -> Character
import reducer base.mean : Vector Double -> Double
```

An import without `elementwise` or `reducer` is `opaque`.

```quone
import readr.read_lines : Character -> Vector Character
```

Foreign imports lower to calls to the corresponding R package function. The
declared Quone type is trusted by the typechecker; the imported R function must
actually obey that type and classification at runtime.

Foreign imports expose a Quone-facing argument order. When the R function uses
a different order, the import may provide a lowering template with placeholders
for Quone arguments.

```quone
import elementwise stringr.str_detect
    as str_detect : Character -> Character -> Logical
    via stringr::str_detect($2, $1)
```

Here the Quone function takes `pattern` first and `string` second, while the R
function takes `string` first and `pattern` second. Without a `via` template,
arguments lower positionally in Quone order.

A `via` template is a constrained lowering form for a single R call expression.
Inline arbitrary R blocks are not part of the high-level language.

## 13. Type Aliases and Dataframe Types

A type alias gives a name to an existing type. It does not create a new runtime
representation.

Dataframe schemas list their columns as vector types.

```quone
type alias Students <-
    dataframe
        { name  : Vector Character
        , class : Vector Character
        , score : Vector Double
        }
```

A dataframe value carries a hidden row count `n`. Each column in its schema is a
`Vector n T`. A well-typed dataframe cannot contain columns with different
lengths.

Grouped dataframes are explicit in the type.

```quone
Grouped { class } Students
```

The grouping key set names columns in the underlying dataframe schema.

## 14. Dataframe Verbs

Quone includes dataframe verbs modeled on `dplyr`. These verbs are higher-level
surface forms built on vectors, hidden lengths, and callable classifications.

For typing, if a dataframe has a column `score : Vector Double`, then inside a
dataframe verb that column is available as `Vector n Double`, where `n` is the
input row count.

Bare column names are surface sugar for an explicit dataframe parameter.

```quone
students |> filter (score > 70)
```

desugars conceptually to:

```quone
students |> filter (\df -> df.score > 70)
```

Similarly:

```quone
students |> mutate { centered = score - mean score }
```

desugars conceptually to:

```quone
students |> mutate (\df -> { centered = df.score - mean df.score })
```

and:

```quone
students |> summarize { avg_score = mean score }
```

desugars conceptually to:

```quone
students |> summarize (\df -> { avg_score = mean df.score })
```

### 14.1 Verb Rules

`filter` must produce `Vector n Logical`. Reducers may appear inside the
predicate, as long as the final predicate has type `Vector n Logical`.

`mutate` must produce either a scalar `T` or a `Vector n T` for each output
field. Scalar results broadcast to length `n`. If a `mutate` field has the same
name as an existing column, it replaces that column in the output schema.
Modified columns keep their existing position; new columns are appended.

`summarize` must produce a scalar result for each output field. Scalar constants
are allowed.

`select`, `rename`, `group_by`, `ungroup`, and `arrange` operate on column names,
grouping state, and schema rather than general value-producing expressions.
Duplicate output column names are rejected.

Some verbs preserve the input row count `n`; others produce a new row count.

- `mutate`, `select`, `rename`, `arrange`, `group_by`, and `ungroup` preserve
  row count.
- `filter` may produce fewer rows.
- `summarize` produces one output row per group and returns an ungrouped
  dataframe.
- joins produce a fresh output row count.

`elementwise` and `reducer` classifications explain which calls are valid inside
verbs. An `elementwise` call preserves the ambient length `n`. A `reducer` call
consumes one or more `Vector n` inputs and returns a scalar.

This allows expressions such as `score - mean score` and
`score > mean score`: `mean score` reduces a `Vector n Double` to `Double`, and
the scalar broadcasts back across the `Vector n Double` in `score`.

For grouped dataframes, reducers inside `mutate` and `filter` operate per
group.

Quone does not have a special `rowwise` verb. Use records, custom functions, or
vectorized expressions instead.

After typing, dataframe verbs lower to the corresponding `dplyr` operations in
generated R.

### 14.2 Select and Rename

`select` is a projection over the input dataframe schema rather than a general
value-producing expression.

```quone
students |> select { name, score }
```

desugars conceptually to:

```quone
students |> select (\df -> { name = df.name, score = df.score })
```

`select` does not rename columns. Use `rename` for that.

```quone
students |> rename { student_name = name }
```

`rename` preserves column order.

### 14.3 Grouping

`group_by` returns a grouped dataframe value that may be assigned to a variable
and used later.

```quone
grouped : Grouped { class } Students
grouped <- students |> group_by { class }

summary <- grouped |> summarize { avg_score = mean score }
```

Use `ungroup` to convert a grouped dataframe back to an ordinary dataframe.

### 14.4 Joins

Quone supports typed equijoins with explicit join keys. Natural joins are not
inferred from matching column names.

```quone
students
    |> left_join schools { school_id = id }
```

This joins `students.school_id` to `schools.id`. The key columns must have the
same element type.

Join inputs must be ungrouped dataframes, and joins return ungrouped
dataframes. Join outputs have a fresh row count `m`, because the output length
is not statically tied to either input length.

Non-key output column names must be unique. If the left and right inputs have
colliding non-key column names, the join is rejected; users must `rename`
first. Quone does not add automatic suffixes.

`inner_join` keeps only matching rows.

`left_join` keeps all left rows. Non-key columns from the right side become
`Maybe` columns because unmatched rows have missing right-side values.

```text
right_name : Vector Character
```

becomes:

```text
right_name : Vector (Maybe Character)
```

`right_join` may be implemented by swapping the inputs to `left_join`. As a
user-facing verb, it still preserves right-join schema semantics: all right rows
are kept, and non-key columns from the left side become `Maybe` columns.

`full_join`, non-equi joins, and many-to-many relationship checks are out of
scope for the initial release.

## 15. CSV Loading and Decoding

`read_csv` loads a CSV file and decodes it into a typed dataframe. It is a
fallible operation and therefore returns `Result`.

```text
read_csv : Csv.DataframeDecoder d -> Character -> Result Csv.Error d
```

Here `d` is a dataframe type.

The declared decoder or schema is used to generate the column parsers for
`readr::read_csv` in generated R. After loading, `readr::problems(...)` and
additional schema checks determine whether the result is `Ok dataframe` or
`Err error`.

CSV decoders ignore extra input columns by default. Columns requested by the
decoder must be present and must decode successfully. The output dataframe
column order follows the decoder/schema order, not the source CSV order.
Duplicate source column names are decode errors.

```quone
students_decoder : Csv.DataframeDecoder Students

students_decoder <-
    Csv.dataframe
        |> Csv.column "name" character
        |> Csv.column "class" character
        |> Csv.column "score" double
```

Decoders may map source CSV column names to Quone column names.

```quone
students_decoder <-
    Csv.dataframe
        |> Csv.column_as "STUDENT_NAME" name character
        |> Csv.column_as "CLASS" class character
        |> Csv.column_as "SCORE" score double
```

Decoders may provide explicit defaults for optional columns.

```quone
students_decoder <-
    Csv.dataframe
        |> Csv.column "name" character
        |> Csv.column "class" character
        |> Csv.optional_column "score" score double 0
```

The decoder can then be used with `read_csv`.

```quone
students <-
    read_csv students_decoder "students.csv"
        |> Result.expect
```

`Result.expect` unwraps `Ok value` and stops execution on `Err error`. It uses
the error value's default rendering; callers do not provide a custom message.

After `Result.expect`, `students` is an ordinary typed dataframe value rather
than a `Result`. If `students` has hidden row count `n`, then its columns may be
used in later dataframe verbs as `Vector n` values.

```quone
students |> mutate { centered = score - mean score }
students |> summarize { avg_score = mean score }
students |> select { name, score }
```

## 16. Runtime Fallibility and Errors

Fallible operations return `Result e a`. Reusable functions should return
`Result` for recoverable failures rather than stopping execution directly.

`Result.expect : Result e a -> a` is intended for script boundaries. It unwraps
successful results and lowers failures to readable R `stop(...)` behavior using
the error value's default rendering.

Initial release error categories include:

- lexical errors
- parse errors
- unbound names
- type mismatch errors
- non-exhaustive `case`
- unknown record field errors
- unknown dataframe column errors
- dataframe shape errors
- CSV file loading errors
- CSV decode errors
- foreign import declaration errors

## 17. R Lowering and Dependencies

Generated R imports only packages required by the program, such as `dplyr`,
`readr`, or `purrr`.

The main lowering targets are:

- base R for primitive values, records, scalar control flow, and ordinary
  function calls
- `purrr` for partial application and higher-order vector operations where
  appropriate
- `dplyr` for dataframe verbs, vectorized `if`, and vectorized `case`
- `readr` for CSV loading

The compiler may generate small helper functions for tagged custom types,
payload extraction, CSV validation, and `Result.expect`, but the generated R
should remain readable and ordinary.

## 18. Initial Release Scope

Included in the initial release:

- modules, imports, and exposing lists
- indentation-sensitive source layout
- primitive scalars and homogeneous vectors
- exact records
- custom types and exhaustive `case`
- vectorized `case` including payload patterns
- `Maybe`-based missingness
- curried functions, lambdas, partial application, `let`, and pipes
- type aliases
- foreign R imports with optional lowering templates
- dataframe schemas and core dataframe verbs
- explicit grouped dataframe types
- typed `inner_join`, `left_join`, and `right_join`
- CSV dataframe decoders and `read_csv`
- `Result`-based runtime fallibility

Out of scope for the initial release:

- typeclasses, traits, or constrained type variables
- tuples
- open records / row polymorphism
- user-defined operators
- general named arguments
- arbitrary inline R blocks
- R-style vector recycling
- implicit primitive missingness
- `rowwise`
- `full_join`, non-equi joins, and many-to-many relationship checks
- generalized data sources beyond CSV
