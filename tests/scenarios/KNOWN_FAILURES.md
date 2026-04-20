# Known scenario-level failures

The scenarios in this directory all currently pass. While building
them out, however, the smoke-test runs surfaced a handful of
end-to-end bugs that aren't (yet) covered by a scenario because the
generated R errors out instead of producing a stable golden. They
are listed here so future scenario work can either:

- wait for the bug to be fixed and then add a scenario that locks
  in the correct behaviour, or
- add a "known-failing" scenario (with a `.fail` suffix or a
  metadata marker) that the harness can opt into.

Each entry includes a minimal `.Q` reproducer; pasting it into a file
under `scratch/` and running `quonec build` is enough to reproduce.

---

## 1. `True` / `False` literals lower to bare R names instead of `TRUE` / `FALSE`

**Reproducer:**

```q
import dplyr.if_else : Logical -> Character -> Character -> Character

main <- if_else True "yes" "no"
```

**Lowered R (current):**

```r
main <- dplyr::if_else(True, "yes", "no")
```

**Runtime error:** `Error in eval(ei, envir) : object 'True' not found`

**Root cause:** `Quone.Generate.R.generate` lowers `EConstructor n`
as `upperText n`, which prints `True` / `False` verbatim. R uses
`TRUE` / `FALSE`. The case-on-Logical optimisation in
`Quone.Generate.R.lowerCase` already handles the construct on the
*pattern* side, but a bare `True` value flowing into a non-case
position never gets the special treatment.

**Workaround in scenarios:** build the Logical from a comparison
(e.g. `1 < 2`) instead of using the literal constructor.

---

## 2. `Maybe` destructuring generates malformed R

**Reproducer:**

```q
unwrap : Double -> Maybe Double -> Double
unwrap fallback m <-
    case m of
        Just x -> x
        Nothing -> fallback

main <- unwrap (-1.0) (Just 42.0)
```

**Lowered R (current, abridged):**

```r
unwrap <- function(fallback, m) {
  ._scrutinee <- m
  if (._scrutinee$tag == "Just") {
    x <- ._scrutinee$values[1]]   # <-- extra closing bracket
    x
  } else if (._scrutinee$tag == "Nothing") fallback
}
```

**Parse error:** `unexpected ']'`.

**Root cause:** the constructor-arm lowering in
`Quone.Generate.R.chainShape` (or the surrounding helper) emits
`._scrutinee$values[1]` followed by a stray `]`, suggesting the
indexing template is being closed once too often.

**Workaround in scenarios:** avoid `Maybe`/algebraic-data
destructuring; use comparisons + nested `case` on Logicals.

---

## 3. `purrr::map_chr` argument order is inverted

**Reproducer:**

```q
import purrr.map_chr :
    (Double -> Character) -> Vector Double -> Vector Character

show : Double -> Character
show x <- "x"

main <- map_chr show [1.0, 2.0, 3.0]
```

**Lowered R (current):**

```r
main <- purrr::map_chr(show, c(1.0, 2.0, 3.0))
```

**Runtime error:** `purrr::map_chr` expects `(.x, .f, ...)`,
i.e. *vector first, function second*. Quone applies arguments in
declared Haskell order, which conflicts with `purrr`'s convention.

**Root cause:** Quone's foreign-import call lowering preserves
positional argument order, but `purrr` uses the "data first" idiom
for piping. There is no per-import "argument-shuffle" mechanism.

**Workaround in scenarios:** call the user-defined function directly
on a single Double instead of mapping it across a Vector.

---

## 4. `summarize` drops the grouping column from the output schema

**Reproducer:**

```q
subjects <-
    dataframe { arm = ["A", "A", "B"], age = [30.0, 31.0, 40.0] }

main <-
    subjects
        |> group_by { arm }
        |> summarize { mean_age = mean age }
        |> arrange { arm }                -- ERROR: no column "arm"
```

**Compile error:** `error[unknown-dataframe-column]: dataframe has no column "arm"` at the `arrange` line.

**Expected behaviour:** `summarize` should keep the grouping
columns in the output schema (this matches dplyr's runtime
behaviour: the resulting tibble has both the group key and the
summary columns).

**Workaround in scenarios:** drop the `arrange { col }` after the
summarize, or sort by the new derived column instead.

---

## 5. Parser rejects foreign-import names with underscores when the type is a chain of dataframes

**Reproducer:**

```q
import dplyr.inner_join : dataframe { id : Vector Double } -> dataframe { id : Vector Double } -> dataframe { id : Vector Double }

main <- 1.0
```

**Compile error:** `error[parse]: expected end of file; found keyword "import"  --> file.Q:1:1-7`

**Bisection notes (recorded in chat):**

| name | type | result |
|------|------|--------|
| `x.f` | chain of three `dataframe { ... }` | builds |
| `dplyr.f` | chain of three `dataframe { ... }` | builds |
| `dplyr.innerjoin` (no underscore) | chain of three `dataframe { ... }` | builds |
| `dplyr.inner_join` (with underscore) | chain of three `dataframe { ... }` | **fails** |
| `dplyr.inner_join` | single dataframe arg | builds |

So the trigger is *underscored name* + *multi-dataframe arrow chain*
in the same import declaration.

**Workaround in scenarios:** name the imported function without an
underscore, or split chained dataframe types into a record alias.
