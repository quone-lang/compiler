#' Scenario 04: dataframe filter + mutate + arrange chain
#'
#' Exercises:
#'   - `dataframe { ... }` literal with three columns of mixed types
#'   - `|>` pipeline with three verbs in a row
#'   - `filter` predicate referring to a column by bare name
#'   - `mutate` adding a derived column referencing two existing columns
#'   - `arrange (desc col)` for descending sort

students <-
    dataframe { name = ["Alice", "Bob", "Carol", "Dan"]
              , score = [92.0, 78.0, 85.0, 64.0]
              , bonus = [5.0, 0.0, 3.0, 0.0]
              }


main <-
    students
        |> filter (score > 70.0)
        |> mutate { total = score + bonus }
        |> arrange (desc total)
