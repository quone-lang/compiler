#' Scenario 05: group_by + summarize + arrange
#'
#' Exercises:
#'   - grouping a dataframe by a Character column
#'   - summarising with two reducers in one call (`mean`, `length`)
#'   - the implicit "drop the grouping" behaviour of summarize
#'   - sorting the summary descending by a derived column

students <-
    dataframe { name = ["Alice", "Bob", "Carol", "Dan", "Eve", "Frank"]
              , dept = ["math", "cs", "math", "cs", "math", "cs"]
              , score = [92.0, 78.0, 85.0, 64.0, 71.0, 89.0]
              }


main <-
    students
        |> group_by { dept }
        |> summarize { n = length name
                     , avg = mean score
                     }
        |> arrange (desc avg)
