#' Scenario 03: records with vector fields and `mean`
#'
#' Exercises:
#'   - record literal with a Character field and a Vector Double field
#'   - record-typed function parameter
#'   - `.field` access on a record (lowers to R `$`)
#'   - calling `mean` (built-in over Vector Double)
#'   - the result of mean (a Double) flowing back into `main`

avg_score : { name : Character, scores : Vector Double } -> Double
avg_score student <- mean student.scores


alice <-
    { name = "Alice"
    , scores = [92.0, 88.0, 85.0, 91.0]
    }


main <- avg_score alice
