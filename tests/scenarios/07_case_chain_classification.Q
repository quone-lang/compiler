#' Scenario 07: nested case chain returning a Character
#'
#' Exercises:
#'   - `case` on a Logical scrutinee with two arms (the if-shaped
#'     special case in section 13.6)
#'   - nested cases producing a 3-way classification
#'   - calling the classifier with literal arguments (no Vector,
#'     no map -- avoiding the purrr::map_chr arg-order issue)
#'   - the result being a plain Character returned via main

classify : Double -> Character
classify x <-
    case x < 0.0 of
        True -> "negative"
        False ->
            case x == 0.0 of
                True -> "zero"
                False -> "positive"


sample : Double
sample <- 0.0 - 3.7 + 5.0


main <- classify sample
