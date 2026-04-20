#' Scenario 01: arithmetic pipeline
#'
#' Exercises:
#'   - Integer + Double arithmetic stays in its own track
#'   - operator precedence (* binds tighter than +)
#'   - parens override precedence
#'   - unary minus on a literal
#'   - multiple top-level bindings interleaved
#'   - the final `main` binding being a Double, not a dataframe

cents : Integer
cents <-
  99 * 3 + 50

dollars : Double
dollars <-
  0.99 * 3.0 + 0.5

discount : Double
discount <-
  dollars - 0.5

rebate : Double
rebate <-
  -0.25 * (1.0 + 0.5)

main <-
  discount * 2.0 + rebate
