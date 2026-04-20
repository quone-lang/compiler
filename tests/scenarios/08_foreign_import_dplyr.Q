#' Scenario 08: foreign import (`dplyr::if_else`) used end-to-end
#'
#' Exercises:
#'   - `import pkg.fn : Type` form at the top of the file
#'   - the imported function being called with a literal Logical
#'     value computed from a comparison (not a True/False literal --
#'     see KNOWN_FAILURES for why)
#'   - the imported call lowering to `pkg::fn(...)` with the right
#'     argument order
#'   - the imported call's result flowing into `main`

import dplyr.if_else : Logical -> Character -> Character -> Character


threshold : Double
threshold <- 80.0


score : Double
score <- 92.5


main <- if_else (score >= threshold) "pass" "fail"
