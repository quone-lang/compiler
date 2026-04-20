#' Scenario 10: record update with `{ rec | field = value }`
#'
#' Exercises:
#'   - a starting record literal
#'   - record update syntax that overrides one field and leaves
#'     the others alone
#'   - field access on the updated record
#'   - the lowering to `purrr::list_modify(...)` per LANGUAGE.md 13.7
#'   - chaining: update, then access, then arithmetic

origin : { x : Double, y : Double, z : Double }
origin <- { x = 0.0, y = 0.0, z = 0.0 }


lifted : { x : Double, y : Double, z : Double }
lifted <- { origin | z = 10.5 }


distance_from_origin : Double
distance_from_origin <-
    (lifted.x ^ 2.0 + lifted.y ^ 2.0 + lifted.z ^ 2.0) ^ 0.5


main <- distance_from_origin
