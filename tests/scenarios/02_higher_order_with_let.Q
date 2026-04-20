#' Scenario 02: higher-order function with `let`
#'
#' Exercises:
#'   - a curried function being applied partially
#'   - `let ... in` introducing intermediate bindings
#'   - intermediates being used in later bindings inside the same let
#'   - calling a user-defined function with the partial as an argument
#'   - pure scalar arithmetic, no dataframe / vector machinery

bmi : Double -> Double -> Double
bmi weight_kg height_cm <-
    let
        height_m <- height_cm / 100.0
        squared <- height_m ^ 2.0
    in
        weight_kg / squared


classify : Double -> Character
classify b <-
    if b < 18.5 then "underweight"
    else if b < 25.0 then "normal"
    else if b < 30.0 then "overweight"
    else "obese"


main <- classify (bmi 82.0 178.0)
