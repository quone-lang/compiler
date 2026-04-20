bmi <- function(weight_kg, height_cm) { { height_m <- height_cm / 100.0; squared <- height_m ^ 2.0; weight_kg / squared } }

classify <- function(b) { if (b < 18.5) "underweight" else if (b < 25.0) "normal" else if (b < 30.0) "overweight" else "obese" }

main <- classify(bmi(82.0, 178.0))