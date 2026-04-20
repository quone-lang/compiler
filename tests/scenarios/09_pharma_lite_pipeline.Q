#' Scenario 09: pharma-lite -- a real-shape analysis on inline data
#'
#' Distilled from `examples/pharma-analysis/pharma-analysis.Q`, but
#' without the CSV I/O so the test is hermetic. Exercises:
#'   - record/dataframe-building with multiple typed columns
#'   - a user-defined function (`bmi`) called inside a `mutate`
#'     where the function refers to columns by their bare names
#'   - chaining group_by + summarize after a mutate
#'   - the implicit "verb on the RHS of |>" lowering

bmi : Double -> Double -> Double
bmi weight_kg height_cm <-
    weight_kg / (height_cm / 100.0) ^ 2.0


subjects <-
    dataframe { subject_id = ["S01", "S02", "S03", "S04", "S05"]
              , arm = ["A", "A", "B", "B", "A"]
              , weight_kg = [82.0, 70.0, 90.0, 65.0, 78.0]
              , height_cm = [178.0, 165.0, 182.0, 160.0, 172.0]
              }


derived <-
    subjects
        |> mutate { bmi = bmi weight_kg height_cm }


main <-
    derived
        |> group_by { arm }
        |> summarize { n = length subject_id
                     , mean_bmi = mean bmi
                     }
