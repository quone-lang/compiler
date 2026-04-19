students <-
    dataframe { name = ["Alice"], score = [92.0], dept = ["math"] }

result <-
    students
        |> filter (score > 70.0)
        |> mutate { pct = score / 100.0 }
        |> arrange (desc score)
