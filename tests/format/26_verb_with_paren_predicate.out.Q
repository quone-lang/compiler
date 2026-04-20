xs <-
    dataframe { total = [ 10.0, 250.0, 80.0, 500.0 ] }


main <-
    xs |> filter (total >= 100.0)
