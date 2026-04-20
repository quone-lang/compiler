orders <-
    dataframe { order_id = ["o1", "o2", "o3", "o4"]
              , total = [10.0, 250.0, 80.0, 500.0]
              }


main <- orders |> select { order_id, total } |> filter (total >= 100.0) |> arrange (desc total)
