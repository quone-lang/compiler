#' Scenario 06: select then filter
#'
#' Exercises:
#'   - `select { col1, col2 }` projecting a subset of columns
#'   - filtering AFTER select (so the filter only sees the projected schema)
#'   - integer columns alongside Double columns
#'   - the absence of a column making it unavailable to the filter
#'     (negative test belongs in invalid/ -- here we just want the
#'     happy path to lower correctly)

orders <-
    dataframe { order_id = ["o1", "o2", "o3", "o4"]
              , total = [10.0, 250.0, 80.0, 500.0]
              , region = ["west", "east", "west", "east"]
              }


main <-
    orders
        |> select { order_id, total }
        |> filter (total >= 100.0)
        |> arrange (desc total)
