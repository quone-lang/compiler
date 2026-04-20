xs <-
    dataframe { a = [1, 2], b = [3, 4], c = [5, 6] }


main <- xs |> select { a, b, c }
