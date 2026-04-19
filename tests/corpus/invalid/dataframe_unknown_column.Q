students <-
    dataframe { name = ["Alice"], score = [92.0] }

bad <- students |> select { not_a_column }
