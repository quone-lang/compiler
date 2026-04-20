classify n <-
    case n of
        0 ->
            "zero"

        _ ->
            "other"


main <-
    classify 1
