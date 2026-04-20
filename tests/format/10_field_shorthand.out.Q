greet user <-
    case user of
        { name } ->
            name

        _ ->
            "anon"


main <-
    greet { name = "Alice" }
