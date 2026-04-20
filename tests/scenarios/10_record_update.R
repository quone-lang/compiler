origin <- list(x = 0.0, y = 0.0, z = 0.0)

lifted <- purrr::list_modify(origin, z = 10.5)

distance_from_origin <- (lifted$x ^ 2.0 + lifted$y ^ 2.0 + lifted$z ^ 2.0) ^ 0.5

main <- distance_from_origin