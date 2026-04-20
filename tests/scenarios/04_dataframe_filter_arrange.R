students <- data.frame(name = c("Alice", "Bob", "Carol", "Dan"), score = c(92.0, 78.0, 85.0, 64.0), bonus = c(5.0, 0.0, 3.0, 0.0))

main <- students |> dplyr::filter(score > 70.0) |> dplyr::mutate(total = score + bonus) |> dplyr::arrange(dplyr::desc(total))