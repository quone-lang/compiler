orders <- data.frame(order_id = c("o1", "o2", "o3", "o4"), total = c(10.0, 250.0, 80.0, 500.0), region = c("west", "east", "west", "east"))

main <- orders |> dplyr::select(order_id = order_id, total = total) |> dplyr::filter(total >= 100.0) |> dplyr::arrange(dplyr::desc(total))