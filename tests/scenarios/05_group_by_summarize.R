students <- data.frame(name = c("Alice", "Bob", "Carol", "Dan", "Eve", "Frank"), dept = c("math", "cs", "math", "cs", "math", "cs"), score = c(92.0, 78.0, 85.0, 64.0, 71.0, 89.0))

main <- students |> dplyr::group_by(dept = dept) |> dplyr::summarize(n = length(name), avg = mean(score)) |> dplyr::arrange(dplyr::desc(avg))