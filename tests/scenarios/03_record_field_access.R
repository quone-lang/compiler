avg_score <- function(student) { mean(student$scores) }

alice <- list(name = "Alice", scores = c(92.0, 88.0, 85.0, 91.0))

main <- avg_score(alice)