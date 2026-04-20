threshold <- 80.0

score <- 92.5

main <- dplyr::if_else(score >= threshold, "pass", "fail")