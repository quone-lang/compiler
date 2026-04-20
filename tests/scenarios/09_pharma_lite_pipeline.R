bmi <- function(weight_kg, height_cm) { weight_kg / (height_cm / 100.0) ^ 2.0 }

subjects <- data.frame(subject_id = c("S01", "S02", "S03", "S04", "S05"), arm = c("A", "A", "B", "B", "A"), weight_kg = c(82.0, 70.0, 90.0, 65.0, 78.0), height_cm = c(178.0, 165.0, 182.0, 160.0, 172.0))

derived <- subjects |> dplyr::mutate(bmi = bmi(weight_kg, height_cm))

main <- derived |> dplyr::group_by(arm = arm) |> dplyr::summarize(n = length(subject_id), mean_bmi = mean(bmi))