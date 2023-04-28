age_groups <- tibble::tibble(
  idade_a = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 100),
  idade_b = c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 99, 1000)
)

age_groups_names <- c(
  "From 0 to 4 years",
  "From 5 to 9 years",
  "From 10 to 14 years",
  "From 15 to 19 years",
  "From 20 to 24 years",
  "From 25 to 29 years",
  "From 30 to 34 years",
  "From 35 to 39 years",
  "From 40 to 44 years",
  "From 45 to 49 years",
  "From 50 to 54 years",
  "From 55 to 59 years",
  "From 60 to 64 years",
  "From 65 to 69 years",
  "From 70 to 74 years",
  "From 75 to 79 years",
  "From 80 to 99 years",
  "From 100 or more"
)
