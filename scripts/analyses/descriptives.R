il_mo <- readRDS("data/illinois_missouri_claims.rds")

il_mo |>
  group_by(state) |>
  summarize(
    n_claims = n(),
    n_unique_codes = n_distinct(code),
    .groups = "drop"
  ) |>
  arrange(state) |>
  print()
