source("R/config.R")

library(dplyr)
library(tidyr)
library(arrow)

# ---- Load data -------------------------------------------------------------

chestnut_claims <- read_parquet(CHESTNUT_CLAIMS_PARQUET)
il_mo_wa_or <- read_parquet(IL_MO_WA_OR_CLAIMS_PARQUET)

# ---- Identify comparable HCPCS codes ---------------------------------------

# Codes Chestnut billed in every month of the first half of 2024
recent_chestnut_hcpcs <- chestnut_claims |>
  filter(
    year_month >= as.Date("2024-01-01"),
    year_month <= as.Date("2024-06-30")
  ) |>
  group_by(HCPCS_CODE) |>
  summarize(n_months = n_distinct(year_month), .groups = "drop") |>
  filter(n_months == 6) |>
  pull(HCPCS_CODE)

# ---- Filter IL/MO/WA/OR to comparable providers ----------------------------

# Providers in IL/MO/WA/OR with at least 3 Chestnut HCPCS codes
# and have a known taxonomy description
il_mo_wa_or_filtered <- il_mo_wa_or |>
  filter(!is.na(bp_taxonomy_description)) |>
  semi_join(
    il_mo_wa_or |>
      filter(HCPCS_CODE %in% recent_chestnut_hcpcs) |>
      distinct(BILLING_PROVIDER_NPI_NUM, HCPCS_CODE) |>
      count(BILLING_PROVIDER_NPI_NUM, name = "n_codes") |>
      filter(n_codes >= 3),
    by = "BILLING_PROVIDER_NPI_NUM"
  )

# ---- Sample WA/OR providers for comparison ---------------------------------

set.seed(123)
wa_or <- il_mo_wa_or |>
  filter(bp_state %in% c("WA", "OR")) |>
  semi_join(
    il_mo_wa_or |>
      filter(bp_state %in% c("WA", "OR")) |>
      distinct(BILLING_PROVIDER_NPI_NUM) |>
      slice_sample(n = 1000),
    by = "BILLING_PROVIDER_NPI_NUM"
  )

# ---- Combine WA/OR sample with Chestnut ------------------------------------

wa_or_chestnut <- bind_rows(
  wa_or |> mutate(dataset = "wa_or"),
  chestnut_claims |> mutate(dataset = "chestnut")
)

# ---- Summarize by provider -------------------------------------------------

wa_or_chestnut_summary <- wa_or_chestnut |>
  group_by(
    dataset,
    BILLING_PROVIDER_NPI_NUM,
    bp_city,
    bp_state,
    bp_taxonomy_description
  ) |>
  summarize(
    months_observed = n_distinct(year_month),
    n_claim_months = n(),
    total_claims = sum(TOTAL_CLAIMS),
    total_paid = sum(TOTAL_PAID),
    paid_per_claim = total_paid / total_claims,
    paid_per_month = total_paid / months_observed,
    n_unique_codes = n_distinct(HCPCS_CODE),
    .groups = "drop"
  )

# ---- Add per-HCPCS proportions (one column per code) -----------------------

hcpcs_props <- wa_or_chestnut |>
  group_by(BILLING_PROVIDER_NPI_NUM, HCPCS_CODE) |>
  summarize(code_claims = sum(TOTAL_CLAIMS), .groups = "drop") |>
  left_join(
    wa_or_chestnut_summary |> select(BILLING_PROVIDER_NPI_NUM, total_claims),
    by = "BILLING_PROVIDER_NPI_NUM"
  ) |>
  mutate(prop = code_claims / total_claims) |>
  select(BILLING_PROVIDER_NPI_NUM, HCPCS_CODE, prop) |>
  pivot_wider(
    names_from = HCPCS_CODE,
    values_from = prop,
    names_prefix = "prop_",
    values_fill = 0
  )

wa_or_chestnut_summary <- wa_or_chestnut_summary |>
  left_join(hcpcs_props, by = "BILLING_PROVIDER_NPI_NUM")
