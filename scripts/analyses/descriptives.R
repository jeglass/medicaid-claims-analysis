library(dplyr)

il_mo <- readRDS("data/illinois_missouri_claims.rds")
wa_or <- readRDS("data/washington_oregon_claims.rds")
chestnut_claims <- readRDS("data/chestnut_claims.rds")

# Keep orgs that have provided any service that Chestnut
# provided in all of the first 6 months of 2024. Also
# filter out any organizations that do not have a taxonomy
# description
recent_chestnut_hcpcs <- chestnut_claims |>
  filter(
    year_month >= as.Date("2024-01-01"),
    year_month <= as.Date("2024-06-30")
  ) |>
  group_by(HCPCS_CODE) |>
  summarize(n_months = n_distinct(year_month), .groups = "drop") |>
  filter(n_months == 6) |>
  pull(HCPCS_CODE)

wa_or_filtered <- wa_or |>
  filter_out(is.na(bp_taxonomy_description)) |>
  semi_join(
    wa_or |>
      filter(HCPCS_CODE %in% recent_chestnut_hcpcs) |>
      distinct(BILLING_PROVIDER_NPI_NUM),
    by = "BILLING_PROVIDER_NPI_NUM"
  )

il_mo_filtered <- il_mo |>
  filter_out(is.na(bp_taxonomy_description)) |>
  semi_join(
    il_mo |>
      filter(HCPCS_CODE %in% recent_chestnut_hcpcs) |>
      distinct(BILLING_PROVIDER_NPI_NUM),
    by = "BILLING_PROVIDER_NPI_NUM"
  )

wa_or_filtered |>
  group_by(BILLING_PROVIDER_NPI_NUM, bp_name) |>
  summarize(
    n_claims = n(),
    n_unique_codes = n_distinct(HCPCS_CODE),
    .groups = "drop"
  )


wa_or_tidy <- wa_or |>
  filter_out(is.na(bp_taxonomy_description)) |>
  group_by(bp_taxonomy_description) |>
  count() |>
  print(n = 400)


wa_or_tidy <- wa_or |>
  filter_out(is.na(bp_taxonomy_description)) |>
  group_by(bp_taxonomy_description) |>
  summarize(
    n_claim_months = n(),
    total_paid = sum(TOTAL_PAID),
    total_claims = sum(TOTAL_CLAIMS),
    avg_pts_per_hcpc_mo = mean(TOTAL_UNIQUE_BENEFICIARIES)
  )


## ADDITIOANL STUFF NOT VALIDATRD YET

#WIDE VERSION
library(dplyr)
library(tidyr)

# total claims per provider (all codes)
provider_totals <- wa_or_filtered |>
  group_by(BILLING_PROVIDER_NPI_NUM) |>
  summarize(total_claims = n(), .groups = "drop")

# counts per provider per chestnut HCPCS, pivot to wide
provider_code_counts_wide <- wa_or_filtered |>
  filter(HCPCS_CODE %in% recent_chestnut_hcpcs) |>
  group_by(BILLING_PROVIDER_NPI_NUM, HCPCS_CODE) |>
  summarize(n_claims = n(), .groups = "drop") |>
  pivot_wider(
    names_from = HCPCS_CODE,
    values_from = n_claims,
    names_prefix = "n_",
    values_fill = 0
  )

# join totals, compute proportions, and attach to wa_or_filtered providers
provider_summary_wide <- provider_totals |>
  left_join(provider_code_counts_wide, by = "BILLING_PROVIDER_NPI_NUM") |>
  # replace any remaining NA counts with 0
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0))) |>
  # create proportion columns named prop_<HCPCS>
  {
    df <- .
    for (code in recent_chestnut_hcpcs) {
      n_col <- paste0("n_", code)
      p_col <- paste0("prop_", code)
      df <- df |> mutate(!!p_col := if_else(total_claims > 0, .data[[n_col]] / total_claims, 0))
    }
    df
  }

# If you want these columns merged back into wa_or_filtered (one row per claim),
# join by provider NPI (this will repeat the provider-level summary on each claim row)
wa_or_filtered_with_summary <- wa_or_filtered |>
  left_join(provider_summary_wide, by = "BILLING_PROVIDER_NPI_NUM")

#LONG VERSION

library(dplyr)

# recent_chestnut_hcpcs assumed already computed as in your script
# recent_chestnut_hcpcs <- ...

# 1) provider counts before / after filtering
n_providers_before <- wa_or |> distinct(BILLING_PROVIDER_NPI_NUM) |> nrow()
wa_or_filtered <- wa_or |>
  semi_join(
    wa_or |>
      filter(HCPCS_CODE %in% recent_chestnut_hcpcs) |>
      distinct(BILLING_PROVIDER_NPI_NUM),
    by = "BILLING_PROVIDER_NPI_NUM"
  )
n_providers_after <- wa_or_filtered |> distinct(BILLING_PROVIDER_NPI_NUM) |> nrow()

# 2) for each kept provider, count how many distinct recent_chestnut_hcpcs they billed
provider_code_counts <- wa_or_filtered |>
  filter(HCPCS_CODE %in% recent_chestnut_hcpcs) |>
  group_by(BILLING_PROVIDER_NPI_NUM) |>
  summarize(
    n_distinct_chestnut_codes = n_distinct(HCPCS_CODE),
    .groups = "drop"
  )

# 3) distribution of counts (how many providers billed 1,2,3,... of the chestnut codes)
count_distribution <- provider_code_counts |>
  count(n_distinct_chestnut_codes, name = "n_providers") |>
  arrange(desc(n_distinct_chestnut_codes))

# 4) optional: show which codes each top provider billed (for inspection)
top_providers_codes <- wa_or_filtered |>
  filter(HCPCS_CODE %in% recent_chestnut_hcpcs) |>
  group_by(BILLING_PROVIDER_NPI_NUM) |>
  summarize(
    codes = paste(sort(unique(HCPCS_CODE)), collapse = ";"),
    n_codes = n_distinct(HCPCS_CODE),
    .groups = "drop"
  ) |>
  arrange(desc(n_codes))

# Print quick diagnostics
tibble(
  providers_before = n_providers_before,
  providers_after = n_providers_after,
  providers_removed = n_providers_before - n_providers_after
)

# View tables
provider_code_counts        # one row per kept provider with count
count_distribution          # distribution of counts
top_providers_codes |> head(20)  # top 20 providers by number of chestnut codes billed

#k-means clustering
https://www.youtube.com/watch?v=opHDQzhO5Fw