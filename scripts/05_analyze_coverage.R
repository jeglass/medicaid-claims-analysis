# ============================================================================
# Analyze HCPCS Code Coverage
# ============================================================================
#
# Purpose: Validate HCPCS/CPT code coverage across the Medicaid dataset and
#          report Chestnut-specific code statistics.
#
# Merges the analysis sections previously split across:
#   - filter_data.R (Chestnut coverage stats and top codes)
#   - analyze_hcpcs_codes.R (coverage vs HCPCS 2026 + HEDIS reference files)
#   - check_coverage_by_volume.R (coverage validation by claim volume)
#
# Prerequisites: Run 04_preprocess.R first to generate data/chestnut_claims.rds
# ============================================================================

source("R/config.R")

library(arrow)
library(dplyr)
library(readr)
library(readxl)

# ============================================================================
# Section 1: Chestnut-specific coverage (from enriched claims)
# ============================================================================

cat("=== Section 1: Chestnut Claims Coverage ===\n\n")

chestnut_claims <- readRDS(CHESTNUT_CLAIMS_RDS)
cat(
  "Loaded",
  format(nrow(chestnut_claims), big.mark = ","),
  "Chestnut claims\n\n"
)

coverage_stats <- chestnut_claims |>
  summarize(
    total_claims = n(),
    unique_codes = n_distinct(HCPCS_CODE),
    codes_with_description = n_distinct(HCPCS_CODE[!is.na(code_description)]),
    codes_with_hedis = n_distinct(HCPCS_CODE[!is.na(hedis_definition)]),
    claims_with_description = sum(!is.na(code_description)),
    pct_codes_covered = 100 *
      n_distinct(HCPCS_CODE[!is.na(code_description)]) /
      n_distinct(HCPCS_CODE),
    pct_claims_covered = 100 * sum(!is.na(code_description)) / n()
  )

cat(
  "Total claims:        ",
  format(coverage_stats$total_claims, big.mark = ","),
  "\n"
)
cat("Unique HCPCS codes:  ", coverage_stats$unique_codes, "\n")
cat(
  "Codes with descriptions:",
  coverage_stats$codes_with_description,
  sprintf("(%.1f%%)", coverage_stats$pct_codes_covered),
  "\n"
)
cat("Codes with HEDIS:    ", coverage_stats$codes_with_hedis, "\n")
cat(
  "Claims with descriptions:",
  format(coverage_stats$claims_with_description, big.mark = ","),
  sprintf("(%.1f%%)", coverage_stats$pct_claims_covered),
  "\n\n"
)

cat("Top 20 most frequent HCPCS codes in Chestnut claims:\n")
top_codes <- chestnut_claims |>
  count(HCPCS_CODE, code_description, code_type, sort = TRUE) |>
  head(20)
print(top_codes, n = 20)

cat("\nClaims by code type:\n")
chestnut_claims |>
  count(code_type, sort = TRUE) |>
  mutate(
    percentage = 100 * n / sum(n),
    code_type = ifelse(is.na(code_type), "Unknown/Missing", code_type)
  ) |>
  print(n = Inf)

missing_codes <- chestnut_claims |>
  filter(is.na(code_description)) |>
  count(HCPCS_CODE, sort = TRUE)

if (nrow(missing_codes) > 0) {
  cat("\nCodes without descriptions (top 10):\n")
  print(head(missing_codes, 10))
  cat("\nNote:", nrow(missing_codes), "unique codes lack descriptions\n")
  cat(
    "This represents",
    sum(missing_codes$n),
    "claims",
    sprintf("(%.1f%%)", 100 * sum(missing_codes$n) / nrow(chestnut_claims)),
    "\n"
  )
} else {
  cat("\nAll codes have descriptions!\n")
}

# ============================================================================
# Section 2: Full Medicaid coverage vs reference files
# ============================================================================

cat("\n\n=== Section 2: Full Medicaid Coverage vs Reference Files ===\n\n")

medicaid_parquet <- open_dataset(MEDICAID_PARQUET)

cat("Extracting unique HCPCS codes from Medicaid data...\n")
medicaid_codes <- medicaid_parquet |>
  select(HCPCS_CODE) |>
  distinct() |>
  collect() |>
  filter(!is.na(HCPCS_CODE)) |>
  arrange(HCPCS_CODE)
cat("Found", nrow(medicaid_codes), "unique HCPCS codes in Medicaid data\n\n")

# Load reference files
hcpcs_2026 <- read_xlsx(
  HCPCS_2026_XLSX,
  range = cell_cols(c("A", "D"))
) |>
  rename(hcpc = HCPC, hcpc_long = `LONG DESCRIPTION`)

cat("Loaded", nrow(hcpcs_2026), "codes from HCPCS Jan 2026 file\n")

hedis_codebook <- read_xlsx(
  HEDIS_2026_XLSX,
  sheet = "Value Sets to Codes"
) |>
  filter(`Code System` == "HCPCS") |>
  rename(code = Code, hedis_definition = Definition) |>
  distinct(code, hedis_definition)

cat("Loaded", nrow(hedis_codebook), "HCPCS codes from HEDIS codebook\n\n")

medicaid_codes <- medicaid_codes |>
  mutate(
    in_hcpcs_2026 = HCPCS_CODE %in% hcpcs_2026$hcpc,
    in_hedis = HCPCS_CODE %in% hedis_codebook$code,
    in_either = in_hcpcs_2026 | in_hedis
  )

cat("Coverage vs reference files (unique codes):\n")
cat(
  "  In HCPCS Jan 2026:",
  sum(medicaid_codes$in_hcpcs_2026),
  "/",
  nrow(medicaid_codes),
  sprintf("(%.1f%%)", 100 * mean(medicaid_codes$in_hcpcs_2026)),
  "\n"
)
cat(
  "  In HEDIS:         ",
  sum(medicaid_codes$in_hedis),
  "/",
  nrow(medicaid_codes),
  sprintf("(%.1f%%)", 100 * mean(medicaid_codes$in_hedis)),
  "\n"
)
cat(
  "  In either source: ",
  sum(medicaid_codes$in_either),
  "/",
  nrow(medicaid_codes),
  sprintf("(%.1f%%)", 100 * mean(medicaid_codes$in_either)),
  "\n"
)
cat(
  "  Missing:          ",
  sum(!medicaid_codes$in_either),
  "/",
  nrow(medicaid_codes),
  sprintf("(%.1f%%)", 100 * mean(!medicaid_codes$in_either)),
  "\n\n"
)

missing_ref <- medicaid_codes |> filter(!in_either) |> select(HCPCS_CODE)
cat("Sample of missing codes:\n")
print(head(missing_ref, 50))

write_csv(medicaid_codes, "doc/hcpcs/medicaid_hcpcs_codes_coverage.csv")
cat(
  "\nSaved coverage analysis to doc/hcpcs/medicaid_hcpcs_codes_coverage.csv\n"
)

# ============================================================================
# Section 3: Coverage by claim volume (comprehensive lookup)
# ============================================================================

cat(
  "\n\n=== Section 3: Coverage by Claim Volume (Comprehensive Lookup) ===\n\n"
)

code_lookup <- read_csv(
  COMPREHENSIVE_LOOKUP_CSV,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)
cat("Loaded", nrow(code_lookup), "codes from comprehensive lookup\n\n")

coverage_by_volume <- medicaid_parquet |>
  mutate(has_description = HCPCS_CODE %in% code_lookup$code) |>
  group_by(has_description) |>
  summarize(n_claims = n()) |>
  collect()

total_claims <- sum(coverage_by_volume$n_claims)
claims_with_desc <- coverage_by_volume |>
  filter(has_description == TRUE) |>
  pull(n_claims)
claims_with_desc <- ifelse(length(claims_with_desc) == 0, 0, claims_with_desc)
pct_covered <- 100 * claims_with_desc / total_claims

cat("Coverage by Claim Volume:\n")
print(coverage_by_volume)
cat("\nTotal claims:             ", format(total_claims, big.mark = ","), "\n")
cat(
  "Claims with descriptions: ",
  format(claims_with_desc, big.mark = ","),
  "\n"
)
cat("Percentage covered:       ", sprintf("%.1f%%", pct_covered), "\n\n")

cat("Top 20 most frequent codes WITHOUT descriptions:\n")
medicaid_parquet |>
  filter(!(HCPCS_CODE %in% code_lookup$code)) |>
  count(HCPCS_CODE, sort = TRUE) |>
  collect() |>
  head(20) |>
  print()

cat("\nTop 20 most frequent codes WITH descriptions:\n")
medicaid_parquet |>
  filter(HCPCS_CODE %in% code_lookup$code) |>
  count(HCPCS_CODE, sort = TRUE) |>
  collect() |>
  head(20) |>
  left_join(
    code_lookup |> select(code, description),
    by = c("HCPCS_CODE" = "code")
  ) |>
  print()

cat("\nConclusion:\n")
if (pct_covered >= 80) {
  cat(sprintf("EXCELLENT: %.1f%% of claims have descriptions.\n", pct_covered))
  cat(
    "The missing codes are likely rare or invalid codes with minimal impact.\n"
  )
} else if (pct_covered >= 60) {
  cat(sprintf("GOOD: %.1f%% of claims have descriptions.\n", pct_covered))
  cat("Consider reviewing the top missing codes.\n")
} else {
  cat(sprintf(
    "NEEDS IMPROVEMENT: Only %.1f%% of claims have descriptions.\n",
    pct_covered
  ))
  cat(
    "Review the top missing codes above - they may be common codes needing descriptions.\n"
  )
}

# ============================================================================
# Section 4: Illinois/Missouri claims coverage
# ============================================================================

cat("\n\n=== Section 4: Illinois/Missouri Claims Coverage ===\n\n")

il_mo_claims <- readRDS(IL_MO_CLAIMS_RDS)
cat(
  "Loaded",
  format(nrow(il_mo_claims), big.mark = ","),
  "Illinois/Missouri claims\n\n"
)

il_mo_coverage_stats <- il_mo_claims |>
  summarize(
    total_claims = n(),
    unique_codes = n_distinct(HCPCS_CODE),
    codes_with_description = n_distinct(HCPCS_CODE[!is.na(code_description)]),
    codes_with_hedis = n_distinct(HCPCS_CODE[!is.na(hedis_definition)]),
    claims_with_description = sum(!is.na(code_description)),
    pct_codes_covered = 100 *
      n_distinct(HCPCS_CODE[!is.na(code_description)]) /
      n_distinct(HCPCS_CODE),
    pct_claims_covered = 100 * sum(!is.na(code_description)) / n()
  )

cat(
  "Total claims:        ",
  format(il_mo_coverage_stats$total_claims, big.mark = ","),
  "\n"
)
cat("Unique HCPCS codes:  ", il_mo_coverage_stats$unique_codes, "\n")
cat(
  "Codes with descriptions:",
  il_mo_coverage_stats$codes_with_description,
  sprintf("(%.1f%%)", il_mo_coverage_stats$pct_codes_covered),
  "\n"
)
cat("Codes with HEDIS:    ", il_mo_coverage_stats$codes_with_hedis, "\n")
cat(
  "Claims with descriptions:",
  format(il_mo_coverage_stats$claims_with_description, big.mark = ","),
  sprintf("(%.1f%%)", il_mo_coverage_stats$pct_claims_covered),
  "\n\n"
)

cat("Top 20 most frequent HCPCS codes in Illinois/Missouri claims:\n")
il_mo_top_codes <- il_mo_claims |>
  count(HCPCS_CODE, code_description, code_type, sort = TRUE) |>
  head(20)
print(il_mo_top_codes, n = 20)

cat("\nClaims by code type:\n")
il_mo_claims |>
  count(code_type, sort = TRUE) |>
  mutate(
    percentage = 100 * n / sum(n),
    code_type = ifelse(is.na(code_type), "Unknown/Missing", code_type)
  ) |>
  print(n = Inf)

il_mo_missing_codes <- il_mo_claims |>
  filter(is.na(code_description)) |>
  count(HCPCS_CODE, sort = TRUE)

if (nrow(il_mo_missing_codes) > 0) {
  cat("\nCodes without descriptions (top 10):\n")
  print(head(il_mo_missing_codes, 10))
  cat("\nNote:", nrow(il_mo_missing_codes), "unique codes lack descriptions\n")
  cat(
    "This represents",
    format(sum(il_mo_missing_codes$n), big.mark = ","),
    "claims",
    sprintf("(%.1f%%)", 100 * sum(il_mo_missing_codes$n) / nrow(il_mo_claims)),
    "\n"
  )
} else {
  cat("\nAll codes have descriptions!\n")
}

cat("\nTop 20 taxonomy types in Illinois/Missouri billing providers:\n")
il_mo_claims |>
  filter(!is.na(bp_taxonomy_description)) |>
  count(bp_taxonomy_description, sort = TRUE) |>
  head(20) |>
  print()

cat("\n=== Coverage Analysis Complete ===\n")
