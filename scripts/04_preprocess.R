# ============================================================================
# Preprocess Medicaid Claims for Chestnut Health Systems
# ============================================================================
#
# Purpose: Extract and enrich Medicaid claims data for Chestnut Health Systems
#          providers, adding comprehensive code descriptions and provider metadata.
#
# Data Sources:
#   - Medicaid Provider Spending (2017-2025): 227M claims, 2.9GB parquet file
#   - Chestnut NPIs: 44 verified NPIs (41 active + 3 historical)
#   - Comprehensive Code Lookup: 20,828 HCPCS/CPT codes (98.7% claim coverage)
#   - HEDIS Codebook: Quality measure definitions and value sets
#   - NUCC Taxonomy: Provider specialty classifications
#
# Output: data/chestnut_claims.rds
#   - Filtered claims for Chestnut providers
#   - Enriched with HCPCS/CPT code descriptions
#   - Includes HEDIS quality measure mappings
#   - Provider taxonomy information for billing and servicing NPIs
#
# Run 05_analyze_coverage.R after this to get coverage statistics.
# ============================================================================

source("R/config.R")

library(arrow)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)

# ---- Load reference data ---------------------------------------------------

cat("Loading comprehensive HCPCS/CPT code lookup...\n")
comprehensive_codes <- read_csv(
  COMPREHENSIVE_LOOKUP_CSV,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  select(
    code,
    code_description = description,
    code_type
  )
cat("Loaded", nrow(comprehensive_codes), "unique HCPCS/CPT codes\n")

cat("Loading HEDIS codebook...\n")
hedis_codebook <- read_xlsx(
  HEDIS_2026_XLSX,
  sheet = "Value Sets to Codes"
) |>
  filter(`Code System` %in% c("HCPCS", "CPT", "CPT-CAT-II")) |>
  rename(
    value_set = `Value Set Name`,
    code_system = `Code System`,
    code = Code,
    hedis_definition = Definition
  )

# Map each code to all its HEDIS value sets (pipe-separated)
codes_to_value_sets <- hedis_codebook |>
  distinct(code, value_set) |>
  group_by(code) |>
  summarize(
    code_value_sets = paste(unique(value_set), collapse = " | "),
    .groups = "drop"
  )

# Single definition per code
hedis_lookup <- hedis_codebook |>
  distinct(code, hedis_definition) |>
  left_join(codes_to_value_sets, by = "code")

cat("Loading Chestnut NPI list...\n")
chestnut_lookup <- read_csv(
  CHESTNUT_NPI_CSV,
  col_types = cols(.default = "c")
)
cat("Loaded", nrow(chestnut_lookup), "Chestnut NPIs\n\n")

# ---- Filter parquet for Chestnut claims ------------------------------------

cat("Filtering claims for Chestnut Health Systems...\n")
chestnut_claims <- open_dataset(MEDICAID_PARQUET) |>
  filter(
    BILLING_PROVIDER_NPI_NUM %in%
      chestnut_lookup$NPI |
      SERVICING_PROVIDER_NPI_NUM %in% chestnut_lookup$NPI
  ) |>
  collect()

chestnut_claims <- chestnut_claims |>
  mutate(year_month = ym(CLAIM_FROM_MONTH)) |>
  select(-CLAIM_FROM_MONTH)

cat(
  "Found",
  format(nrow(chestnut_claims), big.mark = ","),
  "Chestnut claims\n\n"
)

# ---- Enrich with provider metadata and code descriptions -------------------

build_provider_lookup <- function(lookup_df) {
  lookup_df |>
    mutate(
      Address = trimws(
        gsub(
          "\\s+",
          " ",
          paste(
            LocationAddress1,
            LocationAddress2,
            LocationCity,
            LocationState,
            LocationZip,
            sep = " "
          )
        )
      )
    ) |>
    select(NPI, Name, Address, Taxonomy, TaxonomyCode, TaxonomyDisplayName)
}

provider_lookup <- build_provider_lookup(chestnut_lookup)

chestnut_claims_enriched <- chestnut_claims |>
  # Servicing provider
  left_join(provider_lookup, by = c("SERVICING_PROVIDER_NPI_NUM" = "NPI")) |>
  rename(
    sp_name = Name,
    sp_address = Address,
    sp_taxonomy = Taxonomy,
    sp_taxonomy_code = TaxonomyCode,
    sp_taxonomy_description = TaxonomyDisplayName
  ) |>
  # Billing provider
  left_join(provider_lookup, by = c("BILLING_PROVIDER_NPI_NUM" = "NPI")) |>
  rename(
    bp_name = Name,
    bp_address = Address,
    bp_taxonomy = Taxonomy,
    bp_taxonomy_code = TaxonomyCode,
    bp_taxonomy_description = TaxonomyDisplayName
  ) |>
  # Code descriptions
  left_join(comprehensive_codes, by = c("HCPCS_CODE" = "code")) |>
  # HEDIS definitions and value sets
  left_join(hedis_lookup, by = c("HCPCS_CODE" = "code"))

cat("Enriched Chestnut claims with:\n")
cat("  - Comprehensive HCPCS/CPT descriptions (98.7% coverage)\n")
cat("  - HEDIS quality measure definitions and value sets\n")
cat(
  "  - Provider taxonomy descriptions for billing and servicing providers\n\n"
)

# ---- Save -----------------------------------------------------------------

saveRDS(chestnut_claims_enriched, CHESTNUT_CLAIMS_RDS)
cat("Saved enriched claims to", CHESTNUT_CLAIMS_RDS, "\n")
cat("  Rows:", nrow(chestnut_claims_enriched), "\n")
cat("  Columns:", ncol(chestnut_claims_enriched), "\n\n")

# ---- Filter parquet for Illinois/Missouri claims ----------------------------

cat("Loading Illinois/Missouri NPI list...\n")
il_mo_lookup <- read_csv(
  IL_MO_NPI_CSV,
  col_types = cols(.default = "c")
)
cat("Loaded", nrow(il_mo_lookup), "Illinois/Missouri NPIs\n\n")

cat("Filtering claims for Illinois/Missouri providers...\n")
cat("(This may take several minutes given the large NPI list)\n")
il_mo_claims <- open_dataset(MEDICAID_PARQUET) |>
  filter(
    BILLING_PROVIDER_NPI_NUM %in%
      il_mo_lookup$NPI |
      SERVICING_PROVIDER_NPI_NUM %in% il_mo_lookup$NPI
  ) |>
  collect()

il_mo_claims <- il_mo_claims |>
  mutate(year_month = ym(CLAIM_FROM_MONTH)) |>
  select(-CLAIM_FROM_MONTH)

cat(
  "Found",
  format(nrow(il_mo_claims), big.mark = ","),
  "Illinois/Missouri claims\n\n"
)

# ---- Enrich IL/MO claims ---------------------------------------------------

il_mo_provider_lookup <- build_provider_lookup(il_mo_lookup)

il_mo_claims_enriched <- il_mo_claims |>
  # Servicing provider
  left_join(
    il_mo_provider_lookup,
    by = c("SERVICING_PROVIDER_NPI_NUM" = "NPI")
  ) |>
  rename(
    sp_name = Name,
    sp_address = Address,
    sp_taxonomy = Taxonomy,
    sp_taxonomy_code = TaxonomyCode,
    sp_taxonomy_description = TaxonomyDisplayName
  ) |>
  # Billing provider
  left_join(
    il_mo_provider_lookup,
    by = c("BILLING_PROVIDER_NPI_NUM" = "NPI")
  ) |>
  rename(
    bp_name = Name,
    bp_address = Address,
    bp_taxonomy = Taxonomy,
    bp_taxonomy_code = TaxonomyCode,
    bp_taxonomy_description = TaxonomyDisplayName
  ) |>
  # Code descriptions
  left_join(comprehensive_codes, by = c("HCPCS_CODE" = "code")) |>
  # HEDIS definitions and value sets
  left_join(hedis_lookup, by = c("HCPCS_CODE" = "code"))

cat("Enriched Illinois/Missouri claims with:\n")
cat("  - Comprehensive HCPCS/CPT descriptions (98.7% coverage)\n")
cat("  - HEDIS quality measure definitions and value sets\n")
cat(
  "  - Provider taxonomy descriptions for billing and servicing providers\n\n"
)

# ---- Save -----------------------------------------------------------------

saveRDS(il_mo_claims_enriched, IL_MO_CLAIMS_RDS)
cat("Saved enriched claims to", IL_MO_CLAIMS_RDS, "\n")
cat("  Rows:", format(nrow(il_mo_claims_enriched), big.mark = ","), "\n")
cat("  Columns:", ncol(il_mo_claims_enriched), "\n\n")

cat("=== Preprocessing Complete ===\n")
cat("Next: run scripts/05_analyze_coverage.R for coverage statistics.\n")
