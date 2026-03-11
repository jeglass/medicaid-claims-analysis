# ============================================================================
# Preprocess Medicaid Claims for BHRN Providers
# ============================================================================
#
# Purpose: Extract and enrich Medicaid claims data for Behavioral Health
#          Resource Network (BHRN) providers, adding comprehensive code
#          descriptions and provider metadata.
#
# Data Sources:
#   - Medicaid Provider Spending (2017-2025): 227M claims, 2.9GB parquet file
#   - BHRN NPIs: doc/bhrn_npi.xlsx (site name + NPI)
#   - Organizational NPI data: doc/organizational_npi_full.parquet (for enrichment)
#   - Comprehensive Code Lookup: 20,828 HCPCS/CPT codes (98.7% claim coverage)
#   - HEDIS Codebook: Quality measure definitions and value sets
#
# Output: data/bhrn_claims.parquet
#   - Filtered claims for BHRN providers
#   - Enriched with HCPCS/CPT code descriptions
#   - Includes HEDIS quality measure mappings
#   - Provider taxonomy information for billing and servicing NPIs
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
if (!file.exists(HEDIS_2026_XLSX)) {
  warning(
    "HEDIS codebook not found: ",
    HEDIS_2026_XLSX,
    "\n",
    "  hedis_definition and code_value_sets will be NA for all claims.\n",
    "  Set HEDIS_2026_XLSX in R/config.local.R to enable HEDIS enrichment."
  )
  hedis_lookup <- tibble(
    code = character(),
    hedis_definition = character(),
    code_value_sets = character()
  )
} else {
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

  codes_to_value_sets <- hedis_codebook |>
    distinct(code, value_set) |>
    group_by(code) |>
    summarize(
      code_value_sets = paste(unique(value_set), collapse = " | "),
      .groups = "drop"
    )

  hedis_lookup <- hedis_codebook |>
    distinct(code, hedis_definition) |>
    left_join(codes_to_value_sets, by = "code")

  cat("Loaded", nrow(hedis_lookup), "HEDIS code mappings\n")
}

# ---- Load and enrich BHRN NPIs ---------------------------------------------

cat("Loading BHRN NPI list...\n")
bhrn_raw <- read_xlsx(BHRN_NPI_XLSX, col_types = "text") |>
  rename(
    bhrn_site = 1,
    NPI       = 2
  ) |>
  filter(!is.na(NPI))
cat("Loaded", nrow(bhrn_raw), "BHRN NPIs\n")

cat("Enriching BHRN NPIs from NPPES organizational data...\n")
org_npi <- open_dataset(ORG_NPI_PARQUET) |>
  filter(NPI %in% bhrn_raw$NPI) |>
  collect()

bhrn_lookup <- bhrn_raw |>
  left_join(org_npi, by = "NPI")

matched <- sum(!is.na(bhrn_lookup$Name))
cat(
  "Matched", matched, "of", nrow(bhrn_lookup),
  "BHRN NPIs to NPPES records\n\n"
)

# ---- Medicaid coverage analysis for BHRN NPIs ------------------------------

cat("Checking BHRN NPI coverage in Medicaid data...\n")
medicaid_billing_npis   <- open_dataset(MEDICAID_PARQUET) |>
  distinct(BILLING_PROVIDER_NPI_NUM) |>
  collect() |>
  pull()
medicaid_servicing_npis <- open_dataset(MEDICAID_PARQUET) |>
  distinct(SERVICING_PROVIDER_NPI_NUM) |>
  collect() |>
  pull()
medicaid_npis <- unique(c(medicaid_billing_npis, medicaid_servicing_npis))

npi_coverage <- bhrn_raw |>
  mutate(in_medicaid = NPI %in% medicaid_npis)

cat(
  "BHRN NPIs found in Medicaid data:    ",
  sum(npi_coverage$in_medicaid), "of", nrow(npi_coverage), "\n"
)
cat(
  "BHRN NPIs NOT found in Medicaid data:",
  sum(!npi_coverage$in_medicaid), "of", nrow(npi_coverage), "\n\n"
)

org_coverage <- npi_coverage |>
  group_by(bhrn_site) |>
  summarize(
    total_npis   = n(),
    npis_found   = sum(in_medicaid),
    npis_missing = sum(!in_medicaid),
    .groups = "drop"
  ) |>
  arrange(desc(npis_missing), bhrn_site)

cat("--- Organizations with NPIs missing from Medicaid data ---\n")
org_coverage |>
  filter(npis_missing > 0) |>
  print(n = Inf)
cat("\n")

# ---- Filter parquet for BHRN claims ----------------------------------------

cat("Filtering claims for BHRN providers...\n")
bhrn_claims <- open_dataset(MEDICAID_PARQUET) |>
  filter(
    BILLING_PROVIDER_NPI_NUM %in% bhrn_lookup$NPI |
      SERVICING_PROVIDER_NPI_NUM %in% bhrn_lookup$NPI
  ) |>
  collect()

bhrn_claims <- bhrn_claims |>
  mutate(year_month = ym(CLAIM_FROM_MONTH)) |>
  select(-CLAIM_FROM_MONTH)

cat(
  "Found",
  format(nrow(bhrn_claims), big.mark = ","),
  "BHRN claims\n\n"
)

# ---- Build provider lookup for enrichment ----------------------------------

provider_lookup <- bhrn_lookup |>
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
  select(
    NPI,
    bhrn_site,
    Name,
    Address,
    LocationCity,
    LocationState,
    LocationZip,
    Taxonomy,
    TaxonomyCode,
    TaxonomyDisplayName
  )

# ---- Enrich claims with provider metadata and code descriptions ------------

bhrn_claims_enriched <- bhrn_claims |>
  # Servicing provider
  left_join(provider_lookup, by = c("SERVICING_PROVIDER_NPI_NUM" = "NPI")) |>
  rename(
    sp_bhrn_site         = bhrn_site,
    sp_name              = Name,
    sp_address           = Address,
    sp_city              = LocationCity,
    sp_state             = LocationState,
    sp_zip               = LocationZip,
    sp_taxonomy          = Taxonomy,
    sp_taxonomy_code     = TaxonomyCode,
    sp_taxonomy_description = TaxonomyDisplayName
  ) |>
  # Billing provider
  left_join(provider_lookup, by = c("BILLING_PROVIDER_NPI_NUM" = "NPI")) |>
  rename(
    bp_bhrn_site         = bhrn_site,
    bp_name              = Name,
    bp_address           = Address,
    bp_city              = LocationCity,
    bp_state             = LocationState,
    bp_zip               = LocationZip,
    bp_taxonomy          = Taxonomy,
    bp_taxonomy_code     = TaxonomyCode,
    bp_taxonomy_description = TaxonomyDisplayName
  ) |>
  # Code descriptions
  left_join(comprehensive_codes, by = c("HCPCS_CODE" = "code")) |>
  # HEDIS definitions and value sets
  left_join(hedis_lookup, by = c("HCPCS_CODE" = "code"))

cat("Enriched BHRN claims with:\n")
cat("  - BHRN site names for billing and servicing providers\n")
cat("  - Comprehensive HCPCS/CPT descriptions (98.7% coverage)\n")
cat("  - HEDIS quality measure definitions and value sets\n")
cat("  - Provider taxonomy descriptions for billing and servicing providers\n\n")

# ---- Save ------------------------------------------------------------------

write_parquet(bhrn_claims_enriched, BHRN_CLAIMS_PARQUET)
cat("Saved enriched claims to", BHRN_CLAIMS_PARQUET, "\n")
cat("  Rows:", format(nrow(bhrn_claims_enriched), big.mark = ","), "\n")
cat("  Columns:", ncol(bhrn_claims_enriched), "\n\n")

cat("=== BHRN Preprocessing Complete ===\n")
