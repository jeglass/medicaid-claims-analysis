# ============================================================================
# Filter Medicaid Provider Spending Data for Chestnut Health Systems
# ============================================================================
#
# Purpose: Extract and enrich Medicaid claims data for Chestnut Health Systems
#          providers, adding comprehensive code descriptions and provider metadata
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
# Updated: 2026-02-16 - Added comprehensive code lookup for full CPT coverage
# ============================================================================

#library(data.table)
library(arrow)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)

# Define SUD code ranges for filtering
sud_codes <- sprintf("H%04d", 1:2041)
core_sud_codes <- sprintf("H%04d", 1:11)

# Load comprehensive HCPCS/CPT code lookup
# This includes ~20,800 codes from multiple sources:
# - RVU files (2022-2026): CPT codes with descriptions
# - HCPCS Level II files: Alphanumeric codes
# - HEDIS codebook: Quality measure codes
# Coverage: 98.7% of Medicaid claims have descriptions
cat("Loading comprehensive HCPCS/CPT code lookup...\n")
comprehensive_codes <- read_csv(
  "doc/hcpcs/comprehensive_code_lookup.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  select(
    code,
    code_description = description,
    code_type
  )

cat("Loaded", nrow(comprehensive_codes), "unique HCPCS/CPT codes\n")

# Load HEDIS codebook separately for value set information
# HEDIS provides clinical groupings (value sets) useful for quality measures
hedis_codebook <- read_xlsx(
  "doc/HEDIS MY 2026 Volume 2 Value Set Directory_2025-08-01.xlsx",
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

# Create HEDIS lookup with single definition per code
hedis_lookup <- hedis_codebook |>
  distinct(code, hedis_definition) |>
  left_join(codes_to_value_sets, by = "code")

# Load comprehensive Chestnut Health Systems NPI list
# This list was verified against the complete NPPES NPI database (February 2026)
# and includes all 44 Chestnut NPIs (41 active + 3 historical/deactivated)
# to ensure complete capture of claims from 2017-2025
chestnut_lookup <- read_csv(
  "doc/chestnut_npi_full.csv",
  col_types = cols(.default = "c")
)
#sud_code_lookup <- hedis_codebook |>
#  filter(Code %in% sud_codes) |>
#  select(Code, Definition) |>
#  distinct(Code, .keep_all = TRUE)

# hedis_codebook |> count(Code %in% sud_codes) # 145 HCPCS sud codes in HEDIS codebook
# TODO: check to see if all these value sets are used by Chestnut
# Alcohol Counseling or Other Follow Up Care
# Behavioral Health Assessment
# Behavioral Health Encounter
# BH Outpatient
# BH Stand Alone Nonacute Inpatient
# Buprenorphine Implant
# Buprenorphine Injection
# Buprenorphine Naloxone
# Buprenorphine Oral
# Buprenorphine Oral Weekly
# Substance Use Disorder Services
# Substance Use Services

# dt <- fread("medicaid-provider-spending.csv") # 227,083,361 rows
# core_filtered <- dt[HCPCS_CODE %in% core_sud_codes] # 986,057 rows
# write_csv(core_filtered, "core_filtered_data.csv")
# rm(dt)

######## Preprocessing: Do this once

# https://www.cms.gov/medicare/coding-billing/healthcare-common-procedure-system
# https://www.cms.gov/medicare/coding-billing/healthcare-common-procedure-system/quarterly-update

medicaid_parquet <- open_dataset("data/medicaid-provider-spending.parquet")

# Load organizational NPI lookup for filtering all organization claims
cat("Loading organizational NPI lookup...\\n")
organizational_lookup <- read_parquet("doc/organizational_npi_full.parquet")
cat("Loaded", nrow(organizational_lookup), "organizational NPIs\\n\\n")

# Create all_organization_claims dataset
cat("Filtering claims for all organizational providers...\\n")
all_organization_claims <- open_dataset(
  "data/medicaid-provider-spending.parquet"
) |>
  filter(
    BILLING_PROVIDER_NPI_NUM %in% organizational_lookup$NPI |
      SERVICING_PROVIDER_NPI_NUM %in% organizational_lookup$NPI
  ) |>
  collect()

cat("Found", format(nrow(all_organization_claims), big.mark = ","),
    "claims for organizational providers\\n\\n")

# Filter for Chestnut Health Systems claims
cat("Filtering claims for Chestnut Health Systems...\\n")
chestnut_claims <- open_dataset(
  "data/medicaid-provider-spending.parquet"
) |>
  filter(
    BILLING_PROVIDER_NPI_NUM %in%
      chestnut_lookup$NPI |
      SERVICING_PROVIDER_NPI_NUM %in% chestnut_lookup$NPI
  ) |>
  collect()

chestnut_claims <- chestnut_claims |>
  mutate(
    year_month = ym(CLAIM_FROM_MONTH)
  ) |>
  select(-CLAIM_FROM_MONTH)

chestnut_claims_enriched <- chestnut_claims |>
  # Join servicing provider information
  left_join(
    chestnut_lookup |>
      mutate(
        # Combine location address fields into single Address column
        Address = trimws(
          gsub("\\s+", " ",  # Replace multiple spaces with single space
               paste(LocationAddress1, LocationAddress2, LocationCity,
                     LocationState, LocationZip, sep = " "))
        )
      ) |>
      select(
        NPI,
        Name,
        Address,
        Taxonomy,
        TaxonomyCode,
        TaxonomyDisplayName
      ),
    by = c("SERVICING_PROVIDER_NPI_NUM" = "NPI")
  ) |>
  rename(
    sp_name = Name,
    sp_address = Address,
    sp_taxonomy = Taxonomy,
    sp_taxonomy_code = TaxonomyCode,
    sp_taxonomy_description = TaxonomyDisplayName
  ) |>
  # Join billing provider information
  left_join(
    chestnut_lookup |>
      mutate(
        # Combine location address fields into single Address column
        Address = trimws(
          gsub("\\s+", " ",  # Replace multiple spaces with single space
               paste(LocationAddress1, LocationAddress2, LocationCity,
                     LocationState, LocationZip, sep = " "))
        )
      ) |>
      select(
        NPI,
        Name,
        Address,
        Taxonomy,
        TaxonomyCode,
        TaxonomyDisplayName
      ),
    by = c("BILLING_PROVIDER_NPI_NUM" = "NPI")
  ) |>
  rename(
    bp_name = Name,
    bp_address = Address,
    bp_taxonomy = Taxonomy,
    bp_taxonomy_code = TaxonomyCode,
    bp_taxonomy_description = TaxonomyDisplayName
  ) |>
  # Join comprehensive code descriptions (CPT + HCPCS)
  left_join(
    comprehensive_codes,
    by = c("HCPCS_CODE" = "code")
  ) |>
  # Join HEDIS definitions and value sets (for quality measures)
  left_join(
    hedis_lookup,
    by = c("HCPCS_CODE" = "code")
  )

cat("\nEnriched Chestnut claims with:\n")
cat("  - Comprehensive HCPCS/CPT descriptions (98.7% coverage)\n")
cat("  - HEDIS quality measure definitions and value sets\n")
cat(
  "  - Provider taxonomy descriptions for both billing and servicing providers\n\n"
)

# Save enriched dataset
saveRDS(chestnut_claims_enriched, "data/chestnut_claims.rds")
cat("Saved enriched claims to data/chestnut_claims.rds\n")
cat("  Rows:", nrow(chestnut_claims_enriched), "\n")
cat("  Columns:", ncol(chestnut_claims_enriched), "\n\n")

#sud_claims |>
#  left_join(sud_code_lookup, by = c("HCPCS_CODE" = "Code")) |>
#  count(Definition, HCPCS_CODE, sort = TRUE, name = "n") |>
#  print(n = 100)

#sud_claims <- medicaid_parquet <- open_dataset(
#  "data/medicaid-provider-spending.parquet"
#) |>
#  filter(HCPCS_CODE %in% sud_codes) |>
#  collect()
#saveRDS(sud_claims, "sud_claims.rds") # 4,647,182 rows

#sud_claims |> count(HCPCS_CODE) |> print(n = 100)

#sud_claims |>
#  left_join(sud_code_lookup, by = c("HCPCS_CODE" = "Code")) |>
#  count(Definition, HCPCS_CODE, sort = TRUE, name = "n") |>
# print(n = 100)

#library(arrow)
#dataset <- open_dataset(
#  "medicaid-provider-spending.parquet",
#  format = "parquet"
#)
#dataset |> glimpse()

#filtered <- dataset$Filter(is_in(
#  ds$GetField("HCPCS_CODE"),
#  core_sud_codes
#))$ToTable()
#write_parquet(filtered, "filtered_data.parquet")

######## Analysis Section (Optional)
# Run this section to analyze code coverage in the Chestnut claims

# Reload the enriched claims if needed
# chestnut_claims <- readRDS("data/chestnut_claims.rds")

# Check code coverage
cat("\n=== Code Coverage Analysis ===\n")
coverage_stats <- chestnut_claims_enriched |>
  summarize(
    total_claims = n(),
    unique_codes = n_distinct(HCPCS_CODE),
    codes_with_description = sum(!is.na(code_description)),
    codes_with_hedis = sum(!is.na(hedis_definition)),
    claims_with_description = sum(!is.na(code_description)),
    pct_codes_covered = 100 *
      n_distinct(HCPCS_CODE[!is.na(code_description)]) /
      unique_codes,
    pct_claims_covered = 100 * claims_with_description / total_claims
  )

cat("Total claims:", format(coverage_stats$total_claims, big.mark = ","), "\n")
cat("Unique HCPCS codes:", coverage_stats$unique_codes, "\n")
cat(
  "Codes with descriptions:",
  coverage_stats$codes_with_description,
  sprintf("(%.1f%%)", coverage_stats$pct_codes_covered),
  "\n"
)
cat(
  "Claims with descriptions:",
  format(coverage_stats$claims_with_description, big.mark = ","),
  sprintf("(%.1f%%)", coverage_stats$pct_claims_covered),
  "\n\n"
)

# Show top codes by frequency
cat("Top 20 most frequent HCPCS codes in Chestnut claims:\n")
top_codes <- chestnut_claims_enriched |>
  count(HCPCS_CODE, code_description, code_type, sort = TRUE) |>
  head(20)
print(top_codes, n = 20)

cat("\n")

# Show breakdown by code type
cat("Claims by code type:\n")
code_type_summary <- chestnut_claims_enriched |>
  count(code_type, sort = TRUE) |>
  mutate(
    percentage = 100 * n / sum(n),
    code_type = ifelse(is.na(code_type), "Unknown/Missing", code_type)
  )
print(code_type_summary, n = Inf)

cat("\n")

# Show codes without descriptions (if any)
missing_codes <- chestnut_claims_enriched |>
  filter(is.na(code_description)) |>
  count(HCPCS_CODE, sort = TRUE)

if (nrow(missing_codes) > 0) {
  cat("Codes without descriptions (top 10):\n")
  print(head(missing_codes, 10))
  cat(
    "\nNote: ",
    nrow(missing_codes),
    " unique codes lack descriptions\n",
    sep = ""
  )
  cat(
    "This represents ",
    sum(missing_codes$n),
    " claims (",
    sprintf(
      "%.1f%%",
      100 * sum(missing_codes$n) / nrow(chestnut_claims_enriched)
    ),
    ")\n",
    sep = ""
  )
} else {
  cat("✓ All codes have descriptions!\n")
}

cat("\n=== Enrichment Complete ===\n")
