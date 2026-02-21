# ============================================================================
# Project-wide path constants
# ============================================================================
# Source this file at the top of every script:
#   source("R/config.R")
#
# To override paths for your local machine (e.g. data stored outside the repo),
# create R/config.local.R — it is gitignored and will never be committed.
# See R/config.local.example.R for the template.

MEDICAID_PARQUET <- "data/medicaid-provider-spending.parquet"
CHESTNUT_CLAIMS_RDS <- "data/chestnut_claims.rds"
IL_MO_CLAIMS_RDS <- "data/illinois_missouri_claims.rds"
NPI_PARQUET <- "data/npi/npidata_with_taxonomy.parquet"
CHESTNUT_NPI_CSV <- "doc/chestnut_npi_full.csv"
IL_MO_NPI_CSV <- "doc/illinois_missouri_npi_full.csv"
ORG_NPI_PARQUET <- "doc/organizational_npi_full.parquet"
NUCC_TAXONOMY_CSV <- "doc/nucc_taxonomy_251.csv"
HEDIS_2026_XLSX <- "doc/HEDIS MY 2026 Volume 2 Value Set Directory_2025-08-01.xlsx"
HCPCS_2026_XLSX <- "doc/hcpcs/HCPC2026_JAN_ANWEB_01122026.xlsx"
COMPREHENSIVE_LOOKUP_CSV <- "doc/hcpcs/comprehensive_code_lookup.csv"
OUTPUT_GRAPHS_DIR <- "output/graphs"

# Load local overrides if present (gitignored — safe to store absolute paths)
if (file.exists("R/config.local.R")) {
  source("R/config.local.R")
}
