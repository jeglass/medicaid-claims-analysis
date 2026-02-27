# ============================================================================
# Local path overrides  (copy this file to R/config.local.R and edit)
# ============================================================================
# R/config.local.R is gitignored — your changes will never be committed or
# pushed to GitHub, and will survive git pull / sync.
#
# Only override the paths that differ from the defaults in R/config.R.
# You don't need to copy every variable — just the ones you want to change.
#
# Typical use: point large source files at storage outside the repo.

# MEDICAID_PARQUET <- "/path/to/external/medicaid-provider-spending.parquet"
# NPI_PARQUET      <- "/path/to/external/npi/npidata_with_taxonomy.parquet"

# HEDIS codebooks (large xlsx files, often stored outside the repo):
# HEDIS_2026_XLSX  <- "/path/to/external/HEDIS MY 2026 Volume 2 Value Set Directory_2025-08-01.xlsx"
# HEDIS_2025_XLSX  <- "/path/to/external/HEDIS MY 2025 Volume 2 Value Set Directory 2025-03-31.xlsx"
# HEDIS_2024_XLSX  <- "/path/to/external/HEDIS MY 2024 Volume 2 Value Set Directory 2024-04-01.xlsx"

# Derived files can also be redirected if you prefer to keep them elsewhere:
# CHESTNUT_CLAIMS_PARQUET    <- "/path/to/external/chestnut_claims.parquet"
# IL_MO_WA_OR_CLAIMS_PARQUET <- "/path/to/external/il_mo_wa_or_claims.parquet"
