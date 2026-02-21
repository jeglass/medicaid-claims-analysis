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

# Derived files can also be redirected if you prefer to keep them elsewhere:
# CHESTNUT_CLAIMS_RDS <- "/path/to/external/chestnut_claims.rds"
# IL_MO_CLAIMS_RDS    <- "/path/to/external/illinois_missouri_claims.rds"
