# ============================================================================
# Enrich BHRN NPI Excel file with Medicaid coverage and provider metadata
# ============================================================================
#
# Purpose: Updates doc/bhrn_npi.xlsx in place with two enrichments:
#   1. "Found in Medicaid" — whether each NPI appears as a billing or
#      servicing provider in the Medicaid claims data
#   2. Provider metadata from NPPES (sp_name, sp_address, sp_city,
#      sp_state, sp_zip, sp_taxonomy_description)
#
# Output: doc/bhrn_npi.xlsx (overwritten in place)
# ============================================================================

source("R/config.R")

library(arrow)
library(dplyr)
library(readxl)
library(writexl)

bhrn <- read_xlsx(BHRN_NPI_XLSX, col_types = "text")

# ---- Medicaid coverage flag ------------------------------------------------

cat("Checking BHRN NPI coverage in Medicaid data...\n")
billing_npis   <- open_dataset(MEDICAID_PARQUET) |>
  distinct(BILLING_PROVIDER_NPI_NUM) |>
  collect() |>
  pull()
servicing_npis <- open_dataset(MEDICAID_PARQUET) |>
  distinct(SERVICING_PROVIDER_NPI_NUM) |>
  collect() |>
  pull()
medicaid_npis <- unique(c(billing_npis, servicing_npis))

bhrn <- bhrn |>
  mutate(`Found in Medicaid` = ifelse(`NPI Number` %in% medicaid_npis, "Yes", "No"))

cat("Found:", sum(bhrn[["Found in Medicaid"]] == "Yes"), "\n")
cat("Not found:", sum(bhrn[["Found in Medicaid"]] == "No"), "\n\n")

# ---- Provider metadata from NPPES -----------------------------------------

cat("Enriching with NPPES provider metadata...\n")
org_npi <- open_dataset(ORG_NPI_PARQUET) |>
  filter(NPI %in% bhrn[["NPI Number"]]) |>
  collect() |>
  mutate(
    sp_address = trimws(gsub("\\s+", " ",
      paste(LocationAddress1, LocationAddress2, LocationCity, LocationState, LocationZip, sep = " ")))
  ) |>
  select(
    NPI,
    sp_name                 = Name,
    sp_address,
    sp_city                 = LocationCity,
    sp_state                = LocationState,
    sp_zip                  = LocationZip,
    sp_taxonomy_description = TaxonomyDisplayName
  )

bhrn_out <- bhrn |>
  left_join(org_npi, by = c("NPI Number" = "NPI"))

cat("NPIs matched to NPPES:", sum(!is.na(bhrn_out$sp_name)), "of", nrow(bhrn_out), "\n\n")

# ---- Save ------------------------------------------------------------------

write_xlsx(bhrn_out, BHRN_NPI_XLSX)
cat("Saved", nrow(bhrn_out), "rows with", ncol(bhrn_out), "columns to", BHRN_NPI_XLSX, "\n")
