library(arrow)
library(dplyr)
library(readr)
library(readxl)

cat("Analyzing HCPCS codes in Medicaid data...\n\n")

# Load the medicaid data
medicaid_parquet <- open_dataset("data/medicaid-provider-spending.parquet")

# Get unique HCPCS codes from the Medicaid data
cat("Extracting unique HCPCS codes from Medicaid data (this may take a moment)...\n")
medicaid_codes <- medicaid_parquet |>
  select(HCPCS_CODE) |>
  distinct() |>
  collect() |>
  filter(!is.na(HCPCS_CODE)) |>
  arrange(HCPCS_CODE)

cat("Found", nrow(medicaid_codes), "unique HCPCS codes in Medicaid data\n\n")

# Load current HCPCS reference (January 2026)
hcpcs_2026 <- read_xlsx(
  "doc/hcpcs/HCPC2026_JAN_ANWEB_01122026.xlsx",
  range = cell_cols(c("A", "D"))
) |>
  rename(
    hcpc = HCPC,
    hcpc_long = `LONG DESCRIPTION`
  )

cat("Loaded", nrow(hcpcs_2026), "codes from January 2026 HCPCS file\n\n")

# Load HEDIS codebook
hedis_codebook <- read_xlsx(
  "doc/HEDIS MY 2026 Volume 2 Value Set Directory_2025-08-01.xlsx",
  sheet = "Value Sets to Codes"
) |>
  filter(`Code System` == "HCPCS") |>
  rename(
    code = Code,
    hedis_definition = Definition
  ) |>
  distinct(code, hedis_definition)

cat("Loaded", nrow(hedis_codebook), "HCPCS codes from HEDIS codebook\n\n")

# Identify coverage
medicaid_codes <- medicaid_codes |>
  mutate(
    in_hcpcs_2026 = HCPCS_CODE %in% hcpcs_2026$hcpc,
    in_hedis = HCPCS_CODE %in% hedis_codebook$code,
    in_either = in_hcpcs_2026 | in_hedis
  )

# Summary statistics
cat("Coverage Analysis:\n")
cat("  Codes in HCPCS Jan 2026:  ", sum(medicaid_codes$in_hcpcs_2026), "/", nrow(medicaid_codes),
    sprintf("(%.1f%%)", 100 * sum(medicaid_codes$in_hcpcs_2026) / nrow(medicaid_codes)), "\n")
cat("  Codes in HEDIS:           ", sum(medicaid_codes$in_hedis), "/", nrow(medicaid_codes),
    sprintf("(%.1f%%)", 100 * sum(medicaid_codes$in_hedis) / nrow(medicaid_codes)), "\n")
cat("  Codes in either source:   ", sum(medicaid_codes$in_either), "/", nrow(medicaid_codes),
    sprintf("(%.1f%%)", 100 * sum(medicaid_codes$in_either) / nrow(medicaid_codes)), "\n")
cat("  Missing codes:            ", sum(!medicaid_codes$in_either), "/", nrow(medicaid_codes),
    sprintf("(%.1f%%)", 100 * sum(!medicaid_codes$in_either) / nrow(medicaid_codes)), "\n\n")

# Identify missing codes
missing_codes <- medicaid_codes |>
  filter(!in_either) |>
  select(HCPCS_CODE)

cat("Missing HCPCS codes (sample of first 50):\n")
print(head(missing_codes, 50))

# Analyze missing code patterns
missing_codes <- missing_codes |>
  mutate(
    prefix = substr(HCPCS_CODE, 1, 1),
    code_type = case_when(
      grepl("^[0-9]", HCPCS_CODE) ~ "CPT (numeric)",
      grepl("^[A-Z]", HCPCS_CODE) ~ "HCPCS Level II (alpha)",
      TRUE ~ "Other"
    )
  )

cat("\nMissing codes by type:\n")
missing_codes |>
  count(code_type, prefix, sort = TRUE) |>
  print(n = 30)

# Save missing codes for reference
write_csv(missing_codes, "doc/hcpcs/missing_hcpcs_codes.csv")
cat("\nSaved full list of missing codes to doc/hcpcs/missing_hcpcs_codes.csv\n")

# Save all medicaid codes with coverage status
write_csv(medicaid_codes, "doc/hcpcs/medicaid_hcpcs_codes_coverage.csv")
cat("Saved coverage analysis to doc/hcpcs/medicaid_hcpcs_codes_coverage.csv\n")
