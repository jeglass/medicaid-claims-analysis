library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(purrr)

cat("Parsing historical HCPCS and CPT codes...\n\n")

# Initialize empty dataframe for all codes
all_codes <- tibble(
  code = character(),
  description = character(),
  source = character(),
  year = character()
)

# ===== 1. Parse RVU Files (CPT codes) =====
cat("=== Parsing RVU files (CPT codes) ===\n")

rvu_years <- c("22", "23", "24", "25", "26")
for (year in rvu_years) {
  rvu_file <- paste0("doc/hcpcs/historical/RVU", year, "A.zip")

  if (file.exists(rvu_file) && file.size(rvu_file) > 50000) {
    cat("Processing RVU20", year, "A...\n", sep = "")

    # Extract the zip file temporarily
    temp_dir <- tempdir()
    unzip(rvu_file, exdir = temp_dir, overwrite = TRUE)

    # Find the PPRRVU Excel file (more complete than CSV)
    xlsx_files <- list.files(temp_dir, pattern = "PPRRVU.*_nonQPP\\.xlsx$", full.names = TRUE)

    if (length(xlsx_files) > 0) {
      # Read the RVU Excel file - skip first 9 rows (header info)
      rvu_data <- read_excel(
        xlsx_files[1],
        skip = 9,
        col_types = "text"
      )

      # Extract code and description columns (first 2 columns)
      rvu_codes <- rvu_data |>
        select(1, 3) |>  # Column 1 is code, column 3 is description (col 2 is modifier)
        set_names(c("code", "description")) |>
        filter(!is.na(code), code != "", !is.na(description), description != "") |>
        mutate(
          source = paste0("RVU20", year, "A"),
          year = paste0("20", year)
        )

      cat("  Found", nrow(rvu_codes), "codes\n")
      all_codes <- bind_rows(all_codes, rvu_codes)

      # Clean up temp files
      file.remove(xlsx_files)
    }
  }
}

cat("\n")

# ===== 2. Parse current HCPCS file (from January 2026) =====
cat("=== Parsing current HCPCS file ===\n")

if (file.exists("doc/hcpcs/HCPC2026_JAN_ANWEB_01122026.xlsx")) {
  cat("Processing HCPC2026_JAN...\n")

  hcpcs_current <- read_xlsx(
    "doc/hcpcs/HCPC2026_JAN_ANWEB_01122026.xlsx",
    range = cell_cols(c("A", "D"))
  ) |>
    rename(
      code = HCPC,
      description = `LONG DESCRIPTION`
    ) |>
    filter(!is.na(code)) |>
    mutate(
      source = "HCPC2026_JAN",
      year = "2026"
    )

  cat("  Found", nrow(hcpcs_current), "codes\n")
  all_codes <- bind_rows(all_codes, hcpcs_current)
}

cat("\n")

# ===== 3. Parse HEDIS codebook =====
cat("=== Parsing HEDIS codebook ===\n")

if (file.exists("doc/HEDIS MY 2026 Volume 2 Value Set Directory_2025-08-01.xlsx")) {
  cat("Processing HEDIS codebook...\n")

  hedis_codes <- read_xlsx(
    "doc/HEDIS MY 2026 Volume 2 Value Set Directory_2025-08-01.xlsx",
    sheet = "Value Sets to Codes"
  ) |>
    filter(`Code System` == "HCPCS") |>
    rename(
      code = Code,
      description = Definition
    ) |>
    select(code, description) |>
    distinct() |>
    mutate(
      source = "HEDIS_2026",
      year = "2026"
    )

  cat("  Found", nrow(hedis_codes), "unique codes\n")
  all_codes <- bind_rows(all_codes, hedis_codes)
}

cat("\n")

# ===== 4. Consolidate codes =====
cat("=== Consolidating codes ===\n")

# For each code, keep the most recent description
# Priority: Most recent year, then prefer HEDIS/HCPCS over RVU for HCPCS Level II codes
consolidated_codes <- all_codes |>
  filter(!is.na(code), code != "") |>
  mutate(
    code_type = case_when(
      str_detect(code, "^[0-9]{5}$") ~ "CPT",
      str_detect(code, "^[A-Z]") ~ "HCPCS_Level_II",
      TRUE ~ "Other"
    ),
    # Priority score: higher is better
    priority = case_when(
      source %in% c("HEDIS_2026", "HCPC2026_JAN") & code_type == "HCPCS_Level_II" ~ 100,
      source == "RVU2026A" ~ 90,
      source == "RVU2025A" ~ 80,
      source == "RVU2024A" ~ 70,
      source == "RVU2023A" ~ 60,
      source == "RVU2022A" ~ 50,
      TRUE ~ 10
    )
  ) |>
  # Remove duplicates, keeping highest priority
  arrange(code, desc(priority)) |>
  distinct(code, .keep_all = TRUE) |>
  select(code, description, code_type, source, year)

cat("Total unique codes:", nrow(consolidated_codes), "\n")
cat("  CPT codes:         ", sum(consolidated_codes$code_type == "CPT"), "\n")
cat("  HCPCS Level II:    ", sum(consolidated_codes$code_type == "HCPCS_Level_II"), "\n")
cat("  Other:             ", sum(consolidated_codes$code_type == "Other"), "\n\n")

# ===== 5. Save the comprehensive lookup =====
cat("Saving comprehensive HCPCS/CPT lookup...\n")
write_csv(consolidated_codes, "doc/hcpcs/comprehensive_code_lookup.csv")
cat("Saved to doc/hcpcs/comprehensive_code_lookup.csv\n\n")

# ===== 6. Check coverage of Medicaid codes =====
cat("=== Checking Medicaid code coverage ===\n")

medicaid_coverage <- read_csv(
  "doc/hcpcs/medicaid_hcpcs_codes_coverage.csv",
  col_types = cols(.default = col_character(),
                   in_hcpcs_2026 = col_logical(),
                   in_hedis = col_logical(),
                   in_either = col_logical()),
  show_col_types = FALSE
)

# Check how many are now covered
medicaid_coverage <- medicaid_coverage |>
  mutate(
    in_comprehensive = HCPCS_CODE %in% consolidated_codes$code
  )

cat("Original coverage: ", sum(medicaid_coverage$in_either), "/", nrow(medicaid_coverage),
    sprintf("(%.1f%%)", 100 * sum(medicaid_coverage$in_either) / nrow(medicaid_coverage)), "\n")
cat("New coverage:      ", sum(medicaid_coverage$in_comprehensive), "/", nrow(medicaid_coverage),
    sprintf("(%.1f%%)", 100 * sum(medicaid_coverage$in_comprehensive) / nrow(medicaid_coverage)), "\n")
cat("Improvement:       +", sum(medicaid_coverage$in_comprehensive) - sum(medicaid_coverage$in_either), " codes\n\n")

# Show what's still missing
still_missing <- medicaid_coverage |>
  filter(!in_comprehensive) |>
  select(HCPCS_CODE)

cat("Still missing", nrow(still_missing), "codes\n")
cat("Sample of remaining missing codes:\n")
print(head(still_missing, 30))

# Save updated coverage analysis
write_csv(medicaid_coverage, "doc/hcpcs/medicaid_hcpcs_codes_coverage_updated.csv")
cat("\nSaved updated coverage to doc/hcpcs/medicaid_hcpcs_codes_coverage_updated.csv\n")

cat("\nDone!\n")
