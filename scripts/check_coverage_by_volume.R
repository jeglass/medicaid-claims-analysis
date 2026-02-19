library(arrow)
library(dplyr)
library(readr)

cat("Checking HCPCS code coverage by claim volume...\n\n")

# Load comprehensive lookup
code_lookup <- read_csv(
  "doc/hcpcs/comprehensive_code_lookup.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

cat("Loaded", nrow(code_lookup), "codes from comprehensive lookup\n\n")

# Load Medicaid data and check coverage
cat("Analyzing Medicaid dataset...\n")
medicaid_parquet <- open_dataset("data/medicaid-provider-spending.parquet")

# Count claims by whether they have a description
coverage_by_volume <- medicaid_parquet |>
  mutate(has_description = HCPCS_CODE %in% code_lookup$code) |>
  group_by(has_description) |>
  summarize(n_claims = n()) |>
  collect()

cat("\nCoverage by Claim Volume:\n")
print(coverage_by_volume)

total_claims <- sum(coverage_by_volume$n_claims)
claims_with_desc <- coverage_by_volume |>
  filter(has_description == TRUE) |>
  pull(n_claims)
claims_with_desc <- ifelse(length(claims_with_desc) == 0, 0, claims_with_desc)

pct_covered <- 100 * claims_with_desc / total_claims

cat("\n")
cat("Total claims:              ", format(total_claims, big.mark = ","), "\n")
cat("Claims with descriptions:  ", format(claims_with_desc, big.mark = ","), "\n")
cat("Percentage covered:        ", sprintf("%.1f%%", pct_covered), "\n\n")

# Get top 20 missing codes by volume
cat("Top 20 most frequent codes WITHOUT descriptions:\n")
top_missing <- medicaid_parquet |>
  filter(!(HCPCS_CODE %in% code_lookup$code)) |>
  count(HCPCS_CODE, sort = TRUE) |>
  collect() |>
  head(20)

print(top_missing)

cat("\n")

# Get top 20 covered codes by volume
cat("Top 20 most frequent codes WITH descriptions:\n")
top_covered <- medicaid_parquet |>
  filter(HCPCS_CODE %in% code_lookup$code) |>
  count(HCPCS_CODE, sort = TRUE) |>
  collect() |>
  head(20) |>
  left_join(code_lookup |> select(code, description), by = c("HCPCS_CODE" = "code"))

print(top_covered)

cat("\nConclusion:\n")
if (pct_covered >= 80) {
  cat("EXCELLENT: ", sprintf("%.1f%%", pct_covered), " of claims have descriptions.\n", sep = "")
  cat("The missing codes are likely rare or invalid codes with minimal impact.\n")
} else if (pct_covered >= 60) {
  cat("GOOD: ", sprintf("%.1f%%", pct_covered), " of claims have descriptions.\n", sep = "")
  cat("Consider reviewing the top missing codes to determine if they need descriptions.\n")
} else {
  cat("NEEDS IMPROVEMENT: Only ", sprintf("%.1f%%", pct_covered), " of claims have descriptions.\n", sep = "")
  cat("Review the top missing codes above - they may be common codes needing descriptions.\n")
}
