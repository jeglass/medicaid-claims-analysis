library(readr)
library(dplyr)
library(stringr)
library(arrow)

cat("Adding taxonomy descriptions to NPI files...\n\n")

# Read the taxonomy code reference file
taxonomy_ref <- read_csv(
  "doc/nucc_taxonomy_251.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

cat("Loaded", nrow(taxonomy_ref), "taxonomy codes\n\n")

# Create a clean taxonomy lookup table
taxonomy_lookup <- taxonomy_ref |>
  transmute(
    TaxonomyCode = Code,
    TaxonomyGrouping = Grouping,
    TaxonomyClassification = Classification,
    TaxonomySpecialization = Specialization,
    TaxonomyDisplayName = `Display Name`,
    TaxonomySection = Section
  )

# Function to add taxonomy descriptions to an NPI file
add_taxonomy <- function(npi_data, file_description) {
  cat("Processing", file_description, "...\n")
  cat("  Loaded", nrow(npi_data), "NPIs\n")

  # Check if Taxonomy column exists
  if (!"Taxonomy" %in% colnames(npi_data)) {
    cat("  ERROR: No 'Taxonomy' column found!\n")
    cat("  Please run extract_npis.R with taxonomy fields included.\n")
    cat("  See TAXONOMY_WORKFLOW.md for instructions.\n")
    return(NULL)
  }

  # Extract just the taxonomy code (remove any license numbers or extra text)
  # The Taxonomy field format is like "261QF0400X" or "261QF0400X  12345"
  npi_data <- npi_data |>
    mutate(
      TaxonomyCode = str_extract(Taxonomy, "^[A-Z0-9]{10}"), # Extract 10-char code
      Taxonomy_Original = Taxonomy # Keep original for reference
    ) |>
    select(-Taxonomy) # Remove original Taxonomy to avoid duplicate names

  # Check for any NPIs with missing taxonomy codes
  missing_codes <- npi_data |>
    filter(is.na(TaxonomyCode)) |>
    select(NPI, Name, Taxonomy_Original)

  if (nrow(missing_codes) > 0) {
    cat(
      "  Warning:",
      nrow(missing_codes),
      "NPIs have missing or invalid taxonomy codes\n"
    )
  }

  # Join with taxonomy descriptions
  npi_enriched <- npi_data |>
    left_join(
      taxonomy_lookup,
      by = "TaxonomyCode",
      relationship = "many-to-one"
    ) |>
    select(
      NPI,
      Name,
      Taxonomy = Taxonomy_Original,
      TaxonomyCode,
      TaxonomyGrouping,
      TaxonomyClassification,
      TaxonomySpecialization,
      TaxonomyDisplayName,
      everything() # Include any additional columns (no duplicate Taxonomy now)
    )

  # Check for any taxonomy codes that weren't found in the reference file
  unmatched <- npi_enriched |>
    filter(!is.na(TaxonomyCode) & is.na(TaxonomyDisplayName)) |>
    select(NPI, Name, TaxonomyCode)

  if (nrow(unmatched) > 0) {
    cat(
      "  Warning:",
      nrow(unmatched),
      "taxonomy codes not found in reference file\n"
    )
  }

  # Show unique taxonomy types
  cat(
    "  Unique taxonomy types:",
    n_distinct(npi_enriched$TaxonomyDisplayName, na.rm = TRUE),
    "\n"
  )

  return(npi_enriched)
}

# Process Chestnut NPIs
if (file.exists("doc/chestnut_npi_full.csv")) {
  chestnut_npis <- read_csv(
    "doc/chestnut_npi_full.csv",
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )

  chestnut_enriched <- add_taxonomy(
    chestnut_npis,
    "Chestnut Health Systems NPIs"
  )

  if (!is.null(chestnut_enriched)) {
    # Show taxonomy types used by Chestnut
    cat("  Taxonomy types used by Chestnut Health Systems:\n")
    chestnut_enriched |>
      filter(!is.na(TaxonomyDisplayName)) |>
      count(TaxonomyCode, TaxonomyDisplayName, sort = TRUE) |>
      print(n = Inf)

    # Save the enriched file
    write_csv(chestnut_enriched, "doc/chestnut_npi_full.csv")
    cat("  ✓ Saved enriched data to doc/chestnut_npi_full.csv\n\n")
  }
} else {
  cat("Chestnut NPI file not found, skipping...\n\n")
}

# Process organizational NPIs
if (file.exists("doc/organizational_npi_full.parquet")) {
  org_npis <- read_parquet("doc/organizational_npi_full.parquet")

  org_enriched <- add_taxonomy(org_npis, "All organizational NPIs")

  if (!is.null(org_enriched)) {
    # Show top 20 taxonomy types across all organizations
    cat("  Top 20 taxonomy types across all organizations:\n")
    org_enriched |>
      filter(!is.na(TaxonomyDisplayName)) |>
      count(TaxonomyDisplayName, sort = TRUE) |>
      head(20) |>
      print()

    # Save the enriched file
    write_parquet(org_enriched, "doc/organizational_npi_full.parquet")
    cat("  ✓ Saved enriched data to doc/organizational_npi_full.parquet\n\n")
  }
} else {
  cat("Organizational NPI file not found, skipping...\n\n")
}

# Process Illinois and Missouri organizational NPIs
if (file.exists("doc/illinois_missouri_npi_full.csv")) {
  illinois_missouri_npis <- read_csv(
    "doc/illinois_missouri_npi_full.csv",
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )

  illinois_missouri_enriched <- add_taxonomy(
    illinois_missouri_npis,
    "All organizational NPIs"
  )

  if (!is.null(illinois_missouri_enriched)) {
    # Show top 20 taxonomy types across all Illinois and Missouri organizations
    cat(
      "  Top 20 taxonomy types across all Illinois and Missouri  organizations:\n"
    )
    illinois_missouri_enriched |>
      filter(!is.na(TaxonomyDisplayName)) |>
      count(TaxonomyDisplayName, sort = TRUE) |>
      head(20) |>
      print()

    # Save the enriched file
    write_csv(illinois_missouri_enriched, "doc/illinois_missouri_npi_full.csv")
    cat(
      "  ✓ Saved enriched Illinois and Missouri data to doc/illinois_missouri_npi_full.csv\n\n"
    )
  }
} else {
  cat(
    "Illinois and Missouri Organizational NPI file not found, skipping...\n\n"
  )
}

# Process Washington and Oregon organizational NPIs
if (file.exists("doc/washington_oregon_npi_full.csv")) {
  washington_oregon_npis <- read_csv(
    "doc/washington_oregon_npi_full.csv",
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )

  washington_oregon_enriched <- add_taxonomy(
    washington_oregon_npis,
    "Washington and Oregon organizational NPIs"
  )

  if (!is.null(washington_oregon_enriched)) {
    cat(
      "  Top 20 taxonomy types across all Washington and Oregon organizations:\n"
    )
    washington_oregon_enriched |>
      filter(!is.na(TaxonomyDisplayName)) |>
      count(TaxonomyDisplayName, sort = TRUE) |>
      head(20) |>
      print()

    write_csv(washington_oregon_enriched, "doc/washington_oregon_npi_full.csv")
    cat(
      "  ✓ Saved enriched Washington and Oregon data to doc/washington_oregon_npi_full.csv\n\n"
    )
  }
} else {
  cat("Washington and Oregon NPI file not found, skipping...\n\n")
}

cat("\n✓ Done!\n")
