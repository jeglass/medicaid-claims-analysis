library(readr)
library(dplyr)
library(stringr)
library(arrow)

source("R/config.R")

# Search the full NPI database for all Chestnut Health Systems NPIs
# Also save all organizational NPIs for future comparison with Chestnut
#
# NOTE: Taxonomy data is in the FULL NPPES dissemination file, not the core file
# The full file is ~11GB but we only read needed columns

# Do this once - Converting the large CSV to Parquet for faster future loading
# Read from the FULL NPPES dissemination file which includes taxonomy
# cat("Loading NPI database from full NPPES file (this may take 15-20 minutes)...\n")
# npi_data <- read_csv(
#  unz("data/npi/NPPES_Data_Dissemination_February_2026.zip",
#      "npidata_pfile_20050523-20260208.csv"),
#  col_types = cols(
#    NPI = col_character(),
#    `Entity Type Code` = col_character(),
#    `Provider Organization Name (Legal Business Name)` = col_character(),
#    `Provider Last Name (Legal Name)` = col_character(),
#    `Provider First Name` = col_character(),
#    `Provider First Line Business Mailing Address` = col_character(),
#    `Provider Second Line Business Mailing Address` = col_character(),
#    `Provider Business Mailing Address City Name` = col_character(),
#    `Provider Business Mailing Address State Name` = col_character(),
#    `Provider Business Mailing Address Postal Code` = col_character(),
#    `Provider First Line Business Practice Location Address` = col_character(),
#    `Provider Second Line Business Practice Location Address` = col_character(),
#    `Provider Business Practice Location Address City Name` = col_character(),
#    `Provider Business Practice Location Address State Name` = col_character(),
#    `Provider Business Practice Location Address Postal Code` = col_character(),
#    `Healthcare Provider Taxonomy Code_1` = col_character(),
#    `Provider License Number_1` = col_character(),
#    `Healthcare Provider Primary Taxonomy Switch_1` = col_character(),
#    .default = col_skip()
#  ),
#  show_col_types = FALSE
# ) |>
#  rename(
#    npi = NPI,
#    entity = `Entity Type Code`,
#    porgname = `Provider Organization Name (Legal Business Name)`,
#    plname = `Provider Last Name (Legal Name)`,
#    pfname = `Provider First Name`,
#    pmailline1 = `Provider First Line Business Mailing Address`,
#    pmailline2 = `Provider Second Line Business Mailing Address`,
#    pmailcityname = `Provider Business Mailing Address City Name`,
#    pmailstatename = `Provider Business Mailing Address State Name`,
#    pmailzip = `Provider Business Mailing Address Postal Code`,
#    plocline1 = `Provider First Line Business Practice Location Address`,
#    plocline2 = `Provider Second Line Business Practice Location Address`,
#    ploccityname = `Provider Business Practice Location Address City Name`,
#    plocstatename = `Provider Business Practice Location Address State Name`,
#    ploczip = `Provider Business Practice Location Address Postal Code`,
#    taxonomy = `Healthcare Provider Taxonomy Code_1`,
#    license = `Provider License Number_1`,
#    primary_tax = `Healthcare Provider Primary Taxonomy Switch_1`
#  ) |>
#  mutate(
#    entity = if_else(entity == "2", "Organization", "Individual")
#  )

# write_parquet(npi_data, "data/npi/npidata_with_taxonomy.parquet")

cat("Loading NPI database from parquet file...\n")
npi_data <- read_parquet(NPI_PARQUET) |>
  select(
    npi,
    entity,
    porgname,
    plname,
    pfname,
    pmailline1,
    pmailline2,
    pmailcityname,
    pmailstatename,
    pmailzip,
    plocline1,
    plocline2,
    ploccityname,
    plocstatename,
    ploczip,
    taxonomy,
    license,
    primary_tax
  )

cat("Saving a dataset of all organizations...\n")
organizational_npis <- npi_data |>
  filter(entity == "Organization") |> # Organization type (2 = org, 1 = individual)
  select(
    NPI = npi,
    Name = porgname,
    Taxonomy = taxonomy,
    LicenseNumber = license,
    PrimaryTaxonomy = primary_tax,
    MailAddress1 = pmailline1,
    MailAddress2 = pmailline2,
    MailCity = pmailcityname,
    MailState = pmailstatename,
    MailZip = pmailzip,
    LocationAddress1 = plocline1,
    LocationAddress2 = plocline2,
    LocationCity = ploccityname,
    LocationState = plocstatename,
    LocationZip = ploczip
  ) |>
  arrange(NPI)

cat(
  "\nFound",
  nrow(organizational_npis),
  "Organizational NPIs in the database\n"
)

organizational_npis |>
  distinct(Name) |>
  nrow() |>
  (\(n) cat("Found", n, "unique organization names in the NPI data\n"))()

# Save the results
write_parquet(organizational_npis, "doc/organizational_npi_full.parquet")
cat(
  "\nSaved full list of organizational NPIs to doc/organizational_npi_full.parquet\n"
)

cat("Searching for Chestnut Health Systems organizations...\n")

# Search for all variations of Chestnut Health Systems
# Using case-insensitive pattern matching for organization names
chestnut_npis <- npi_data |>
  filter(
    entity == "Organization" & # Organization type (2 = org, 1 = individual)
      str_detect(
        porgname,
        regex("chestnut.*health.*systems?", ignore_case = TRUE)
      )
  ) |>
  select(
    NPI = npi,
    Name = porgname,
    Taxonomy = taxonomy,
    LicenseNumber = license,
    PrimaryTaxonomy = primary_tax,
    MailAddress1 = pmailline1,
    MailAddress2 = pmailline2,
    MailCity = pmailcityname,
    MailState = pmailstatename,
    MailZip = pmailzip,
    LocationAddress1 = plocline1,
    LocationAddress2 = plocline2,
    LocationCity = ploccityname,
    LocationState = plocstatename,
    LocationZip = ploczip
  ) |>
  arrange(NPI)

cat(
  "\nFound",
  nrow(chestnut_npis),
  "Chestnut Health Systems NPIs in the database\n"
)
cat("Unique names found:\n")
print(unique(chestnut_npis$Name))

# Save the results
write_csv(chestnut_npis, "doc/chestnut_npi_full.csv")
cat("\nSaved full list to doc/chestnut_npi_full.csv\n")

cat("Identifying all Illinois and Missouri organizations...\n")
# Search for Illinois (IL) and Missouri (MO) organizations using the plocstatename field of the NPI database
illinois_missouri_npis <- npi_data |>
  filter(
    entity == "Organization" & # Organization type (2 = org, 1 = individual)
      plocstatename %in% c("IL", "MO")
  ) |>
  select(
    NPI = npi,
    Name = porgname,
    Taxonomy = taxonomy,
    LicenseNumber = license,
    PrimaryTaxonomy = primary_tax,
    MailAddress1 = pmailline1,
    MailAddress2 = pmailline2,
    MailCity = pmailcityname,
    MailState = pmailstatename,
    MailZip = pmailzip,
    LocationAddress1 = plocline1,
    LocationAddress2 = plocline2,
    LocationCity = ploccityname,
    LocationState = plocstatename,
    LocationZip = ploczip
  ) |>
  arrange(NPI)

cat(
  "\nFound",
  nrow(illinois_missouri_npis),
  "Illinois or Missouri NPIs in the database\n"
)
cat("First 20 Unique Illinois or Missouri names found:\n")
print(unique(illinois_missouri_npis$Name[1:20]))

# Save the results
write_csv(illinois_missouri_npis, "doc/illinois_missouri_npi_full.csv")
cat("\nSaved full list to doc/illinois_missouri_npi_full.csv\n")

cat("\nSummary:\n")
cat("  Chestnut NPIs found:", nrow(chestnut_npis), "\n")
cat("  Unique Chestnut names:", length(unique(chestnut_npis$Name)), "\n")
