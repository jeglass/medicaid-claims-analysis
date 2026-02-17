# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This project analyzes Medicaid provider spending data, specifically focused on Substance Use Disorder (SUD) treatment claims from Chestnut Health Systems providers in Illinois. It cross-references healthcare procedure codes (HCPCS) with quality measure definitions (HEDIS) to identify and categorize behavioral health services.

## Data Architecture

### Primary Data Files

- **`data/medicaid-provider-spending.parquet`** (2.9GB): Complete Medicaid provider spending dataset with 227M+ rows. Contains provider NPIs, HCPCS codes, spending amounts, and claim dates.
- **`data/chestnut_claims.rds`**: Pre-filtered dataset of ~5,081 claims from Chestnut Health Systems providers, enriched with HEDIS definitions and provider metadata.
- **`data/sud_claims.rds`**: Pre-filtered dataset of ~4.6M SUD-related claims (HCPCS codes H0001-H2041).

### Reference Data

- **`doc/chestnut_npi.csv`**: List of Chestnut Health Systems NPIs with provider names, addresses, taxonomy codes, and PECOS status.
- **`doc/HEDIS MY 2026 Volume 2 Value Set Directory_2025-08-01.xlsx`**: HEDIS quality measure codebook mapping HCPCS codes to value sets (e.g., "Substance Use Disorder Services", "Behavioral Health Assessment").
- **`doc/hcpcs/HCPC2026_JAN_ANWEB_01122026.xlsx`**: Official CMS HCPCS Level II codes with long descriptions for January 2026.
- **`data/npi/`**: Contains National Provider Identifier (NPI) core files and taxonomy codes from NPPES.

## Code Structure

### Main Script: `scripts/filter_data.R`

This script performs the core data processing workflow:

1. **Load reference data** (lines 7-56):
   - HCPCS Level II codes with descriptions
   - HEDIS codebook filtered to HCPCS codes only
   - Chestnut provider NPI lookup table
   - Creates `codes_to_value_sets` mapping each HCPCS code to all its HEDIS value sets (pipe-separated)

2. **Preprocessing section** (lines 82-131):
   - Filters the large parquet file to only Chestnut providers (by billing or servicing NPI)
   - Enriches claims with provider metadata (name, address, type, taxonomy) for both billing and servicing providers
   - Joins HEDIS definitions and value sets
   - Joins HCPCS long descriptions
   - Saves enriched dataset as `data/chestnut_claims.rds`

3. **Analysis section** (lines 165-189):
   - Loads pre-processed chestnut_claims
   - Identifies HCPCS codes present in Chestnut data but missing from HEDIS codebook
   - Compares definition coverage between HEDIS and HCPCS Level II

### Legacy Script: `old.R`

Earlier prototype using data.table. Now superseded by `filter_data.R` which uses arrow for more efficient parquet handling.

## Running the Analysis

### Prerequisites

Required R packages:
```r
install.packages(c("arrow", "dplyr", "readr", "readxl"))
```

### Typical Workflow

1. **Pre-process Chestnut claims** (run once or when data updates):
   ```r
   # In scripts/filter_data.R, run lines 82-131
   source("scripts/filter_data.R")  # Runs entire script including preprocessing
   ```

2. **Analyze processed data** (run lines 165-189 or modify for custom analysis):
   ```r
   chestnut_claims <- readRDS("data/chestnut_claims.rds")
   # Your analysis here
   ```

### Working with the Large Parquet File

The main dataset is too large to load entirely into memory. Use arrow to filter efficiently:

```r
library(arrow)
medicaid_parquet <- open_dataset("data/medicaid-provider-spending.parquet")

# Filter then collect - arrow pushes filter down to parquet file
filtered_data <- medicaid_parquet |>
  filter(HCPCS_CODE %in% target_codes) |>
  collect()
```

## Key Healthcare Coding Concepts

- **HCPCS (Healthcare Common Procedure Coding System)**: CMS procedure and service codes. Level II codes (alphanumeric like H0001) cover non-physician services including behavioral health.
- **SUD codes**: HCPCS codes H0001-H2041 represent substance use disorder treatment services.
- **HEDIS (Healthcare Effectiveness Data and Information Set)**: Quality measure specifications. Defines "value sets" grouping codes by clinical purpose (e.g., "Substance Use Disorder Services").
- **NPI (National Provider Identifier)**: Unique 10-digit provider identifiers. Claims have both billing and servicing provider NPIs.
- **Taxonomy Codes**: NUCC provider classification codes (e.g., 324500000X = Substance Abuse Rehabilitation Facility).

## Data Schema Notes

### Medicaid Provider Spending Parquet Fields (relevant subset)

- `BILLING_PROVIDER_NPI_NUM`, `SERVICING_PROVIDER_NPI_NUM`: Provider identifiers
- `HCPCS_CODE`: Procedure code
- `CLAIM_FROM_MONTH`: Date in YYYYMM format (converted to year_month using lubridate::ym)
- Various spending and service count fields

### Enriched Chestnut Claims Fields

Provider fields are prefixed with `sp_` (servicing provider) or `bp_` (billing provider):
- `sp_name`, `bp_name`: Provider organization names
- `sp_taxonomy`, `bp_taxonomy`: Provider type codes
- `hedis_definition`: Single HEDIS definition for the HCPCS code
- `code_value_sets`: All HEDIS value sets for this code (pipe-separated)
- `hcpc_long`: Official HCPCS long description

## Important Verifications in Code

- **Line 43-51**: Verified no HCPCS codes have multiple definitions in HEDIS codebook (safe to join without duplication)
- **Line 122**: Uses distinct HEDIS definitions when joining to avoid row multiplication
