# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This project analyzes Medicaid provider spending data, specifically focused on Substance Use Disorder (SUD) treatment claims from Chestnut Health Systems providers in Illinois. It cross-references healthcare procedure codes (HCPCS/CPT) with quality measure definitions (HEDIS) to identify and categorize behavioral health services.

## Data Architecture

### Primary Data Files

- **`data/medicaid-provider-spending.parquet`** (2.9GB): Complete Medicaid provider spending dataset with 227M+ rows. Contains provider NPIs, HCPCS codes, spending amounts, and claim dates.
- **`data/chestnut_claims.rds`**: Pre-filtered dataset of Chestnut Health Systems claims, enriched with HEDIS definitions, comprehensive code descriptions, and provider metadata.
- **`data/sud_claims.rds`**: Pre-filtered dataset of ~4.6M SUD-related claims (HCPCS codes H0001-H2041).
- **`data/npi/npidata_with_taxonomy.parquet`**: Full NPPES NPI database with taxonomy codes (converted from the ~11GB CSV for faster loading).
- **`data/npi/core_202509.parquet`**: Core NPI data (September 2025 snapshot).

### Reference Data

- **`doc/chestnut_npi_full.csv`**: Complete list of 44 Chestnut Health Systems NPIs (41 active + 3 historical), verified against NPPES February 2026. Includes provider names, addresses, taxonomy codes, and NUCC taxonomy descriptions.
- **`doc/organizational_npi_full.parquet`**: All organizational (Entity Type 2) NPIs from NPPES, enriched with taxonomy descriptions. Used for filtering all-organization claims.
- **`doc/nucc_taxonomy_251.csv`**: NUCC taxonomy code reference (v25.1) with grouping, classification, specialization, and display names.
- **`doc/HEDIS MY 2026 Volume 2 Value Set Directory_2025-08-01.xlsx`**: HEDIS quality measure codebook (primary) mapping HCPCS/CPT codes to value sets.
- **`doc/HEDIS MY 2025 Volume 2 Value Set Directory 2025-03-31.xlsx`**: 2025 HEDIS codebook (for historical comparison).
- **`doc/HEDIS MY 2024 Volume 2 Value Set Directory 2024-04-01.xlsx`**: 2024 HEDIS codebook (for historical comparison).
- **`doc/hcpcs/HCPC2026_JAN_ANWEB_01122026.xlsx`**: Official CMS HCPCS Level II codes with long descriptions for January 2026.
- **`doc/hcpcs/comprehensive_code_lookup.csv`**: Consolidated lookup of ~20,800 HCPCS/CPT codes from RVU files (2022-2026), HCPCS Level II, and HEDIS. Achieves 98.7% Medicaid claim coverage.
- **`doc/hcpcs/historical/`**: Downloaded RVU and HCPCS quarterly files from CMS (2017-2026).

## Code Structure

### NPI Extraction: `scripts/extract_npis.R`

Searches the full NPPES dissemination file for Chestnut Health Systems NPIs. Run once when NPI data needs refreshing.

- Reads from `data/npi/npidata_with_taxonomy.parquet` (converted from 11GB CSV)
- Saves all organizational NPIs to `doc/organizational_npi_full.parquet`
- Saves Chestnut NPIs to `doc/chestnut_npi_full.csv`

### Taxonomy Enrichment: `scripts/add_taxonomy_to_npis.R`

Adds NUCC taxonomy descriptions to NPI files using `doc/nucc_taxonomy_251.csv`. Updates `doc/chestnut_npi_full.csv` and `doc/organizational_npi_full.parquet` in place with `TaxonomyCode`, `TaxonomyGrouping`, `TaxonomyClassification`, `TaxonomySpecialization`, `TaxonomyDisplayName` fields.

### HCPCS Code Analysis: `scripts/analyze_hcpcs_codes.R`

Analyzes coverage of HCPCS codes in the Medicaid dataset against HCPCS Jan 2026 and HEDIS codebooks. Saves coverage reports to `doc/hcpcs/`.

### Historical Code Parsing: `scripts/parse_historical_codes.R`

Builds `doc/hcpcs/comprehensive_code_lookup.csv` from multiple sources:
- RVU files (2022-2026) for CPT codes
- HCPCS Level II Jan 2026 file
- HEDIS 2026 codebook

Prioritizes most recent descriptions. Run `scripts/download_hcpcs_historical.sh` first to fetch CMS files.

### Coverage Validation: `scripts/check_coverage_by_volume.R`

Validates the comprehensive code lookup against the full Medicaid parquet by claim volume. Confirmed 98.7% coverage.

### Main Script: `scripts/filter_data.R`

Core data processing workflow:

1. **Load reference data**:
   - Comprehensive HCPCS/CPT code lookup (~20,800 codes)
   - HEDIS codebook filtered to HCPCS/CPT codes, creates `codes_to_value_sets` (pipe-separated)
   - Chestnut NPI lookup from `doc/chestnut_npi_full.csv`

2. **Preprocessing** (run once):
   - Also filters all organizational providers into `all_organization_claims`
   - Filters Chestnut providers from the parquet (billing or servicing NPI)
   - Enriches with provider metadata (name, address, taxonomy) for both billing and servicing providers
   - Joins comprehensive code descriptions and HEDIS definitions/value sets
   - Saves enriched dataset as `data/chestnut_claims.rds`

3. **Analysis section**:
   - Code coverage statistics (total claims, unique codes, % with descriptions)
   - Top 20 most frequent HCPCS codes
   - Claims breakdown by code type (CPT vs HCPCS Level II)

### Visualization Scripts

All visualizations use `plotly` for interactive HTML output saved to `output/graphs/`:

- **`scripts/visualize_chestnut_by_npi.R`**: Charts spending/claims over time by billing NPI.
- **`scripts/visualize_chestnut_by_value_sets.R`**: Charts by HEDIS value set groupings; uses first value set from pipe-separated `code_value_sets` as display label.
- **`scripts/visualize_chestnut_codes_by_taxonomy.R`**: Charts by billing provider taxonomy type.
- **`scripts/visualize_chestnut_metrics.R`**: Additional metrics charts (spending, service counts, etc.).

### Legacy Script: `old.R`

Earlier prototype using data.table. Now superseded by `filter_data.R`.

## Running the Analysis

### Prerequisites

Required R packages:
```r
install.packages(c("arrow", "dplyr", "readr", "readxl", "lubridate", "plotly", "stringr", "purrr", "tidyr", "htmlwidgets"))
```

### Typical Workflow

1. **Build comprehensive code lookup** (run once or when CMS files update):
   ```r
   # Download historical files first
   bash scripts/download_hcpcs_historical.sh
   # Then build lookup
   source("scripts/parse_historical_codes.R")
   ```

2. **Refresh NPI data** (run once or when NPPES data updates):
   ```r
   source("scripts/extract_npis.R")       # Find Chestnut NPIs
   source("scripts/add_taxonomy_to_npis.R") # Add taxonomy descriptions
   ```

3. **Pre-process Chestnut claims** (run once or when data updates):
   ```r
   source("scripts/filter_data.R")
   ```

4. **Analyze / visualize**:
   ```r
   chestnut_claims <- readRDS("data/chestnut_claims.rds")
   # Run visualization scripts as needed
   source("scripts/visualize_chestnut_by_npi.R")
   source("scripts/visualize_chestnut_by_value_sets.R")
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

- **HCPCS (Healthcare Common Procedure Coding System)**: CMS procedure and service codes. Level II codes (alphanumeric like H0001) cover non-physician services including behavioral health. CPT codes (5-digit numeric) cover physician services.
- **SUD codes**: HCPCS codes H0001-H2041 represent substance use disorder treatment services.
- **HEDIS (Healthcare Effectiveness Data and Information Set)**: Quality measure specifications. Defines "value sets" grouping codes by clinical purpose (e.g., "Substance Use Disorder Services").
- **NPI (National Provider Identifier)**: Unique 10-digit provider identifiers. Claims have both billing and servicing provider NPIs. Entity Type 2 = Organization.
- **Taxonomy Codes**: NUCC provider classification codes (e.g., 324500000X = Substance Abuse Rehabilitation Facility). Reference: `doc/nucc_taxonomy_251.csv`.
- **RVU (Relative Value Unit)**: CMS files containing CPT code descriptions, published annually.

## Data Schema Notes

### Medicaid Provider Spending Parquet Fields (relevant subset)

- `BILLING_PROVIDER_NPI_NUM`, `SERVICING_PROVIDER_NPI_NUM`: Provider identifiers
- `HCPCS_CODE`: Procedure code (HCPCS Level II or CPT)
- `CLAIM_FROM_MONTH`: Date in YYYYMM format (converted to year_month using lubridate::ym)
- `TOTAL_CLAIMS`: Claim count field
- Various spending and service count fields

### Enriched Chestnut Claims Fields

Provider fields are prefixed with `sp_` (servicing provider) or `bp_` (billing provider):
- `sp_name`, `bp_name`: Provider organization names
- `sp_address`, `bp_address`: Combined location address strings
- `sp_taxonomy`, `bp_taxonomy`: Raw taxonomy code strings from NPPES
- `sp_taxonomy_code`, `bp_taxonomy_code`: Extracted 10-character NUCC taxonomy codes
- `sp_taxonomy_description`, `bp_taxonomy_description`: Human-readable taxonomy names
- `code_description`: HCPCS/CPT code description from comprehensive lookup
- `code_type`: "CPT" or "HCPCS_Level_II"
- `hedis_definition`: Single HEDIS definition for the HCPCS code (if in HEDIS)
- `code_value_sets`: All HEDIS value sets for this code (pipe-separated, if in HEDIS)
