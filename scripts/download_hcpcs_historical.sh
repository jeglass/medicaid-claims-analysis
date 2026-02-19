#!/bin/bash

# Script to download historical HCPCS and RVU files for comprehensive code mapping
# This covers CPT codes (numeric) and HCPCS Level II codes (alphanumeric) from 2017-2026

set -e
cd /Volumes/External/BiggerData/medicaid/doc/hcpcs

echo "Creating historical codes directory..."
mkdir -p historical

echo "==========================================="
echo "Downloading RVU (Relative Value Unit) files"
echo "These contain CPT code descriptions"
echo "==========================================="

# RVU files contain CPT codes with short descriptions
# Format: ZIP files containing Excel spreadsheets

declare -a years=("17" "18" "19" "20" "21" "22" "23" "24" "25" "26")

for year in "${years[@]}"; do
    echo "Downloading RVU20${year}A..."
    curl -L -o "historical/RVU${year}A.zip" \
        "https://www.cms.gov/files/zip/rvu${year}a.zip" || echo "RVU${year}A not available"
done

echo ""
echo "==========================================="
echo "Downloading Historical HCPCS Quarterly Files"
echo "These contain HCPCS Level II codes (alphanumeric)"
echo "==========================================="

# HCPCS Quarterly files from CMS
# Available: 2021-2026 (confirmed from website)

declare -a quarters=("JAN" "APR" "JUL" "OCT")
declare -a hcpcs_years=("2021" "2022" "2023" "2024" "2025" "2026")

for year in "${hcpcs_years[@]}"; do
    for quarter in "${quarters[@]}"; do
        # Skip future quarters
        if [ "$year" = "2026" ] && [ "$quarter" != "JAN" ]; then
            continue
        fi

        echo "Downloading HCPCS ${year} ${quarter}..."

        # Try to download the ANWEB file (Excel format)
        curl -L -o "historical/HCPC${year}_${quarter}_ANWEB.zip" \
            "https://www.cms.gov/files/zip/hcpc${year:2:2}${quarter:0:1}anweb.zip" || \
        curl -L -o "historical/HCPC${year}_${quarter}_ANWEB.zip" \
            "https://www.cms.gov/medicare/coding-billing/healthcare-common-procedure-system/hcpcs-quarterly-update/alpha-numeric-hcpcs/hcpc${year:2:2}${quarter:0:1}anweb.zip" || \
            echo "  ${year} ${quarter} not available"
    done
done

echo ""
echo "==========================================="
echo "Download Summary"
echo "==========================================="
echo "Files downloaded to: doc/hcpcs/historical/"
ls -lh historical/ | wc -l
echo "files total"

echo ""
echo "Next steps:"
echo "1. Run parse_historical_codes.R to extract all codes"
echo "2. Merge with current codes to create comprehensive lookup"
