source("R/config.R")
source("R/viz_helpers.R")

library(dplyr)
library(readr)
library(lubridate)

cat("Loading Chestnut claims data...\n")
chestnut_claims <- readRDS(CHESTNUT_CLAIMS_RDS)

cat("Loaded", format(nrow(chestnut_claims), big.mark = ","), "claims\n")
cat("Date range:", as.Date(min(chestnut_claims$year_month, na.rm = TRUE)),
    "to", as.Date(max(chestnut_claims$year_month, na.rm = TRUE)), "\n\n")

dir.create(OUTPUT_GRAPHS_DIR, recursive = TRUE, showWarnings = FALSE)

# Get unique billing NPIs with their information
billing_npis <- chestnut_claims |>
  filter(!is.na(BILLING_PROVIDER_NPI_NUM)) |>
  group_by(BILLING_PROVIDER_NPI_NUM) |>
  summarize(
    bp_name = first(bp_name[!is.na(bp_name)]),
    bp_address = first(bp_address[!is.na(bp_address)]),
    bp_taxonomy_description = first(bp_taxonomy_description[!is.na(bp_taxonomy_description)]),
    total_claims = sum(TOTAL_CLAIMS, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_claims))

cat("Found", nrow(billing_npis), "unique billing NPIs\n\n")

all_plots <- list()
plot_info <- list()

# Aggregate graph across ALL providers
cat("Creating aggregate graph across all providers...\n")

top_codes_all <- chestnut_claims |>
  filter(!is.na(year_month)) |>
  group_by(HCPCS_CODE, code_description) |>
  summarize(total = sum(TOTAL_CLAIMS, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total)) |>
  head(20)

monthly_data_all <- chestnut_claims |>
  filter(!is.na(year_month), HCPCS_CODE %in% top_codes_all$HCPCS_CODE) |>
  group_by(year_month, HCPCS_CODE, code_description) |>
  summarize(TOTAL_CLAIMS = sum(TOTAL_CLAIMS, na.rm = TRUE), .groups = "drop") |>
  arrange(year_month)

total_claims_all <- sum(chestnut_claims$TOTAL_CLAIMS, na.rm = TRUE)

cat("  Total claims (all providers):", format(total_claims_all, big.mark = ","), "\n")
cat("  Top 20 codes:", paste(head(top_codes_all$HCPCS_CODE, 10), collapse = ", "), "...\n")

fig_all <- plot_ly(
  monthly_data_all,
  x = ~year_month, y = ~TOTAL_CLAIMS, color = ~HCPCS_CODE,
  type = 'scatter', mode = 'lines+markers',
  text = ~paste0("<b>", HCPCS_CODE, "</b><br>", code_description, "<br>",
                 "<b>Claims:</b> ", TOTAL_CLAIMS, "<br>",
                 "<b>Date:</b> ", format(year_month, "%Y-%m")),
  hoverinfo = 'text', line = list(width = 2.5), marker = list(size = 7)
) |>
  layout(
    title = list(
      text = paste0("<b>ALL CHESTNUT PROVIDERS - AGGREGATED</b><br>",
                    "<sub>Top 20 HCPCS Codes Across All Locations</sub><br>",
                    "<sub style='font-size:10px;'>Total Claims: ",
                    format(total_claims_all, big.mark = ","), "</sub>"),
      font = list(size = 14)
    ),
    xaxis = list(title = "<b>Month</b>", tickformat = "%Y-%m", tickangle = -45),
    yaxis = list(title = "<b>Total Claims</b>", rangemode = "tozero"),
    hovermode = 'closest',
    legend = list(title = list(text = "<b>HCPCS Code</b>"), orientation = "v",
                  x = 1.02, y = 1, bgcolor = "rgba(255,255,255,0.8)",
                  bordercolor = "#ccc", borderwidth = 1),
    margin = list(r = 200, b = 100, t = 100)
  )

all_plots[[1]] <- fig_all
plot_info[[1]] <- list(npi = "ALL", name = "All Providers (Aggregated)",
                       address = "All Chestnut Locations", claims = total_claims_all,
                       city = "All", taxonomy_short = "Aggregate")

cat("\n")

# Process each NPI
for (i in seq_len(nrow(billing_npis))) {
  npi              <- billing_npis$BILLING_PROVIDER_NPI_NUM[i]
  provider_name    <- billing_npis$bp_name[i]
  provider_address <- billing_npis$bp_address[i]
  taxonomy         <- billing_npis$bp_taxonomy_description[i]

  cat("Processing NPI:", npi, "\n")
  cat("  Name:", provider_name, "\n")
  cat("  Address:", provider_address, "\n")

  npi_data <- chestnut_claims |>
    filter(BILLING_PROVIDER_NPI_NUM == npi, !is.na(year_month))

  if (nrow(npi_data) == 0) { cat("  No claims found, skipping\n\n"); next }

  top_codes <- npi_data |>
    group_by(HCPCS_CODE, code_description) |>
    summarize(total = sum(TOTAL_CLAIMS, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |>
    head(20)

  total_claims_npi <- sum(npi_data$TOTAL_CLAIMS, na.rm = TRUE)
  cat("  Total claims:", format(total_claims_npi, big.mark = ","), "\n")

  monthly_data <- npi_data |>
    filter(HCPCS_CODE %in% top_codes$HCPCS_CODE) |>
    group_by(year_month, HCPCS_CODE, code_description) |>
    summarize(TOTAL_CLAIMS = sum(TOTAL_CLAIMS, na.rm = TRUE), .groups = "drop") |>
    arrange(year_month)

  fig <- plot_ly(
    monthly_data,
    x = ~year_month, y = ~TOTAL_CLAIMS, color = ~HCPCS_CODE,
    type = 'scatter', mode = 'lines+markers',
    text = ~paste0("<b>", HCPCS_CODE, "</b><br>", code_description, "<br>",
                   "<b>Claims:</b> ", TOTAL_CLAIMS, "<br>",
                   "<b>Date:</b> ", format(year_month, "%Y-%m")),
    hoverinfo = 'text', line = list(width = 2.5), marker = list(size = 7)
  ) |>
    layout(
      title = list(
        text = paste0("<b>", provider_name, "</b><br>",
                      "<sub>", provider_address, "</sub><br>",
                      "<sub style='font-size:10px;'>", taxonomy, " | NPI: ", npi, "</sub>"),
        font = list(size = 13)
      ),
      xaxis = list(title = "<b>Month</b>", tickformat = "%Y-%m", tickangle = -45),
      yaxis = list(title = "<b>Total Claims</b>", rangemode = "tozero"),
      hovermode = 'closest',
      legend = list(title = list(text = "<b>HCPCS Code</b>"), orientation = "v",
                    x = 1.02, y = 1, bgcolor = "rgba(255,255,255,0.8)",
                    bordercolor = "#ccc", borderwidth = 1),
      margin = list(r = 200, b = 100, t = 100)
    )

  all_plots[[i + 1]] <- fig
  plot_info[[i + 1]] <- list(
    npi           = npi,
    name          = provider_name,
    address       = provider_address,
    claims        = total_claims_npi,
    city          = extract_city(provider_address),
    taxonomy_short = shorten_taxonomy(taxonomy)
  )

  cat("\n")
}

# Build and write dashboard HTML
cat("Creating combined HTML file with all", length(all_plots), "graphs...\n")

html <- make_dashboard_html(
  plots    = all_plots,
  plot_info = plot_info,
  title    = "Chestnut Health Systems - HCPCS Trends by Provider",
  subtitle = "Top 20 HCPCS Codes by Provider Over Time"
)

output_file <- file.path(OUTPUT_GRAPHS_DIR, "chestnut_all_providers.html")
writeLines(html, output_file)

cat("\nSuccess! All graphs saved to:", output_file, "\n")
cat("  Total graphs:", length(all_plots), "\n")
cat("  Total claims:", format(sum(sapply(plot_info, function(x) x$claims)), big.mark = ","), "\n")
