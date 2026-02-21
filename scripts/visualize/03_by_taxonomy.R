source("R/config.R")
source("R/viz_helpers.R")

library(dplyr)
library(readr)
library(htmlwidgets)

cat("Loading Chestnut claims data...\n")
chestnut_claims <- readRDS(CHESTNUT_CLAIMS_RDS)

cat("Loaded", nrow(chestnut_claims), "claims\n")
cat("Date range:", min(chestnut_claims$year_month, na.rm = TRUE), "to",
    max(chestnut_claims$year_month, na.rm = TRUE), "\n\n")

dir.create(OUTPUT_GRAPHS_DIR, recursive = TRUE, showWarnings = FALSE)

taxonomies <- chestnut_claims |>
  filter(!is.na(bp_taxonomy_description)) |>
  distinct(bp_taxonomy_description) |>
  pull(bp_taxonomy_description)

cat("Found", length(taxonomies), "unique billing provider taxonomy types\n\n")

for (taxonomy in taxonomies) {
  cat("Processing taxonomy:", taxonomy, "\n")

  taxonomy_data <- chestnut_claims |>
    filter(bp_taxonomy_description == taxonomy, !is.na(year_month))

  if (nrow(taxonomy_data) == 0) { cat("  No claims found, skipping\n\n"); next }

  # BUG FIX: was n() (count rows), now correctly sums TOTAL_CLAIMS
  top_codes <- taxonomy_data |>
    group_by(HCPCS_CODE, code_description) |>
    summarize(total = sum(TOTAL_CLAIMS, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |>
    head(10)

  cat("  Top 10 codes:", paste(top_codes$HCPCS_CODE, collapse = ", "), "\n")

  time_series_data <- taxonomy_data |>
    filter(HCPCS_CODE %in% top_codes$HCPCS_CODE) |>
    group_by(year_month, BILLING_PROVIDER_NPI_NUM, bp_name, bp_address,
             HCPCS_CODE, code_description) |>
    summarize(TOTAL_CLAIMS = sum(TOTAL_CLAIMS, na.rm = TRUE), .groups = "drop") |>
    arrange(year_month)

  providers <- time_series_data |>
    distinct(BILLING_PROVIDER_NPI_NUM, bp_address) |>
    arrange(bp_address)

  cat("  Creating graphs for", nrow(providers), "providers\n")

  plots <- list()

  for (i in seq_len(nrow(providers))) {
    provider_npi     <- providers$BILLING_PROVIDER_NPI_NUM[i]
    provider_address <- providers$bp_address[i]

    provider_data <- time_series_data |>
      filter(BILLING_PROVIDER_NPI_NUM == provider_npi)

    if (nrow(provider_data) == 0) next

    fig <- plot_ly(provider_data,
      x = ~year_month, y = ~TOTAL_CLAIMS, color = ~HCPCS_CODE,
      type = 'scatter', mode = 'lines+markers',
      text = ~paste0("Code: ", HCPCS_CODE, "<br>",
                     "Description: ", code_description, "<br>",
                     "Claims: ", TOTAL_CLAIMS, "<br>",
                     "Date: ", format(year_month, "%Y-%m")),
      hoverinfo = 'text', line = list(width = 2), marker = list(size = 6)
    ) |>
      layout(
        title = list(
          text = paste0("<b>", provider_address, "</b><br>",
                        "<sub>Top 10 HCPCS Codes - ", taxonomy, "</sub>"),
          font = list(size = 14)
        ),
        xaxis = list(title = "Date", tickformat = "%Y-%m"),
        yaxis = list(title = "Total Claims"),
        hovermode = 'closest',
        legend = list(title = list(text = "HCPCS Code"), orientation = "v",
                      x = 1.05, y = 1),
        margin = list(r = 200)
      )

    plots[[i]] <- fig
  }

  safe_name  <- substr(gsub("[^A-Za-z0-9_]", "_", taxonomy), 1, 50)
  output_file <- file.path(OUTPUT_GRAPHS_DIR, paste0(safe_name, ".html"))

  html_content <- paste0(
    '<!DOCTYPE html>\n<html>\n<head>\n',
    '    <title>', taxonomy, ' - Chestnut Health Systems HCPCS Trends</title>\n',
    '    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>\n',
    '    <style>\n',
    '        body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }\n',
    '        .header { text-align: center; padding: 20px; background-color: white;\n',
    '                  margin-bottom: 20px; border-radius: 5px;\n',
    '                  box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n',
    '        .plot-container { background-color: white; margin-bottom: 30px; padding: 20px;\n',
    '                          border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n',
    '        h1 { color: #333; margin: 0; }\n',
    '        h2 { color: #666; font-weight: normal; margin-top: 10px; }\n',
    '    </style>\n</head>\n<body>\n',
    '    <div class="header">\n',
    '        <h1>Chestnut Health Systems - HCPCS Code Trends</h1>\n',
    '        <h2>', taxonomy, '</h2>\n',
    '        <p>Interactive time series showing top 10 HCPCS codes by provider location</p>\n',
    '    </div>\n'
  )

  for (i in seq_along(plots)) {
    if (is.null(plots[[i]])) next
    html_content <- paste0(html_content,
      '    <div class="plot-container">\n',
      '        <div id="plot', i, '" style="width:100%;height:500px;"></div>\n',
      '    </div>\n')
  }

  html_content <- paste0(html_content, '    <script>\n')
  for (i in seq_along(plots)) {
    if (is.null(plots[[i]])) next
    plot_json <- plotly_json(plots[[i]], FALSE)
    html_content <- paste0(html_content,
      '        Plotly.newPlot("plot', i, '", ', plot_json, ');\n')
  }
  html_content <- paste0(html_content, '    </script>\n</body>\n</html>\n')

  writeLines(html_content, output_file)
  cat("  Saved:", output_file, "\n\n")
}

cat("Complete! Graphs saved to", OUTPUT_GRAPHS_DIR, "\n\n")
cat("Generated files:\n")
for (f in list.files(OUTPUT_GRAPHS_DIR, pattern = "\\.html$", full.names = FALSE)) {
  cat("  -", f, "\n")
}
