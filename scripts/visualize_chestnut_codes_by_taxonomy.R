library(dplyr)
library(readr)
library(plotly)
library(htmlwidgets)

cat("Loading Chestnut claims data...\n")
chestnut_claims <- readRDS("data/chestnut_claims.rds")

cat("Loaded", nrow(chestnut_claims), "claims\n")
cat("Date range:", min(chestnut_claims$year_month, na.rm = TRUE), "to",
    max(chestnut_claims$year_month, na.rm = TRUE), "\n\n")

# Create output directory for graphs
dir.create("output/graphs", recursive = TRUE, showWarnings = FALSE)

# Get unique taxonomy descriptions
taxonomies <- chestnut_claims |>
  filter(!is.na(bp_taxonomy_description)) |>
  distinct(bp_taxonomy_description) |>
  pull(bp_taxonomy_description)

cat("Found", length(taxonomies), "unique billing provider taxonomy types\n\n")

# Process each taxonomy separately
for (taxonomy in taxonomies) {
  cat("Processing taxonomy:", taxonomy, "\n")

  # Filter to this taxonomy
  taxonomy_data <- chestnut_claims |>
    filter(bp_taxonomy_description == taxonomy, !is.na(year_month))

  if (nrow(taxonomy_data) == 0) {
    cat("  No claims found, skipping\n\n")
    next
  }

  # Get top 10 HCPCS codes for this taxonomy
  top_codes <- taxonomy_data |>
    count(HCPCS_CODE, code_description, sort = TRUE) |>
    head(10)

  cat("  Top 10 codes:", paste(top_codes$HCPCS_CODE, collapse = ", "), "\n")

  # Filter to only top 10 codes and aggregate by time, provider, and code
  time_series_data <- taxonomy_data |>
    filter(HCPCS_CODE %in% top_codes$HCPCS_CODE) |>
    group_by(
      year_month,
      BILLING_PROVIDER_NPI_NUM,
      bp_name,
      bp_address,
      HCPCS_CODE,
      code_description
    ) |>
    summarize(
      TOTAL_CLAIMS = n(),
      .groups = "drop"
    ) |>
    arrange(year_month)

  # Get unique provider addresses for this taxonomy
  providers <- time_series_data |>
    distinct(BILLING_PROVIDER_NPI_NUM, bp_address) |>
    arrange(bp_address)

  cat("  Creating graphs for", nrow(providers), "providers\n")

  # Create a plot for each provider
  plots <- list()

  for (i in 1:nrow(providers)) {
    provider_npi <- providers$BILLING_PROVIDER_NPI_NUM[i]
    provider_address <- providers$bp_address[i]

    # Filter to this provider
    provider_data <- time_series_data |>
      filter(BILLING_PROVIDER_NPI_NUM == provider_npi)

    if (nrow(provider_data) == 0) next

    # Create plotly line chart
    fig <- plot_ly(provider_data,
                   x = ~year_month,
                   y = ~TOTAL_CLAIMS,
                   color = ~HCPCS_CODE,
                   type = 'scatter',
                   mode = 'lines+markers',
                   text = ~paste0(
                     "Code: ", HCPCS_CODE, "<br>",
                     "Description: ", code_description, "<br>",
                     "Claims: ", TOTAL_CLAIMS, "<br>",
                     "Date: ", format(year_month, "%Y-%m")
                   ),
                   hoverinfo = 'text',
                   line = list(width = 2),
                   marker = list(size = 6)
    ) |>
      layout(
        title = list(
          text = paste0("<b>", provider_address, "</b><br>",
                       "<sub>Top 10 HCPCS Codes - ", taxonomy, "</sub>"),
          font = list(size = 14)
        ),
        xaxis = list(
          title = "Date",
          tickformat = "%Y-%m"
        ),
        yaxis = list(
          title = "Total Claims"
        ),
        hovermode = 'closest',
        legend = list(
          title = list(text = "HCPCS Code"),
          orientation = "v",
          x = 1.05,
          y = 1
        ),
        margin = list(r = 200)  # Space for legend
      )

    plots[[i]] <- fig
  }

  # Save combined HTML file with all providers for this taxonomy
  # Create a safe filename
  safe_taxonomy_name <- gsub("[^A-Za-z0-9_]", "_", taxonomy)
  safe_taxonomy_name <- substr(safe_taxonomy_name, 1, 50)  # Limit length

  output_file <- paste0("output/graphs/", safe_taxonomy_name, ".html")

  # Create HTML with all plots stacked vertically
  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
    <title>', taxonomy, ' - Chestnut Health Systems HCPCS Trends</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 20px;
            background-color: #f5f5f5;
        }
        .header {
            text-align: center;
            padding: 20px;
            background-color: white;
            margin-bottom: 20px;
            border-radius: 5px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .plot-container {
            background-color: white;
            margin-bottom: 30px;
            padding: 20px;
            border-radius: 5px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        h1 {
            color: #333;
            margin: 0;
        }
        h2 {
            color: #666;
            font-weight: normal;
            margin-top: 10px;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>Chestnut Health Systems - HCPCS Code Trends</h1>
        <h2>', taxonomy, '</h2>
        <p>Interactive time series showing top 10 HCPCS codes by provider location</p>
    </div>
')

  # Add each plot
  for (i in 1:length(plots)) {
    if (!is.null(plots[[i]])) {
      plot_div <- paste0('
    <div class="plot-container">
        <div id="plot', i, '" style="width:100%;height:500px;"></div>
    </div>
')
      html_content <- paste0(html_content, plot_div)
    }
  }

  # Add JavaScript to render plots
  html_content <- paste0(html_content, '
    <script>
')

  for (i in 1:length(plots)) {
    if (!is.null(plots[[i]])) {
      plot_json <- plotly_json(plots[[i]], FALSE)
      html_content <- paste0(html_content, '
        Plotly.newPlot("plot', i, '", ', plot_json, ');
')
    }
  }

  html_content <- paste0(html_content, '
    </script>
</body>
</html>
')

  # Write to file
  writeLines(html_content, output_file)
  cat("  Saved:", output_file, "\n\n")
}

cat("Complete! Graphs saved to output/graphs/\n")
cat("\nGenerated files:\n")
files <- list.files("output/graphs", pattern = "\\.html$", full.names = FALSE)
for (f in files) {
  cat("  -", f, "\n")
}

cat("\nOpen any HTML file in your web browser to view interactive graphs.\n")
