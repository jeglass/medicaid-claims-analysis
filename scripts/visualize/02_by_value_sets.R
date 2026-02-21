library(dplyr)
library(readr)
library(plotly)
library(lubridate)

cat("Loading Chestnut claims data...\n")
chestnut_claims <- readRDS("data/chestnut_claims.rds")

cat("Loaded", format(nrow(chestnut_claims), big.mark = ","), "claims\n")
cat("Date range:", as.Date(min(chestnut_claims$year_month, na.rm = TRUE)),
    "to", as.Date(max(chestnut_claims$year_month, na.rm = TRUE)), "\n\n")

# Create output directory
dir.create("output/graphs", recursive = TRUE, showWarnings = FALSE)

# Add display label: use first value set if available, otherwise use code
chestnut_claims <- chestnut_claims |>
  mutate(
    display_label = ifelse(
      !is.na(code_value_sets),
      # Extract first value set from pipe-separated list
      sub(" \\|.*$", "", code_value_sets),
      # If no value set, use the HCPCS code
      HCPCS_CODE
    ),
    # Keep track of original code for hover text
    original_code = HCPCS_CODE
  )

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

# Create list to store all plots
all_plots <- list()
plot_info <- list()

# First, create an aggregate graph across ALL providers
cat("Creating aggregate graph across all providers...\n")

# Get top 20 display labels overall
top_labels_all <- chestnut_claims |>
  filter(!is.na(year_month)) |>
  group_by(display_label) |>
  summarize(total = sum(TOTAL_CLAIMS, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total)) |>
  head(20)

# Aggregate by month across all providers
# Include the codes that contributed to each label for hover text
monthly_data_all <- chestnut_claims |>
  filter(!is.na(year_month), display_label %in% top_labels_all$display_label) |>
  group_by(year_month, display_label) |>
  summarize(
    TOTAL_CLAIMS = sum(TOTAL_CLAIMS, na.rm = TRUE),
    # Get unique codes that contributed
    codes = paste(unique(original_code), collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(year_month)

total_claims_all <- sum(chestnut_claims$TOTAL_CLAIMS, na.rm = TRUE)

cat("  Total claims (all providers):", format(total_claims_all, big.mark = ","), "\n")
cat("  Top 20 labels:", paste(head(top_labels_all$display_label, 10), collapse = ", "), "...\n")

# Create aggregate plot
fig_all <- plot_ly(
  monthly_data_all,
  x = ~year_month,
  y = ~TOTAL_CLAIMS,
  color = ~display_label,
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste0(
    "<b>", display_label, "</b><br>",
    "Codes: ", codes, "<br>",
    "<b>Claims:</b> ", TOTAL_CLAIMS, "<br>",
    "<b>Date:</b> ", format(year_month, "%Y-%m")
  ),
  hoverinfo = 'text',
  line = list(width = 2.5),
  marker = list(size = 7)
) |>
  layout(
    title = list(
      text = paste0(
        "<b>ALL CHESTNUT PROVIDERS - AGGREGATED</b><br>",
        "<sub>Top 20 HEDIS Value Sets / Codes</sub><br>",
        "<sub style='font-size:10px;'>Total Claims: ", format(total_claims_all, big.mark = ","), "</sub>"
      ),
      font = list(size = 14)
    ),
    xaxis = list(
      title = "<b>Month</b>",
      tickformat = "%Y-%m",
      tickangle = -45
    ),
    yaxis = list(
      title = "<b>Total Claims</b>",
      rangemode = "tozero"
    ),
    hovermode = 'closest',
    legend = list(
      title = list(text = "<b>Value Set / Code</b>"),
      orientation = "v",
      x = 1.02,
      y = 1,
      bgcolor = "rgba(255,255,255,0.8)",
      bordercolor = "#ccc",
      borderwidth = 1
    ),
    margin = list(r = 250, b = 100, t = 100)
  )

all_plots[[1]] <- fig_all
plot_info[[1]] <- list(
  npi = "ALL",
  name = "All Providers (Aggregated)",
  address = "All Chestnut Locations",
  claims = total_claims_all,
  city = "All",
  taxonomy_short = "Aggregate"
)

cat("\n")

# Process each NPI
for (i in 1:nrow(billing_npis)) {
  npi <- billing_npis$BILLING_PROVIDER_NPI_NUM[i]
  provider_name <- billing_npis$bp_name[i]
  provider_address <- billing_npis$bp_address[i]
  taxonomy <- billing_npis$bp_taxonomy_description[i]

  cat("Processing NPI:", npi, "\n")
  cat("  Name:", provider_name, "\n")
  cat("  Address:", provider_address, "\n")

  # Filter to this NPI
  npi_data <- chestnut_claims |>
    filter(BILLING_PROVIDER_NPI_NUM == npi, !is.na(year_month))

  if (nrow(npi_data) == 0) {
    cat("  No claims found, skipping\n\n")
    next
  }

  # Get top 20 display labels for this NPI
  top_labels <- npi_data |>
    group_by(display_label) |>
    summarize(total = sum(TOTAL_CLAIMS, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total)) |>
    head(20)

  total_claims_npi <- sum(npi_data$TOTAL_CLAIMS, na.rm = TRUE)
  cat("  Total claims:", format(total_claims_npi, big.mark = ","), "\n")
  cat("  Top 20 labels:", paste(head(top_labels$display_label, 10), collapse = ", "), "...\n")

  # Aggregate by month and display label (for top 20 labels only)
  monthly_data <- npi_data |>
    filter(display_label %in% top_labels$display_label) |>
    group_by(year_month, display_label) |>
    summarize(
      TOTAL_CLAIMS = sum(TOTAL_CLAIMS, na.rm = TRUE),
      codes = paste(unique(original_code), collapse = ", "),
      .groups = "drop"
    ) |>
    arrange(year_month)

  cat("  Monthly data points:", nrow(monthly_data), "\n")

  # Create the plot
  fig <- plot_ly(
    monthly_data,
    x = ~year_month,
    y = ~TOTAL_CLAIMS,
    color = ~display_label,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste0(
      "<b>", display_label, "</b><br>",
      "Codes: ", codes, "<br>",
      "<b>Claims:</b> ", TOTAL_CLAIMS, "<br>",
      "<b>Date:</b> ", format(year_month, "%Y-%m")
    ),
    hoverinfo = 'text',
    line = list(width = 2.5),
    marker = list(size = 7)
  ) |>
    layout(
      title = list(
        text = paste0(
          "<b>", provider_name, "</b><br>",
          "<sub>", provider_address, "</sub><br>",
          "<sub style='font-size:10px;'>", taxonomy, " | NPI: ", npi, "</sub>"
        ),
        font = list(size = 13)
      ),
      xaxis = list(
        title = "<b>Month</b>",
        tickformat = "%Y-%m",
        tickangle = -45
      ),
      yaxis = list(
        title = "<b>Total Claims</b>",
        rangemode = "tozero"
      ),
      hovermode = 'closest',
      legend = list(
        title = list(text = "<b>Value Set / Code</b>"),
        orientation = "v",
        x = 1.02,
        y = 1,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "#ccc",
        borderwidth = 1
      ),
      margin = list(r = 250, b = 100, t = 100)
    )

  # Extract city from address for nav label
  extract_city <- function(address) {
    parts <- strsplit(address, " +")[[1]]
    il_pos <- which(parts == "IL")
    if (length(il_pos) == 0) return("Unknown")

    street_types <- c("ST", "DR", "AVE", "RD", "BLVD", "LN", "CT", "PL", "WAY", "PKWY", "CIR", "TER")
    street_pos <- 0
    for (j in 1:(il_pos - 1)) {
      if (parts[j] %in% street_types) street_pos <- j
    }

    if (street_pos == 0 || street_pos >= il_pos - 1) {
      start_pos <- max(1, il_pos - 2)
      city_parts <- parts[start_pos:(il_pos - 1)]
    } else {
      city_parts <- parts[(street_pos + 1):(il_pos - 1)]
    }

    city_parts <- city_parts[!city_parts %in% c("BLDG", "A", "B", "C", "D")]
    city <- paste(city_parts, collapse = " ")
    return(tools::toTitleCase(tolower(city)))
  }

  city <- extract_city(provider_address)

  # Shorten taxonomy for nav label
  taxonomy_short <- gsub(" \\(.*\\)", "", taxonomy)
  taxonomy_short <- gsub("Clinic/Center.*", "Clinic", taxonomy_short)
  taxonomy_short <- gsub("Mental Health.*", "Mental Health", taxonomy_short)
  taxonomy_short <- gsub("Substance Abuse.*", "SUD", taxonomy_short)
  taxonomy_short <- gsub("Federally Qualified Health Center", "FQHC", taxonomy_short)

  all_plots[[i + 1]] <- fig  # +1 because first slot is aggregate
  plot_info[[i + 1]] <- list(
    npi = npi,
    name = provider_name,
    address = provider_address,
    claims = total_claims_npi,
    city = city,
    taxonomy_short = taxonomy_short
  )

  cat("\n")
}

# Create comprehensive HTML file with all plots
cat("Creating combined HTML file with all", length(all_plots), "graphs...\n")

html_content <- '
<!DOCTYPE html>
<html>
<head>
    <title>Chestnut Health Systems - HEDIS Value Sets by Provider</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            margin: 0;
            padding: 0;
            background: linear-gradient(to bottom, #f8f9fa 0%, #e9ecef 100%);
        }
        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px 20px;
            text-align: center;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        .header h1 {
            margin: 0 0 10px 0;
            font-size: 32px;
            font-weight: 600;
        }
        .header p {
            margin: 5px 0;
            font-size: 16px;
            opacity: 0.95;
        }
        .nav {
            background: white;
            padding: 15px 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.05);
            position: sticky;
            top: 0;
            z-index: 1000;
        }
        .nav-links {
            display: flex;
            flex-wrap: wrap;
            gap: 10px;
            justify-content: center;
        }
        .nav-link {
            padding: 8px 16px;
            background: #f1f3f5;
            border-radius: 6px;
            text-decoration: none;
            color: #495057;
            font-size: 13px;
            transition: all 0.2s;
            white-space: nowrap;
            max-width: 200px;
            overflow: visible;
            text-overflow: clip;
        }
        .nav-link:hover {
            background: #667eea;
            color: white;
            transform: translateY(-2px);
        }
        .container {
            max-width: 1400px;
            margin: 0 auto;
            padding: 30px 20px;
        }
        .plot-card {
            background: white;
            margin-bottom: 40px;
            padding: 25px;
            border-radius: 12px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
            transition: transform 0.2s, box-shadow 0.2s;
        }
        .plot-card:hover {
            transform: translateY(-4px);
            box-shadow: 0 8px 24px rgba(0,0,0,0.12);
        }
        .plot-meta {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 15px 0;
            border-bottom: 2px solid #e9ecef;
            margin-bottom: 15px;
        }
        .plot-number {
            font-size: 18px;
            font-weight: 600;
            color: #667eea;
        }
        .plot-claims {
            font-size: 14px;
            color: #6c757d;
        }
        .footer {
            text-align: center;
            padding: 30px;
            color: #6c757d;
            font-size: 14px;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>🏥 Chestnut Health Systems</h1>
        <p>Top 20 HEDIS Value Sets / Codes by Provider Over Time</p>
        <p style="font-size:14px; opacity:0.8;">Interactive time series showing claim volumes by month (grouped by HEDIS quality measures)</p>
    </div>

    <div class="nav">
        <div class="nav-links" id="nav-links">
            <!-- Navigation links will be inserted here -->
        </div>
    </div>

    <div class="container">
'

# Add navigation links
nav_html <- ""
for (i in 1:length(all_plots)) {
  if (!is.null(plot_info[[i]])) {
    if (plot_info[[i]]$npi == "ALL") {
      label <- "📊 All Providers"
    } else {
      label <- paste0(plot_info[[i]]$city, " ", plot_info[[i]]$taxonomy_short)
    }
    nav_html <- paste0(nav_html,
      '<a href="#plot', i, '" class="nav-link">',
      label,
      '</a>')
  }
}

# Add each plot
plots_html <- ""
for (i in 1:length(all_plots)) {
  if (!is.null(all_plots[[i]]) && !is.null(plot_info[[i]])) {
    plots_html <- paste0(plots_html, '
        <div class="plot-card" id="plot', i, '">
            <div class="plot-meta">
                <div class="plot-number">Provider ', i, ' of ', length(all_plots), '</div>
                <div class="plot-claims">Total Claims: ', format(plot_info[[i]]$claims, big.mark = ","), '</div>
            </div>
            <div id="graph', i, '" style="width:100%;height:550px;"></div>
        </div>
    ')
  }
}

html_content <- paste0(html_content, plots_html, '
    </div>

    <div class="footer">
        Generated on ', format(Sys.Date(), "%B %d, %Y"), '<br>
        Data: Chestnut Health Systems Medicaid Claims<br>
        Grouped by HEDIS Quality Measure Value Sets
    </div>

    <script>
        // Insert navigation links
        document.getElementById("nav-links").innerHTML = `', nav_html, '`;

        // Render all plots
')

for (i in 1:length(all_plots)) {
  if (!is.null(all_plots[[i]])) {
    plot_json <- plotly_json(all_plots[[i]], FALSE)
    html_content <- paste0(html_content, '
        Plotly.newPlot("graph', i, '", ', plot_json, ');
    ')
  }
}

html_content <- paste0(html_content, '
    </script>
</body>
</html>
')

# Write the combined file
output_file <- "output/graphs/chestnut_by_value_sets.html"
writeLines(html_content, output_file)

cat("\n✓ Success! All graphs saved to:", output_file, "\n")
cat("\nSummary:\n")
cat("  Total NPIs:", length(all_plots), "\n")
cat("  Total claims:", format(sum(sapply(plot_info, function(x) x$claims)), big.mark = ","), "\n")
cat("\nOpen the file in your web browser to view all interactive graphs.\n")
cat("Graphs show HEDIS value sets when available, otherwise show individual codes.\n")
