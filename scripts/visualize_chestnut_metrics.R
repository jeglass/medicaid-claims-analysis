library(dplyr)
library(readr)
library(plotly)
library(lubridate)
library(tidyr)

cat("Loading Chestnut claims data...\n")
chestnut_claims <- readRDS("data/chestnut_claims.rds")

cat("Loaded", format(nrow(chestnut_claims), big.mark = ","), "claims\n")
cat("Date range:", as.Date(min(chestnut_claims$year_month, na.rm = TRUE)),
    "to", as.Date(max(chestnut_claims$year_month, na.rm = TRUE)), "\n\n")

# Create output directory
dir.create("output/graphs", recursive = TRUE, showWarnings = FALSE)

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

# Aggregate metrics by month across all providers
monthly_metrics_all <- chestnut_claims |>
  filter(!is.na(year_month)) |>
  group_by(year_month) |>
  summarize(
    TOTAL_UNIQUE_BENEFICIARIES = sum(TOTAL_UNIQUE_BENEFICIARIES, na.rm = TRUE),
    TOTAL_CLAIMS = sum(TOTAL_CLAIMS, na.rm = TRUE),
    TOTAL_PAID = sum(TOTAL_PAID, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year_month)

total_claims_all <- sum(chestnut_claims$TOTAL_CLAIMS, na.rm = TRUE)
total_paid_all <- sum(chestnut_claims$TOTAL_PAID, na.rm = TRUE)
total_beneficiaries_all <- sum(chestnut_claims$TOTAL_UNIQUE_BENEFICIARIES, na.rm = TRUE)

cat("  Total claims:", format(total_claims_all, big.mark = ","), "\n")
cat("  Total beneficiaries:", format(total_beneficiaries_all, big.mark = ","), "\n")
cat("  Total paid: $", format(round(total_paid_all), big.mark = ","), "\n", sep = "")

# Create aggregate plot
fig_all <- plot_ly()

fig_all <- fig_all |> add_trace(
  data = monthly_metrics_all,
  x = ~year_month,
  y = ~TOTAL_CLAIMS,
  name = "Total Claims",
  type = 'scatter',
  mode = 'lines+markers',
  line = list(width = 2.5, color = '#667eea'),
  marker = list(size = 7, color = '#667eea'),
  text = ~paste0(
    "<b>Total Claims</b><br>",
    "Claims: ", format(TOTAL_CLAIMS, big.mark = ","), "<br>",
    "Date: ", format(year_month, "%Y-%m")
  ),
  hoverinfo = 'text'
)

fig_all <- fig_all |> add_trace(
  data = monthly_metrics_all,
  x = ~year_month,
  y = ~TOTAL_UNIQUE_BENEFICIARIES,
  name = "Unique Beneficiaries",
  type = 'scatter',
  mode = 'lines+markers',
  line = list(width = 2.5, color = '#f093fb'),
  marker = list(size = 7, color = '#f093fb'),
  text = ~paste0(
    "<b>Unique Beneficiaries</b><br>",
    "Beneficiaries: ", format(TOTAL_UNIQUE_BENEFICIARIES, big.mark = ","), "<br>",
    "Date: ", format(year_month, "%Y-%m")
  ),
  hoverinfo = 'text'
)

fig_all <- fig_all |> add_trace(
  data = monthly_metrics_all,
  x = ~year_month,
  y = ~TOTAL_PAID,
  name = "Total Paid ($)",
  type = 'scatter',
  mode = 'lines+markers',
  yaxis = 'y2',
  line = list(width = 2.5, color = '#4facfe'),
  marker = list(size = 7, color = '#4facfe'),
  text = ~paste0(
    "<b>Total Paid</b><br>",
    "Amount: $", format(round(TOTAL_PAID), big.mark = ","), "<br>",
    "Date: ", format(year_month, "%Y-%m")
  ),
  hoverinfo = 'text'
)

fig_all <- fig_all |> layout(
  title = list(
    text = paste0(
      "<b>ALL CHESTNUT PROVIDERS - AGGREGATED</b><br>",
      "<sub>Key Metrics Across All Locations</sub><br>",
      "<sub style='font-size:10px;'>Claims: ", format(total_claims_all, big.mark = ","),
      " | Beneficiaries: ", format(total_beneficiaries_all, big.mark = ","),
      " | Paid: $", format(round(total_paid_all), big.mark = ","), "</sub>"
    ),
    font = list(size = 14)
  ),
  xaxis = list(
    title = "<b>Month</b>",
    tickformat = "%Y-%m",
    tickangle = -45
  ),
  yaxis = list(
    title = "<b>Claims / Beneficiaries</b>",
    rangemode = "tozero",
    side = "left"
  ),
  yaxis2 = list(
    title = "<b>Total Paid ($)</b>",
    rangemode = "tozero",
    overlaying = "y",
    side = "right"
  ),
  hovermode = 'closest',
  legend = list(
    title = list(text = "<b>Metrics</b>"),
    orientation = "v",
    x = 1.15,
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
  beneficiaries = total_beneficiaries_all,
  paid = total_paid_all,
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

  # Aggregate metrics by month (sum across all HCPCS codes)
  monthly_metrics <- npi_data |>
    group_by(year_month) |>
    summarize(
      TOTAL_UNIQUE_BENEFICIARIES = sum(TOTAL_UNIQUE_BENEFICIARIES, na.rm = TRUE),
      TOTAL_CLAIMS = sum(TOTAL_CLAIMS, na.rm = TRUE),
      TOTAL_PAID = sum(TOTAL_PAID, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(year_month)

  total_claims_npi <- sum(npi_data$TOTAL_CLAIMS, na.rm = TRUE)
  total_paid_npi <- sum(npi_data$TOTAL_PAID, na.rm = TRUE)
  total_beneficiaries <- sum(npi_data$TOTAL_UNIQUE_BENEFICIARIES, na.rm = TRUE)

  cat("  Total claims:", format(total_claims_npi, big.mark = ","), "\n")
  cat("  Total beneficiaries:", format(total_beneficiaries, big.mark = ","), "\n")
  cat("  Total paid: $", format(round(total_paid_npi), big.mark = ","), "\n", sep = "")
  cat("  Monthly data points:", nrow(monthly_metrics), "\n")

  # Create the plot with dual y-axes
  # Left axis: Claims and Beneficiaries (similar scales)
  # Right axis: Paid (much larger values)

  fig <- plot_ly()

  # Add Total Claims (left axis)
  fig <- fig |> add_trace(
    data = monthly_metrics,
    x = ~year_month,
    y = ~TOTAL_CLAIMS,
    name = "Total Claims",
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 2.5, color = '#667eea'),
    marker = list(size = 7, color = '#667eea'),
    text = ~paste0(
      "<b>Total Claims</b><br>",
      "Claims: ", format(TOTAL_CLAIMS, big.mark = ","), "<br>",
      "Date: ", format(year_month, "%Y-%m")
    ),
    hoverinfo = 'text'
  )

  # Add Total Unique Beneficiaries (left axis)
  fig <- fig |> add_trace(
    data = monthly_metrics,
    x = ~year_month,
    y = ~TOTAL_UNIQUE_BENEFICIARIES,
    name = "Unique Beneficiaries",
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 2.5, color = '#f093fb'),
    marker = list(size = 7, color = '#f093fb'),
    text = ~paste0(
      "<b>Unique Beneficiaries</b><br>",
      "Beneficiaries: ", format(TOTAL_UNIQUE_BENEFICIARIES, big.mark = ","), "<br>",
      "Date: ", format(year_month, "%Y-%m")
    ),
    hoverinfo = 'text'
  )

  # Add Total Paid (right axis)
  fig <- fig |> add_trace(
    data = monthly_metrics,
    x = ~year_month,
    y = ~TOTAL_PAID,
    name = "Total Paid ($)",
    type = 'scatter',
    mode = 'lines+markers',
    yaxis = 'y2',
    line = list(width = 2.5, color = '#4facfe'),
    marker = list(size = 7, color = '#4facfe'),
    text = ~paste0(
      "<b>Total Paid</b><br>",
      "Amount: $", format(round(TOTAL_PAID), big.mark = ","), "<br>",
      "Date: ", format(year_month, "%Y-%m")
    ),
    hoverinfo = 'text'
  )

  # Layout with dual y-axes
  fig <- fig |> layout(
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
      title = "<b>Claims / Beneficiaries</b>",
      rangemode = "tozero",
      side = "left"
    ),
    yaxis2 = list(
      title = "<b>Total Paid ($)</b>",
      rangemode = "tozero",
      overlaying = "y",
      side = "right"
    ),
    hovermode = 'closest',
    legend = list(
      title = list(text = "<b>Metrics</b>"),
      orientation = "v",
      x = 1.15,
      y = 1,
      bgcolor = "rgba(255,255,255,0.8)",
      bordercolor = "#ccc",
      borderwidth = 1
    ),
    margin = list(r = 250, b = 100, t = 100)
  )

  # Extract city from address for nav label
  # Address format: "123 STREET NAME CITY NAME IL 12345"
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
    beneficiaries = total_beneficiaries,
    paid = total_paid_npi,
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
    <title>Chestnut Health Systems - Key Metrics by Provider</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            margin: 0;
            padding: 0;
            background: linear-gradient(to bottom, #f8f9fa 0%, #e9ecef 100%);
        }
        .header {
            background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
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
        .metrics-legend {
            background: white;
            padding: 20px;
            margin: 20px auto;
            max-width: 1200px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.05);
            display: flex;
            justify-content: space-around;
            flex-wrap: wrap;
            gap: 20px;
        }
        .metric-box {
            text-align: center;
            flex: 1;
            min-width: 200px;
        }
        .metric-value {
            font-size: 28px;
            font-weight: 600;
            margin: 5px 0;
        }
        .metric-label {
            font-size: 14px;
            color: #6c757d;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }
        .metric-claims { color: #667eea; }
        .metric-beneficiaries { color: #f093fb; }
        .metric-paid { color: #4facfe; }
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
            background: #4facfe;
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
            color: #4facfe;
        }
        .plot-stats {
            font-size: 13px;
            color: #6c757d;
            text-align: right;
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
        <h1>📊 Chestnut Health Systems</h1>
        <p>Key Metrics by Provider Over Time</p>
        <p style="font-size:14px; opacity:0.8;">Beneficiaries, Claims, and Payments tracked monthly</p>
    </div>
'

# Get overall totals from the aggregate (first plot)
if (!is.null(plot_info[[1]])) {
  total_claims_summary <- plot_info[[1]]$claims
  total_beneficiaries_summary <- plot_info[[1]]$beneficiaries
  total_paid_summary <- plot_info[[1]]$paid
} else {
  total_claims_summary <- 0
  total_beneficiaries_summary <- 0
  total_paid_summary <- 0
}

html_content <- paste0(html_content, '
    <div class="metrics-legend">
        <div class="metric-box">
            <div class="metric-value metric-claims">', format(total_claims_summary, big.mark = ","), '</div>
            <div class="metric-label">Total Claims</div>
        </div>
        <div class="metric-box">
            <div class="metric-value metric-beneficiaries">', format(total_beneficiaries_summary, big.mark = ","), '</div>
            <div class="metric-label">Total Beneficiaries</div>
        </div>
        <div class="metric-box">
            <div class="metric-value metric-paid">$', format(round(total_paid_summary), big.mark = ","), '</div>
            <div class="metric-label">Total Paid</div>
        </div>
    </div>

    <div class="nav">
        <div class="nav-links" id="nav-links">
            <!-- Navigation links will be inserted here -->
        </div>
    </div>

    <div class="container">
')

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
                <div class="plot-stats">
                    Claims: ', format(plot_info[[i]]$claims, big.mark = ","), ' |
                    Beneficiaries: ', format(plot_info[[i]]$beneficiaries, big.mark = ","), ' |
                    Paid: $', format(round(plot_info[[i]]$paid), big.mark = ","), '
                </div>
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
        Metrics: Unique Beneficiaries, Total Claims, Total Paid
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
output_file <- "output/graphs/chestnut_metrics_by_provider.html"
writeLines(html_content, output_file)

cat("\n✓ Success! Metrics graphs saved to:", output_file, "\n")
cat("\nSummary:\n")
cat("  Total graphs:", length(all_plots), "(1 aggregate + ", length(all_plots) - 1, " individual providers)\n")
cat("  Total claims:", format(total_claims_summary, big.mark = ","), "\n")
cat("  Total beneficiaries:", format(total_beneficiaries_summary, big.mark = ","), "\n")
cat("  Total paid: $", format(round(total_paid_summary), big.mark = ","), "\n", sep = "")
cat("\nOpen the file in your web browser to view all interactive graphs.\n")
