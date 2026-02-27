source("R/config.R")
source("R/viz_helpers.R")

library(dplyr)
library(lubridate)
library(tidyr)
library(arrow)

cat("Loading Chestnut claims data...\n")
chestnut_claims <- read_parquet(CHESTNUT_CLAIMS_PARQUET)

cat("Loaded", format(nrow(chestnut_claims), big.mark = ","), "claims\n")
cat(
  "Date range:",
  as.Date(min(chestnut_claims$year_month, na.rm = TRUE)),
  "to",
  as.Date(max(chestnut_claims$year_month, na.rm = TRUE)),
  "\n\n"
)

dir.create(OUTPUT_GRAPHS_DIR, recursive = TRUE, showWarnings = FALSE)

billing_npis <- chestnut_claims |>
  filter(!is.na(BILLING_PROVIDER_NPI_NUM)) |>
  group_by(BILLING_PROVIDER_NPI_NUM) |>
  summarize(
    bp_name = first(bp_name[!is.na(bp_name)]),
    bp_address = first(bp_address[!is.na(bp_address)]),
    bp_taxonomy_description = first(bp_taxonomy_description[
      !is.na(bp_taxonomy_description)
    ]),
    total_claims = sum(TOTAL_CLAIMS, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_claims))

cat("Found", nrow(billing_npis), "unique billing NPIs\n\n")

all_plots <- list()
plot_info <- list()

# Aggregate graph across ALL providers
cat("Creating aggregate graph across all providers...\n")

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
total_beneficiaries_all <- sum(
  chestnut_claims$TOTAL_UNIQUE_BENEFICIARIES,
  na.rm = TRUE
)

cat("  Total claims:", format(total_claims_all, big.mark = ","), "\n")

make_metrics_fig <- function(monthly_metrics, title_html) {
  fig <- plot_ly() |>
    add_trace(
      data = monthly_metrics,
      x = ~year_month,
      y = ~TOTAL_CLAIMS,
      name = "Total Claims",
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2.5, color = '#667eea'),
      marker = list(size = 7, color = '#667eea'),
      text = ~ paste0(
        "<b>Total Claims</b><br>",
        "Claims: ",
        format(TOTAL_CLAIMS, big.mark = ","),
        "<br>",
        "Date: ",
        format(year_month, "%Y-%m")
      ),
      hoverinfo = 'text'
    ) |>
    add_trace(
      data = monthly_metrics,
      x = ~year_month,
      y = ~TOTAL_UNIQUE_BENEFICIARIES,
      name = "Unique Beneficiaries",
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2.5, color = '#f093fb'),
      marker = list(size = 7, color = '#f093fb'),
      text = ~ paste0(
        "<b>Unique Beneficiaries</b><br>",
        "Beneficiaries: ",
        format(TOTAL_UNIQUE_BENEFICIARIES, big.mark = ","),
        "<br>",
        "Date: ",
        format(year_month, "%Y-%m")
      ),
      hoverinfo = 'text'
    ) |>
    add_trace(
      data = monthly_metrics,
      x = ~year_month,
      y = ~TOTAL_PAID,
      name = "Total Paid ($)",
      type = 'scatter',
      mode = 'lines+markers',
      yaxis = 'y2',
      line = list(width = 2.5, color = '#4facfe'),
      marker = list(size = 7, color = '#4facfe'),
      text = ~ paste0(
        "<b>Total Paid</b><br>",
        "Amount: $",
        format(round(TOTAL_PAID), big.mark = ","),
        "<br>",
        "Date: ",
        format(year_month, "%Y-%m")
      ),
      hoverinfo = 'text'
    ) |>
    layout(
      title = list(text = title_html, font = list(size = 13)),
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
  return(fig)
}

fig_all <- make_metrics_fig(
  monthly_metrics_all,
  paste0(
    "<b>ALL CHESTNUT PROVIDERS - AGGREGATED</b><br>",
    "<sub>Key Metrics Across All Locations</sub><br>",
    "<sub style='font-size:10px;'>Claims: ",
    format(total_claims_all, big.mark = ","),
    " | Beneficiaries: ",
    format(total_beneficiaries_all, big.mark = ","),
    " | Paid: $",
    format(round(total_paid_all), big.mark = ","),
    "</sub>"
  )
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
for (i in seq_len(nrow(billing_npis))) {
  npi <- billing_npis$BILLING_PROVIDER_NPI_NUM[i]
  provider_name <- billing_npis$bp_name[i]
  provider_address <- billing_npis$bp_address[i]
  taxonomy <- billing_npis$bp_taxonomy_description[i]

  cat("Processing NPI:", npi, "\n")

  npi_data <- chestnut_claims |>
    filter(BILLING_PROVIDER_NPI_NUM == npi, !is.na(year_month))

  if (nrow(npi_data) == 0) {
    cat("  No claims found, skipping\n\n")
    next
  }

  monthly_metrics <- npi_data |>
    group_by(year_month) |>
    summarize(
      TOTAL_UNIQUE_BENEFICIARIES = sum(
        TOTAL_UNIQUE_BENEFICIARIES,
        na.rm = TRUE
      ),
      TOTAL_CLAIMS = sum(TOTAL_CLAIMS, na.rm = TRUE),
      TOTAL_PAID = sum(TOTAL_PAID, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(year_month)

  total_claims_npi <- sum(npi_data$TOTAL_CLAIMS, na.rm = TRUE)
  total_paid_npi <- sum(npi_data$TOTAL_PAID, na.rm = TRUE)
  total_beneficiaries <- sum(npi_data$TOTAL_UNIQUE_BENEFICIARIES, na.rm = TRUE)

  cat("  Total claims:", format(total_claims_npi, big.mark = ","), "\n")

  fig <- make_metrics_fig(
    monthly_metrics,
    paste0(
      "<b>",
      provider_name,
      "</b><br>",
      "<sub>",
      provider_address,
      "</sub><br>",
      "<sub style='font-size:10px;'>",
      taxonomy,
      " | NPI: ",
      npi,
      "</sub>"
    )
  )

  all_plots[[i + 1]] <- fig
  plot_info[[i + 1]] <- list(
    npi = npi,
    name = provider_name,
    address = provider_address,
    claims = total_claims_npi,
    beneficiaries = total_beneficiaries,
    paid = total_paid_npi,
    city = extract_city(provider_address),
    taxonomy_short = shorten_taxonomy(taxonomy)
  )

  cat("\n")
}

# Build and write dashboard HTML
cat("Creating combined HTML file with all", length(all_plots), "graphs...\n")

totals <- plot_info[[1]]

metrics_legend_html <- paste0(
  '    <div style="background:white;padding:20px;margin:20px auto;max-width:1200px;',
  'border-radius:8px;box-shadow:0 2px 4px rgba(0,0,0,0.05);',
  'display:flex;justify-content:space-around;flex-wrap:wrap;gap:20px;">\n',
  '        <div style="text-align:center;flex:1;min-width:200px;">\n',
  '            <div style="font-size:28px;font-weight:600;color:#667eea;margin:5px 0;">',
  format(totals$claims, big.mark = ","),
  '</div>\n',
  '            <div style="font-size:14px;color:#6c757d;text-transform:uppercase;">Total Claims</div>\n',
  '        </div>\n',
  '        <div style="text-align:center;flex:1;min-width:200px;">\n',
  '            <div style="font-size:28px;font-weight:600;color:#f093fb;margin:5px 0;">',
  format(totals$beneficiaries, big.mark = ","),
  '</div>\n',
  '            <div style="font-size:14px;color:#6c757d;text-transform:uppercase;">Total Beneficiaries</div>\n',
  '        </div>\n',
  '        <div style="text-align:center;flex:1;min-width:200px;">\n',
  '            <div style="font-size:28px;font-weight:600;color:#4facfe;margin:5px 0;">$',
  format(round(totals$paid), big.mark = ","),
  '</div>\n',
  '            <div style="font-size:14px;color:#6c757d;text-transform:uppercase;">Total Paid</div>\n',
  '        </div>\n',
  '    </div>\n'
)

html <- make_dashboard_html(
  plots = all_plots,
  plot_info = plot_info,
  title = "Chestnut Health Systems - Key Metrics by Provider",
  subtitle = "Beneficiaries, Claims, and Payments tracked monthly",
  header_gradient = "linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)",
  extra_header_html = metrics_legend_html,
  card_stats_fn = function(info) {
    paste0(
      "Claims: ",
      format(info$claims, big.mark = ","),
      " | ",
      "Beneficiaries: ",
      format(info$beneficiaries, big.mark = ","),
      " | ",
      "Paid: $",
      format(round(info$paid), big.mark = ",")
    )
  }
)

output_file <- file.path(OUTPUT_GRAPHS_DIR, "chestnut_metrics_by_provider.html")
writeLines(html, output_file)

cat("\nSuccess! Metrics graphs saved to:", output_file, "\n")
cat("  Total graphs:", length(all_plots), "\n")
cat("  Total claims:", format(totals$claims, big.mark = ","), "\n")
cat(
  "  Total beneficiaries:",
  format(totals$beneficiaries, big.mark = ","),
  "\n"
)
cat(
  "  Total paid: $",
  format(round(totals$paid), big.mark = ","),
  "\n",
  sep = ""
)
