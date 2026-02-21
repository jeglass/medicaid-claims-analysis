# ============================================================================
# Shared visualization helpers
# ============================================================================
# Source this file at the top of every visualization script:
#   source("R/viz_helpers.R")

library(plotly)

# Extract city name from a Chestnut provider address string.
# Address format: "123 STREET NAME CITY NAME IL 12345"
extract_city <- function(address) {
  if (is.na(address) || address == "") return("Unknown")
  parts <- strsplit(address, " +")[[1]]
  il_pos <- which(parts == "IL")
  if (length(il_pos) == 0) return("Unknown")

  street_types <- c("ST", "DR", "AVE", "RD", "BLVD", "LN", "CT", "PL", "WAY",
                    "PKWY", "CIR", "TER")
  street_pos <- 0
  for (j in seq_len(il_pos - 1)) {
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

# Shorten a NUCC taxonomy display name to a compact nav label.
shorten_taxonomy <- function(taxonomy) {
  if (is.na(taxonomy)) return("Unknown")
  t <- gsub(" \\(.*\\)", "", taxonomy)
  t <- gsub("Clinic/Center.*", "Clinic", t)
  t <- gsub("Mental Health.*", "Mental Health", t)
  t <- gsub("Substance Abuse.*", "SUD", t)
  t <- gsub("Federally Qualified Health Center", "FQHC", t)
  return(t)
}

# Build a self-contained dashboard HTML file embedding all Plotly charts.
#
# plots     - named list of plotly figure objects (NULL slots are skipped)
# plot_info - parallel list of info lists, each with:
#               npi, name, address, claims, city, taxonomy_short
#               (metrics script also passes: beneficiaries, paid)
# title     - page <title> and <h1> text
# subtitle  - subtitle shown in header
# header_gradient - CSS gradient string for the header background
# extra_header_html - optional HTML string inserted after the header div
# card_stats_fn - optional function(info) -> HTML string for the plot-meta
#                 right-hand side; defaults to "Total Claims: N"
make_dashboard_html <- function(
    plots,
    plot_info,
    title,
    subtitle,
    header_gradient = "linear-gradient(135deg, #667eea 0%, #764ba2 100%)",
    extra_header_html = "",
    card_stats_fn = NULL
) {
  if (is.null(card_stats_fn)) {
    card_stats_fn <- function(info) {
      paste0("Total Claims: ", format(info$claims, big.mark = ","))
    }
  }

  css <- '
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            margin: 0;
            padding: 0;
            background: linear-gradient(to bottom, #f8f9fa 0%, #e9ecef 100%);
        }
        .header {
            color: white;
            padding: 30px 20px;
            text-align: center;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        .header h1 { margin: 0 0 10px 0; font-size: 32px; font-weight: 600; }
        .header p  { margin: 5px 0; font-size: 16px; opacity: 0.95; }
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
        .nav-link:hover { background: #667eea; color: white; transform: translateY(-2px); }
        .container { max-width: 1400px; margin: 0 auto; padding: 30px 20px; }
        .plot-card {
            background: white;
            margin-bottom: 40px;
            padding: 25px;
            border-radius: 12px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
            transition: transform 0.2s, box-shadow 0.2s;
        }
        .plot-card:hover { transform: translateY(-4px); box-shadow: 0 8px 24px rgba(0,0,0,0.12); }
        .plot-meta {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 15px 0;
            border-bottom: 2px solid #e9ecef;
            margin-bottom: 15px;
        }
        .plot-number { font-size: 18px; font-weight: 600; color: #667eea; }
        .plot-claims { font-size: 14px; color: #6c757d; }
        .footer { text-align: center; padding: 30px; color: #6c757d; font-size: 14px; }
'

  n <- length(plots)

  # Build navigation links
  nav_html <- ""
  for (i in seq_len(n)) {
    info <- plot_info[[i]]
    if (is.null(info)) next
    label <- if (info$npi == "ALL") {
      "\U0001F4CA All Providers"
    } else {
      paste0(info$city, " ", info$taxonomy_short)
    }
    nav_html <- paste0(nav_html,
      '<a href="#plot', i, '" class="nav-link">', label, '</a>')
  }

  # Build plot card divs
  cards_html <- ""
  for (i in seq_len(n)) {
    if (is.null(plots[[i]]) || is.null(plot_info[[i]])) next
    info <- plot_info[[i]]
    stats_html <- card_stats_fn(info)
    cards_html <- paste0(cards_html, '
        <div class="plot-card" id="plot', i, '">
            <div class="plot-meta">
                <div class="plot-number">Provider ', i, ' of ', n, '</div>
                <div class="plot-claims">', stats_html, '</div>
            </div>
            <div id="graph', i, '" style="width:100%;height:550px;"></div>
        </div>')
  }

  # Build Plotly JS calls
  plot_js <- ""
  for (i in seq_len(n)) {
    if (is.null(plots[[i]])) next
    plot_json <- plotly_json(plots[[i]], FALSE)
    plot_js <- paste0(plot_js,
      '\n        Plotly.newPlot("graph', i, '", ', plot_json, ');')
  }

  html <- paste0(
    '<!DOCTYPE html>\n<html>\n<head>\n',
    '    <title>', title, '</title>\n',
    '    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>\n',
    '    <style>\n', css, '\n    </style>\n</head>\n<body>\n',
    '    <div class="header" style="background: ', header_gradient, ';">\n',
    '        <h1>', title, '</h1>\n',
    '        <p>', subtitle, '</p>\n',
    '    </div>\n',
    extra_header_html,
    '    <div class="nav">\n',
    '        <div class="nav-links" id="nav-links"></div>\n',
    '    </div>\n',
    '    <div class="container">\n', cards_html, '\n    </div>\n',
    '    <div class="footer">Generated on ', format(Sys.Date(), "%B %d, %Y"),
    '<br>Data: Chestnut Health Systems Medicaid Claims</div>\n',
    '    <script>\n',
    '        document.getElementById("nav-links").innerHTML = `', nav_html, '`;\n',
    plot_js, '\n',
    '    </script>\n</body>\n</html>\n'
  )

  return(html)
}
