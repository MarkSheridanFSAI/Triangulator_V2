# Look and feel, and pieces of page used in several places ---------------------------

# The red from the original Triangulator. (The original title text used pure red,
# #FF0000; this slightly deeper red, also from the original, keeps white text readable.)
BRAND_RED <- "#c0392b"

# The original app's heading font. It loads from Google Fonts, as before; if
# there's no internet connection a similar built-in font is used instead.
HEADING_FONT_LINK <- "https://fonts.googleapis.com/css2?family=Lobster&display=swap"

triangulator_theme <- function() {
  system_fonts <- bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                                         "Helvetica Neue", "Arial", "sans-serif")
  bslib::bs_theme(
    version = 5,
    primary = "#2457a6",
    success = "#2e7d4f",
    base_font = system_fonts,
    heading_font = system_fonts,
    "border-radius" = "0.5rem"
  )
}

app_css <- function() {
  "
  .navbar-brand, .tri-heading, .tri-page h4, .tri-page h5, .sidebar-title {
    font-family: 'Lobster', 'Brush Script MT', 'Segoe Script', cursive; font-weight: 400;
    letter-spacing: 0.01em; }
  .navbar-brand { font-size: 1.75rem; }
  .tri-heading { font-size: 1.2rem; }
  .sidebar-title { font-size: 1.4rem; }
  .tri-page h5 { font-size: 1.45rem; }
  .tri-page { max-width: 1200px; margin: 0 auto; padding: 1rem 0.25rem 3rem; }
  .step-badge { display: inline-flex; align-items: center; justify-content: center;
                width: 1.8rem; height: 1.8rem; border-radius: 50%; flex-shrink: 0;
                background: var(--bs-primary); color: #fff; font-weight: 600; }
  .how-it-works { display: flex; flex-wrap: wrap; gap: 0.75rem; margin-bottom: 1.25rem; }
  .how-it-works > div { flex: 1 1 180px; background: var(--bs-light); border-radius: 0.5rem;
                        padding: 0.75rem 1rem; display: flex; gap: 0.6rem; align-items: center; }
  .field-help { font-size: 0.85rem; color: var(--bs-secondary-color); margin-top: -0.6rem;
                margin-bottom: 0.9rem; }
  .placeholder-box { text-align: center; color: var(--bs-secondary-color); padding: 1.5rem; }
  .placeholder-box .fa { font-size: 2rem; margin-bottom: 0.5rem; opacity: 0.6; }
  .tri-page h6 { font-weight: 600; }
  .section-note { color: var(--bs-secondary-color); margin-bottom: 0.75rem; }
  .bslib-value-box .value-box-value { font-size: 1.6rem; }
  .headline-boxes { display: grid; gap: 0.75rem;
                    grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); }
  .headline-boxes .value-box-value { font-size: 1.35rem; white-space: nowrap; }
  .headline-boxes .bslib-value-box { margin-bottom: 0; }
  .headline-boxes .value-box-area { padding: 0.75rem 1rem !important; }
  @media (max-width: 575px) {
    .headline-boxes { grid-template-columns: repeat(2, minmax(0, 1fr)); }
    .headline-boxes .value-box-value { font-size: 1.1rem; }
  }
  h6.mapping-group { margin-top: 0.75rem; text-transform: uppercase; letter-spacing: 0.04em;
                     font-size: 0.8rem; color: var(--bs-secondary-color); }
  "
}

step_badge <- function(n) tags$span(class = "step-badge", n)

step_header <- function(n, title) {
  card_header(class = "d-flex align-items-center gap-2", step_badge(n), tags$span(class = "tri-heading", title))
}

placeholder <- function(text, icon_name = "circle-info") {
  div(class = "placeholder-box", icon(icon_name), tags$p(text))
}

how_it_works <- function() {
  steps <- list(c("upload", "Upload your claims file"), c("table-columns", "Match your columns"),
                c("list-check", "Check your data"), c("chart-column", "Explore the dashboard"))
  div(class = "how-it-works",
      lapply(seq_along(steps), function(i) div(step_badge(i), tags$span(steps[[i]][2]))))
}

# An interactive table with copy / CSV / Excel buttons. Text is always escaped.
dt_table <- function(df, amount_cols = NULL, count_cols = NULL, percent_cols = NULL,
                     factor_cols = NULL, file_name = "triangulator_table", page_length = 15,
                     selection = "none", buttons = TRUE, caption = NULL, ...) {
  options <- utils::modifyList(
    list(scrollX = TRUE, pageLength = page_length, autoWidth = FALSE,
         dom = if (buttons) "Bfrtip" else "frtip"),
    list(...)
  )
  if (buttons) {
    options$buttons <- list(list(extend = "copy"),
                            list(extend = "csv", filename = file_name),
                            list(extend = "excel", filename = file_name))
  }
  out <- DT::datatable(df, rownames = FALSE, selection = selection, escape = TRUE,
                       extensions = if (buttons) "Buttons" else character(0),
                       options = options, caption = caption, class = "compact stripe hover")
  if (length(amount_cols))  out <- DT::formatRound(out, amount_cols, digits = 0)
  if (length(count_cols))   out <- DT::formatRound(out, count_cols, digits = 0)
  if (length(percent_cols)) out <- DT::formatPercentage(out, percent_cols, digits = 1)
  if (length(factor_cols))  out <- DT::formatRound(out, factor_cols, digits = 3)
  out
}

# Show date-time columns as plain dates (or date and time if they hold times).
readable_dates <- function(df) {
  for (col in names(df)) {
    x <- df[[col]]
    if (inherits(x, "POSIXt")) {
      times <- format(x[!is.na(x)], "%H:%M:%S")
      df[[col]] <- if (all(times == "00:00:00")) format(x, "%Y-%m-%d") else format(x, "%Y-%m-%d %H:%M")
    }
  }
  df
}

# Format the given columns of a table according to a measure's kind.
dt_by_kind <- function(df, value_cols, kind, ...) {
  switch(kind,
         percent = dt_table(df, percent_cols = value_cols, ...),
         factor  = dt_table(df, factor_cols = value_cols, ...),
         count   = dt_table(df, count_cols = value_cols, ...),
         dt_table(df, amount_cols = value_cols, ...))
}

# ---- Pages ---------------------------------------------------------------------------

dashboard_placeholder_ui <- function() {
  div(class = "tri-page",
      card(card_body(
        class = "placeholder-box py-5",
        icon("chart-column"),
        tags$h4("Your dashboard will appear here"),
        tags$p("Upload a claims file on the Import page (or try the sample data) and click ",
               tags$strong("Build my dashboard"), "."),
        actionButton("go_import", "Go to Import", icon = icon("upload"), class = "btn-primary")
      )))
}

dashboard_ui <- function() {
  div(class = "tri-page",
      uiOutput("dashboard_header"),
      navset_underline(
        id = "dashboard_tabs",
        nav_panel("Summary", icon = icon("gauge"), div(class = "pt-3", mod_summary_ui("summary"))),
        nav_panel("Claim sizes", icon = icon("chart-simple"), div(class = "pt-3", mod_sizes_ui("sizes"))),
        nav_panel("Origin analysis", icon = icon("chart-area"), div(class = "pt-3", mod_origin_ui("origin"))),
        nav_panel("Triangles", icon = icon("table-cells"), div(class = "pt-3", mod_triangle_ui("triangle"))),
        nav_panel("Reserves", icon = icon("scale-balanced"), div(class = "pt-3", mod_reserves_ui("reserves"))),
        nav_panel("Reserve ranges", icon = icon("chart-line"), div(class = "pt-3", mod_ranges_ui("ranges")))
      ))
}

GLOSSARY <- list(
  "Line-by-line claims data" = "A file with one row for each claim at each date its figures were recorded (a 'snapshot'). The same claim appears several times as it develops.",
  "Origin period" = "The period a claim belongs to: when the loss happened (accident), when it was reported, or when the policy started (underwriting).",
  "Development period" = "How many periods after the origin period the figures were recorded. Period 1 is the origin period itself.",
  "Triangle" = "A grid with origin periods down the side and development periods across the top. Older origins have more development, which gives the triangle shape.",
  "Paid / Incurred / Outstanding" = "Paid is the total paid so far. Outstanding is the case estimate still held. Incurred is paid plus outstanding.",
  "Settled claim" = "A claim whose outstanding case estimate is zero.",
  "Development factor (link ratio)" = "The value in one development period divided by the value in the period before. It shows how much a cohort of claims grew.",
  "Chain-ladder" = "A standard method that uses average development factors to project each origin period to its expected final ('ultimate') value.",
  "IBNER" = "Incurred But Not Enough Reported: expected growth on claims already reported.",
  "IBNR" = "Incurred But Not Reported: claims that have already happened but haven't been reported yet.",
  "URR (unexpired risk)" = "Claims still to happen on policies already written.",
  "Reserve range" = "How far the eventual cost could reasonably be from the best estimate. Triangulator measures it with Mack's method and shows the standard error and percentiles (for example, the 75th percentile is an amount the outcome should stay below 3 times out of 4).",
  "Large claims" = "Claims whose incurred amount is ever above the threshold you choose. They can be left out of the triangles so they don't distort the patterns."
)

help_page_ui <- function() {
  div(class = "tri-page",
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(7, 5)),
        card(card_header(class = "tri-heading", "Quick start"),
             card_body(
               tags$ol(
                 tags$li("Go to ", tags$strong("Import"), " and upload your claims file, or click ",
                         tags$strong("Try it with sample data"), "."),
                 tags$li("Check that each item points at the right column. Triangulator guesses from your column names."),
                 tags$li("Click ", tags$strong("Check my data"), ". Look at any warnings, then click ",
                         tags$strong("Build my dashboard"), "."),
                 tags$li("Explore the tabs on the dashboard. Every chart has a ", tags$strong("Table"),
                         " view with copy, CSV and Excel buttons, and you can download everything as one Excel report.")
               ),
               tags$h6(class = "mt-4", "What your file needs"),
               tags$ul(
                 tags$li("One row per claim per valuation date."),
                 tags$li("Columns for: claim ID, policy ID, valuation date, policy start date, incident date, report date, paid to date and outstanding case estimate."),
                 tags$li("Optionally, one or more grouping columns such as line of business."),
                 tags$li("Dates can be real Excel dates or text like 2023-01-31 or 31/01/2023 (text dates are read as day/month/year)."),
                 tags$li("Excel (.xlsx, .xls) or CSV, up to 100 MB and 2 million rows.")
               ),
               tags$h6(class = "mt-4", "Your data stays private"),
               tags$p("When you run Triangulator on your own computer, your file never leaves it. Nothing is saved after you close the app."),
               tags$h6(class = "mt-4", "Please note"),
               tags$p("Triangulator is a companion for exploring claims data. The reserve figures are automatic first estimates and must not be relied on for booking reserves. Use at your own risk.")
             )),
        card(card_header(class = "tri-heading", "Glossary"),
             card_body(do.call(accordion, c(
               lapply(names(GLOSSARY), function(term) accordion_panel(term, GLOSSARY[[term]])),
               list(open = FALSE)
             ))))
      ))
}
