# Small shared helpers, labels and settings ------------------------------------

# "a %||% b" means "a, unless a is missing (NULL), in which case b".
`%||%` <- function(a, b) if (is.null(a)) b else a

# Where the app's files live (www/ etc). Tests point this at the project folder.
app_path <- function(...) file.path(getOption("triangulator.app_dir", "."), ...)

# Write a time-stamped line to the R console so you can follow what the app is
# doing (and see where it stopped if something goes wrong).
log_info <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " [Triangulator] ", paste0(...))
}

# An error whose message is written for the person using the app. Anything
# raised with stop_friendly() is shown to them word for word.
stop_friendly <- function(...) {
  msg <- paste0(...)
  stop(structure(class = c("friendly_error", "error", "condition"),
                 list(message = msg, call = NULL)))
}

# Run some code; if it fails, show a clear pop-up message instead of crashing
# the app, write the details to the console, and return NULL.
run_safely <- function(expr, doing = "doing that", session = shiny::getDefaultReactiveDomain()) {
  tryCatch(
    expr,
    error = function(e) {
      friendly <- inherits(e, "friendly_error")
      log_info("ERROR while ", doing, ": ", conditionMessage(e))
      if (!is.null(session)) {
        shiny::showNotification(
          shiny::tagList(
            shiny::tags$strong(paste0("Sorry, there was a problem ", doing, ".")),
            shiny::tags$br(),
            if (friendly) conditionMessage(e)
            else paste0("Details: ", conditionMessage(e),
                        " (more information is in the R console)")
          ),
          type = "error", duration = NULL
        )
      }
      NULL
    }
  )
}

# ---- Choices shown in the app ------------------------------------------------

BASIS_CHOICES <- c(
  "Accident (incident date)"         = "incident",
  "Reported (report date)"           = "reporting",
  "Underwriting (policy start date)" = "underwriting"
)

PERIOD_CHOICES <- c("Monthly" = "monthly", "Quarterly" = "quarterly", "Yearly" = "yearly")

# Every measure the triangles hold, with a plain-English name.
MEASURE_LABELS <- c(
  ITOT                 = "Incurred (paid + outstanding)",
  PTOT                 = "Paid",
  ITOT_minus_PTOT      = "Outstanding (incurred - paid)",
  NTOT                 = "Claims reported (count)",
  STOT                 = "Claims settled (count)",
  NTOT_minus_STOT      = "Claims open (count)",
  ITOT_divided_by_NTOT = "Average incurred per claim",
  PTOT_divided_by_STOT = "Paid per settled claim",
  PTOT_divided_by_ITOT = "Paid as % of incurred",
  STOT_divided_by_NTOT = "Settled as % of reported",
  ITOT_links           = "Development factors: incurred",
  PTOT_links           = "Development factors: paid",
  NTOT_links           = "Development factors: reported count",
  STOT_links           = "Development factors: settled count"
)

measure_label <- function(code) unname(ifelse(code %in% names(MEASURE_LABELS), MEASURE_LABELS[code], code))

# Grouped choices for a drop-down list.
measure_choices <- function(include_factors = TRUE) {
  pick <- function(codes) stats::setNames(codes, MEASURE_LABELS[codes])
  out <- list(
    "Amounts" = pick(c("ITOT", "PTOT", "ITOT_minus_PTOT")),
    "Counts"  = pick(c("NTOT", "STOT", "NTOT_minus_STOT")),
    "Ratios"  = pick(c("ITOT_divided_by_NTOT", "PTOT_divided_by_STOT",
                       "PTOT_divided_by_ITOT", "STOT_divided_by_NTOT"))
  )
  if (include_factors) {
    out[["Development factors"]] <- pick(c("ITOT_links", "PTOT_links", "NTOT_links", "STOT_links"))
  }
  out
}

# How a measure should be displayed: "amount", "count", "percent" or "factor".
measure_kind <- function(code) {
  if (code %in% c("PTOT_divided_by_ITOT", "STOT_divided_by_NTOT")) return("percent")
  if (grepl("_links$", code)) return("factor")
  if (code %in% c("NTOT", "STOT", "NTOT_minus_STOT")) return("count")
  "amount"
}

# Format numbers for display.
fmt_amount <- function(x) scales::comma(x, accuracy = 1)

# ---- How amounts are shown (chosen at the top of the dashboard) -----------------

UNIT_CHOICES <- c("Full amounts" = "1", "Thousands (k)" = "1000", "Millions (m)" = "1000000")
CURRENCY_CHOICES <- c("No symbol" = "", "£" = "£", "€" = "€", "$" = "$")

display_settings <- function(divisor = 1, symbol = "") {
  divisor <- suppressWarnings(as.numeric(divisor))
  if (length(divisor) != 1 || is.na(divisor) || divisor <= 0) divisor <- 1
  symbol <- if (is.null(symbol) || is.na(symbol)) "" else as.character(symbol)
  word <- if (divisor == 1e6) "millions" else if (divisor == 1e3) "thousands" else ""
  note <- if (divisor == 1 && symbol == "") NULL
          else paste("Amounts in", paste(c(if (symbol != "") symbol, if (word != "") word), collapse = " "))
  list(divisor = divisor, symbol = symbol,
       suffix = if (divisor == 1e6) "m" else if (divisor == 1e3) "k" else "",
       digits = if (divisor > 1) 1 else 0, note = note)
}
DEFAULT_DISPLAY <- display_settings()

# An amount as text, e.g. "£1,234", "£1.2k" or "-£3.4m".
fmt_money <- function(x, ds = DEFAULT_DISPLAY) {
  out <- paste0(ifelse(x < 0, "-", ""), ds$symbol,
                scales::comma(abs(x) / ds$divisor, accuracy = 10^-ds$digits), ds$suffix)
  out[is.na(x)] <- ""
  out
}

# Turn an origin code (e.g. 202303) into a readable label ("2023-03", "2023 Q1", "2023").
format_origin <- function(origin, time) {
  origin <- as.integer(origin)
  year <- origin %/% 100L
  month <- origin %% 100L
  switch(time,
         monthly   = sprintf("%d-%02d", year, month),
         quarterly = sprintf("%d Q%d", year, month %/% 3L),
         yearly    = as.character(year))
}

# One readable label per row for the chosen grouping columns ("motor / property").
group_label <- function(df, groupings) {
  if (length(groupings) == 0) return(rep("All claims", nrow(df)))
  do.call(paste, c(lapply(groupings, function(g) as.character(df[[g]])), sep = " / "))
}

# Text that goes into charts is escaped so values in an uploaded file can never
# be treated as web-page code.
plot_safe <- function(x) htmltools::htmlEscape(as.character(x))

# Consistent colours for groups.
group_colours <- function(n) scales::viridis_pal(end = 0.9)(max(n, 1))[seq_len(n)]
