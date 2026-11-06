# The downloadable Excel report ------------------------------------------------------

build_report_workbook <- function(d) {
  wb <- openxlsx::createWorkbook()
  header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F1", border = "bottom")
  note_style <- openxlsx::createStyle(fontColour = "#555555", textDecoration = "italic")
  number <- openxlsx::createStyle(numFmt = "#,##0")

  add_sheet <- function(name, df, note = NULL, number_cols = NULL) {
    openxlsx::addWorksheet(wb, name)
    start <- 1
    if (!is.null(note)) {
      openxlsx::writeData(wb, name, note, startRow = 1)
      openxlsx::addStyle(wb, name, note_style, rows = 1, cols = 1)
      start <- 3
    }
    openxlsx::writeData(wb, name, as.data.frame(df), startRow = start, headerStyle = header)
    if (length(number_cols) && nrow(df) > 0) {
      openxlsx::addStyle(wb, name, number, rows = start + seq_len(nrow(df)), cols = number_cols,
                         gridExpand = TRUE)
    }
    openxlsx::setColWidths(wb, name, cols = seq_len(ncol(df)), widths = "auto")
    openxlsx::freezePane(wb, name, firstActiveRow = start + 1)
  }
  show_groups <- function(df) display_groups(df, d)

  # About
  m <- d$mapping
  about <- data.frame(
    Item = c("Report created", "Source file", "Rows used", "Claims", "Latest valuation date",
             vapply(SINGLE_FIELDS, function(f) paste("Column used for", field_label(f)), character(1)),
             "Grouping columns"),
    Value = c(format(Sys.time(), "%Y-%m-%d %H:%M"), d$file_name, scales::comma(nrow(d$claims)),
              scales::comma(nrow(d$current)), format(d$valuation),
              vapply(SINGLE_FIELDS, function(f) m[[f]], character(1)),
              if (length(m$groupings)) paste(m$groupings, collapse = ", ") else "(none)"),
    stringsAsFactors = FALSE
  )
  add_sheet("About", about,
            note = "Created by Triangulator. Reserve figures are automatic first estimates - don't rely on them for booking reserves.")

  checks <- d$checks$summary %>%
    transmute(Result = status, Check = check, `What it means` = explanation, `Rows affected` = rows_affected)
  add_sheet("Data checks", checks, note = paste0("Data score: ", scales::percent(d$checks$score, accuracy = 0.1)))

  summ <- summary_by_group(d$current, d$groupings)
  names(summ)[match(names(SUMMARY_LABELS), names(summ))] <- unname(SUMMARY_LABELS)
  add_sheet("Summary", show_groups(summ), note = "Each claim's latest position.")

  sizes <- claim_size_buckets(d$current, d$groupings, suggest_bucket_size(d$current))
  sizes$bucket <- as.character(sizes$bucket)
  names(sizes)[1] <- "Incurred band"
  add_sheet("Claim sizes", sizes, note = "Number of claims by latest incurred amount.",
            number_cols = 2:ncol(sizes))

  for (time in PERIOD_CHOICES) {
    comp <- tryCatch(reserve_components(d$triangles, d$groupings, time), friendly_error = function(e) NULL)
    if (is.null(comp)) next
    tbl <- reserve_table(comp, d$groupings)
    if (isTRUE(d$synthetic_grouping)) tbl$`All groups` <- NULL
    add_sheet(paste("Reserves", time), tbl, number_cols = 2:ncol(tbl),
              note = paste("Initial reserve estimates from", time, "triangles (basic chain-ladder)."))
  }

  for (key in names(d$triangles)) {
    tri <- d$triangles[[key]]
    if (is.null(tri)) next
    time <- sub("^[a-z]+_", "", key)
    tri <- tri %>%
      mutate(measure = measure_label(variable), origin_period = format_origin(origin, time)) %>%
      select(all_of(d$groupings), measure, variable, origin_period, development, value)
    add_sheet(key, show_groups(tri),
              note = paste("All claims (none left out). One row per triangle cell; filter the 'measure' column",
                           "and use a pivot table to lay it out as a grid."))
  }
  wb
}
