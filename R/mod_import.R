# Import page: upload -> match columns -> check data -> build ----------------------

SAMPLE_DATA_FILE <- "sample_database.xlsx"
SAMPLE_MAPPING_FILE <- "sample_column_mapping.csv"

# Check the mapping, convert the columns and run the data checks.
# Returns list(status = "mapping", problems) or list(status = "checked", ...).
check_upload <- function(df, mapping) {
  problems <- validate_mapping(df, mapping)
  if (length(problems)) return(list(status = "mapping", problems = problems))
  std <- standardise_claims(df, mapping)
  if (length(std$problems)) return(list(status = "mapping", problems = std$problems))
  groupings <- mapping$groupings
  provided <- SINGLE_FIELDS[vapply(SINGLE_FIELDS, function(f) (mapping[[f]] %||% "") != "", logical(1))]
  list(
    status = "checked",
    std = std$data,
    checks = run_data_checks(std$data, provided),
    valuation_dates = dplyr::n_distinct(std$data$date_stamp, na.rm = TRUE),
    mapping = mapping,
    combinations = if (length(groupings)) nrow(dplyr::distinct(std$data[groupings])) else 1
  )
}

# A short, high-level list of the checks that didn't pass (the full table is in a dropdown).
check_overview <- function(ch) {
  flagged <- ch$summary[ch$summary$status != "Passed", ]
  if (nrow(flagged) == 0) {
    return(div(class = "alert alert-success mt-3 d-flex gap-2", icon("circle-check"),
               tags$span("All ", nrow(ch$summary), " checks passed.")))
  }
  div(class = "mt-3",
      tags$h6(paste0(nrow(flagged), " of ", nrow(ch$summary), " checks need a look:")),
      tags$ul(class = "mb-0", lapply(seq_len(nrow(flagged)), function(i) {
        tags$li(tags$strong(flagged$check[i], .noWS = "after"), ": ",
                scales::comma(flagged$rows_affected[i]),
                if (flagged$rows_affected[i] == 1) " row. " else " rows. ",
                tags$span(class = "text-muted", flagged$explanation[i]))
      })))
}

mod_import_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "tri-page",
    div(class = "import-lead",
        tags$h4("Quick claims analysis, no coding needed"),
        tags$p("Upload a claims listing or a set of bordereaux and Triangulator gives you instant diagnostics: ",
               "where your claims stand, how they compare across groups, how they develop over time, and a ",
               "first look at reserves. Your data stays on your computer.")),
    how_it_works(),
    card(
      class = "mb-4",
      step_header(1, "Upload your claims data"),
      card_body(
        fillable = FALSE,
        tags$p("Use a ", tags$strong("line-by-line"), " claims file: one row per claim, with its paid and ",
               "outstanding amounts. To see how claims develop over time, include each claim at several dates ",
               "(for example, monthly bordereaux combined into one file). Excel (.xlsx or .xls) or CSV, up to 100 MB."),
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(7, 5)),
          fileInput(ns("file"), "Choose your file", accept = c(".xlsx", ".xls", ".csv"), width = "100%"),
          div(class = "pt-md-4",
              actionButton(ns("use_sample"), "Try it with sample data", icon = icon("wand-magic-sparkles"),
                           class = "btn-outline-primary w-100"),
              div(class = "small text-muted mt-2",
                  "See the expected layout: ",
                  downloadLink(ns("dl_sample"), "sample data"), " \u00b7 ",
                  downloadLink(ns("dl_sample_map"), "sample column mapping")))
        ),
        uiOutput(ns("sheet_ui")),
        uiOutput(ns("file_status"))
      )
    ),
    card(class = "mb-4", step_header(2, "Tell Triangulator which column is which"),
         card_body(fillable = FALSE, uiOutput(ns("mapping_ui")))),
    card(class = "mb-4", step_header(3, "Check your data"),
         card_body(fillable = FALSE, uiOutput(ns("checks_ui")))),
    card(class = "mb-4", step_header(4, "Build your dashboard"),
         card_body(fillable = FALSE, uiOutput(ns("build_ui"))))
  )
}

# Returns a reactive value that is set when the user clicks "Build my dashboard".
mod_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    upload <- reactiveVal(NULL)       # the file the user loaded
    check_state <- reactiveVal(NULL)  # results of "Check my data"
    built <- reactiveVal(NULL)        # what the dashboard is built from

    load_file <- function(path, file_name, sheet = NULL, is_sample = FALSE) {
      check_state(NULL)
      df <- withProgress(message = "Reading your file...", value = 0.4, {
        run_safely(read_claims_file(path, file_name, sheet), doing = "reading your file")
      })
      if (is.null(df)) {
        upload(NULL)
        return(invisible())
      }
      sheets <- list_sheets(path, file_name)
      log_info("Loaded '", file_name, "': ", nrow(df), " rows, ", ncol(df), " columns")
      upload(list(df = df, file_name = file_name, path = path, sheets = sheets,
                  sheet = sheet %||% sheets[1], is_sample = is_sample))
    }

    observeEvent(input$file, load_file(input$file$datapath, input$file$name))
    observeEvent(input$use_sample, {
      load_file(app_path("www", SAMPLE_DATA_FILE), SAMPLE_DATA_FILE, is_sample = TRUE)
      if (!is.null(upload())) session$sendCustomMessage("triangulator-scroll", list(id = ns("run_checks")))
    })
    observeEvent(input$sheet, {
      u <- upload()
      req(u, !identical(input$sheet, u$sheet))
      load_file(u$path, u$file_name, input$sheet, u$is_sample)
    })

    output$sheet_ui <- renderUI({
      u <- upload()
      req(u, length(u$sheets) > 1)
      selectInput(ns("sheet"), "This file has several sheets. Which one holds the claims?",
                  choices = u$sheets, selected = u$sheet)
    })

    output$file_status <- renderUI({
      u <- upload()
      req(u)
      tagList(
        div(class = "alert alert-success d-flex align-items-center gap-2 mt-3 mb-2",
            icon("circle-check"),
            tags$span("Loaded ", tags$strong(u$file_name, .noWS = "outside"), ": ",
                      scales::comma(nrow(u$df)), " rows and ", ncol(u$df), " columns.")),
        accordion(open = FALSE, accordion_panel("Preview the first 10 rows", icon = icon("eye"),
                                                DT::DTOutput(ns("preview"))))
      )
    })
    output$preview <- DT::renderDT({
      u <- upload()
      req(u)
      dt_table(readable_dates(utils::head(u$df, 10)), buttons = FALSE, page_length = 10, dom = "t")
    })

    # ---- Step 2: column mapping ----
    starting_mapping <- reactive({
      u <- upload()
      req(u)
      if (u$is_sample) {
        read_mapping_file(app_path("www", SAMPLE_MAPPING_FILE), names(u$df))$mapping
      } else {
        guess_mapping(u$df)
      }
    })

    output$mapping_ui <- renderUI({
      u <- upload()
      if (is.null(u)) return(placeholder("Upload a file (or try the sample data) to continue."))
      start <- starting_mapping()
      cols <- names(u$df)
      choices <- c("Choose a column..." = "", cols)
      field_input <- function(field) {
        div(selectInput(ns(field), FIELDS[[field]]$label, choices = choices,
                        selected = start[[field]], width = "100%"),
            div(class = "field-help", FIELDS[[field]]$help))
      }
      two_cols <- function(...) layout_columns(col_widths = breakpoints(sm = 12, md = c(6, 6)), ...)
      fields <- tagList(
        tags$h6(class = "mapping-group", "Identifiers"),
        two_cols(field_input("claim_id"), field_input("policy_id")),
        tags$h6(class = "mapping-group", "Dates"),
        two_cols(field_input("date_stamp"), field_input("date_incident")),
        two_cols(field_input("date_reported"), field_input("date_issue")),
        tags$h6(class = "mapping-group", "Amounts"),
        two_cols(field_input("paid"), field_input("case")),
        tags$h6(class = "mapping-group", "Groupings (optional)"),
        selectizeInput(ns("groupings"), "Split the results by", choices = cols,
                       selected = start$groupings, multiple = TRUE, width = "100%",
                       options = list(placeholder = "Leave empty to look at all claims together")),
        div(class = "field-help",
            "For example line of business or region. Results are shown for each combination, ",
            "so choose columns with only a few different values."),
        tags$hr(),
        div(class = "d-flex flex-wrap gap-3 align-items-end",
            fileInput(ns("mapping_file"), "Load a saved mapping (.csv)", accept = ".csv"),
            div(class = "mb-3",
                downloadButton(ns("save_mapping"), "Save this mapping", class = "btn-outline-secondary")))
      )
      if (u$is_sample) {
        # The sample's columns are already right, so keep this step tidied away.
        tagList(
          div(class = "alert alert-success d-flex gap-2",
              icon("circle-check"),
              tags$span("The sample data's columns have been filled in for you, so you can go straight to ",
                        tags$strong("step 3", .noWS = "after"), ".")),
          accordion(open = FALSE,
                    accordion_panel("See how the columns were matched", icon = icon("table-columns"), fields))
        )
      } else {
        tagList(
          div(class = "alert alert-info d-flex gap-2",
              icon("lightbulb"),
              tags$span("We've made a best guess from your column names. Please check each one, then go to step 3. ",
                        "Items marked optional can be left blank.")),
          fields
        )
      }
    })

    current_mapping <- reactive({
      m <- lapply(SINGLE_FIELDS, function(f) input[[f]] %||% "")
      names(m) <- SINGLE_FIELDS
      m$groupings <- input$groupings %||% character(0)
      m
    })

    # Check results only count while the column choices are the ones they were run with.
    checks_current <- reactive({
      st <- check_state()
      !is.null(st) && identical(st$mapping_used, current_mapping())
    })

    observeEvent(input$mapping_file, {
      u <- upload()
      req(u)
      res <- run_safely(read_mapping_file(input$mapping_file$datapath, names(u$df)),
                        doing = "loading the mapping file")
      req(res)
      for (f in SINGLE_FIELDS) updateSelectInput(session, f, selected = res$mapping[[f]])
      updateSelectizeInput(session, "groupings", selected = res$mapping$groupings)
      if (length(res$problems)) {
        showNotification(tagList(tags$strong("Mapping loaded, with some gaps:"),
                                 tags$ul(lapply(res$problems, tags$li))),
                         type = "warning", duration = NULL)
      } else {
        showNotification("Mapping loaded.", type = "message")
      }
    })

    output$save_mapping <- downloadHandler(
      filename = function() "triangulator_column_mapping.csv",
      content = function(file) write_mapping_file(current_mapping(), file)
    )
    output$dl_sample <- downloadHandler(
      filename = function() SAMPLE_DATA_FILE,
      content = function(file) file.copy(app_path("www", SAMPLE_DATA_FILE), file)
    )
    output$dl_sample_map <- downloadHandler(
      filename = function() "sample_column_mapping.csv",
      content = function(file) file.copy(app_path("www", SAMPLE_MAPPING_FILE), file)
    )

    # ---- Step 3: checks ----
    output$checks_ui <- renderUI({
      if (is.null(upload())) return(placeholder("Complete steps 1 and 2 first."))
      tagList(
        tags$p("We'll check each column holds the right kind of data, then look for common ",
               "problems such as missing or out-of-order dates."),
        div(actionButton(ns("run_checks"), "Check my data", icon = icon("list-check"), class = "btn-primary")),
        uiOutput(ns("check_results"))
      )
    })

    observeEvent(input$run_checks, {
      u <- upload()
      req(u)
      mapping <- current_mapping()
      result <- withProgress(message = "Checking your data...", value = 0.5, {
        run_safely(check_upload(u$df, mapping), doing = "checking your data")
      })
      if (!is.null(result)) result$mapping_used <- mapping
      session$sendCustomMessage("triangulator-scroll", list(id = ns("results_top"), block = "start"))
      if (!is.null(result) && result$status == "checked") {
        log_info("Data checks done: score ", round(100 * result$checks$score, 2), "%, ",
                 result$checks$n_errors, " problem(s), ", result$checks$n_warnings, " warning(s)")
      }
      check_state(result)
    })

    output$check_results <- renderUI({
      st <- check_state()
      req(st)
      if (!checks_current()) {
        return(div(class = "alert alert-info mt-3",
                   "Your column choices have changed. Click ", tags$strong("Check my data"), " again."))
      }
      if (st$status == "mapping") {
        return(div(class = "alert alert-danger mt-3",
                   tags$strong("Please fix these before continuing:"),
                   tags$ul(class = "mb-0", lapply(st$problems, tags$li))))
      }
      ch <- st$checks
      score_theme <- if (ch$n_errors > 0) "danger" else if (ch$score >= 0.99) "success" else "warning"
      tagList(
        div(id = ns("results_top")),
        layout_column_wrap(
          width = "220px", fill = FALSE, class = "mt-3",
          value_box("Data score", scales::percent(ch$score, accuracy = 0.1), theme = score_theme,
                    showcase = icon("clipboard-check")),
          value_box("Problems", ch$n_errors, theme = if (ch$n_errors) "danger" else "light",
                    showcase = icon("circle-xmark")),
          value_box("Warnings", ch$n_warnings, theme = if (ch$n_warnings) "warning" else "light",
                    showcase = icon("triangle-exclamation"))
        ),
        if (isTRUE(st$valuation_dates == 1)) {
          div(class = "alert alert-info mt-3 d-flex gap-2", icon("circle-info"),
              tags$span(tags$strong("One valuation date. "), SINGLE_SNAPSHOT_MESSAGE,
                        " The summary, claim sizes, groups at a glance and trends will still work."))
        },
        if (st$combinations > 50) {
          div(class = "alert alert-warning mt-3",
              "Your grouping columns make ", st$combinations, " combinations. ",
              "That will be slow and the charts will be crowded - consider fewer grouping columns.")
        },
        if (ch$n_errors > 0) {
          div(class = "alert alert-danger mt-3",
              "Some rows are missing a claim ID or valuation date and will be left out. ",
              "You can carry on, but it's best to fix them in your file and upload it again.")
        },
        check_overview(ch),
        accordion(
          open = FALSE, class = "mt-3",
          accordion_panel(
            paste0("See all ", nrow(ch$summary), " checks in detail"), icon = icon("list-check"),
            tags$p(class = "small text-muted",
                   "The data score is the share of all row-by-row checks that passed. ",
                   "Click a check in the table to see the rows involved."),
            DT::DTOutput(ns("checks_table")),
            uiOutput(ns("failing_rows"))
          )
        )
      )
    })

    # ---- Step 4: build ----
    output$build_ui <- renderUI({
      st <- check_state()
      if (is.null(upload())) return(placeholder("Complete steps 1 to 3 first."))
      if (is.null(st) || !checks_current() || st$status != "checked") {
        return(placeholder("Check your data in step 3 first."))
      }
      ch <- st$checks
      tagList(
        tags$p("Triangulator will now build every view of your data. This usually takes a few seconds."),
        div(class = "d-flex flex-wrap align-items-center gap-3",
            actionButton(ns("build"), "Build my dashboard", icon = icon("play"), class = "btn-success btn-lg"),
            tags$span(class = "text-muted",
                      if (ch$n_errors + ch$n_warnings > 0) "Warnings don't stop the app from working."
                      else "Everything looks good.")))
    })

    output$checks_table <- DT::renderDT({
      st <- check_state()
      req(st, checks_current(), st$status == "checked")
      tbl <- st$checks$summary %>%
        transmute(Result = status, Check = check, `What it means` = explanation, `Rows affected` = rows_affected)
      dt_table(tbl, count_cols = "Rows affected", buttons = FALSE, selection = "single",
               page_length = 20, dom = "t", ordering = FALSE) %>%
        DT::formatStyle("Result", fontWeight = "bold",
                        backgroundColor = DT::styleEqual(c("Passed", "Warning", "Problem"),
                                                         c("#d1e7dd", "#fff3cd", "#f8d7da")))
    })

    output$failing_rows <- renderUI({
      st <- check_state()
      req(st, checks_current(), st$status == "checked")
      sel <- input$checks_table_rows_selected
      if (is.null(sel)) return(NULL)
      rows <- st$checks$failing_rows[[sel]]
      title <- st$checks$summary$check[sel]
      if (length(rows) == 0) {
        return(div(class = "alert alert-light mt-3", "No rows to show: '", title, "' passed."))
      }
      div(class = "mt-4",
          tags$h6(paste0("Rows that failed: ", title,
                         if (length(rows) > 1000) " (first 1,000 shown)" else "")),
          DT::DTOutput(ns("failing_table")))
    })

    output$failing_table <- DT::renderDT({
      st <- check_state()
      sel <- input$checks_table_rows_selected
      req(st, checks_current(), st$status == "checked", sel)
      rows <- utils::head(st$checks$failing_rows[[sel]], 1000)
      req(length(rows) > 0)
      df <- readable_dates(upload()$df[rows, , drop = FALSE])
      df <- cbind(`Row in your file` = rows + 1L, df)
      dt_table(df, page_length = 10, file_name = "rows_to_check")
    })

    observeEvent(input$build, {
      st <- check_state()
      req(st, checks_current(), st$status == "checked")
      fin <- run_safely(finalise_claims(st$std, st$mapping$groupings), doing = "preparing your data")
      req(fin)
      built(list(claims = fin$data, groupings = fin$groupings,
                 synthetic_grouping = length(st$mapping$groupings) == 0,
                 checks = st$checks, mapping = st$mapping, file_name = upload()$file_name,
                 id = paste0(format(Sys.time(), "%H%M%OS3"), sample.int(1e6, 1))))
    })

    built
  })
}
