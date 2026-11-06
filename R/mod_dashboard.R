# Dashboard tabs: Summary, Claim sizes, Origin analysis ----------------------------
# Each module receives `data`, a reactive holding everything built from the upload.

# Rename the internal "all_claims" column when the user chose no groupings.
display_groups <- function(df, d) {
  if (isTRUE(d$synthetic_grouping)) names(df)[names(df) == "all_claims"] <- "Group"
  df
}

# ---- Summary ----------------------------------------------------------------------

mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$p(class = "section-note",
           "Each claim's latest position. A claim counts as settled when its outstanding case estimate is zero."),
    navset_card_tab(
      full_screen = TRUE,
      nav_panel("Charts",
                layout_columns(
                  col_widths = breakpoints(sm = 12, lg = c(6, 6)),
                  div(tags$h6("Incurred: settled vs open claims"), plotlyOutput(ns("amounts"), height = "320px")),
                  div(tags$h6("Number of claims: settled vs open"), plotlyOutput(ns("counts"), height = "320px"))
                ),
                tags$h6(class = "mt-3", "Average incurred per claim"),
                plotlyOutput(ns("averages"), height = "340px")),
      nav_panel("Table", DT::DTOutput(ns("table")))
    )
  )
}

mod_summary_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    summ <- reactive({
      d <- data()
      req(d)
      summary_by_group(d$current, d$groupings)
    })
    output$amounts  <- renderPlotly(plot_status_amounts(summ(), data()$groupings))
    output$counts   <- renderPlotly(plot_status_counts(summ(), data()$groupings))
    output$averages <- renderPlotly(plot_averages(summ(), data()$groupings))
    output$table <- DT::renderDT({
      d <- data()
      tbl <- summ()
      label_cols <- names(SUMMARY_LABELS)
      names(tbl)[match(label_cols, names(tbl))] <- unname(SUMMARY_LABELS)
      tbl <- display_groups(tbl, d)
      pct <- unname(SUMMARY_LABELS[c("paid_pct_of_incurred", "settled_pct_of_reported")])
      dt_table(tbl, amount_cols = setdiff(unname(SUMMARY_LABELS), pct), percent_cols = pct,
               file_name = "claims_summary")
    })
  })
}

# ---- Claim sizes -------------------------------------------------------------------

mod_sizes_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 270, title = "Options",
      uiOutput(ns("controls")),
      tags$p(class = "small text-muted",
             "Claims are placed by their latest incurred amount. Each band includes its upper limit, ",
             "so a claim of exactly 5,000 is in 0 - 5,000.")
    ),
    navset_card_tab(
      full_screen = TRUE,
      title = "Number of claims by size",
      nav_panel("Chart", plotlyOutput(ns("chart"), height = "440px")),
      nav_panel("Table", DT::DTOutput(ns("table")))
    )
  )
}

mod_sizes_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$controls <- renderUI({
      d <- data()
      req(d)
      selectInput(session$ns("size"), "Band width",
                  choices = stats::setNames(BUCKET_CHOICES, fmt_amount(BUCKET_CHOICES)),
                  selected = suggest_bucket_size(d$current))
    })
    outputOptions(output, "controls", suspendWhenHidden = FALSE)  # sidebar may be collapsed
    buckets <- reactive({
      d <- data()
      req(d, input$size)
      claim_size_buckets(d$current, d$groupings, as.numeric(input$size))
    })
    output$chart <- renderPlotly(plot_claim_sizes(buckets()))
    output$table <- DT::renderDT({
      b <- buckets()
      b$bucket <- as.character(b$bucket)
      names(b)[1] <- "Incurred band"
      dt_table(b, count_cols = names(b)[-1], file_name = "claim_sizes", page_length = 25)
    })
  })
}

# ---- Origin analysis --------------------------------------------------------------

mod_origin_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 270, title = "Options",
      selectInput(ns("basis"), "Origin basis", BASIS_CHOICES, selected = "incident"),
      selectInput(ns("time"), "Period length", PERIOD_CHOICES, selected = "quarterly"),
      selectInput(ns("measure"), "Measure", measure_choices(include_factors = FALSE),
                  selected = "ITOT_minus_PTOT"),
      tags$p(class = "small text-muted",
             "Shows each origin period's latest value: where it stands at the most recent valuation.")
    ),
    tags$h5(class = "mb-0", textOutput(ns("title"), inline = TRUE)),
    navset_card_tab(
      full_screen = TRUE,
      nav_panel("Chart", plotlyOutput(ns("chart"), height = "440px")),
      nav_panel("Table", DT::DTOutput(ns("table")))
    )
  )
}

mod_origin_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    oa <- reactive({
      d <- data()
      req(d, input$basis, input$time, input$measure)
      tri <- d$triangles[[paste(input$basis, input$time, sep = "_")]]
      validate(need(!is.null(tri), "None of your rows have the date needed for this origin basis."))
      origin_analysis(tri, d$groupings, input$measure, input$time)
    })
    output$title <- renderText(paste0(measure_label(input$measure), " by origin period"))
    output$chart <- renderPlotly(plot_origin(oa(), measure_kind(input$measure)))
    output$table <- DT::renderDT({
      tbl <- oa()
      names(tbl)[1] <- "Origin period"
      dt_by_kind(tbl, names(tbl)[-1], measure_kind(input$measure), file_name = "origin_analysis",
                 page_length = 25)
    })
  })
}
