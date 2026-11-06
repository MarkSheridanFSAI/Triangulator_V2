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
    tab_intro("The big picture, using each claim's latest position. A claim counts as settled when its ",
              "outstanding case estimate is zero. 'Groups at a glance' compares your groups side by side."),
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
      nav_panel("Groups at a glance",
                tags$p(class = "small text-muted",
                       "Key diagnostics for each group, side by side. In each column the highest group is shaded ",
                       tags$span(class = "glance-high", "orange"), " and the lowest ",
                       tags$span(class = "glance-low", "blue", .noWS = "after"), ". ",
                       textOutput(ns("glance_notes"), inline = TRUE)),
                DT::DTOutput(ns("glance"))),
      nav_panel("Table", DT::DTOutput(ns("table")))
    )
  )
}

mod_summary_server <- function(id, data, display = reactive(DEFAULT_DISPLAY)) {
  moduleServer(id, function(input, output, session) {
    summ <- reactive({
      d <- data()
      req(d)
      summary_by_group(d$current, d$groupings)
    })
    output$amounts  <- renderPlotly(plot_status_amounts(summ(), data()$groupings, display()))
    output$counts   <- renderPlotly(plot_status_counts(summ(), data()$groupings))
    output$averages <- renderPlotly(plot_averages(summ(), data()$groupings, display()))
    large_threshold <- reactive(suggest_large_threshold(data()$current))
    glance <- reactive({
      d <- data()
      req(d)
      group_scorecard(d$claims, d$current, d$groupings, large_threshold())
    })
    output$glance_notes <- renderText({
      paste0("A large claim is one whose incurred has ever been above ", fmt_money(large_threshold(), display()),
             " (about the largest 5% of claims). Days to report run from the incident date to the report date. ",
             "Days to settle run from the report date to the first valuation date from which the claim ",
             "stayed settled, so they're only as precise as how often your data is valued.")
    })
    output$glance <- DT::renderDT({
      g <- glance()
      tbl <- g
      names(tbl) <- unname(GLANCE_LABELS[names(tbl)])
      # Hidden marker columns say which cells are the highest / lowest of the groups.
      compared <- c("average_claim", "settled_pct", "paid_pct", "large_pct", "days_to_report", "days_to_settle")
      groups_only <- g$group != "All groups"
      markers <- paste0("marker_", compared)
      for (i in seq_along(compared)) {
        vals <- g[[compared[i]]]
        flag <- rep("", nrow(g))
        if (sum(groups_only) > 1 && length(unique(stats::na.omit(vals[groups_only]))) > 1) {
          flag[groups_only & vals == max(vals[groups_only], na.rm = TRUE)] <- "high"
          flag[groups_only & vals == min(vals[groups_only], na.rm = TRUE)] <- "low"
        }
        flag[is.na(flag)] <- ""
        tbl[[markers[i]]] <- flag
      }
      pct <- c("Settled %", "Paid % of incurred", "Large claims %")
      days <- c("Average days to report", "Average days to settle")
      hidden <- which(names(tbl) %in% markers) - 1L
      out <- dt_table(tbl, amount_cols = "Average claim", count_cols = "Claims", factor_cols = days, percent_cols = pct,
                      file_name = "groups_at_a_glance", dom = "Bt", ordering = FALSE, display = display(),
                      columnDefs = list(list(visible = FALSE, targets = hidden)))
      out <- DT::formatRound(out, days, digits = 1)
      for (i in seq_along(compared)) {
        out <- DT::formatStyle(out, GLANCE_LABELS[[compared[i]]], valueColumns = markers[i],
                               backgroundColor = DT::styleEqual(c("high", "low"), c("#fde2c4", "#d6e4f5")))
      }
      out %>% DT::formatStyle("Group", target = "row",
                              fontWeight = DT::styleEqual("All groups", "bold"))
    })

    output$table <- DT::renderDT({
      d <- data()
      tbl <- summ()
      label_cols <- names(SUMMARY_LABELS)
      names(tbl)[match(label_cols, names(tbl))] <- unname(SUMMARY_LABELS)
      tbl <- display_groups(tbl, d)
      pct <- unname(SUMMARY_LABELS[c("paid_pct_of_incurred", "settled_pct_of_reported")])
      counts <- unname(SUMMARY_LABELS[c("claims_reported", "claims_settled", "claims_open")])
      dt_table(tbl, amount_cols = setdiff(unname(SUMMARY_LABELS), c(pct, counts)), count_cols = counts,
               percent_cols = pct, file_name = "claims_summary", display = display())
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
    tab_intro("How many claims fall into each size band, using each claim's latest incurred amount. ",
              "Useful for spotting large losses and seeing what a typical claim looks like."),
    navset_card_tab(
      full_screen = TRUE,
      title = tags$span(class = "tri-heading", "Number of claims by size"),
      nav_panel("Chart", plotlyOutput(ns("chart"), height = "440px")),
      nav_panel("Table", DT::DTOutput(ns("table")))
    )
  )
}

mod_sizes_server <- function(id, data, display = reactive(DEFAULT_DISPLAY)) {
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
      ds <- display()
      claim_size_buckets(d$current, d$groupings, as.numeric(input$size), label_fn = function(x) fmt_money(x, ds))
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
      selectInput(ns("basis"), "Group claims by", BASIS_CHOICES, selected = "incident"),
      selectInput(ns("time"), "Period length", PERIOD_CHOICES, selected = "quarterly"),
      selectInput(ns("measure"), "Measure", measure_choices(include_factors = FALSE),
                  selected = "ITOT_minus_PTOT"),
      checkboxInput(ns("compare"), "Compare each group with all groups", value = FALSE),
      tags$p(class = "small text-muted",
             "Shows each origin period's latest value: where it stands at the most recent valuation. ",
             "When comparing, amounts and counts show each group's share of the total, and ratios show ",
             "how far each group is above or below the figure for all groups.")
    ),
    tab_intro("Compare periods side by side: where each period's claims stand today. ",
              "For example, choose 'Average incurred per claim' to see whether claims are getting more expensive."),
    tags$h5(class = "mb-0", textOutput(ns("title"), inline = TRUE)),
    navset_card_tab(
      full_screen = TRUE,
      nav_panel("Chart", plotlyOutput(ns("chart"), height = "440px")),
      nav_panel("Table", DT::DTOutput(ns("table")))
    )
  )
}

mod_origin_server <- function(id, data, display = reactive(DEFAULT_DISPLAY)) {
  moduleServer(id, function(input, output, session) {
    oa <- reactive({
      d <- data()
      req(d, input$basis, input$time, input$measure)
      tri <- d$triangles[[paste(input$basis, input$time, sep = "_")]]
      validate(need(!is.null(tri), basis_missing_message(input$basis)))
      out <- origin_analysis(tri, d$groupings, input$measure, input$time)
      if (comparing()) {
        validate(need(ncol(out) > 2, "There's only one group, so there's nothing to compare."))
        all_groups <- origin_analysis(select_groups(tri, d$groupings, list()), character(0),
                                      input$measure, input$time)
        out <- compare_origin_analysis(out, all_groups, input$measure)
      }
      out
    })
    comparing <- reactive(isTRUE(input$compare))
    shown_kind <- reactive(if (comparing()) relative_kind(input$measure) else measure_kind(input$measure))
    output$title <- renderText({
      req(input$measure)
      paste0(measure_label(input$measure), " by origin period",
             if (comparing()) paste0(" - ", relative_label(input$measure)) else "")
    })
    output$chart <- renderPlotly({
      plot_origin(oa(), shown_kind(), display(),
                  stack = !comparing() && measure_kind(input$measure) %in% c("amount", "count"))
    })
    output$table <- DT::renderDT({
      tbl <- oa()
      names(tbl)[1] <- "Origin period"
      dt_by_kind(tbl, names(tbl)[-1], shown_kind(), file_name = "origin_analysis",
                 page_length = 25, display = display())
    })
  })
}
