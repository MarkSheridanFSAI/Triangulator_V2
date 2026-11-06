# Dashboard tab: initial reserve estimates -----------------------------------------------

RESERVE_EXPLANATIONS <- list(
  "Paid" = "What has been paid on reported claims so far.",
  "Case reserves" = "The outstanding case estimates already held on reported claims.",
  "IBNER" = "Expected further growth on claims already reported (chain-ladder on the reported-date triangles).",
  "IBNR" = "Claims that have happened but haven't been reported yet (accident triangles compared with reported triangles).",
  "Unexpired risk (URR)" = "Claims still to happen on policies already written (underwriting triangles compared with accident triangles).",
  "Ultimate" = "The total of all of the above: the expected final cost.",
  "Reserve ranges" = "The Reserve ranges tab shows how uncertain these figures are. With incurred amounts and a single group its best estimate matches this tab; with paid amounts or all groups combined it will differ (the box on that tab explains why)."
)

mod_reserves_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 270, title = "Options",
      selectInput(ns("time"), "Period length", PERIOD_CHOICES, selected = "quarterly"),
      uiOutput(ns("group_ui")),
      tags$p(class = "small text-muted",
             "Development factors are volume-weighted averages over all origin periods, based on incurred amounts.")
    ),
    div(class = "alert alert-warning d-flex gap-2",
        icon("triangle-exclamation"),
        tags$span(tags$strong("A first look only. "),
                  "These estimates come from a basic chain-ladder applied automatically, so they can be volatile. ",
                  "Don't rely on them for booking reserves.")),
    navset_card_tab(
      full_screen = TRUE,
      title = tags$span(class = "tri-heading", "Initial reserve estimates"),
      nav_panel("Waterfall", plotOutput(ns("waterfall"), height = "460px")),
      nav_panel("Table", DT::DTOutput(ns("table"))),
      nav_panel("What do these mean?",
                tags$dl(class = "row mb-0",
                        lapply(names(RESERVE_EXPLANATIONS), function(term) tagList(
                          tags$dt(class = "col-sm-3", term),
                          tags$dd(class = "col-sm-9", RESERVE_EXPLANATIONS[[term]])
                        ))))
    )
  )
}

mod_reserves_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    components <- reactive({
      d <- data()
      req(d, input$time)
      res <- tryCatch(reserve_components(d$triangles, d$groupings, input$time),
                      friendly_error = function(e) e)
      if (inherits(res, "friendly_error")) validate(need(FALSE, conditionMessage(res)))
      res %>% mutate(group = group_label(., d$groupings))
    })

    output$group_ui <- renderUI({
      d <- data()
      req(d)
      if (isTRUE(d$synthetic_grouping)) return(NULL)
      labels <- sort(unique(group_label(d$current, d$groupings)))
      selectInput(session$ns("group"), "Group",
                  choices = c("All groups combined" = ALL_GROUPS, stats::setNames(labels, labels)),
                  selected = ALL_GROUPS)
    })

    outputOptions(output, "group_ui", suspendWhenHidden = FALSE)  # sidebar may be collapsed

    output$waterfall <- renderPlot({
      comp <- components()
      d <- data()
      chosen <- if (isTRUE(d$synthetic_grouping)) ALL_GROUPS else input$group
      req(chosen)
      if (!identical(chosen, ALL_GROUPS)) comp <- comp[comp$group == chosen, ]
      validate(need(nrow(comp) > 0, "No reserve figures for this group."))
      totals <- tapply(comp$value, comp$metric, sum, na.rm = TRUE)
      values <- stats::setNames(as.numeric(totals[RESERVE_METRICS]), RESERVE_METRICS)
      title <- if (identical(chosen, ALL_GROUPS)) "All groups" else chosen
      reserve_waterfall(values, title)
    }, res = 96)

    output$table <- DT::renderDT({
      d <- data()
      tbl <- reserve_table(components() %>% select(-group), d$groupings)
      if (isTRUE(d$synthetic_grouping)) tbl$`All groups` <- NULL
      dt_table(tbl, amount_cols = names(tbl)[-1], file_name = "initial_reserves", dom = "Bt",
               ordering = FALSE)
    })
  })
}
