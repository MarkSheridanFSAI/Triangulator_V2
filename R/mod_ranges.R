# Dashboard tab: reserve ranges (uncertainty) ----------------------------------------

# What the reserve from a given triangle covers, in words.
range_covers <- function(basis, measure) {
  parts <- c(if (measure == "PTOT") "case reserves", "IBNER",
             if (basis %in% c("incident", "underwriting")) "IBNR",
             if (basis == "underwriting") "unexpired risk (URR)")
  listed <- if (length(parts) > 1) paste(paste(utils::head(parts, -1), collapse = ", "), "and", utils::tail(parts, 1)) else parts
  paste0("This reserve covers ", listed, ". ",
         if (measure == "PTOT") "It projects payments, so it is everything still to be paid."
         else "It projects incurred amounts, so it sits on top of the case reserves already held.")
}

RANGE_COLUMN_LABELS <- c(origin = "Origin period", latest = "Latest value", ultimate = "Ultimate",
                         reserve = "Reserve", std_error = "Standard error", cv = "Uncertainty (CV)")

mod_ranges_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 280, title = "Options",
      selectInput(ns("basis"), "Origin basis", BASIS_CHOICES, selected = "incident"),
      selectInput(ns("time"), "Period length", PERIOD_CHOICES, selected = "quarterly"),
      selectInput(ns("measure"), "Project", c("Paid amounts" = "PTOT", "Incurred amounts" = "ITOT"),
                  selected = "PTOT"),
      uiOutput(ns("group_ui")),
      tags$p(class = "small text-muted", textOutput(ns("covers"), inline = TRUE))
    ),
    div(class = "alert alert-warning d-flex gap-2",
        icon("triangle-exclamation"),
        tags$span(tags$strong("A guide to uncertainty, not a precise answer. "),
                  "Ranges use Mack's method on the chain-ladder shown here. They assume the past ",
                  "development pattern will continue, and don't allow for things like inflation ",
                  "shocks or changes in claims handling.")),
    uiOutput(ns("headline")),
    uiOutput(ns("comparison")),
    navset_card_tab(
      full_screen = TRUE,
      title = tags$span(class = "tri-heading", "Reserve ranges"),
      nav_panel("Range of outcomes",
                plotlyOutput(ns("distribution"), height = "420px"),
                tags$p(class = "small text-muted mt-2 mb-0",
                       "The curve shows how likely different reserve amounts are. The shaded areas hold ",
                       "the middle 50% and 90% of outcomes; the dashed lines mark the percentiles.")),
      nav_panel("By origin period",
                plotlyOutput(ns("by_origin"), height = "420px"),
                tags$p(class = "small text-muted mt-2 mb-0",
                       "Bars show each origin period's reserve; the lines show plus or minus one standard error.")),
      nav_panel("Table",
                tags$h6("Percentiles of the total reserve"),
                DT::DTOutput(ns("percentiles")),
                tags$h6(class = "mt-4", "By origin period"),
                DT::DTOutput(ns("table"))),
      nav_panel("How it works",
                tags$p("Triangulator uses ", tags$strong("Mack's method"), " (Mack, 1993), the standard way to measure ",
                       "how uncertain a chain-ladder reserve is. It looks at how much the development factors ",
                       "varied between origin periods in the past, and combines two sources of uncertainty:"),
                tags$ul(
                  tags$li(tags$strong("Process error:"), " claims are random, so the outcome would vary even if we knew the true development pattern."),
                  tags$li(tags$strong("Parameter error:"), " the development factors are estimated from limited data, so they could be wrong.")
                ),
                tags$p("The ", tags$strong("standard error"), " measures the typical size of the difference between the estimate and ",
                       "the eventual outcome. The ", tags$strong("CV"), " (coefficient of variation) is the standard error as a percentage ",
                       "of the reserve: higher means more uncertain."),
                tags$p("Percentiles assume the total reserve follows a lognormal distribution with that mean and standard error. ",
                       "For example, the 75th percentile is the amount the outcome is expected to stay below 3 times out of 4. ",
                       "The 99.5th percentile is often used for 1-in-200-year capital measures."),
                tags$p("Uncertainty for 'All groups combined' is worked out on the combined triangle, so it is usually ",
                       "lower than adding up each group's range."),
                tags$h6(class = "mt-3", "Why the best estimate can differ from the Reserves tab"),
                tags$p("With ", tags$strong("incurred amounts"), " and a ", tags$strong("single group"),
                       ", the best estimate here is exactly the same chain-ladder as the Reserves tab. It can differ when:"),
                tags$ul(
                  tags$li(tags$strong("You project paid amounts."), " The Reserves tab always projects incurred amounts ",
                          "(paid plus case estimates). Paid projections ignore case estimates, so they give a different, ",
                          "independent view. Comparing the two is a useful check in itself."),
                  tags$li(tags$strong("You look at all groups combined."), " Here the groups are added together into one ",
                          "triangle first. The Reserves tab projects each group on its own and adds up the results.")
                ),
                tags$p("The box under the headline figures shows the matching Reserves tab figure for your current choices."))
    )
  )
}

mod_ranges_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
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

    output$covers <- renderText({
      req(input$basis, input$measure)
      range_covers(input$basis, input$measure)
    })

    ranges <- reactive({
      d <- data()
      req(d, input$basis, input$time, input$measure)
      tri <- d$triangles[[paste(input$basis, input$time, sep = "_")]]
      validate(need(!is.null(tri), "None of your rows have the date needed for this origin basis."))
      chosen <- if (isTRUE(d$synthetic_grouping)) ALL_GROUPS else input$group
      req(chosen)
      if (!identical(chosen, ALL_GROUPS)) tri <- tri[group_label(tri, d$groupings) == chosen, ]
      validate(need(nrow(tri) > 0, "No data for this group."))
      combined <- select_groups(tri, d$groupings, list())
      res <- tryCatch(reserve_ranges(combined, input$measure), friendly_error = function(e) e)
      if (inherits(res, "friendly_error")) validate(need(FALSE, conditionMessage(res)))
      res$time <- input$time
      res
    })

    output$headline <- renderUI({
      r <- ranges()
      tot <- r$total
      pct <- r$percentiles
      tagList(
      if (tot$reserve < 0) {
        div(class = "alert alert-info",
            tags$strong("The best estimate is negative. "),
            "In this data, amounts have tended to fall as claims develop (for example because case ",
            "estimates were set cautiously), so the chain-ladder expects them to keep falling. ",
            "The uncertainty percentage isn't meaningful for a negative reserve, so it shows n/a.")
      },
      div(class = "headline-boxes mb-3",
          value_box("Best estimate reserve", fmt_amount(tot$reserve)),
          value_box("Standard error", fmt_amount(tot$std_error)),
          value_box("Uncertainty (CV)", if (is.na(tot$cv)) "n/a" else scales::percent(tot$cv, accuracy = 1)),
          value_box("75th percentile", fmt_amount(pct$reserve[pct$percentile == 0.75])),
          value_box("99.5th percentile", fmt_amount(pct$reserve[pct$percentile == 0.995]))))
    })

    output$comparison <- renderUI({
      r <- ranges()
      d <- data()
      comps <- tryCatch(reserve_components(d$triangles, d$groupings, input$time),
                        friendly_error = function(e) NULL)
      if (is.null(comps)) return(NULL)
      chosen <- if (isTRUE(d$synthetic_grouping)) ALL_GROUPS else input$group
      n_groups <- length(unique(group_label(d$current, d$groupings)))
      cmp <- compare_with_reserves(r$total$reserve, comps, d$groupings, input$basis, input$measure,
                                   chosen, n_groups)
      labels <- unname(RESERVE_LABELS[cmp$parts])
      bold <- function(...) tags$strong(..., .noWS = "outside")
      matches <- length(cmp$reasons) == 0
      div(class = paste("alert mb-3", if (matches) "alert-success" else "alert-light border"),
          tags$p(class = "mb-1",
                 bold("Compared with the Reserves tab: "),
                 paste0("the matching figure there (", paste(labels, collapse = " + "), ", ",
                        tolower(names(PERIOD_CHOICES)[PERIOD_CHOICES == input$time]), ") is "),
                 bold(fmt_amount(cmp$reserves_tab)),
                 if (matches) ". This tab's best estimate matches it."
                 else tagList(". This tab's best estimate is ", bold(fmt_amount(abs(cmp$difference))),
                              paste0(if (cmp$difference > 0) " higher" else " lower", ". Why:"))),
          if (!matches) tags$ul(class = "mb-0 small", lapply(cmp$reasons, tags$li)))
    })

    output$distribution <- renderPlotly({
      r <- ranges()
      plot_reserve_distribution(r$total$reserve, r$total$std_error, r$percentiles)
    })

    output$by_origin <- renderPlotly({
      r <- ranges()
      plot_reserve_by_origin(r$by_origin, r$time)
    })

    output$percentiles <- DT::renderDT({
      p <- ranges()$percentiles
      tbl <- dplyr::tibble(Percentile = p$label, Reserve = p$reserve)
      dt_table(tbl, amount_cols = "Reserve", dom = "Bt", ordering = FALSE, file_name = "reserve_percentiles")
    })

    output$table <- DT::renderDT({
      r <- ranges()
      tbl <- r$by_origin %>%
        mutate(origin = format_origin(origin, r$time)) %>%
        select(names(RANGE_COLUMN_LABELS))
      total <- r$total %>% mutate(origin = "Total") %>% select(names(RANGE_COLUMN_LABELS))
      tbl <- bind_rows(tbl, total)
      names(tbl) <- unname(RANGE_COLUMN_LABELS)
      dt_table(tbl, amount_cols = c("Latest value", "Ultimate", "Reserve", "Standard error"),
               percent_cols = "Uncertainty (CV)", page_length = 50, ordering = FALSE,
               file_name = "reserve_ranges")
    })
  })
}
