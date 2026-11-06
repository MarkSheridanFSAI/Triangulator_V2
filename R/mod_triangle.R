# Dashboard tab: explore a triangle and drill into the claims behind a cell -----------

MOVEMENT_LABELS <- c(
  policy_id = "Policy ID", claim_id = "Claim ID",
  share_of_change = "Share of the cell's change", percent_change = "% change in incurred",
  ITOT_movement = "Change in incurred", PTOT_movement = "Change in paid",
  NTOT_movement = "New claims", STOT_movement = "Newly settled"
)

# A sensible default large-claim threshold: about the 95th percentile of
# claims' latest incurred, rounded to two significant figures.
suggest_large_threshold <- function(current) {
  q <- stats::quantile(current$ITOT, 0.95, na.rm = TRUE, names = FALSE)
  if (is.na(q) || q <= 0) 100000 else signif(q, 2)
}

mod_triangle_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 300, title = "Choose a triangle",
      selectInput(ns("basis"), "Origin basis", BASIS_CHOICES, selected = "incident"),
      selectInput(ns("time"), "Period length", PERIOD_CHOICES, selected = "quarterly"),
      selectInput(ns("measure"), "Measure", measure_choices(), selected = "ITOT"),
      uiOutput(ns("group_controls")),
      tags$hr(),
      checkboxInput(ns("exclude_large"), "Leave out large claims", value = FALSE),
      conditionalPanel("input.exclude_large", ns = ns, uiOutput(ns("threshold_ui")))
    ),
    tags$h5(class = "mb-0", textOutput(ns("title"), inline = TRUE)),
    navset_card_tab(
      full_screen = TRUE,
      nav_panel("Heatmap",
                plotlyOutput(ns("heatmap"), height = "520px"),
                tags$p(class = "small text-muted mt-2 mb-0",
                       "Rows are origin periods, columns are development periods. ",
                       "A red circle marks a value more than 3 standard deviations from the average of ",
                       "its development period - worth a closer look.")),
      nav_panel("Table", DT::DTOutput(ns("grid")))
    ),
    card(
      card_header(class = "fw-semibold", "What changed in one cell?"),
      card_body(
        tags$p(class = "small text-muted",
               "Pick a cell to see which claims changed since their previous valuation. ",
               "Click a claim in the table to see its full history."),
        layout_columns(col_widths = breakpoints(sm = 12, md = c(6, 6)),
                       uiOutput(ns("origin_ui")), uiOutput(ns("dev_ui"))),
        uiOutput(ns("movement_summary")),
        DT::DTOutput(ns("movements"))
      )
    ),
    uiOutput(ns("claim_card"))
  )
}

mod_triangle_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cache <- new.env(parent = emptyenv())   # triangles without large claims, kept for re-use

    output$group_controls <- renderUI({
      d <- data()
      req(d)
      if (isTRUE(d$synthetic_grouping)) return(NULL)
      lapply(seq_along(d$groupings), function(i) {
        g <- d$groupings[i]
        values <- sort(unique(d$claims[[g]]))
        selectInput(ns(paste0("group_", i)), g,
                    choices = c("All (combined)" = ALL_GROUPS, stats::setNames(values, values)),
                    selected = ALL_GROUPS)
      })
    })

    selection <- reactive({
      d <- data()
      req(d)
      if (isTRUE(d$synthetic_grouping)) return(list(all_claims = ALL_GROUPS))
      sel <- lapply(seq_along(d$groupings), function(i) input[[paste0("group_", i)]])
      req(all(!vapply(sel, is.null, logical(1))))
      stats::setNames(sel, d$groupings)
    })

    output$threshold_ui <- renderUI({
      d <- data()
      req(d)
      numericInput(ns("threshold"), "A large claim has incurred above",
                   value = suggest_large_threshold(d$current), min = 0, step = 1000)
    })
    threshold_typed <- debounce(reactive(input$threshold), 800)
    threshold <- reactive({
      if (!isTRUE(input$exclude_large)) return(Inf)
      t <- threshold_typed()
      validate(need(is.numeric(t) && !is.na(t) && t > 0,
                    "Enter a large-claim threshold above zero."))
      t
    })

    # The triangles for the chosen basis and period, for every group.
    full_triangles <- reactive({
      d <- data()
      req(d, input$basis, input$time)
      key <- paste(input$basis, input$time, sep = "_")
      t <- threshold()
      tri <- if (is.infinite(t)) {
        d$triangles[[key]]
      } else {
        cache_key <- paste(d$id, key, t)
        if (is.null(cache[[cache_key]])) {
          cache[[cache_key]] <- withProgress(message = "Rebuilding without large claims...", value = 0.5, {
            create_triangles(d$claims, d$groupings, input$basis, input$time, large_loss = t)
          })
        }
        cache[[cache_key]]
      }
      validate(need(!is.null(tri), "None of your rows have the date needed for this origin basis."))
      tri
    })

    shown <- reactive(select_groups(full_triangles(), data()$groupings, selection()))

    # The sidebar starts collapsed on phones; build its controls even while hidden.
    outputOptions(output, "group_controls", suspendWhenHidden = FALSE)
    outputOptions(output, "threshold_ui", suspendWhenHidden = FALSE)

    output$title <- renderText({
      req(input$measure, input$basis, input$time)
      paste0(measure_label(input$measure), " - ",
             names(BASIS_CHOICES)[BASIS_CHOICES == input$basis], ", ",
             tolower(names(PERIOD_CHOICES)[PERIOD_CHOICES == input$time]),
             if (isTRUE(input$exclude_large)) " - large claims left out" else "")
    })

    output$heatmap <- renderPlotly(plot_triangle_heatmap(shown(), input$measure, input$time))

    output$grid <- DT::renderDT({
      grid <- triangle_grid(shown(), input$measure, input$time)
      names(grid)[1] <- "Origin"
      dt_by_kind(grid, names(grid)[-1], measure_kind(input$measure), file_name = "triangle",
                 page_length = 50, ordering = FALSE,
                 caption = "Rows: origin period. Columns: development period.")
    })

    # ---- Drill-down ----
    movements <- reactive({
      d <- data()
      req(d, input$basis, input$time)
      claim_movements(d$claims, d$groupings, input$basis, input$time, large_loss = threshold())
    })

    output$origin_ui <- renderUI({
      tri <- shown()
      origins <- sort(unique(tri$origin))
      selectInput(ns("cell_origin"), "Origin period",
                  choices = stats::setNames(origins, format_origin(origins, input$time)),
                  selected = utils::tail(origins, 1))
    })

    output$dev_ui <- renderUI({
      tri <- shown()
      req(input$cell_origin)
      devs <- sort(unique(tri$development[tri$origin == as.integer(input$cell_origin)]))
      req(length(devs) > 0)
      selectInput(ns("cell_dev"), "Development period", choices = devs, selected = max(devs))
    })

    cell <- reactive({
      req(input$cell_origin, input$cell_dev, movements())
      cell_movements(movements(), data()$groupings, selection(),
                     origin = as.integer(input$cell_origin), development = as.integer(input$cell_dev))
    })

    output$movement_summary <- renderUI({
      m <- cell()
      if (nrow(m) == 0) {
        return(div(class = "alert alert-light", "No claims changed in this cell."))
      }
      bold <- function(...) tags$strong(..., .noWS = "outside")
      div(class = "alert alert-light",
          bold(scales::comma(nrow(m)), if (nrow(m) == 1) " claim" else " claims"), " changed. ",
          "Incurred moved by ", bold(fmt_amount(sum(m$ITOT_movement))),
          " and paid by ", bold(fmt_amount(sum(m$PTOT_movement))), ". ",
          fmt_amount(sum(m$NTOT_movement)), " new claim(s) and ",
          fmt_amount(sum(m$STOT_movement)), " net newly settled.")
    })

    output$movements <- DT::renderDT({
      d <- data()
      m <- cell()
      tbl <- m
      if (isTRUE(d$synthetic_grouping)) tbl$all_claims <- NULL
      names(tbl)[match(names(MOVEMENT_LABELS), names(tbl))] <- unname(MOVEMENT_LABELS)
      out <- dt_table(tbl,
                      amount_cols = unname(MOVEMENT_LABELS[c("ITOT_movement", "PTOT_movement")]),
                      count_cols = unname(MOVEMENT_LABELS[c("NTOT_movement", "STOT_movement")]),
                      percent_cols = unname(MOVEMENT_LABELS[c("share_of_change", "percent_change")]),
                      selection = "single", page_length = 10, file_name = "claim_movements")
      share <- m$share_of_change[is.finite(m$share_of_change)]
      if (length(share) > 0 && diff(range(share)) > 0) {
        out <- DT::formatStyle(out, MOVEMENT_LABELS[["share_of_change"]],
                               background = DT::styleColorBar(range(share), "#f4b6c2"),
                               backgroundSize = "95% 60%", backgroundRepeat = "no-repeat",
                               backgroundPosition = "center")
      }
      out
    })

    selected_claim <- reactive({
      sel <- input$movements_rows_selected
      m <- cell()
      if (is.null(sel) || sel > nrow(m)) return(NULL)
      m$claim_id[sel]
    })

    output$claim_card <- renderUI({
      claim <- selected_claim()
      if (is.null(claim)) return(NULL)
      card(card_header(class = "fw-semibold", paste("History of claim", claim)),
           card_body(DT::DTOutput(ns("claim_history"))))
    })

    output$claim_history <- DT::renderDT({
      claim <- selected_claim()
      req(claim)
      d <- data()
      groups <- if (isTRUE(d$synthetic_grouping)) character(0) else d$groupings
      hist <- d$claims[d$claims$claim_id == claim, ] %>%
        arrange(date_stamp, row_in_file) %>%
        transmute(`Valuation date` = format(date_stamp), `Policy ID` = policy_id,
                  across(all_of(groups)),
                  `Policy start` = format(date_issue), `Incident date` = format(date_incident),
                  `Report date` = format(date_reported),
                  Paid = paid, `Case estimate` = case, Incurred = ITOT,
                  `Row in your file` = row_in_file + 1L)
      dt_table(hist, amount_cols = c("Paid", "Case estimate", "Incurred"), page_length = 25,
               file_name = paste0("claim_", claim), ordering = FALSE)
    })
  })
}
