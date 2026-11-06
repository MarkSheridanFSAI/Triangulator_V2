# The Triangulator app: page layout, server logic and run_app() ------------------
#
# Where the rest of the code lives:
#   prepare.R, checks.R          reading the file, matching columns, data checks
#   triangles.R, reserves.R      the calculations; ranges.R for reserve ranges
#   compare.R                    comparing groups
#   overviews.R, plots.R         dashboard summaries and charts
#   mod_*.R                      the pages and tabs of the app
#   export.R                     the Excel report

#' Start the Triangulator app
#'
#' Opens Triangulator in your web browser. Upload a claims listing or bordereaux
#' (or try the built-in sample data) to get claims diagnostics, triangles and
#' first-look reserve estimates.
#'
#' @param launch.browser Open the app in your web browser (default `TRUE`).
#' @param ... Other settings passed to [shiny::runApp()], such as `port`.
#' @return Nothing; the app runs until you close it (press Esc or the red Stop button).
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(launch.browser = TRUE, ...) {
  old <- options(shiny.maxRequestSize = 100 * 1024^2)   # allow uploads up to 100 MB
  on.exit(options(old), add = TRUE)
  shiny::runApp(shinyApp(app_ui(), app_server), launch.browser = launch.browser, ...)
}

# Small browser-side helpers: scroll smoothly to a part of the page (waiting
# briefly for it to appear), or back to the top.
SCROLL_JS <- "
Shiny.addCustomMessageHandler('triangulator-scroll', function(msg) {
  var tries = 0;
  (function go() {
    var el = document.getElementById(msg.id);
    if (el && el.offsetParent !== null) {
      el.scrollIntoView({ behavior: 'smooth', block: msg.block || 'center' });
    } else if (tries++ < 40) {
      setTimeout(go, 100);
    }
  })();
});
Shiny.addCustomMessageHandler('triangulator-top', function(msg) { window.scrollTo(0, 0); });
"

app_version <- function() {
  tryCatch(as.character(utils::packageVersion("Triangulator")), error = function(e) "")
}

app_ui <- function() {
  page_navbar(
    title = tags$span(icon("table-cells"), " Triangulator"),
    id = "main_nav",
    window_title = "Triangulator",
    theme = triangulator_theme(),
    fillable = FALSE,
    navbar_options = navbar_options(bg = BRAND_RED, theme = "dark"),
    header = tagList(useBusyIndicators(),
                     tags$head(tags$link(rel = "stylesheet", href = HEADING_FONT_LINK),
                               tags$style(HTML(app_css())),
                               tags$script(HTML(SCROLL_JS)))),
    footer = div(class = "tri-footer",
                 paste0("Triangulator ", app_version(),
                        " \u00b7 Free and open source \u00b7 A companion for exploring claims data, ",
                        "not a substitute for actuarial advice")),
    nav_panel("Import", value = "import", icon = icon("upload"), mod_import_ui("import")),
    nav_panel("Dashboard", value = "dashboard", icon = icon("chart-column"), uiOutput("dashboard")),
    nav_panel("Help", value = "help", icon = icon("circle-question"), help_page_ui()),
    nav_spacer(),
    nav_item(tags$a(icon("github"), "GitHub", href = "https://github.com/MarkSheridanFSAI/Triangulator_V2",
                    target = "_blank", rel = "noopener noreferrer"))
  )
}

app_server <- function(input, output, session) {
  imported <- mod_import_server("import")
  app_data <- reactiveVal(NULL)

  # When the user clicks "Build my dashboard": build every triangle once, up front.
  observeEvent(imported(), {
    x <- imported()
    req(x)
    result <- withProgress(message = "Building your triangles", value = 0, {
      run_safely(doing = "building your triangles", {
        log_info("Building triangles for ", nrow(x$claims), " rows")
        started <- Sys.time()
        triangles <- build_all_triangles(x$claims, x$groupings, progress = function(i, n, detail) {
          setProgress(value = i / n, detail = detail)
        })
        log_info("Triangles built in ", round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1), "s")
        c(x, list(triangles = triangles, current = current_positions(x$claims),
                  valuation = max(x$claims$date_stamp),
                  single_snapshot = dplyr::n_distinct(x$claims$date_stamp) == 1))
      })
    })
    req(result)
    app_data(result)
    nav_select("main_nav", "dashboard")
    session$sendCustomMessage("triangulator-top", list())
    showNotification("Your dashboard is ready.", type = "message")
  })

  output$dashboard <- renderUI({
    if (is.null(app_data())) dashboard_placeholder_ui()
    else dashboard_ui(units = isolate(input$units), currency = isolate(input$currency))
  })
  observeEvent(input$go_import, nav_select("main_nav", "import"))

  # How amounts are shown everywhere on the dashboard (full, thousands, millions; currency).
  display <- reactive(display_settings(input$units %||% "1", input$currency %||% ""))

  output$dashboard_header <- renderUI({
    d <- app_data()
    req(d)
    h <- headline_figures(d$current, d$claims)
    tagList(
      if (isTRUE(d$single_snapshot)) {
        div(class = "alert alert-info d-flex gap-2", icon("circle-info"),
            tags$span(tags$strong("One valuation date. "), SINGLE_SNAPSHOT_MESSAGE))
      },
      div(
        class = "headline-boxes",
        value_box("Claims", scales::comma(h$claims)),
        value_box("Open claims", scales::comma(h$open)),
        value_box("Paid to date", fmt_money(h$paid, display())),
        value_box("Incurred", fmt_money(h$incurred, display())),
        value_box("Latest valuation", format(h$valuation, "%d %b %Y"))
      )
    )
  })
  output$data_source <- renderUI({
    d <- app_data()
    req(d)
    tags$span(class = "text-muted", "Data from ", tags$strong(d$file_name))
  })

  mod_summary_server("summary", app_data, display)
  mod_sizes_server("sizes", app_data, display)
  mod_origin_server("origin", app_data, display)
  mod_triangle_server("triangle", app_data, display)
  mod_reserves_server("reserves", app_data, display)
  mod_ranges_server("ranges", app_data, display)

  output$download_report <- downloadHandler(
    filename = function() paste0("Triangulator_report_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"),
    content = function(file) {
      d <- app_data()
      req(d)
      withProgress(message = "Preparing your Excel report...", value = 0.5, {
        tryCatch({
          openxlsx::saveWorkbook(build_report_workbook(d), file, overwrite = TRUE)
          log_info("Excel report created")
        }, error = function(e) {
          log_info("ERROR while creating the Excel report: ", conditionMessage(e))
          stop(e)
        })
      })
    }
  )
}
