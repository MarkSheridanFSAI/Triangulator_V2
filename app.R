# Triangulator: a reserving companion app ----------------------------------------
#
# To start the app: open this file in RStudio or Positron and click "Run App",
# or type shiny::runApp() in the console.
#
# The code lives in the R/ folder (Shiny loads it automatically):
#   prepare.R, checks.R          reading the file, matching columns, data checks
#   triangles.R, reserves.R      the calculations
#   overviews.R, plots.R         dashboard summaries and charts
#   mod_*.R                      the pages and tabs of the app
#   export.R                     the Excel report

options(shiny.maxRequestSize = 100 * 1024^2)   # allow uploads up to 100 MB

ui <- page_navbar(
  title = tags$span(icon("table-cells"), " Triangulator"),
  id = "main_nav",
  window_title = "Triangulator",
  theme = triangulator_theme(),
  fillable = FALSE,
  navbar_options = navbar_options(bg = BRAND_RED, theme = "dark"),
  header = tagList(useBusyIndicators(),
                  tags$head(tags$link(rel = "stylesheet", href = HEADING_FONT_LINK),
                            tags$style(HTML(app_css())))),
  nav_panel("Import", value = "import", icon = icon("upload"), mod_import_ui("import")),
  nav_panel("Dashboard", value = "dashboard", icon = icon("chart-column"), uiOutput("dashboard")),
  nav_panel("Help", value = "help", icon = icon("circle-question"), help_page_ui()),
  nav_spacer(),
  nav_item(tags$a(icon("github"), "GitHub", href = "https://github.com/MarkSheridanFSAI/Triangulator_V2",
                  target = "_blank", rel = "noopener noreferrer"))
)

server <- function(input, output, session) {
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
                  valuation = max(x$claims$date_stamp)))
      })
    })
    req(result)
    app_data(result)
    nav_select("main_nav", "dashboard")
    showNotification("Your dashboard is ready.", type = "message")
  })

  output$dashboard <- renderUI({
    if (is.null(app_data())) dashboard_placeholder_ui() else dashboard_ui()
  })
  observeEvent(input$go_import, nav_select("main_nav", "import"))

  output$dashboard_header <- renderUI({
    d <- app_data()
    req(d)
    h <- headline_figures(d$current, d$claims)
    tagList(
      div(
        class = "headline-boxes",
        value_box("Claims", scales::comma(h$claims)),
        value_box("Open claims", scales::comma(h$open)),
        value_box("Paid to date", fmt_amount(h$paid)),
        value_box("Incurred", fmt_amount(h$incurred)),
        value_box("Latest valuation", format(h$valuation, "%d %b %Y"))
      ),
      div(class = "d-flex flex-wrap justify-content-between align-items-center gap-2 my-3",
          tags$span(class = "text-muted", "Data from ", tags$strong(d$file_name)),
          downloadButton("download_report", "Download Excel report", class = "btn-primary"))
    )
  })

  mod_summary_server("summary", app_data)
  mod_sizes_server("sizes", app_data)
  mod_origin_server("origin", app_data)
  mod_triangle_server("triangle", app_data)
  mod_reserves_server("reserves", app_data)
  mod_ranges_server("ranges", app_data)

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

shinyApp(ui, server)
