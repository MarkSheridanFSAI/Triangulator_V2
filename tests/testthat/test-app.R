testthat::local_edition(3)

# These tests drive the app's pages without a browser, using shiny::testServer().

set_mapping <- function(session, m) {
  do.call(session$setInputs, c(m[SINGLE_FIELDS], list(groupings = m$groupings)))
}

test_that("import page: sample data -> check -> build", {
  testServer(mod_import_server, {
    session$setInputs(use_sample = 1)
    expect_equal(upload()$file_name, SAMPLE_DATA_FILE)
    expect_equal(nrow(upload()$df), 7202)

    set_mapping(session, starting_mapping())
    session$setInputs(run_checks = 1)
    expect_equal(check_state()$status, "checked")
    expect_true(checks_current())
    expect_equal(check_state()$checks$n_errors, 0)

    session$setInputs(build = 1)
    expect_equal(nrow(built()$claims), 7202)
    expect_equal(built()$groupings, c("grouping_1_test", "grouping_2_test"))

    # Changing a column choice makes the check results out of date...
    session$setInputs(paid = "case_test")
    expect_false(checks_current())
    # ...and checking again explains the problem.
    session$setInputs(run_checks = 2)
    expect_equal(check_state()$status, "mapping")
    expect_match(check_state()$problems, "chosen for Paid to date and Outstanding case estimate")
  })
})

test_that("import page: an unreadable file shows a message instead of crashing", {
  bad <- tempfile(fileext = ".xlsx")
  writeLines("not really excel", bad)
  testServer(mod_import_server, {
    session$setInputs(file = list(datapath = bad, name = "claims.xlsx"))
    expect_null(upload())
  })
})

test_that("triangle tab: groups, measures and the drill-down work together", {
  d <- sample_app_data()
  testServer(mod_triangle_server, args = list(data = reactive(d)), {
    session$setInputs(basis = "incident", time = "quarterly", measure = "ITOT",
                      group_1 = "motor", group_2 = "property", exclude_large = FALSE)
    tri <- shown()
    motor_property <- d$triangles$incident_quarterly
    motor_property <- motor_property[motor_property$grouping_1_test == "motor" &
                                       motor_property$grouping_2_test == "property" &
                                       motor_property$variable == "ITOT", ]
    expect_equal(sum(tri$value[tri$variable == "ITOT"]), sum(motor_property$value))

    session$setInputs(cell_origin = "202312", cell_dev = "1")
    expect_equal(nrow(cell()), dplyr::n_distinct(cell()$claim_id))
    expect_true(all(cell()$grouping_1_test == "motor"))
  })
})

test_that("reserves tab: components for every group, matching the table", {
  d <- sample_app_data()
  testServer(mod_reserves_server, args = list(data = reactive(d)), {
    session$setInputs(time = "quarterly", group = ALL_GROUPS)
    expect_equal(nrow(components()), 4 * length(RESERVE_METRICS))
    expect_equal(sum(components()$value),
                 reserve_table(components() %>% dplyr::select(-group), d$groupings)$`All groups`[6])
  })
})

test_that("the Excel report can be built, including with no grouping columns", {
  d <- sample_app_data()
  path <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(build_report_workbook(d), path)
  sheets <- openxlsx::getSheetNames(path)
  expect_true(all(c("About", "Data checks", "Summary", "Claim sizes", "Reserves quarterly",
                    "Reserve ranges", "Groups at a glance", "incident_quarterly") %in% sheets))

  s <- sample_prepared()
  fin <- finalise_claims(s$std, character(0))
  d0 <- d
  d0$claims <- fin$data
  d0$groupings <- fin$groupings
  d0$synthetic_grouping <- TRUE
  d0$mapping$groupings <- character(0)
  d0$triangles <- build_all_triangles(fin$data, fin$groupings)
  d0$current <- current_positions(fin$data)
  expect_no_error(openxlsx::saveWorkbook(build_report_workbook(d0), tempfile(fileext = ".xlsx")))
})
