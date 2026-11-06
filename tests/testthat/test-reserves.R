testthat::local_edition(3)

test_that("chain-ladder ultimates match a hand calculation", {
  # Incurred triangle: 2020 = 150, 200, 200 / 2021 = 300, 400 / 2022 = 0
  # Factor dev 1 -> 2 = (200 + 400) / (150 + 300) = 4/3; dev 2 -> 3 = 200 / 200 = 1
  tiny <- prepare_tiny()
  tri <- create_triangles(tiny$data, tiny$groupings, "incident", "yearly")
  ult <- chain_ladder_ultimates(tri[tri$variable == "ITOT", ], tiny$groupings)
  expect_equal(ult$factor, c(1, 1, 4 / 3))
  expect_equal(ult$ultimate, c(200, 400, 0))
})

test_that("reserve components add up to the underwriting ultimate", {
  s <- sample_prepared()
  triangles <- build_all_triangles(s$claims, s$groupings)
  comp <- reserve_components(triangles, s$groupings, "yearly")
  table <- reserve_table(comp, s$groupings)
  expect_equal(table$Component, c(unname(RESERVE_LABELS), "Ultimate (total)"))
  expect_equal(table$`All groups`[6], sum(comp$value))
})

test_that("without policy start dates, URR is left blank and the rest still works", {
  tiny <- prepare_tiny()
  full <- reserve_components(build_all_triangles(tiny$data, tiny$groupings), tiny$groupings, "yearly")
  tiny$data$date_issue <- as.Date(NA)
  comp <- reserve_components(build_all_triangles(tiny$data, tiny$groupings), tiny$groupings, "yearly")
  expect_true(is.na(comp$value[comp$metric == "URR"]))
  expect_equal(comp$value[comp$metric != "URR"], full$value[full$metric != "URR"])
  table <- reserve_table(comp, tiny$groupings)
  expect_equal(table$`All groups`[6], sum(comp$value, na.rm = TRUE))
})

test_that("reserves explain clearly when incident dates are missing", {
  tiny <- prepare_tiny()
  tiny$data$date_incident <- as.Date(NA)
  triangles <- build_all_triangles(tiny$data, tiny$groupings)
  expect_error(reserve_components(triangles, tiny$groupings, "yearly"), "incident", class = "friendly_error")
})
