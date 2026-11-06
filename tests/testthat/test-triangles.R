testthat::local_edition(3)

test_that("period numbers and labels work for months, quarters and years", {
  dates <- as.Date(c("2023-01-15", "2023-03-31", "2023-04-01", "2023-12-31"))
  expect_equal(period_label(period_index(dates, "monthly"), "monthly"), c(202301L, 202303L, 202304L, 202312L))
  expect_equal(period_label(period_index(dates, "quarterly"), "quarterly"), c(202303L, 202303L, 202306L, 202312L))
  expect_equal(period_label(period_index(dates, "yearly"), "yearly"), rep(202312L, 4))
  # consecutive periods have consecutive numbers, across a year end
  expect_equal(diff(period_index(as.Date(c("2022-12-01", "2023-01-01")), "monthly")), 1L)
  expect_equal(diff(period_index(as.Date(c("2022-10-01", "2023-01-01")), "quarterly")), 1L)
})

test_that("a hand-worked triangle comes out right", {
  tiny <- prepare_tiny()
  tri <- create_triangles(tiny$data, tiny$groupings, "incident", "yearly")

  expect_equal(cells(tri, "ITOT"),
               c("202012/1" = 150, "202012/2" = 200, "202012/3" = 200,
                 "202112/1" = 300, "202112/2" = 400,
                 "202212/1" = 0))
  expect_equal(cells(tri, "PTOT"),
               c("202012/1" = 100, "202012/2" = 200, "202012/3" = 200,
                 "202112/1" = 0, "202112/2" = 150, "202212/1" = 0))
  expect_equal(unname(cells(tri, "NTOT")), c(1, 1, 1, 1, 1, 0))
  expect_equal(unname(cells(tri, "STOT")), c(0, 1, 1, 0, 0, 0))
  expect_equal(unname(cells(tri, "ITOT_minus_PTOT")), c(50, 0, 0, 300, 250, 0))
  expect_equal(unname(cells(tri, "ITOT_links")), c(200 / 150, 1, NA, 400 / 300, NA, NA))
  # dividing by zero gives a blank, not infinity
  expect_true(is.na(cells(tri, "ITOT_divided_by_NTOT")[["202212/1"]]))
})

test_that("the triangle runs up to the latest valuation date (not the latest origin)", {
  # Claim B's 2022 valuation must be used even though no claim has a 2022 incident.
  tiny <- prepare_tiny()
  tri <- create_triangles(tiny$data, tiny$groupings, "incident", "yearly")
  expect_equal(cells(tri, "ITOT")[["202112/2"]], 400)
  expect_true("202212/1" %in% names(cells(tri, "ITOT")))
})

test_that("dates before 2000 work (the old version only handled 2000-2099)", {
  tiny <- prepare_tiny(year_shift = -25)
  tri <- create_triangles(tiny$data, tiny$groupings, "incident", "yearly")
  expect_equal(unname(cells(tri, "ITOT")), c(150, 200, 200, 300, 400, 0))
  expect_equal(min(tri$origin), 199512L)
})

test_that("large-loss threshold is compared as a number, even when given as text", {
  tiny <- prepare_tiny()
  # Claim B reaches 400, so a 350 threshold removes it.
  numeric_threshold <- create_triangles(tiny$data, tiny$groupings, "incident", "yearly", large_loss = 350)
  text_threshold <- create_triangles(tiny$data, tiny$groupings, "incident", "yearly", large_loss = "350")
  expect_equal(unname(cells(numeric_threshold, "ITOT")), c(150, 200, 200, 0, 0, 0))
  expect_equal(numeric_threshold, text_threshold)
  # "1000" is above every claim, so nothing is removed (text comparison used to get this wrong)
  expect_equal(create_triangles(tiny$data, tiny$groupings, "incident", "yearly", large_loss = "1000"),
               create_triangles(tiny$data, tiny$groupings, "incident", "yearly"))
})

test_that("no grouping columns is allowed", {
  tiny <- prepare_tiny(groupings = character(0))
  expect_equal(tiny$groupings, "all_claims")
  tri <- create_triangles(tiny$data, tiny$groupings, "incident", "yearly")
  expect_equal(unname(cells(tri, "ITOT")), c(150, 200, 200, 300, 400, 0))
})

test_that("claim movements explain the change in each cell", {
  tiny <- prepare_tiny()
  mv <- claim_movements(tiny$data, tiny$groupings, "incident", "yearly")
  cell <- cell_movements(mv, tiny$groupings, list(line = "x"), origin = 202112L, development = 2L)
  expect_equal(cell$claim_id, "B")
  expect_equal(cell$ITOT_movement, 100)
  expect_equal(cell$share_of_change, 1)
  expect_equal(cell$percent_change, 100 / 300)
})

test_that("combining groups adds the base figures and recalculates ratios", {
  tiny <- prepare_tiny()
  tri <- create_triangles(tiny$data, tiny$groupings, "incident", "yearly")
  combined <- select_groups(tri, tiny$groupings, list(line = ALL_GROUPS))
  expect_equal(unname(cells(combined, "ITOT")), unname(cells(tri, "ITOT")))
  expect_equal(unname(cells(combined, "ITOT_links")), unname(cells(tri, "ITOT_links")))
})

test_that("a basis with no dates gives no triangle instead of an error", {
  tiny <- prepare_tiny()
  tiny$data$date_issue <- as.Date(NA)
  expect_null(create_triangles(tiny$data, tiny$groupings, "underwriting", "yearly"))
})
