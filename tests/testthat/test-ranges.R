testthat::local_edition(3)

# The "RAA" triangle is the standard textbook example (Mack 1993; also used by
# the ChainLadder R package). Its published Mack results are checked below.
raa <- rbind(
  c(5012, 8269, 10907, 11805, 13539, 16181, 18009, 18608, 18662, 18834),
  c(106, 4285, 5396, 10666, 13782, 15599, 15496, 16169, 16704, NA),
  c(3410, 8992, 13873, 16141, 18735, 22214, 22863, 23466, NA, NA),
  c(5655, 11555, 15766, 21266, 23425, 26083, 27067, NA, NA, NA),
  c(1092, 9565, 15836, 22169, 25955, 26180, NA, NA, NA, NA),
  c(1513, 6445, 11702, 12935, 15852, NA, NA, NA, NA, NA),
  c(557, 4020, 10946, 12314, NA, NA, NA, NA, NA, NA),
  c(1351, 6947, 13112, NA, NA, NA, NA, NA, NA, NA),
  c(3133, 5395, NA, NA, NA, NA, NA, NA, NA, NA),
  c(2063, NA, NA, NA, NA, NA, NA, NA, NA, NA)
)
rownames(raa) <- 1981:1990

test_that("Mack's method reproduces the published RAA results", {
  res <- mack_chain_ladder(raa)
  expect_equal(round(res$total$reserve), 52135)
  expect_equal(round(res$total$std_error), 26909)
  expect_equal(round(res$by_origin$reserve), c(0, 154, 617, 1636, 2747, 3649, 5435, 10907, 10650, 16339))
  expect_equal(round(res$by_origin$std_error), c(0, 206, 623, 747, 1469, 2002, 2209, 5358, 6333, 24566))
})

test_that("Mack's best estimate equals the chain-ladder on the Reserves tab", {
  s <- sample_prepared()
  tri <- create_triangles(s$claims, s$groupings, "incident", "quarterly")
  one_group <- tri[tri$grouping_1_test == "motor" & tri$grouping_2_test == "liability", ]
  mack <- reserve_ranges(select_groups(one_group, s$groupings, list()), "ITOT")
  cl <- chain_ladder_ultimates(one_group[one_group$variable == "ITOT", ], s$groupings)
  expect_equal(mack$by_origin$ultimate, cl$ultimate)
  expect_true(mack$total$std_error > 0)
})

test_that("percentiles go up, and the median sits below the mean for a lognormal", {
  p <- reserve_percentiles(1000, 300)
  expect_true(all(diff(p) > 0))
  expect_lt(p[1], 1000)
  expect_equal(reserve_percentiles(1000, 0), rep(1000, length(RANGE_PERCENTILES)))
  # a negative reserve falls back to a normal distribution centred on it
  expect_equal(reserve_percentiles(-500, 100)[1], -500)
})

test_that("too few origin periods gives a clear message", {
  expect_error(mack_chain_ladder(raa[1:2, 1:2]), "at least 3 origin periods", class = "friendly_error")
})

test_that("the ranges tab works on the sample data for every group choice", {
  d <- sample_app_data()
  testServer(mod_ranges_server, args = list(data = reactive(d)), {
    session$setInputs(basis = "incident", time = "quarterly", measure = "PTOT", group = ALL_GROUPS)
    all_groups <- ranges()$total
    expect_gt(all_groups$reserve, 0)
    expect_gt(all_groups$cv, 0)
    session$setInputs(group = "motor / liability")
    expect_lt(ranges()$total$ultimate, all_groups$ultimate)
    # On incurred the sample's reserve is negative (incurred tends to fall), so CV is n/a
    session$setInputs(measure = "ITOT", group = ALL_GROUPS)
    expect_lt(ranges()$total$reserve, 0)
    expect_true(is.na(ranges()$total$cv))
    expect_true(is.finite(ranges()$total$std_error))
  })
})
