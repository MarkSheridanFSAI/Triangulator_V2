testthat::local_edition(3)

# These tests compare today's results on the sample data with results saved
# from the original (V1.1) code, so any change to the numbers gets noticed.

original <- readRDS(test_path("fixtures", "original_v1_results.rds"))

tidy <- function(d) {
  d <- as.data.frame(d)
  d[do.call(order, d), , drop = FALSE]
}

test_that("the sample data's columns are guessed correctly", {
  s <- sample_prepared()
  expected <- read_mapping_file(app_path("www", "sample_column_mapping.csv"),
                                names(s$raw))
  expect_length(expected$problems, 0)
  expect_equal(s$mapping[SINGLE_FIELDS], expected$mapping[SINGLE_FIELDS])
  expect_equal(s$mapping$groupings, expected$mapping$groupings)
})

test_that("triangles match the original version exactly", {
  s <- sample_prepared()
  for (key in c("incident_quarterly", "underwriting_yearly", "reporting_monthly")) {
    parts <- strsplit(key, "_")[[1]]
    new <- create_triangles(s$claims, s$groupings, parts[1], parts[2])
    expect_equal(tidy(new), tidy(original[[key]]), ignore_attr = TRUE, tolerance = 1e-9, label = key)
  }
})

test_that("large-loss triangles match the original version (given a numeric threshold)", {
  s <- sample_prepared()
  new <- create_triangles(s$claims, s$groupings, "incident", "quarterly", large_loss = 5000)
  expect_equal(tidy(new), tidy(original$large_incident_quarterly_5000), ignore_attr = TRUE, tolerance = 1e-9)
})

test_that("reserves match the original version", {
  s <- sample_prepared()
  triangles <- build_all_triangles(s$claims, s$groupings)
  for (time in c("monthly", "quarterly", "yearly")) {
    new <- reserve_components(triangles, s$groupings, time)
    old <- original$reserves[[time]]
    old_long <- tidyr::pivot_longer(old, -metric, names_to = "group", values_to = "value")
    new$group <- paste(new$grouping_1_test, new$grouping_2_test, sep = "_")
    joined <- merge(new, old_long, by = c("group", "metric"), suffixes = c("_new", "_old"))
    expect_equal(nrow(joined), 20)
    expect_equal(joined$value_new, joined$value_old, tolerance = 1e-9, label = time)
  }
})

test_that("summaries count each claim once", {
  s <- sample_prepared()
  current <- current_positions(s$claims)
  expect_equal(nrow(current), dplyr::n_distinct(s$claims$claim_id))
  summ <- summary_by_group(current, s$groupings)
  expect_equal(sum(summ$claims_reported), nrow(current))
  expect_equal(sum(summ$paid), sum(current$PTOT))
  buckets <- claim_size_buckets(current, s$groupings, 5000)
  expect_equal(sum(buckets$`All groups`), nrow(current))
})

test_that("summing claim movements reproduces the change in each triangle cell", {
  s <- sample_prepared()
  tri <- create_triangles(s$claims, s$groupings, "incident", "quarterly")
  mv <- claim_movements(s$claims, s$groupings, "incident", "quarterly")
  change <- tri[tri$variable == "ITOT", ]
  change <- change[do.call(order, change[c(s$groupings, "origin", "development")]), ]
  change$change <- ave(change$value, change$grouping_1_test, change$grouping_2_test, change$origin,
                       FUN = function(v) c(NA, diff(v)))
  moved <- aggregate(ITOT_movement ~ grouping_1_test + grouping_2_test + origin + development, data = mv, FUN = sum)
  both <- merge(change[change$development >= 2, ], moved, all.x = TRUE)
  both$ITOT_movement[is.na(both$ITOT_movement)] <- 0
  expect_equal(both$change, both$ITOT_movement, tolerance = 1e-6)
})
