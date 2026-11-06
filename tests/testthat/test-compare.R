testthat::local_edition(3)

test_that("development pattern matches a hand calculation", {
  # Incurred factors: dev 1 -> 2 = 4/3, dev 2 -> 3 = 1, so % developed = 75%, 100%, 100%
  tiny <- prepare_tiny()
  tri <- create_triangles(tiny$data, tiny$groupings, "incident", "yearly")
  pat <- development_pattern(tri, tiny$groupings, "ITOT")
  expect_equal(pat$group, rep("x", 3))
  expect_equal(pat$percent_developed, c(0.75, 1, 1))
})

test_that("development pattern adds an 'All groups' line when there are several groups", {
  d <- sample_app_data()
  pat <- development_pattern(d$triangles$incident_quarterly, d$groupings, "PTOT")
  expect_setequal(unique(pat$group), c("home / liability", "home / property", "motor / liability",
                                       "motor / property", "All groups"))
  # the last development period is always 100% developed
  last <- pat[pat$development == max(pat$development), ]
  expect_equal(last$percent_developed, rep(1, 5))
})

test_that("comparisons with all groups use shares for amounts and differences for ratios", {
  expect_equal(compare_to_all(25, 100, "ITOT"), 0.25)
  expect_equal(compare_to_all(110, 100, "ITOT_divided_by_NTOT"), 0.10)
  expect_true(is.na(compare_to_all(5, 0, "ITOT")))
  expect_equal(relative_kind("NTOT"), "percent")
  expect_equal(relative_kind("ITOT_links"), "difference")
})

test_that("a group's triangle compared with all groups adds up to 100% across groups", {
  d <- sample_app_data()
  tri <- d$triangles$incident_quarterly
  all_groups <- select_groups(tri, d$groupings, list())
  shares <- lapply(c("liability", "property"), function(peril) {
    g <- select_groups(tri, d$groupings, list(grouping_1_test = ALL_GROUPS, grouping_2_test = peril))
    compare_triangles(g, all_groups, "ITOT")$value
  })
  total <- shares[[1]] + shares[[2]]
  expect_equal(total[!is.na(total)], rep(1, sum(!is.na(total))))
})

test_that("settlement dates: first valuation from which the claim stayed settled", {
  tiny <- prepare_tiny()
  settled <- settlement_dates(tiny$data)
  expect_equal(settled[["A"]], as.Date("2021-06-30"))
  expect_true(is.na(settled[["B"]]))
})

test_that("groups at a glance matches a hand calculation", {
  tiny <- prepare_tiny()
  current <- current_positions(tiny$data)
  g <- group_scorecard(tiny$data, current, tiny$groupings, large_threshold = 350)
  expect_equal(nrow(g), 1)                         # one group, so no 'All groups' row
  expect_equal(g$claims, 2)
  expect_equal(g$average_claim, 300)               # (200 + 400) / 2
  expect_equal(g$settled_pct, 0.5)
  expect_equal(g$paid_pct, 350 / 600)
  expect_equal(g$large_pct, 0.5)                   # claim B reached 400
  expect_equal(g$days_to_report, (31 + 28) / 2)
  expect_equal(g$days_to_settle, 455)              # 1 Apr 2020 to 30 Jun 2021, claim A only
})

test_that("groups at a glance on the sample data includes an all-groups row", {
  d <- sample_app_data()
  g <- group_scorecard(d$claims, d$current, d$groupings, suggest_large_threshold(d$current))
  expect_equal(g$group[nrow(g)], "All groups")
  expect_equal(g$claims[nrow(g)], nrow(d$current))
  expect_equal(sum(g$claims[-nrow(g)]), nrow(d$current))
})

test_that("display settings format amounts in thousands and millions with a symbol", {
  expect_equal(fmt_money(1234567), "1,234,567")
  expect_equal(fmt_money(1234567, display_settings(1000, "£")), "£1,234.6k")
  expect_equal(fmt_money(-2500000, display_settings(1e6, "$")), "-$2.5m")
  expect_equal(display_settings(1000, "")$note, "Amounts in thousands")
  expect_null(display_settings(1, "")$note)
})

test_that("the dashboard tabs work with comparisons switched on", {
  d <- sample_app_data()
  testServer(mod_triangle_server, args = list(data = reactive(d)), {
    session$setInputs(basis = "incident", time = "quarterly", measure = "ITOT_divided_by_NTOT",
                      group_1 = "motor", group_2 = ALL_GROUPS, exclude_large = FALSE, compare = TRUE)
    expect_equal(shown_kind(), "difference")
    expect_true(all(abs(shown_values()$value) < 5, na.rm = TRUE))
  })
  testServer(mod_origin_server, args = list(data = reactive(d)), {
    session$setInputs(basis = "incident", time = "quarterly", measure = "NTOT", compare = TRUE)
    shares <- rowSums(oa()[, -1])
    expect_equal(unname(shares), rep(1, nrow(oa())))
  })
  testServer(mod_summary_server, args = list(data = reactive(d)), {
    expect_equal(glance()$group[nrow(glance())], "All groups")
  })
})

test_that("claim size bands always have distinct labels, whatever the display setting", {
  # Regression test: in millions, 5,000-wide bands were all labelled "0.0m".
  d <- sample_app_data()
  for (divisor in c(1, 1000, 1e6)) {
    ds <- display_settings(divisor, "£")
    for (size in BUCKET_CHOICES) {
      accuracy <- min(10^-ds$digits, 10^floor(log10(size / ds$divisor)))
      b <- claim_size_buckets(d$current, d$groupings, size, label_fn = function(x) fmt_money(x, ds, accuracy))
      expect_false(anyDuplicated(levels(b$bucket)) > 0, label = paste(divisor, size))
      expect_equal(sum(b$`All groups`), nrow(d$current))
    }
  }
  # the safety net: a formatter that makes every label the same falls back to plain numbers
  b <- claim_size_buckets(d$current, d$groupings, 5000, label_fn = function(x) rep("same", length(x)))
  expect_equal(as.character(b$bucket[1]), "0 - 5,000")
})
