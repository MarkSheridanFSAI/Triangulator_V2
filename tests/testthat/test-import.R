testthat::local_edition(3)

test_that("dates are read from many formats", {
  expect_equal(as_claim_date(as.Date("2023-01-31"))$value, as.Date("2023-01-31"))
  expect_equal(as_claim_date(as.POSIXct("2023-01-31", tz = "UTC"))$value, as.Date("2023-01-31"))
  expect_equal(as_claim_date(c("2023-01-31", NA, ""))$value, as.Date(c("2023-01-31", NA, NA)))
  expect_equal(as_claim_date(c("31/01/2023", "05/02/2023"))$value, as.Date(c("2023-01-31", "2023-02-05")))
  expect_equal(as_claim_date(c("31/01/23"))$value, as.Date("2023-01-31"))
  expect_equal(as_claim_date(44957)$value, as.Date("2023-01-31"))          # Excel date number
  expect_equal(as_claim_date("44957")$value, as.Date("2023-01-31"))
  expect_equal(as_claim_date(20230131)$value, as.Date("2023-01-31"))       # YYYYMMDD
  expect_equal(as_claim_date(c(NA, NA))$value, as.Date(c(NA, NA)))
})

test_that("bad dates give a helpful message", {
  res <- as_claim_date(c("2023-01-31", "not a date"))
  expect_null(res$value)
  expect_match(res$problem, "not a date")
  expect_match(as_claim_date(c(5000000, 1))$problem, "numbers that aren't dates")
})

test_that("amounts are read, including text with commas and currency signs", {
  expect_equal(as_claim_amount(c("1,234.50", "£100", "(50)", "", NA))$value, c(1234.5, 100, -50, NA, NA))
  expect_match(as_claim_amount(c("12", "twelve"))$problem, "twelve")
})

test_that("IDs stored as numbers aren't turned into scientific notation", {
  expect_equal(as_id(c(12000000, NA)), c("12000000", NA))
})

test_that("mapping problems are explained", {
  raw <- tiny_claims()
  expect_length(validate_mapping(raw, tiny_mapping()), 0)

  m <- tiny_mapping(); m$paid <- ""
  expect_match(validate_mapping(raw, m), "Choose a column for Paid to date")

  m <- tiny_mapping(); m$case <- "paid_amt"
  expect_match(validate_mapping(raw, m), "chosen for Paid to date and Outstanding case estimate")

  m <- tiny_mapping(groupings = "paid_amt")
  expect_match(validate_mapping(raw, m), "both as a grouping")

  raw$origin <- "a"
  expect_match(validate_mapping(raw, tiny_mapping(groupings = "origin")), "uses internally")
})

test_that("a mapping can be saved and loaded again", {
  path <- tempfile(fileext = ".csv")
  write_mapping_file(tiny_mapping(), path)
  loaded <- read_mapping_file(path, names(tiny_claims()))
  expect_length(loaded$problems, 0)
  expect_equal(loaded$mapping[names(tiny_mapping())], tiny_mapping())
})

test_that("loading a mapping that doesn't fit the data explains what's missing", {
  path <- tempfile(fileext = ".csv")
  m <- tiny_mapping(); m$paid <- "PAID_COLUMN"; m$groupings <- c("line", "region")
  write_mapping_file(m, path)
  loaded <- read_mapping_file(path, names(tiny_claims()))
  expect_equal(loaded$mapping$paid, "")
  expect_equal(loaded$mapping$groupings, "line")
  expect_length(loaded$problems, 2)
  expect_error(read_mapping_file(test_path("setup.R"), "x"), class = "friendly_error")
})

test_that("unreadable or unsuitable files are rejected politely", {
  expect_error(read_claims_file("x.pdf", "x.pdf"), "Excel file", class = "friendly_error")
  bad <- tempfile(fileext = ".xlsx"); writeLines("not really excel", bad)
  expect_error(read_claims_file(bad, "claims.xlsx"), "couldn't read", class = "friendly_error")
  csv <- tempfile(fileext = ".csv"); writeLines("a,b", csv)
  expect_error(read_claims_file(csv, "claims.csv"), "no rows", class = "friendly_error")
})

test_that("CSV files can be uploaded", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(tiny_claims(), csv, row.names = FALSE)
  raw <- read_claims_file(csv, "claims.csv")
  std <- standardise_claims(raw, tiny_mapping())
  expect_length(std$problems, 0)
  expect_equal(std$data$date_stamp, tiny_claims()$valued)
})

test_that("CSV files with semicolons (European settings) can be uploaded", {
  csv <- tempfile(fileext = ".csv")
  utils::write.csv2(tiny_claims(), csv, row.names = FALSE)
  raw <- read_claims_file(csv, "claims.csv")
  expect_equal(ncol(raw), ncol(tiny_claims()))
  std <- standardise_claims(raw, tiny_mapping())
  expect_length(std$problems, 0)
  expect_equal(std$data$paid, tiny_claims()$paid_amt)
})

test_that("data checks find the problems they should", {
  raw <- tiny_claims()
  raw$claim_ref[1] <- NA                       # missing claim ID (error)
  raw$reported[2] <- as.Date("2019-01-01")     # reported before incident
  raw$paid_amt[3] <- -5                        # negative paid
  raw <- rbind(raw, raw[5, ])                  # duplicate claim + valuation date
  std <- standardise_claims(raw, tiny_mapping())$data
  chk <- run_data_checks(std)
  affected <- stats::setNames(chk$summary$rows_affected, chk$summary$check)
  expect_equal(affected[["Claim ID is filled in"]], 1)
  expect_equal(affected[["Claim reported on or after the incident date"]], 1)
  expect_equal(affected[["Paid to date is not negative"]], 1)
  expect_equal(affected[["Only one row per claim per valuation date"]], 1)
  expect_equal(chk$n_errors, 1)
  expect_equal(chk$failing_rows[[1]], 1L)
})

test_that("blank amounts and groupings are filled in when finalising", {
  raw <- tiny_claims()
  raw$case_amt[1] <- NA
  raw$line[2] <- NA
  std <- standardise_claims(raw, tiny_mapping())$data
  fin <- finalise_claims(std, "line")$data
  expect_equal(fin$case[1], 0)
  expect_equal(fin$STOT[1], 1)
  expect_equal(fin$line[2], "(blank)")
})
