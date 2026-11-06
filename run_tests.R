# Run all of Triangulator's automated tests.
# Open this file in RStudio or Positron and click "Source".
# It should finish with "DONE" and no failures.

pkgload::load_all(".", quiet = TRUE)
testthat::test_dir("tests/testthat", reporter = "summary", load_package = "none")
