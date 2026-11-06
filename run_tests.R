# Run all of Triangulator's automated tests.
# In RStudio or Positron: open this file and click "Source" (or run: source("run_tests.R")).
# Every line should end in dots and "DONE" with no failures.

testthat::test_dir("tests/testthat", reporter = "summary")
