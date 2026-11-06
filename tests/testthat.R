# Runs the automated tests during package checks (R CMD check).
library(testthat)
library(Triangulator)

test_check("Triangulator")
