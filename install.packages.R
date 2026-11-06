# Run this once to install everything Triangulator needs.
# (Select all and click "Run", or type: source("install.packages.R"))

install_if_missing <- function(packages) {
  missing <- packages[!packages %in% rownames(installed.packages())]
  if (length(missing)) {
    install.packages(missing)
    message("Installed missing packages: ", paste(missing, collapse = ", "))
  } else {
    message("All packages are already installed.")
  }
}

# Needed to run the app
install_if_missing(c(
  "shiny", "bslib", "dplyr", "tidyr", "data.table", "readxl", "openxlsx",
  "DT", "ggplot2", "plotly", "scales", "waterfalls"
))

# Needed only to run the automated tests
install_if_missing("testthat")
