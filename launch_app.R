# Start Triangulator straight from this project folder (for working on the code).
# Open this file in RStudio or Positron and click "Source".
#
# Most people don't need this: install the app once with
#   remotes::install_github("MarkSheridanFSAI/Triangulator_V2", upgrade = "never")
# and then start it with Triangulator::run_app().

pkgload::load_all(".", quiet = TRUE)
run_app()
