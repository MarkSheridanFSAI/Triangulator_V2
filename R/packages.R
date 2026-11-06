# Packages used by Triangulator ------------------------------------------------
# Shiny loads every file in this R/ folder automatically when the app starts.
# The automated tests (tests/testthat) load them too.
#
# Why each package is needed:
#   shiny, bslib      - the app itself and its modern look (bslib comes with shiny)
#   dplyr, tidyr      - reshaping and summarising the claims data
#   data.table        - fast triangle building
#   readxl            - reading Excel uploads
#   openxlsx          - writing the Excel report
#   DT                - interactive tables
#   ggplot2, plotly   - charts
#   scales            - number formatting (1,234 and 12%)
#   waterfalls        - the reserves waterfall chart

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(readxl)
  library(openxlsx)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(scales)
  library(waterfalls)
})
