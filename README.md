# Triangulator

A reserving companion app. Give it your line-by-line claims data and it builds
claims development triangles, summaries and first-look reserve estimates for you.

## Getting started

1. Install [R](https://cloud.r-project.org/) (version 4.3 or newer) and
   [RStudio](https://posit.co/download/rstudio-desktop/) or [Positron](https://positron.posit.co/).
2. Download this project from GitHub and open `Triangulator_V2.Rproj`.
3. Run `install.packages.R` once (open it and click **Source**).
4. Open `app.R` and click **Run App**.
5. On the **Import** page, upload your file, or click **Try it with sample data**.

## What it does

- **Import:** reads Excel or CSV files, guesses which column is which, and checks your
  data for common problems (with the exact rows to look at).
- **Summary:** paid, outstanding and incurred amounts, open and settled claims, and average claim sizes for each group.
- **Claim sizes:** how many claims fall into each size band.
- **Origin analysis:** where each origin period stands at the latest valuation.
- **Triangles:** heatmaps and tables on accident, reported or underwriting basis
  (monthly, quarterly or yearly), for any measure, optionally without large claims.
  Drill into any cell to see which claims moved, and into any claim's full history.
- **Reserves:** a basic chain-ladder estimate split into case reserves, IBNER, IBNR and unexpired risk.
- **Excel report:** everything in one workbook.

The reserve figures are automatic first estimates. Don't rely on them for booking reserves.

## Your file

One row per claim per valuation date, with columns for: claim ID, policy ID,
valuation date, policy start date, incident date, report date, paid to date
(a running total) and outstanding case estimate. Grouping columns such as line of
business are optional. See `www/sample_database.xlsx` for an example.

## Project layout

| Path | What it is |
|---|---|
| `app.R` | Starts the app and lays out the pages |
| `R/` | All the code; Shiny loads it automatically |
| `R/prepare.R`, `R/checks.R` | Reading files, matching columns, data checks |
| `R/triangles.R`, `R/reserves.R` | The calculations |
| `R/mod_*.R` | The individual pages and tabs |
| `tests/testthat/` | Automated tests |
| `www/` | Sample data and documentation |

## Testing

Open `run_tests.R` and click **Source**. The tests check the calculations against
hand-worked examples and against results saved from the original V1.1 version.

## Feedback

Please report bugs and ideas in the GitHub Issues tab, with an example if you can.

Use at your own risk.
