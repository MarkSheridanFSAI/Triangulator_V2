# Triangulator 2.0.0

A complete rebuild of Triangulator V1.1, now installable as an R package:
`remotes::install_github("MarkSheridanFSAI/Triangulator_V2")`, then `Triangulator::run_app()`.

## Calculation fixes

* The large-loss filter now compares amounts as numbers. It previously compared them as text, so it
  removed the wrong claims (for example, 600 counted as larger than 5,000).
* Dates before 2000 and after 2099 now work (they were silently dropped).
* Triangles now run up to the latest valuation date, so recent development is never cut off.
* Summaries count each claim once, even when a claim has duplicate rows.
* Group filters in the claim drill-down now match each grouping column separately.
* Chain-ladder factors that divide by zero no longer produce infinite reserves.

On the sample data, all triangles and reserve figures match V1.1 exactly, apart from these fixes.

## New

* Guided import: "Try it with sample data", automatic column matching (including bordereau headings),
  Excel (any sheet) or CSV, saved column mappings, and clear messages when something is wrong.
* Data checks in plain English, with the exact rows to look at.
* Policy ID and policy start date are now optional.
* Single bordereaux are supported, with a clear explanation of which views need several valuation dates.
* Groups at a glance: average claim, % settled, paid % of incurred, large-claim share, days to report
  and days to settle, side by side, with the highest and lowest group shaded.
* Development patterns by group, and "compare with all groups" on the Triangles and Trends tabs.
* Reserve ranges using Mack's method (checked against the published RAA results), with a comparison to
  the Reserves tab.
* Amounts in full, thousands or millions, with an optional currency symbol.
* One-click Excel report.
* Modern layout that also works on tablets and phones.

## Under the hood

* Fast on large files: a 460,000-row file builds its dashboard in about 9 seconds.
* Errors show a helpful message instead of freezing the app; each step is logged to the R Console.
* Uploaded text can never run as web-page code.
* 186 automated checks, run on Windows, macOS and Linux on every push.
* Fewer dependencies: removed shinyalert, shinyjs, kableExtra, knitr, validate, cowplot, patchwork,
  lubridate, later, purrr and rmarkdown.
