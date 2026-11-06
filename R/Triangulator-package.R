# Triangulator package notes -------------------------------------------------------
#
# Start the app with Triangulator::run_app(). The package has no other public
# functions; everything else supports the app.

# Column names used inside dplyr and data.table code. Listing them here stops
# R's package checks from mistaking them for undefined variables.
utils::globalVariables(c(
  ".", "all_value", "bucket", "check", "claim_id", "column_total", "date_incident", "date_issue",
  "date_reported", "date_stamp", "development", "development_to", "explanation", "group",
  "i", "ITOT", "ITOT_movement", "metric", "n", "NTOT", "NTOT_movement", "open", "origin",
  "origin_index", "paid", "case", "policy_id", "PTOT", "PTOT_movement", "row_in_file",
  "rows_affected", "severity", "settled", "share_of_change", "percent_change", "status",
  "STOT", "STOT_movement", "total", "ultimate", "value", "variable", "large",
  "days_to_report", "days_to_settle", "INCURRED", "PAID", "reporting_ult", "incident_ult",
  "underwriting_ult", "CASE", "IBNER", "IBNR", "URR", "latest", "measure", "origin_period",
  "to_ultimate", "claims_reported", "claims_settled", "incurred"
))
