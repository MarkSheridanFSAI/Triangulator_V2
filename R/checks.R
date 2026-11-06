# Data checks ---------------------------------------------------------------------
# Each check looks at every row and flags the rows that fail it.
#   "error"   - must be fixed before triangles can be built
#   "warning" - worth looking at, but the app can carry on

run_data_checks <- function(std) {
  d <- std
  # Treat "can't tell" (a missing value) as a pass for comparison checks; the
  # missing values have their own checks.
  flag <- function(x) !is.na(x) & x

  # Paid to date should be a running total, so it shouldn't go down over time.
  order_idx <- order(d$claim_id, d$date_stamp, d$row_in_file, na.last = TRUE)
  ordered <- d[order_idx, c("claim_id", "paid")]
  same_claim <- c(FALSE, ordered$claim_id[-1] == ordered$claim_id[-nrow(ordered)])
  paid_down <- same_claim & c(FALSE, ordered$paid[-1] < ordered$paid[-nrow(ordered)] - 0.005)
  paid_drops <- logical(nrow(d))
  paid_drops[order_idx] <- flag(paid_down)

  # Several rows for the same claim on the same valuation date: only the last one is used.
  dup <- duplicated(d[, c("claim_id", "date_stamp")], fromLast = TRUE) &
    !is.na(d$claim_id) & !is.na(d$date_stamp)

  checks <- list(
    list("error", "Claim ID is filled in",
         "Every row needs a claim ID. Rows without one are left out.",
         is.na(d$claim_id)),
    list("error", "Valuation date is filled in",
         "Every row needs a valuation date. Rows without one are left out.",
         is.na(d$date_stamp)),
    list("warning", "Policy ID is filled in",
         "Shown as blank in the claim tables. Doesn't affect the triangles.",
         is.na(d$policy_id)),
    list("warning", "Policy start date is filled in",
         "Rows without one are left out of underwriting triangles and the reserves.",
         is.na(d$date_issue)),
    list("warning", "Incident date is filled in",
         "Rows without one are left out of accident triangles and the reserves.",
         is.na(d$date_incident)),
    list("warning", "Report date is filled in",
         "Rows without one are left out of reported triangles and the reserves.",
         is.na(d$date_reported)),
    list("warning", "Paid to date is filled in",
         "Blank values are treated as zero.",
         is.na(d$paid)),
    list("warning", "Outstanding case estimate is filled in",
         "Blank values are treated as zero (which counts the claim as settled).",
         is.na(d$case)),
    list("warning", "Paid to date is not negative",
         "Negative amounts are used as they are. Check they are intentional.",
         flag(d$paid < 0)),
    list("warning", "Case estimate is not negative",
         "Negative amounts are used as they are. Check they are intentional.",
         flag(d$case < 0)),
    list("warning", "Paid to date never goes down",
         "Paid to date is a running total, so it normally only goes up. A drop may mean payments are recorded per period instead.",
         paid_drops),
    list("warning", "Policy started on or before the valuation date",
         "A row can't normally be recorded before the policy started.",
         flag(d$date_issue > d$date_stamp)),
    list("warning", "Incident happened on or after the policy start date",
         "Claims normally happen while the policy is in force.",
         flag(d$date_incident < d$date_issue)),
    list("warning", "Claim reported on or after the incident date",
         "A claim can't normally be reported before it happened.",
         flag(d$date_reported < d$date_incident)),
    list("warning", "Claim reported on or before the valuation date",
         "A row can't normally be recorded before the claim was reported.",
         flag(d$date_reported > d$date_stamp)),
    list("warning", "Only one row per claim per valuation date",
         "Where a claim has several rows on the same valuation date, only the last one in the file is used.",
         dup)
  )

  summary <- dplyr::tibble(
    severity      = vapply(checks, `[[`, character(1), 1),
    check         = vapply(checks, `[[`, character(1), 2),
    explanation   = vapply(checks, `[[`, character(1), 3),
    rows_affected = vapply(checks, function(ch) sum(ch[[4]]), numeric(1))
  )
  summary$status <- dplyr::case_when(
    summary$rows_affected == 0      ~ "Passed",
    summary$severity == "error"     ~ "Problem",
    TRUE                            ~ "Warning"
  )
  failing_rows <- lapply(checks, function(ch) d$row_in_file[ch[[4]]])

  total_items <- nrow(d) * length(checks)
  list(
    summary      = summary,
    failing_rows = failing_rows,
    score        = if (total_items > 0) 1 - sum(summary$rows_affected) / total_items else NA_real_,
    n_errors     = sum(summary$status == "Problem"),
    n_warnings   = sum(summary$status == "Warning")
  )
}
