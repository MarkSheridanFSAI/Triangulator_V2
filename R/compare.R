# Comparing groups -----------------------------------------------------------------------

# Volume-weighted chain-ladder development factors for a cumulative triangle matrix
# (rows = origin periods, columns = development periods, NA for future cells).
# A factor that can't be worked out (e.g. dividing by zero) is set to 1.
development_factors <- function(m) {
  n_dev <- ncol(m)
  factor <- column_total <- rep(NA_real_, max(n_dev - 1, 0))
  for (k in seq_len(n_dev - 1)) {
    obs <- which(!is.na(m[, k + 1]) & !is.na(m[, k]))
    column_total[k] <- sum(m[obs, k])
    factor[k] <- sum(m[obs, k + 1]) / column_total[k]
    if (!is.finite(factor[k])) factor[k] <- 1
  }
  list(factor = factor, column_total = column_total)
}

# ---- Development pattern ------------------------------------------------------------

# For each group (and all groups together), the share of the projected ultimate
# reached by each development period: 1 / (product of the remaining factors).
development_pattern <- function(tri, groupings, measure) {
  pattern_for <- function(t) {
    f <- development_factors(triangle_matrix(t, measure))$factor
    to_ultimate <- rev(cumprod(rev(c(f, 1))))
    pct <- 1 / to_ultimate
    pct[!is.finite(pct)] <- NA_real_
    dplyr::tibble(development = seq_along(pct), percent_developed = pct)
  }
  labels <- group_label(tri, groupings)
  groups <- sort(unique(labels))
  out <- lapply(groups, function(g) {
    dplyr::bind_cols(group = g, pattern_for(select_groups(tri[labels == g, ], groupings, list())))
  })
  if (length(groups) > 1) {
    out <- c(out, list(dplyr::bind_cols(group = "All groups",
                                        pattern_for(select_groups(tri, groupings, list())))))
  }
  dplyr::bind_rows(out)
}

PATTERN_CHOICES <- c("Incurred" = "ITOT", "Paid" = "PTOT", "Claims reported" = "NTOT",
                     "Claims settled" = "STOT")

# ---- Comparing one group with all groups ---------------------------------------------

# Amounts and counts are compared as a share of the all-groups total; ratios and
# factors as a percentage above or below the all-groups figure.
SHARE_MEASURES <- c("ITOT", "PTOT", "ITOT_minus_PTOT", "NTOT", "STOT", "NTOT_minus_STOT")
relative_mode <- function(measure) if (measure %in% SHARE_MEASURES) "share" else "difference"
relative_kind <- function(measure) if (relative_mode(measure) == "share") "percent" else "difference"
relative_label <- function(measure) {
  if (relative_mode(measure) == "share") "share of the all-groups total"
  else "% above or below all groups"
}

compare_to_all <- function(value, all_value, measure) {
  out <- if (relative_mode(measure) == "share") value / all_value else value / all_value - 1
  out[!is.finite(out)] <- NA_real_
  out
}

# Triangle cells of one group relative to the same cells for all groups.
compare_triangles <- function(group_tri, all_tri, measure) {
  g <- group_tri[group_tri$variable == measure, c("variable", "origin", "development", "value")]
  a <- all_tri[all_tri$variable == measure, c("origin", "development", "value")]
  names(a)[3] <- "all_value"
  out <- dplyr::left_join(g, a, by = c("origin", "development"))
  out$value <- compare_to_all(out$value, out$all_value, measure)
  out$all_value <- NULL
  out
}

# Origin analysis table (from origin_analysis()) relative to all groups.
compare_origin_analysis <- function(oa, oa_all, measure) {
  all_value <- oa_all[[2]][match(oa$origin, oa_all$origin)]
  for (col in setdiff(names(oa), "origin")) oa[[col]] <- compare_to_all(oa[[col]], all_value, measure)
  oa
}

# ---- Groups at a glance ----------------------------------------------------------------

# Date each settled claim settled: the first valuation date from which it stayed
# settled. NA for claims that are open at their latest valuation.
settlement_dates <- function(claims) {
  d <- sort_by_claim(claims)
  dt <- data.table::data.table(claim_id = d$claim_id, i = seq_len(nrow(d)), open = d$STOT == 0)
  rows <- dt[, .(first = min(i), last = max(i),
                 last_open = if (any(open)) max(i[open]) else NA_integer_), by = claim_id]
  settle_row <- ifelse(is.na(rows$last_open), rows$first, rows$last_open + 1L)
  settle_row[settle_row > rows$last] <- NA_integer_
  stats::setNames(d$date_stamp[settle_row], rows$claim_id)
}

GLANCE_LABELS <- c(group = "Group", claims = "Claims", average_claim = "Average claim",
                   settled_pct = "Settled %", paid_pct = "Paid % of incurred",
                   large_pct = "Large claims %", days_to_report = "Average days to report",
                   days_to_settle = "Average days to settle")

# One row per group (plus all groups) of key claims diagnostics.
group_scorecard <- function(claims, current, groupings, large_threshold) {
  ever_max <- tapply(claims$ITOT, claims$claim_id, max)
  settled_on <- settlement_dates(claims)
  per_claim <- dplyr::tibble(
    group = group_label(current, groupings),
    ITOT = current$ITOT, PTOT = current$PTOT, STOT = current$STOT,
    large = as.numeric(ever_max[current$claim_id] > large_threshold),
    days_to_report = as.numeric(current$date_reported - current$date_incident),
    days_to_settle = as.numeric(settled_on[current$claim_id] - current$date_reported)
  )
  summarise_rows <- function(x) {
    x %>% summarise(
      claims = n(),
      average_claim = mean(ITOT),
      settled_pct = mean(STOT),
      paid_pct = if (sum(ITOT) != 0) sum(PTOT) / sum(ITOT) else NA_real_,
      large_pct = mean(large),
      days_to_report = if (any(!is.na(days_to_report))) mean(days_to_report, na.rm = TRUE) else NA_real_,
      days_to_settle = if (any(!is.na(days_to_settle))) mean(days_to_settle, na.rm = TRUE) else NA_real_
    )
  }
  by_group <- per_claim %>% group_by(group) %>% summarise_rows() %>% ungroup() %>% arrange(group)
  if (nrow(by_group) > 1) {
    by_group <- bind_rows(by_group, dplyr::bind_cols(group = "All groups", summarise_rows(per_claim)))
  }
  by_group
}
