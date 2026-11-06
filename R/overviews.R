# Dashboard summaries -------------------------------------------------------------

# Each claim's latest position (its last row at its latest valuation date).
current_positions <- function(claims) {
  d <- sort_by_claim(claims)
  d[!same_as_next(d$claim_id), ]
}

safe_mean <- function(x) if (length(x)) mean(x) else NA_real_
safe_sd   <- function(x) if (length(x) > 1) stats::sd(x) else NA_real_

# Totals and averages for each group, at each claim's latest position.
# A claim counts as settled when its outstanding case estimate is zero.
summary_by_group <- function(current, groupings) {
  current %>%
    group_by(across(all_of(groupings))) %>%
    summarise(
      claims_reported         = sum(NTOT),
      claims_settled          = sum(STOT),
      claims_open             = claims_reported - claims_settled,
      paid                    = sum(PTOT),
      outstanding             = sum(ITOT) - sum(PTOT),
      incurred                = sum(ITOT),
      incurred_settled_claims = sum(ITOT[STOT == 1]),
      incurred_open_claims    = sum(ITOT[STOT == 0]),
      paid_pct_of_incurred    = if_else(incurred != 0, paid / incurred, NA_real_),
      settled_pct_of_reported = claims_settled / claims_reported,
      average_settled_claim   = safe_mean(ITOT[STOT == 1]),
      average_open_claim      = safe_mean(ITOT[STOT == 0]),
      average_claim           = safe_mean(ITOT),
      sd_settled_claims       = safe_sd(ITOT[STOT == 1]),
      sd_open_claims          = safe_sd(ITOT[STOT == 0]),
      sd_all_claims           = safe_sd(ITOT),
      .groups = "drop"
    )
}

SUMMARY_LABELS <- c(
  claims_reported = "Claims reported", claims_settled = "Claims settled", claims_open = "Claims open",
  paid = "Paid", outstanding = "Outstanding", incurred = "Incurred",
  incurred_settled_claims = "Incurred on settled claims", incurred_open_claims = "Incurred on open claims",
  paid_pct_of_incurred = "Paid % of incurred", settled_pct_of_reported = "Settled % of reported",
  average_settled_claim = "Average settled claim", average_open_claim = "Average open claim",
  average_claim = "Average claim", sd_settled_claims = "Std. deviation: settled claims",
  sd_open_claims = "Std. deviation: open claims", sd_all_claims = "Std. deviation: all claims"
)

# Suggest a bucket size that gives a readable number of buckets (25 or fewer).
BUCKET_CHOICES <- c(1000, 5000, 10000, 20000, 50000, 100000, 500000, 1000000)
suggest_bucket_size <- function(current) {
  top <- max(c(current$ITOT, 0), na.rm = TRUE)
  ok <- BUCKET_CHOICES[ceiling(top / BUCKET_CHOICES) <= 25]
  if (length(ok)) ok[1] else max(BUCKET_CHOICES)
}

# Number of claims in each incurred size band, per group (wide table).
# Bands include their upper limit: 0 - 5,000 holds claims from 0 up to 5,000.
claim_size_buckets <- function(current, groupings, size, max_buckets = 200, label_fn = fmt_amount) {
  size <- as.numeric(size)
  if (is.na(size) || size <= 0) stop_friendly("The bucket size must be a positive number.")
  inc <- current$ITOT
  negative <- inc < 0
  k <- pmax(ceiling(inc / size) - 1, 0)
  top <- max(c(k[!negative], 0))
  n_buckets <- min(top + 1, max_buckets)
  k <- pmin(k, n_buckets - 1)
  lower <- (seq_len(n_buckets) - 1) * size
  make_labels <- function(fn) {
    out <- paste(fn(lower), "-", fn(lower + size))
    if (top + 1 > max_buckets) out[n_buckets] <- paste0(fn(lower[n_buckets]), " and above")
    out
  }
  labels <- make_labels(label_fn)
  # Safety net: if the chosen format would make two bands look the same, use plain numbers.
  if (anyDuplicated(labels)) labels <- make_labels(fmt_amount)
  levels <- c(if (any(negative)) "Below 0", labels)
  bucket <- factor(ifelse(negative, "Below 0", labels[k + 1]), levels = levels)

  counts <- dplyr::tibble(bucket = bucket, group = group_label(current, groupings)) %>%
    count(bucket, group) %>%
    tidyr::complete(bucket, group, fill = list(n = 0))
  counts %>%
    tidyr::pivot_wider(names_from = group, values_from = n) %>%
    mutate(`All groups` = rowSums(across(-bucket))) %>%
    arrange(bucket)
}

# The latest value (the diagonal) for each origin period, one column per group.
origin_analysis <- function(tri, groupings, measure, time) {
  tri %>%
    filter(variable == measure) %>%
    group_by(across(all_of(c(groupings, "origin")))) %>%
    filter(development == max(development)) %>%
    ungroup() %>%
    mutate(group = group_label(., groupings)) %>%
    select(origin, group, value) %>%
    arrange(origin) %>%
    tidyr::pivot_wider(names_from = group, values_from = value) %>%
    mutate(origin = format_origin(origin, time))
}

# Headline numbers for the top of the dashboard.
headline_figures <- function(current, claims) {
  list(
    claims    = nrow(current),
    open      = sum(current$STOT == 0),
    paid      = sum(current$PTOT),
    incurred  = sum(current$ITOT),
    valuation = max(claims$date_stamp)
  )
}
