# Reserve ranges: how uncertain is the chain-ladder estimate? -----------------------
#
# Uses Mack's method (Mack, 1993, "Distribution-free calculation of the standard
# error of chain ladder reserve estimates"). For each origin period and in total
# it gives the best estimate reserve and its standard error, combining:
#   - process error:   claims are random, even if the development pattern were known
#   - parameter error: the development factors are themselves only estimates
# The percentiles assume the reserve follows a lognormal distribution with that
# mean and standard error, which is a common convention.

RANGE_PERCENTILES <- c(0.50, 0.75, 0.90, 0.95, 0.995)

# A one-measure triangle (long format) as a matrix: rows = origin periods
# (oldest first), columns = development periods, NA for future cells.
triangle_matrix <- function(tri, measure) {
  d <- tri[tri$variable == measure, ]
  origins <- sort(unique(d$origin))
  devs <- seq_len(max(d$development))
  m <- matrix(NA_real_, length(origins), length(devs), dimnames = list(origins, devs))
  m[cbind(match(d$origin, origins), d$development)] <- d$value
  m
}

# Mack's method on a cumulative triangle matrix (as made by triangle_matrix()).
# Returns list(by_origin = table, total = one-row table, factors = table).
mack_chain_ladder <- function(m) {
  n_origin <- nrow(m)
  n_dev <- ncol(m)
  if (n_dev < 3 || n_origin < 3) {
    stop_friendly("Reserve ranges need at least 3 origin periods. Try a shorter period length (e.g. quarterly or monthly).")
  }
  latest_dev <- apply(m, 1, function(r) max(which(!is.na(r))))
  latest <- unname(m[cbind(seq_len(n_origin), latest_dev)])

  # Development factors (volume-weighted, the same as the Reserves tab) and their
  # variance parameters sigma^2.
  f <- sigma2 <- column_total <- rep(NA_real_, n_dev - 1)
  for (k in seq_len(n_dev - 1)) {
    obs <- which(!is.na(m[, k + 1]) & !is.na(m[, k]))
    column_total[k] <- sum(m[obs, k])
    f[k] <- sum(m[obs, k + 1]) / column_total[k]
    if (!is.finite(f[k])) f[k] <- 1
    use <- obs[m[obs, k] > 0]
    if (length(use) >= 2) {
      ratios <- m[use, k + 1] / m[use, k]
      sigma2[k] <- sum(m[use, k] * (ratios - f[k])^2) / (length(use) - 1)
    }
  }
  sigma2 <- fill_missing_sigma2(sigma2)

  # Project the future cells.
  full <- m
  for (k in seq_len(n_dev - 1)) {
    future <- is.na(full[, k + 1])
    full[future, k + 1] <- full[future, k] * f[k]
  }
  ultimate <- unname(full[, n_dev])
  reserve <- ultimate - latest

  # Per-origin mean squared error (process + parameter), and the extra term that
  # links origin periods together for the total.
  ok <- function(x) ifelse(is.finite(x), x, 0)
  mse <- cross_factor <- numeric(n_origin)
  for (i in seq_len(n_origin)) {
    ks <- seq_len(n_dev - 1)
    ks <- ks[ks >= latest_dev[i]]
    if (length(ks) == 0 || ultimate[i] == 0) next
    a <- ok(sigma2[ks] / f[ks]^2)
    mse[i] <- ultimate[i]^2 * sum(a * (ok(1 / full[i, ks]) + ok(1 / column_total[ks])))
    cross_factor[i] <- sum(2 * a * ok(1 / column_total[ks]))
  }
  newer_ultimates <- rev(cumsum(rev(ultimate))) - ultimate     # sum of ultimates of newer origins
  total_mse <- sum(mse) + sum(ultimate * newer_ultimates * cross_factor)

  by_origin <- dplyr::tibble(
    origin = as.integer(rownames(m)),
    latest = latest, development_to_date = latest_dev,
    ultimate = ultimate, reserve = reserve, std_error = sqrt(mse),
    cv = ifelse(reserve > 0, sqrt(mse) / reserve, NA_real_)
  )
  total <- dplyr::tibble(latest = sum(latest), ultimate = sum(ultimate), reserve = sum(reserve),
                         std_error = sqrt(total_mse))
  total$cv <- ifelse(total$reserve > 0, total$std_error / total$reserve, NA_real_)
  factors <- dplyr::tibble(from = seq_len(n_dev - 1), to = seq_len(n_dev - 1) + 1,
                           factor = f, sigma = sqrt(sigma2))
  list(by_origin = by_origin, total = total, factors = factors)
}

# Sigma^2 can't be estimated where there are fewer than two pairs of values
# (always true for the last development period). Gaps are filled with Mack's own
# rule from his 1993 paper, which uses the two previous values:
#   sigma2[k] = min(sigma2[k-1]^2 / sigma2[k-2], sigma2[k-2], sigma2[k-1])
# Any gap that rule can't fill uses a straight line through log(sigma).
fill_missing_sigma2 <- function(sigma2) {
  for (k in seq_along(sigma2)) {
    if (!is.finite(sigma2[k]) && k >= 3 && all(is.finite(sigma2[k - 1:2])) && sigma2[k - 2] > 0) {
      sigma2[k] <- min(sigma2[k - 1]^2 / sigma2[k - 2], sigma2[k - 2], sigma2[k - 1])
    }
  }
  k <- seq_along(sigma2)
  known <- which(is.finite(sigma2) & sigma2 > 0)
  if (all(is.finite(sigma2))) {
    # nothing left to fill
  } else if (length(known) >= 2) {
    fit <- stats::lm(log(sqrt(sigma2[known])) ~ k[known])
    predicted <- exp(stats::coef(fit)[[1]] + stats::coef(fit)[[2]] * k)^2
    sigma2[!is.finite(sigma2)] <- predicted[!is.finite(sigma2)]
  } else if (length(known) == 1) {
    sigma2[!is.finite(sigma2)] <- sigma2[known]
  } else {
    sigma2[!is.finite(sigma2)] <- 0
  }
  sigma2
}

# Percentiles of the reserve, assuming a lognormal distribution (or a normal
# distribution if the reserve isn't positive).
reserve_percentiles <- function(reserve, std_error, probs = RANGE_PERCENTILES) {
  if (!is.finite(reserve) || !is.finite(std_error) || std_error <= 0) return(rep(reserve, length(probs)))
  if (reserve > 0) {
    s2 <- log(1 + (std_error / reserve)^2)
    stats::qlnorm(probs, meanlog = log(reserve) - s2 / 2, sdlog = sqrt(s2))
  } else {
    stats::qnorm(probs, mean = reserve, sd = std_error)
  }
}

percentile_label <- function(p) paste0(format(p * 100, drop0trailing = TRUE), "th percentile")

# Everything the Reserve ranges tab needs for one triangle (a single measure).
reserve_ranges <- function(tri, measure = "PTOT") {
  res <- mack_chain_ladder(triangle_matrix(tri, measure))
  pct <- reserve_percentiles(res$total$reserve, res$total$std_error)
  res$percentiles <- dplyr::tibble(percentile = RANGE_PERCENTILES, label = percentile_label(RANGE_PERCENTILES),
                                   reserve = pct)
  res
}

# ---- Comparing with the Reserves tab ------------------------------------------------

# The Reserves tab components that cover the same thing as a reserve projected
# on this basis and measure (see range_covers()).
comparable_components <- function(basis, measure) {
  c(if (measure == "PTOT") "CASE", "IBNER",
    if (basis %in% c("incident", "underwriting")) "IBNR",
    if (basis == "underwriting") "URR")
}

# How the Reserve ranges best estimate compares with the Reserves tab.
# `components` comes from reserve_components() (same period length);
# `chosen` is a group label or ALL_GROUPS.
compare_with_reserves <- function(range_reserve, components, groupings, basis, measure, chosen,
                                  n_groups) {
  parts <- comparable_components(basis, measure)
  comp <- components[components$metric %in% parts, ]
  if (!identical(chosen, ALL_GROUPS)) comp <- comp[group_label(comp, groupings) == chosen, ]
  reserves_tab <- sum(comp$value, na.rm = TRUE)
  reasons <- character(0)
  if (measure == "PTOT") {
    reasons <- c(reasons, paste(
      "This tab is projecting paid amounts, while the Reserves tab projects incurred amounts.",
      "The two use different information (payments only vs payments plus case estimates), so they",
      "normally give different answers. Choose 'Incurred amounts' to compare like with like."))
  }
  if (identical(chosen, ALL_GROUPS) && n_groups > 1) {
    reasons <- c(reasons, paste(
      "Here all groups are combined into one triangle before projecting. The Reserves tab projects",
      "each group separately and adds the results up. Groups that develop differently give different",
      "answers. Choose a single group to compare like with like."))
  }
  difference <- range_reserve - reserves_tab
  if (length(reasons) == 0 && abs(difference) > max(1, 1e-6 * abs(reserves_tab))) {
    reasons <- paste("Some rows are missing a policy start, incident or report date, so the triangles",
                     "on different bases include slightly different claims.")
  }
  list(reserves_tab = reserves_tab, difference = difference, parts = parts, reasons = reasons)
}
