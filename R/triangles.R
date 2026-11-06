# Building claims development triangles -----------------------------------------
#
# How it works, in plain English:
#  1. Each row is a "snapshot" of a claim at a valuation date.
#  2. Every date is turned into a period number (month, quarter or year).
#  3. The origin period comes from the basis date (incident, report or policy
#     start). Development period 1 is the origin period itself, 2 the next, etc.
#  4. For each claim we keep its last snapshot in each period. That snapshot's
#     figures apply from its own development period until the period before
#     the claim's next snapshot (or up to the latest valuation for the last one).
#  5. A triangle cell is the total, over claims, of the figures that apply in
#     that cell.

BASIS_COLUMNS <- c(underwriting = "date_issue", incident = "date_incident", reporting = "date_reported")
BASE_VARIABLES <- c("PTOT", "ITOT", "NTOT", "STOT")

# Period number for a date. Consecutive periods have consecutive numbers.
period_index <- function(date, time) {
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  switch(time,
         monthly   = year * 12L + month - 1L,
         quarterly = year * 4L + (month - 1L) %/% 3L,
         yearly    = year,
         stop("Unknown period: ", time))
}

# Label for a period number: YYYYMM, where MM is the last month of the period
# (202303 = March 2023 / first quarter of 2023; 202312 = the year 2023).
period_label <- function(index, time) {
  switch(time,
         monthly   = (index %/% 12L) * 100L + index %% 12L + 1L,
         quarterly = (index %/% 4L) * 100L + (index %% 4L + 1L) * 3L,
         yearly    = index * 100L + 12L,
         stop("Unknown period: ", time))
}

# One row per claim per period (its last snapshot in that period), with the
# development period it applies from and to. Returns NULL if no rows have the
# basis date. The latest valuation period is stored as an attribute.
claim_snapshots <- function(claims, basis, time) {
  basis_col <- BASIS_COLUMNS[[basis]]
  d <- claims[!is.na(claims[[basis_col]]), ]
  if (nrow(d) == 0) return(NULL)
  # The latest valuation period in the data is the triangle's "diagonal".
  valuation <- max(period_index(claims$date_stamp, time),
                   period_index(d[[basis_col]], time), na.rm = TRUE)
  d$origin_index <- period_index(d[[basis_col]], time)
  d$stamp_index  <- period_index(d$date_stamp, time)
  d <- d %>%
    arrange(claim_id, date_stamp) %>%
    group_by(claim_id, stamp_index) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    arrange(claim_id, date_stamp) %>%
    group_by(claim_id) %>%
    mutate(development    = stamp_index - origin_index + 1L,
           development_to = coalesce(lead(development) - 1L, valuation - origin_index + 1L)) %>%
    ungroup() %>%
    mutate(origin = period_label(origin_index, time))
  attr(d, "valuation_index") <- valuation
  d
}

# Claims that are ever above the large-loss threshold (incurred).
large_claim_ids <- function(snapshots, threshold) {
  threshold <- suppressWarnings(as.numeric(threshold))
  if (length(threshold) != 1 || is.na(threshold) || !is.finite(threshold)) return(character(0))
  unique(snapshots$claim_id[snapshots$ITOT > threshold])
}

# Build the triangles for one basis and period, for every group, with all
# measures. Returns a long table: groupings..., variable, origin, development, value.
create_triangles <- function(claims, groupings, basis = "incident", time = "quarterly", large_loss = Inf) {
  snaps <- claim_snapshots(claims, basis, time)
  if (is.null(snaps)) return(NULL)
  valuation <- attr(snaps, "valuation_index")
  first_origin <- min(snaps$origin_index)
  n <- valuation - first_origin + 1L

  # An empty triangle shape (all zeros) so there are no gaps.
  origins <- dplyr::tibble(origin_index = first_origin:valuation) %>%
    mutate(origin = period_label(origin_index, time), i = origin_index - first_origin + 1L)
  blank <- tidyr::crossing(origins, development = seq_len(n)) %>%
    filter(development <= n - i + 1L) %>%
    transmute(origin, development, value = 0)

  long <- snaps %>%
    select(all_of(groupings), claim_id, origin, development, development_to, all_of(BASE_VARIABLES)) %>%
    tidyr::pivot_longer(all_of(BASE_VARIABLES), names_to = "variable", values_to = "value")
  blank_all <- long %>%
    distinct(across(all_of(c(groupings, "variable")))) %>%
    tidyr::crossing(blank)

  large <- large_claim_ids(snaps, large_loss)
  dt <- data.table::as.data.table(long[!long$claim_id %in% large, ])
  cells <- data.table::rbindlist(lapply(seq_len(n), function(x) {
    dt[development <= x & development_to >= x,
       .(development = x, value = sum(value)),
       keyby = c(groupings, "variable", "origin")]
  }))

  base <- bind_rows(dplyr::as_tibble(cells), blank_all) %>%
    group_by(across(all_of(c(groupings, "variable", "origin", "development")))) %>%
    summarise(value = sum(value), .groups = "drop")

  add_derived_measures(base, groupings)
}

# Add ratios, differences and development factors to the four base measures.
add_derived_measures <- function(base, groupings) {
  base <- base %>% filter(variable %in% BASE_VARIABLES)
  bind_rows(
    base,
    triangle_calculator(base, groupings, "ITOT", "NTOT", "divided_by"),
    triangle_calculator(base, groupings, "PTOT", "STOT", "divided_by"),
    triangle_calculator(base, groupings, "ITOT", "PTOT", "minus"),
    triangle_calculator(base, groupings, "NTOT", "STOT", "minus"),
    triangle_calculator(base, groupings, "PTOT", "ITOT", "divided_by"),
    triangle_calculator(base, groupings, "STOT", "NTOT", "divided_by"),
    triangle_links(base, groupings, "ITOT"),
    triangle_links(base, groupings, "PTOT"),
    triangle_links(base, groupings, "NTOT"),
    triangle_links(base, groupings, "STOT")
  )
}

# Combine two measures cell by cell. Division by zero gives a blank (NA).
triangle_calculator <- function(triangles, groupings, variable_1, variable_2, operation) {
  op <- switch(operation,
               plus = `+`, minus = `-`, times = `*`, divided_by = `/`,
               stop("Unknown operation: ", operation))
  triangles %>%
    filter(variable %in% c(variable_1, variable_2)) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    mutate(value = op(.data[[variable_1]], .data[[variable_2]]),
           value = if_else(is.finite(value), value, NA_real_),
           variable = paste(variable_1, operation, variable_2, sep = "_")) %>%
    select(all_of(groupings), variable, origin, development, value)
}

# Development factors (link ratios): next development period / this one.
triangle_links <- function(triangles, groupings, variable_1) {
  triangles %>%
    filter(variable == variable_1) %>%
    arrange(across(all_of(c(groupings, "origin", "development")))) %>%
    group_by(across(all_of(c(groupings, "origin")))) %>%
    mutate(value = lead(value) / value) %>%
    ungroup() %>%
    mutate(value = if_else(is.finite(value), value, NA_real_),
           variable = paste0(variable_1, "_links")) %>%
    select(all_of(groupings), variable, origin, development, value)
}

# All nine triangle sets (3 bases x 3 periods), named e.g. "incident_quarterly".
build_all_triangles <- function(claims, groupings, progress = NULL) {
  keys <- expand.grid(time = c("monthly", "quarterly", "yearly"),
                      basis = c("underwriting", "incident", "reporting"),
                      stringsAsFactors = FALSE)
  out <- list()
  for (k in seq_len(nrow(keys))) {
    basis <- keys$basis[k]
    time <- keys$time[k]
    if (!is.null(progress)) {
      progress(k - 1, nrow(keys), paste(names(PERIOD_CHOICES)[PERIOD_CHOICES == time],
                                        names(BASIS_CHOICES)[BASIS_CHOICES == basis]))
    }
    out[[paste(basis, time, sep = "_")]] <- create_triangles(claims, groupings, basis, time)
  }
  out
}

# Combine the chosen groups into one triangle. `selection` is a named list with
# one entry per grouping column: a value, or ALL_GROUPS to include every value.
ALL_GROUPS <- "__all__"

select_groups <- function(triangles, groupings, selection) {
  base <- triangles %>% filter(variable %in% BASE_VARIABLES)
  for (g in groupings) {
    chosen <- selection[[g]]
    if (!is.null(chosen) && !identical(chosen, ALL_GROUPS)) base <- base[base[[g]] == chosen, ]
  }
  base <- base %>%
    group_by(variable, origin, development) %>%
    summarise(value = sum(value), .groups = "drop")
  add_derived_measures(base, character(0))
}

# ---- Claim movements (what changed in each cell) --------------------------------

# Each snapshot with the change since that claim's previous snapshot.
claim_movements <- function(claims, groupings, basis, time, large_loss = Inf) {
  snaps <- claim_snapshots(claims, basis, time)
  if (is.null(snaps)) return(NULL)
  large <- large_claim_ids(snaps, large_loss)
  snaps %>%
    filter(!claim_id %in% large) %>%
    group_by(claim_id) %>%
    mutate(across(all_of(BASE_VARIABLES), ~ .x - coalesce(lag(.x), 0), .names = "{.col}_movement")) %>%
    ungroup()
}

# The claims that moved in one cell of the triangle, biggest change first.
cell_movements <- function(movements, groupings, selection, origin, development) {
  m <- movements[movements$origin == origin & movements$development == development, ]
  for (g in groupings) {
    chosen <- selection[[g]]
    if (!is.null(chosen) && !identical(chosen, ALL_GROUPS)) m <- m[m[[g]] == chosen, ]
  }
  total <- sum(m$ITOT_movement, na.rm = TRUE)
  m %>%
    arrange(desc(abs(ITOT_movement))) %>%
    mutate(share_of_change = ITOT_movement / na_if(total, 0),
           percent_change  = ITOT_movement / na_if(ITOT - ITOT_movement, 0)) %>%
    select(policy_id, claim_id, all_of(groupings), share_of_change, percent_change,
           ITOT_movement, PTOT_movement, NTOT_movement, STOT_movement)
}

# A triangle as a grid: one row per origin, one column per development period.
triangle_grid <- function(tri, measure, time) {
  tri %>%
    filter(variable == measure) %>%
    arrange(origin, development) %>%
    mutate(origin = format_origin(origin, time)) %>%
    select(origin, development, value) %>%
    tidyr::pivot_wider(names_from = development, values_from = value)
}
