# Initial reserve estimates (basic chain-ladder) -----------------------------------
#
# For each group, using incurred (ITOT) triangles:
#   PAID   = latest paid to date
#   CASE   = latest incurred - paid (the case reserves already held)
#   IBNER  = chain-ladder ultimate on the reported basis - latest incurred
#            (growth expected on claims already reported)
#   IBNR   = ultimate on the accident basis - ultimate on the reported basis
#            (claims that have happened but aren't reported yet)
#   URR    = ultimate on the underwriting basis - ultimate on the accident basis
#            (claims still to happen on policies already written)
# These are automatic first estimates, not a substitute for a proper reserving review.

RESERVE_METRICS <- c("PAID", "CASE", "IBNER", "IBNR", "URR")
RESERVE_LABELS <- c(PAID = "Paid", CASE = "Case reserves", IBNER = "IBNER",
                    IBNR = "IBNR", URR = "Unexpired risk (URR)")

# Chain-ladder ultimate for each origin period of a one-measure triangle.
# Development factors are volume-weighted averages over all origin periods.
chain_ladder_ultimates <- function(tri, groupings) {
  latest <- tri %>%
    group_by(across(all_of(c(groupings, "origin")))) %>%
    filter(development == max(development)) %>%
    ungroup() %>%
    select(all_of(groupings), origin, latest = value)
  # Column totals; each column is linked to the newest origin period in it.
  columns <- tri %>%
    group_by(across(all_of(c(groupings, "development")))) %>%
    summarise(origin = max(origin), column_total = sum(value), .groups = "drop") %>%
    select(-development)
  latest %>%
    left_join(columns, by = c(groupings, "origin")) %>%
    arrange(across(all_of(c(groupings, "origin")))) %>%
    group_by(across(all_of(groupings))) %>%
    mutate(
      # factor from this origin's latest development period to the next one
      factor = lag(column_total) / (column_total - latest),
      factor = if_else(is.finite(factor), factor, 1),
      to_ultimate = cumprod(factor),
      ultimate = latest * to_ultimate
    ) %>%
    ungroup()
}

latest_total <- function(tri, groupings, measure) {
  tri %>%
    filter(variable == measure) %>%
    group_by(across(all_of(c(groupings, "origin")))) %>%
    filter(development == max(development)) %>%
    group_by(across(all_of(groupings))) %>%
    summarise(total = sum(value), .groups = "drop")
}

ultimate_total <- function(tri, groupings) {
  chain_ladder_ultimates(tri %>% filter(variable == "ITOT"), groupings) %>%
    group_by(across(all_of(groupings))) %>%
    summarise(total = sum(ultimate), .groups = "drop")
}

# Reserve components for every group. `triangles` is the list from
# build_all_triangles(); `time` is "monthly", "quarterly" or "yearly".
# Returns a long table: groupings..., metric, value.
reserve_components <- function(triangles, groupings, time) {
  get_tri <- function(basis) {
    tri <- triangles[[paste(basis, time, sep = "_")]]
    if (is.null(tri)) {
      stop_friendly("Reserves need every row to have a policy start, incident and report date. ",
                    "The ", names(BASIS_CHOICES)[BASIS_CHOICES == basis], " triangle is empty.")
    }
    tri
  }
  reporting <- get_tri("reporting")
  by <- groupings
  latest_total(reporting, by, "ITOT") %>% rename(INCURRED = total) %>%
    left_join(latest_total(reporting, by, "PTOT") %>% rename(PAID = total), by = by) %>%
    left_join(ultimate_total(reporting, by) %>% rename(reporting_ult = total), by = by) %>%
    left_join(ultimate_total(get_tri("incident"), by) %>% rename(incident_ult = total), by = by) %>%
    left_join(ultimate_total(get_tri("underwriting"), by) %>% rename(underwriting_ult = total), by = by) %>%
    mutate(CASE  = INCURRED - PAID,
           IBNER = reporting_ult - INCURRED,
           IBNR  = incident_ult - reporting_ult,
           URR   = underwriting_ult - incident_ult) %>%
    select(all_of(groupings), all_of(RESERVE_METRICS)) %>%
    tidyr::pivot_longer(all_of(RESERVE_METRICS), names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, levels = RESERVE_METRICS))
}

# Wide table: one row per component plus the ultimate, one column per group.
reserve_table <- function(components, groupings) {
  wide <- components %>%
    mutate(group = group_label(., groupings)) %>%
    select(metric, group, value) %>%
    tidyr::pivot_wider(names_from = group, values_from = value) %>%
    arrange(metric)
  wide$`All groups` <- rowSums(wide[, -1, drop = FALSE])
  totals <- wide %>% summarise(across(-metric, sum))
  wide$metric <- unname(RESERVE_LABELS[as.character(wide$metric)])
  bind_rows(wide, dplyr::tibble(metric = "Ultimate (total)") %>% bind_cols(totals)) %>%
    rename(Component = metric)
}

# Waterfall chart for one group (or the total of all groups).
reserve_waterfall <- function(values, title, ds = DEFAULT_DISPLAY) {
  values[is.na(values)] <- 0
  labels_text <- fmt_money(unname(values), ds)
  total_text <- fmt_money(sum(values), ds)
  values <- values / ds$divisor
  short_labels <- c(PAID = "Paid", CASE = "Case", IBNER = "IBNER", IBNR = "IBNR", URR = "URR")
  waterfalls::waterfall(
    values = unname(values), labels = unname(short_labels[names(values)]),
    rect_text_labels = labels_text,
    calc_total = TRUE, total_axis_text = "Ultimate",
    total_rect_text = total_text,
    fill_by_sign = FALSE,
    fill_colours = c("#9db4d9", "#c3d3ea", "#f2c98a", "#f0a58a", "#a8d5b0")[seq_along(values)],
    total_rect_color = "#d9dee5", total_rect_border_color = "#555555",
    total_rect_text_color = "black", rect_border = "#555555", rect_text_size = 1.2
  ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(prefix = ds$symbol, suffix = ds$suffix)) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
}
