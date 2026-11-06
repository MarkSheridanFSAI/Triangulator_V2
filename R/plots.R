# Charts ------------------------------------------------------------------------------
# Built with plotly so they are interactive (hover for values, zoom, download as PNG).

finish_plot <- function(p, y_format = ",.0f", ...) {
  p %>%
    plotly::layout(
      font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, Arial, sans-serif"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.18),
      margin = list(t = 20, r = 10),
      yaxis = list(tickformat = y_format, title = ""),
      xaxis = list(title = ""),
      ...
    ) %>%
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

empty_plot <- function(message) {
  plotly::plot_ly() %>%
    plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                   annotations = list(text = message, showarrow = FALSE, font = list(size = 16))) %>%
    plotly::config(displayModeBar = FALSE)
}

# Stacked bars, one colour per group. `long` has columns x, group, value.
stacked_bars <- function(long, y_format = ",.0f", x_order = NULL) {
  groups <- unique(long$group)
  if (!is.null(x_order)) long$x <- factor(long$x, levels = x_order)
  p <- plotly::plot_ly()
  cols <- group_colours(length(groups))
  for (i in seq_along(groups)) {
    sub <- long[long$group == groups[i], ]
    p <- p %>% plotly::add_bars(x = sub$x, y = sub$value, name = plot_safe(groups[i]),
                                marker = list(color = cols[i]))
  }
  finish_plot(p, y_format = y_format, barmode = "stack")
}

plot_status_amounts <- function(summary, groupings) {
  long <- dplyr::tibble(
    group = rep(group_label(summary, groupings), 2),
    x     = rep(c("Settled claims", "Open claims"), each = nrow(summary)),
    value = c(summary$incurred_settled_claims, summary$incurred_open_claims)
  )
  stacked_bars(long, x_order = c("Settled claims", "Open claims"))
}

plot_status_counts <- function(summary, groupings) {
  long <- dplyr::tibble(
    group = rep(group_label(summary, groupings), 2),
    x     = rep(c("Settled claims", "Open claims"), each = nrow(summary)),
    value = c(summary$claims_settled, summary$claims_open)
  )
  stacked_bars(long, x_order = c("Settled claims", "Open claims"))
}

plot_averages <- function(summary, groupings) {
  labels <- plot_safe(group_label(summary, groupings))
  measures <- c(average_settled_claim = "Settled claims", average_open_claim = "Open claims",
                average_claim = "All claims")
  cols <- c("#2457a6", "#e8a33d", "#6c757d")
  p <- plotly::plot_ly()
  for (i in seq_along(measures)) {
    p <- p %>% plotly::add_bars(x = labels, y = summary[[names(measures)[i]]], name = measures[[i]],
                                marker = list(color = cols[i]))
  }
  finish_plot(p, barmode = "group")
}

plot_claim_sizes <- function(buckets) {
  groups <- setdiff(names(buckets), c("bucket", "All groups"))
  long <- tidyr::pivot_longer(buckets[c("bucket", groups)], -bucket, names_to = "group", values_to = "value")
  long$x <- as.character(long$bucket)
  stacked_bars(long, x_order = levels(buckets$bucket)) %>%
    plotly::layout(xaxis = list(tickangle = -45))
}

# Latest value per origin period: stacked areas for amounts and counts, lines for ratios.
plot_origin <- function(oa, kind) {
  groups <- setdiff(names(oa), "origin")
  cols <- group_colours(length(groups))
  stack <- kind %in% c("amount", "count")
  p <- plotly::plot_ly()
  for (i in seq_along(groups)) {
    p <- p %>% plotly::add_trace(
      x = oa$origin, y = oa[[groups[i]]], name = plot_safe(groups[i]), type = "scatter",
      mode = if (stack) "lines" else "lines+markers", stackgroup = if (stack) "one" else NULL,
      line = list(color = cols[i]), fillcolor = if (stack) cols[i] else NULL
    )
  }
  finish_plot(p, y_format = y_format_for(kind)) %>%
    plotly::layout(xaxis = list(type = "category", tickangle = -45))
}

y_format_for <- function(kind) switch(kind, percent = ".0%", factor = ".2f", ",.0f")
hover_format_for <- function(kind) switch(kind, percent = ".1%", factor = ".3f", ",.0f")

# Heatmap of one triangle. Red circles mark values more than 3 standard
# deviations from the average of their development period.
plot_triangle_heatmap <- function(tri, measure, time) {
  kind <- measure_kind(measure)
  d <- tri[tri$variable == measure, ]
  if (nrow(d) == 0 || all(is.na(d$value))) return(empty_plot("No values to show for this selection"))
  origin_levels <- format_origin(sort(unique(d$origin), decreasing = TRUE), time)
  d$origin_label <- factor(format_origin(d$origin, time), levels = origin_levels)
  d <- d %>%
    group_by(development) %>%
    mutate(z = (value - mean(value, na.rm = TRUE)) / stats::sd(value, na.rm = TRUE)) %>%
    ungroup()
  outliers <- d[!is.na(d$z) & abs(d$z) > 3, ]

  p <- plotly::plot_ly(
    d, x = ~development, y = ~origin_label, z = ~value, type = "heatmap",
    colorscale = "Viridis", xgap = 1, ygap = 1,
    hovertemplate = paste0("Origin %{y}<br>Development period %{x}<br>Value %{z:",
                           hover_format_for(kind), "}<extra></extra>"),
    colorbar = list(title = "", tickformat = y_format_for(kind), orientation = "h",
                    x = 0.5, xanchor = "center", y = 1.02, yanchor = "bottom",
                    len = 0.7, thickness = 12)
  )
  if (nrow(outliers)) {
    p <- p %>% plotly::add_markers(
      data = outliers, x = ~development, y = ~origin_label, inherit = FALSE,
      marker = list(color = "rgba(0,0,0,0)", size = 14, line = list(color = "red", width = 2.5)),
      hoverinfo = "skip", showlegend = FALSE
    )
  }
  finish_plot(p, y_format = NULL) %>%
    plotly::layout(xaxis = list(title = "Development period", dtick = 1),
                   yaxis = list(title = "", type = "category", tickformat = NULL))
}
