# TODO: Change functions to return plots instead of plotting, to allow choosing between ggplot and plotly

#' Plot retraction rates per year by country class and scenario
#'
#' @param retraction_rates Output of [compute_retraction_rates()].
#' @param year_limits Range of publication years shown.
#' @return A ggplot object.
#' @export
plot_retraction_rates <- function(retraction_rates, year_limits = c(2000, 2023)) {
  ggplot(
    retraction_rates,
    aes(publication_year, retraction_rate_per_100k, colour = country_class)
  ) +
    geom_line() +
    facet_wrap(~scenario, ncol = 2) +
    scale_x_continuous(
      limits = year_limits,
      breaks = seq(year_limits[1], year_limits[2], by = 5)
    ) +
    labs(y = "Country class retraction fraction", x = "Year", colour = NULL) +
    theme_bw()
}

#' Autorange every axis of a plotly figure
#'
#' [plotly::ggplotly()] creates one axis per facet row/column (`xaxis`,
#' `xaxis2`, `yaxis`, `yaxis2`, ...), so `layout(xaxis = ..., yaxis = ...)`
#' only affects the first ones. This sets `autorange` on all of them, so
#' every subplot rescales when traces are toggled in the legend.
#'
#' @param widget A plotly figure, e.g. the output of [plotly::ggplotly()].
#' @return The plotly figure with every axis autoranged.
#' @export
autorange_axes <- function(widget) {
  widget <- plotly::plotly_build(widget)
  axes <- grep("^[xy]axis[0-9]*$", names(widget$x$layout), value = TRUE)
  for (axis in axes) {
    widget$x$layout[[axis]]$autorange <- TRUE
  }
  widget
}

#' Scatter plot of reason counts in two groups coloured by significance
#'
#' Shows a dashed boundary below which reasons are considered low count and,
#' for each period, the line of the expected count ratio.
#'
#' @param results Output of [flag_low_counts()].
#' @param ratios Output of [group_count_ratios()].
#' @inheritParams flag_low_counts
#' @param boundary_period Period whose ratio defines the low-count boundary.
#' @return A ggplot object.
#' @export
plot_reason_counts_scatter <- function(results, ratios,
                                       group_a = "United Kingdom",
                                       group_b = "Other OECD",
                                       threshold = 500, alpha = 0.05,
                                       boundary_period = grep("^Pre-", names(ratios), value = TRUE)[1]) {
  count_a <- paste0("count_", group_a)
  count_b <- paste0("count_", group_b)

  plot_df <- results %>%
    mutate(
      sig_class = case_when(
        p_adj_filtered < alpha & !low_count ~ "Significant (adequate count)",
        p_adj < alpha & low_count ~ "Significant (low count)",
        TRUE ~ "Not significant"
      )
    )

  boundary <- tibble(
    x = c(0, threshold),
    y = (threshold - x) / ratios[[boundary_period]]
  ) %>%
    filter(y >= 0)

  max_x <- max(plot_df[[count_b]])
  mean_lines <- bind_rows(lapply(names(ratios), function(p) {
    tibble(x = c(0, max_x), y = x / ratios[[p]], label = p)
  }))
  line_labels <- mean_lines %>% group_by(label) %>% slice_tail(n = 1) %>% ungroup()

  ggplot(
    plot_df,
    aes(x = .data[[count_b]], y = .data[[count_a]], colour = sig_class, shape = period)
  ) +
    geom_line(data = boundary, aes(x = x, y = y), inherit.aes = FALSE,
              linetype = "dashed") +
    geom_line(data = mean_lines, aes(x = x, y = y, group = label),
              inherit.aes = FALSE) +
    geom_text(data = line_labels, aes(x = x, y = y, label = label),
              inherit.aes = FALSE, hjust = 1, vjust = -1) +
    geom_point(alpha = 0.7) +
    scale_colour_manual(values = c("grey70", "red", "orange")) +
    labs(
      x = paste0("Count (", group_b, ")"),
      y = paste0("Count (", group_a, ")"),
      colour = paste0("FDR < ", alpha)
    ) +
    coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
    theme_minimal()
}

#' Dumbbell plot of the proportions of significant reasons in two groups
#'
#' @param sig_long Output of [significant_reasons_long()].
#' @return A ggplot object.
#' @export
plot_significant_reasons <- function(sig_long) {
  ggplot(sig_long, aes(proportion, flag)) +
    geom_line(aes(group = interaction(period, flag)), colour = "grey80") +
    geom_point(aes(colour = group), size = 3) +
    facet_grid(period ~ ., scales = "free_y", space = "free_y") +
    scale_y_reordered()
}

#' Plot one value per country for one scenario, highlighting the UK
#'
#' @param dat Output of [compute_ai_rates_by_country()].
#' @param scenario_name Scenario to plot.
#' @param x Column to plot on the (log) x axis.
#' @param xlab X axis label.
#' @param label_size Size of the country labels, in mm (ggplot2 units).
#' @param label_gap Number of spaces between each point and its label.
#' @return A ggplot object.
#' @export
plot_scenario <- function(dat, scenario_name,
                          x = "ai_retraction_rate_per_100k",
                          xlab = "AI Retractions per 100,000 papers (log scale)",
                          label_size = 2.5,
                          label_gap = 2) {
  plot_dat <- dat %>%
    filter(scenario == scenario_name) %>%
    arrange(.data[[x]]) %>%
    mutate(Country = factor(Country, levels = Country),
           # Non-breaking spaces keep a gap between the point and its label
           # (plotly collapses normal spaces and ignores nudges in pixels)
           label = paste0(strrep(" ", label_gap), Country))

  uk_dat <- filter(plot_dat, Country == "United Kingdom")
  non_uk_dat <- filter(plot_dat, Country != "United Kingdom")

  ggplot(plot_dat, aes(x = .data[[x]], y = Country)) +
    geom_point(colour = "grey40", size = 1) +
    geom_point(data = uk_dat, colour = "red", size = 1) +
    geom_text(data = non_uk_dat, aes(label = label), colour = "grey40",
              hjust = 0, vjust = 0.5, size = label_size) +
    geom_text(data = uk_dat, aes(label = label), colour = "red",
              hjust = 0, vjust = 0.5, fontface = "bold", size = label_size) +
    scale_x_log10(expand = expansion(mult = c(0.02, 0.2))) +
    labs(title = scenario_name, x = xlab, y = NULL) +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

#' Height in pixels needed so that the country labels do not overlap
#'
#' Each country takes one unit on the y axis, so the height is the number of
#' countries times the height of a line of text, plus the plot margins.
#'
#' @param n_countries Number of countries (rows) in the plot.
#' @param label_size Size of the labels, in mm (ggplot2 units).
#' @param line_spacing Line height relative to the font size.
#' @param margins Pixels taken by the title, x axis and margins.
#' @param min_height Minimum height in pixels.
#' @return The height in pixels.
#' @export
scenario_plot_height <- function(n_countries, label_size = 2.5,
                                 line_spacing = 1.3, margins = 110,
                                 min_height = 300) {
  font_px <- label_size * .pt * 96 / 72
  # The discrete y axis adds 0.6 units of padding on each side
  max(min_height, ceiling((n_countries + 0.2) * font_px * line_spacing + margins))
}

#' Plot every scenario with [plot_scenario()]
#'
#' @inheritParams plot_scenario
#' @param ... Passed to [plot_scenario()].
#' @param interactive If `TRUE`, each plot is converted with
#'   [plotly::ggplotly()] using a height from [scenario_plot_height()], and
#'   the plots are returned as an [htmltools::tagList()] that renders
#'   directly in Quarto/R Markdown.
#' @return A named list of ggplot objects, one per scenario, or a tagList of
#'   plotly widgets when `interactive = TRUE`.
#' @export
plot_all_scenarios <- function(dat, ..., label_size = 2.5, interactive = FALSE) {
  scenarios <- unique(dat$scenario)
  plots <- stats::setNames(
    lapply(scenarios, function(s) plot_scenario(dat, s, ..., label_size = label_size)),
    scenarios
  )
  if (!interactive) {
    return(plots)
  }
  widgets <- lapply(plots, function(p) {
    widget <- plotly::ggplotly(p, height = scenario_plot_height(nrow(p$data), label_size))
    # ggplotly() ignores hjust/vjust in geom_text, so place the labels to
    # the right of the points explicitly
    plotly::style(widget, textposition = "middle right")
  })
  do.call(htmltools::tagList, widgets)
}

#' Plot a yearly AI retraction measure by country class and scenario
#'
#' @param data Output of [compute_ai_counts_ts()] or
#'   [compute_ai_proportion_ts()].
#' @param y Column to plot on the (log) y axis.
#' @param ylab Y axis label.
#' @param year_limits Range of publication years shown.
#' @return A ggplot object.
#' @export
plot_ai_ts <- function(data, y = "ai_retractions",
                       ylab = "AI Retractions (counts)",
                       year_limits = c(2000, 2025)) {
  ggplot(data, aes(publication_year, .data[[y]], colour = country_class)) +
    geom_point() +
    facet_wrap(~scenario, ncol = 2) +
    scale_x_continuous(
      limits = year_limits,
      breaks = seq(year_limits[1], year_limits[2], by = 5)
    ) +
    scale_y_log10() +
    labs(y = ylab, x = "Year", colour = NULL) +
    theme_bw()
}

#' Plot the evolution of the top retraction reasons over time
#'
#' One line per reason ranked in the top `n_top` of at least one year (every
#' reason if `n_top = NULL`), restricted to `reasons_list` if given, showing
#' the proportion of all that year's retractions citing the reason.
#'
#' @inheritParams compute_top_reasons_over_time
#' @return A ggplot object.
#' @export
plot_retraction_reasons_over_time <- function(retraction_data,
                                              n_top = 3,
                                              year_col = "retraction_year",
                                              min_retractions_per_year = 100,
                                              year_limits = NULL,
                                              exclude_reasons = NULL,
                                              reasons_list = NULL) {
  title <- if (!is.null(n_top)) {
    paste0("Top ", n_top, " retraction reasons of each year")
  } else if (!is.null(reasons_list)) {
    "Selected retraction reasons"
  } else {
    "All retraction reasons"
  }

  compute_top_reasons_over_time(
    retraction_data,
    n_top = n_top,
    year_col = year_col,
    min_retractions_per_year = min_retractions_per_year,
    year_limits = year_limits,
    exclude_reasons = exclude_reasons,
    reasons_list = reasons_list
  ) %>%
    ggplot(aes(year, proportion, colour = Reason)) +
    geom_line() +
    geom_point(size = 1) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
      x = "Year",
      y = "Proportion of retractions citing the reason",
      colour = NULL,
      title = title
    ) +
    theme_bw()
}
