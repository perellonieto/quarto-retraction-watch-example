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

#' Convert a ggplot to plotly keeping the panel heights of `space = "free_y"`
#'
#' [plotly::ggplotly()] ignores `facet_grid(space = "free_y")` and gives every
#' row of panels the same height, so a panel with few categories takes as
#' much space as one with many. This function resizes each row of panels in
#' proportion to its y range, as ggplot2 does, keeping the gaps between rows,
#' and moves the facet strips (labels and boxes on the right) with them.
#'
#' @param p A ggplot object faceted by rows with [ggplot2::facet_grid()].
#' @param ... Passed to [plotly::ggplotly()] (e.g. `height`).
#' @return A plotly object.
#' @export
ggplotly_free_space <- function(p, ...) {
  widget <- plotly::plotly_build(plotly::ggplotly(p, ...))
  built <- ggplot2::ggplot_build(p)
  panel_heights <- vapply(built$layout$panel_params,
                          function(params) diff(params$y.range), numeric(1))
  row_heights <- tapply(panel_heights, built$layout$layout$ROW, max)

  layout <- widget$x$layout
  y_axes <- paste0("yaxis", c("", seq_along(row_heights)[-1]))
  if (!all(y_axes %in% names(layout)) ||
      length(grep("^yaxis[0-9]*$", names(layout))) != length(row_heights)) {
    stop("Expected one plotly y axis per row of panels (facet_grid by rows).")
  }

  # Old domains, from the top row to the bottom one
  old <- lapply(y_axes, function(axis) layout[[axis]]$domain)
  gaps <- vapply(seq_along(old)[-1],
                 function(i) old[[i - 1]][1] - old[[i]][2], numeric(1))
  available <- old[[1]][2] - old[[length(old)]][1] - sum(gaps)
  heights <- available * row_heights / sum(row_heights)
  top <- old[[1]][2]
  new <- vector("list", length(old))
  for (i in seq_along(old)) {
    new[[i]] <- c(top - heights[[i]], top)
    top <- top - heights[[i]] - if (i < length(old)) gaps[[i]] else 0
  }
  # Avoid floating point drift at the bottom edge
  new[[length(new)]][1] <- old[[length(old)]][1]

  # Maps a y position (in paper units) inside an old domain to the new one
  rescale_y <- function(y) {
    for (i in seq_along(old)) {
      if (y >= old[[i]][1] - 1e-9 && y <= old[[i]][2] + 1e-9) {
        return(new[[i]][1] + (y - old[[i]][1]) / diff(old[[i]]) * diff(new[[i]]))
      }
    }
    y
  }

  for (i in seq_along(y_axes)) {
    widget$x$layout[[y_axes[i]]]$domain <- new[[i]]
  }
  # Facet strips: labels at the right edge (x = 1) and their boxes, whose
  # vertical extent is the domain of their row
  widget$x$layout$annotations <- lapply(layout$annotations, function(a) {
    if (identical(a$yref, "paper") && identical(a$xref, "paper") && a$x >= 1) {
      a$y <- rescale_y(a$y)
    }
    a
  })
  widget$x$layout$shapes <- lapply(layout$shapes, function(s) {
    if (identical(s$yref, "paper")) {
      s$y0 <- rescale_y(s$y0)
      s$y1 <- rescale_y(s$y1)
    }
    s
  })
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
    # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
    suppressWarnings(geom_point(aes(text = paste("flag:", .data$flag)), alpha = 0.7)) +
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
#' @param scenario_name Scenario to plot. If several are given, the countries
#'   are ordered within each scenario and the plot has no title, ready to be
#'   faceted by `scenario` (as done by [plot_all_scenarios()]).
#' @param x Column to plot on the x axis. If the columns `<x>_lower` and
#'   `<x>_upper` exist they are drawn as confidence intervals. Rows where
#'   `x` is `NA` are dropped.
#' @param xlab X axis label.
#' @param label_size Size of the country labels, in mm (ggplot2 units).
#' @param label_gap Number of spaces between each point (or the end of its
#'   interval) and its label.
#' @param sigma Scale of the pseudo-log x axis: linear below `sigma` and
#'   logarithmic above, so that zeros can be shown. Defaults to the smallest
#'   positive value of `x` in `dat`, so all scenarios share the same axis.
#' @return A ggplot object.
#' @export
plot_scenario <- function(dat, scenario_name,
                          x = "ai_retraction_rate_per_100k",
                          xlab = "AI retractions per 100,000 papers (pseudo-log scale, 95% CI)",
                          label_size = 2.5,
                          label_gap = 2,
                          sigma = default_sigma(dat[[x]])) {
  several <- length(scenario_name) > 1
  lower <- paste0(x, "_lower")
  upper <- paste0(x, "_upper")
  has_ci <- all(c(lower, upper) %in% names(dat))
  plot_dat <- dat %>%
    filter(scenario %in% scenario_name, !is.na(.data[[x]])) %>%
    arrange(.data[[x]]) %>%
    mutate(is_uk = Country == "United Kingdom",
           # Labels start after the interval, so they do not cover it
           label_x = if (has_ci) pmax(.data[[x]], .data[[upper]], na.rm = TRUE) else .data[[x]],
           # Non-breaking spaces keep a gap between the point and its label
           # (plotly collapses normal spaces and ignores nudges in pixels)
           label = paste0(strrep("\u00a0", label_gap), Country),
           Country = if (several) {
             reorder_within(Country, .data[[x]], scenario)
           } else {
             factor(Country, levels = Country)
           })

  uk_dat <- filter(plot_dat, is_uk)
  non_uk_dat <- filter(plot_dat, !is_uk)

  p <- ggplot(plot_dat, aes(x = .data[[x]], y = Country))
  if (has_ci) {
    p <- p +
      geom_segment(aes(x = .data[[lower]], xend = .data[[upper]], yend = Country),
                   colour = "grey75", linewidth = 0.4)
    # ggplotly() fails on a segment layer without rows, e.g. when the UK is
    # not among the countries plotted
    if (nrow(uk_dat) > 0) {
      p <- p +
        geom_segment(data = uk_dat,
                     aes(x = .data[[lower]], xend = .data[[upper]], yend = Country),
                     colour = "red", alpha = 0.5, linewidth = 0.4)
    }
  }
  p +
    geom_point(colour = "grey40", size = 1) +
    geom_point(data = uk_dat, colour = "red", size = 1) +
    geom_text(data = non_uk_dat, aes(x = label_x, label = label), colour = "grey40",
              hjust = 0, vjust = 0.5, size = label_size) +
    geom_text(data = uk_dat, aes(x = label_x, label = label), colour = "red",
              hjust = 0, vjust = 0.5, fontface = "bold", size = label_size) +
    scale_x_continuous(trans = scales::pseudo_log_trans(sigma = sigma, base = 10),
                       breaks = pseudo_log_breaks(sigma),
                       labels = scales::label_number(drop0trailing = TRUE, big.mark = ","),
                       expand = expansion(mult = c(0.02, 0.2))) +
    labs(title = if (!several) scenario_name, x = xlab, y = NULL) +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

# Smallest positive value, used as the linear range of the pseudo-log axis
default_sigma <- function(values) {
  positive <- values[!is.na(values) & values > 0]
  if (length(positive) == 0) 1 else min(positive)
}

# Zero plus the powers of ten from about sigma upwards
pseudo_log_breaks <- function(sigma) {
  function(limits) {
    powers <- 10^seq(floor(log10(sigma)), ceiling(log10(max(limits[2], sigma))))
    c(0, powers[powers >= sigma / 2 & powers <= limits[2]])
  }
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
#' @param ncol If not `NULL`, combine all the scenarios into one figure with
#'   this number of columns: a ggplot faceted by scenario or, when
#'   `interactive = TRUE`, a [plotly::subplot()] where each row is as tall as
#'   its tallest scenario.
#' @return A named list of ggplot objects, one per scenario, or a tagList of
#'   plotly widgets when `interactive = TRUE`. With `ncol`, a single ggplot or
#'   plotly figure. Scenarios where `x` is always `NA` (e.g. the rate in
#'   domestic-only scenarios) are left out.
#' @export
plot_all_scenarios <- function(dat, x = "ai_retraction_rate_per_100k", ...,
                               label_size = 2.5, interactive = FALSE,
                               ncol = NULL) {
  scenarios <- unique(dat$scenario[!is.na(dat[[x]])])
  if (!interactive && !is.null(ncol)) {
    return(plot_scenario(dat, scenarios, x = x, ..., label_size = label_size) +
             facet_wrap(~scenario, ncol = ncol, scales = "free_y"))
  }
  plots <- stats::setNames(
    lapply(scenarios, function(s) plot_scenario(dat, s, x = x, ..., label_size = label_size)),
    scenarios
  )
  if (!interactive) {
    return(plots)
  }
  heights <- vapply(plots, function(p) scenario_plot_height(nrow(p$data), label_size),
                    numeric(1))
  widgets <- Map(function(p, height) {
    widget <- plotly::ggplotly(p, height = height)
    # ggplotly() ignores hjust/vjust in geom_text, so place the labels to
    # the right of the points explicitly
    plotly::style(widget, textposition = "middle right")
  }, plots, heights)
  if (is.null(ncol)) {
    return(do.call(htmltools::tagList, unname(widgets)))
  }

  # subplot() keeps a single title, so show each one as an annotation, which
  # subplot() moves to the top of its panel
  widgets <- Map(function(widget, title) {
    widget <- plotly::plotly_build(widget)
    widget$x$layout$title <- NULL
    widget$x$layout$annotations <- c(widget$x$layout$annotations, list(list(
      text = title, x = 0.5, y = 1, xref = "paper", yref = "paper",
      xanchor = "center", yanchor = "bottom", showarrow = FALSE,
      font = list(size = 14)
    )))
    widget
  }, widgets, names(widgets))
  # Lay out each row in pixels: a band for the title, the panel and a band for
  # the x axis ticks and title. subplot() takes fractions of the plotting area
  # (the figure without its outer margins) and removes the top and bottom
  # margins only between rows, so the bands of the first and last rows go in
  # the outer margins instead
  title_px <- 30
  axis_px <- 60
  outer_margin <- list(t = title_px, b = axis_px)
  panel_px <- vapply(plots, function(p) {
    scenario_plot_height(nrow(p$data), label_size, margins = 0, min_height = 150)
  }, numeric(1))
  row_px <- as.vector(tapply(panel_px, (seq_along(panel_px) - 1) %/% ncol, max)) +
    title_px + axis_px
  row_px[1] <- row_px[1] - title_px
  row_px[length(row_px)] <- row_px[length(row_px)] - axis_px
  area_px <- sum(row_px)
  fig <- plotly::subplot(unname(widgets), nrows = length(row_px),
                         heights = row_px / area_px,
                         margin = c(0.02, 0.02, title_px / area_px, axis_px / area_px),
                         titleX = TRUE)
  fig <- plotly::layout(fig, margin = outer_margin)
  # subplot() copies layout.height from the first widget, which plotly.js
  # uses over the widget height, so set both
  fig$height <- area_px + outer_margin$t + outer_margin$b
  fig$x$layout$height <- fig$height
  fig
}

#' Plot a yearly AI retraction measure by country class and scenario
#'
#' @param data Output of [compute_ai_counts_ts()] or
#'   [compute_ai_proportion_ts()].
#' @param y Column to plot on the y axis. If the columns `<y>_lower` and
#'   `<y>_upper` exist they are drawn as confidence bands.
#' @param ylab Y axis label.
#' @param year_limits Range of publication years shown.
#' @param labels Labels of the y axis breaks, e.g. [scales::label_percent()]
#'   for proportions.
#' @return A ggplot object, with a pseudo-log y axis (see [plot_scenario()])
#'   so that zeros are shown.
#' @export
plot_ai_ts <- function(data, y = "ai_retractions",
                       ylab = "AI Retractions (counts)",
                       year_limits = c(2000, 2025),
                       labels = scales::label_number(drop0trailing = TRUE, big.mark = ",")) {
  data <- filter(data, publication_year >= year_limits[1],
                 publication_year <= year_limits[2])
  sigma <- default_sigma(data[[y]])
  lower <- paste0(y, "_lower")
  upper <- paste0(y, "_upper")

  p <- ggplot(data, aes(publication_year, .data[[y]], colour = country_class))
  if (all(c(lower, upper) %in% names(data))) {
    p <- p + geom_ribbon(aes(ymin = .data[[lower]], ymax = .data[[upper]],
                             fill = country_class),
                         colour = NA, alpha = 0.15)
  }
  p +
    geom_line() +
    geom_point() +
    facet_wrap(~scenario, ncol = 2) +
    scale_x_continuous(
      limits = year_limits,
      breaks = seq(year_limits[1], year_limits[2], by = 5)
    ) +
    scale_y_continuous(trans = scales::pseudo_log_trans(sigma = sigma, base = 10),
                       breaks = pseudo_log_breaks(sigma), labels = labels) +
    labs(y = ylab, x = "Year", colour = NULL, fill = NULL) +
    theme_bw()
}

#' Plot the yearly proportion of selected reasons for several groups
#'
#' One panel per group, stacked vertically with a shared y axis and the
#' same colour for each reason in every panel, so the groups can be compared
#' directly. Proportions are computed as in [compute_top_reasons_over_time()].
#'
#' @param data_list Named list of retraction data, one per group (e.g.
#'   `list("United Kingdom" = uk, "Other OECD" = oecd)`). The names are the
#'   panel titles.
#' @param reasons_list Character vector of reasons to plot.
#' @param min_retractions_per_year Years with fewer retractions are dropped.
#'   Either one value or one per group.
#' @inheritParams compute_top_reasons_over_time
#' @return A ggplot object.
#' @export
plot_reasons_over_time_by_group <- function(data_list, reasons_list,
                                            year_col = "retraction_year",
                                            min_retractions_per_year = 0,
                                            year_limits = NULL) {
  min_retractions_per_year <- rep_len(min_retractions_per_year, length(data_list))
  proportions <- bind_rows(Map(function(data, min_per_year) {
    compute_top_reasons_over_time(data, n_top = NULL, year_col = year_col,
                                  min_retractions_per_year = min_per_year,
                                  year_limits = year_limits,
                                  reasons_list = reasons_list)
  }, data_list, min_retractions_per_year), .id = "group") %>%
    mutate(group = factor(group, levels = names(data_list)),
           Reason = factor(Reason, levels = reasons_list))

  ggplot(proportions, aes(year, proportion, colour = Reason)) +
    geom_line() +
    geom_point(size = 1) +
    facet_wrap(~group, ncol = 1) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x = "Year", y = "Proportion of retractions citing the reason",
         colour = NULL) +
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
                                              reasons_list = NULL,
                                              exclude_mass_retractions = FALSE) {
  title <- if (!is.null(n_top)) {
    paste0("Top ", n_top, " retraction reasons of each year")
  } else if (!is.null(reasons_list)) {
    "Selected retraction reasons"
  } else {
    "All retraction reasons"
  }

  if (exclude_mass_retractions) {
    title <- paste(title, "(no mass retractions)")
    retraction_data <- retraction_data[retraction_data$Mass_Retraction == "Other", ]
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

#' Plot the difference in the yearly proportions of retraction reasons
#'
#' For each reason in `reasons_list`, plots the proportion of `data_a`
#' retractions citing the reason minus the proportion of `data_b`
#' retractions citing it, year by year. Proportions are computed with
#' [compute_top_reasons_over_time()], so they are relative to all the
#' retractions of the year in each dataset. Positive values mean the reason
#' is cited more often in `data_a`. Only years present in both datasets are
#' shown.
#'
#' @param data_a,data_b Two outputs of [load_retraction_data()], or subsets
#'   of it (e.g. one per country group).
#' @param reasons_list Character vector (or list) of the reasons to compare.
#' @param labels Names of the two datasets used in the axis label and title.
#' @inheritParams compute_top_reasons_over_time
#' @return A ggplot object.
#' @export
plot_reason_proportion_differences <- function(data_a,
                                               data_b,
                                               reasons_list,
                                               labels = c("Group A", "Group B"),
                                               year_col = "retraction_year",
                                               year_limits = NULL,
                                               min_retractions_per_year = 0) {
  differences <- yearly_reason_proportion_differences(
    data_a, data_b, reasons_list, year_col, year_limits,
    min_retractions_per_year
  )

  ggplot(differences, aes(year, difference, colour = Reason)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_line() +
    geom_point(size = 1) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
      x = "Year",
      y = paste0("Proportion difference (", labels[1], " - ", labels[2], ")"),
      colour = NULL,
      title = paste0("Difference in yearly proportions of retraction reasons: ",
                     labels[1], " vs ", labels[2])
    ) +
    theme_bw()
}

#' Forest plot of the yearly proportions of retraction reasons in two datasets
#'
#' Summarises [plot_reason_proportion_differences()] with one row per reason
#' in `reasons_list` and dataset: the point is the mean, across years, of the
#' yearly proportion of the dataset's retractions citing the reason, and the
#' whiskers span the interquartile range (25th to 75th percentile) of those
#' yearly proportions. Proportions are computed as in
#' [compute_top_reasons_over_time()], every year counts the same, whatever
#' its number of retractions, and only years present in both datasets are
#' used. Reasons are sorted by their mean proportion in `data_a`.
#'
#' @inheritParams plot_reason_proportion_differences
#' @return A ggplot object. Its data has one row per reason and dataset with
#'   columns `Reason`, `group` (from `labels`), `mean_proportion`, `q25`,
#'   `q75`, `n_years` and `y` (the vertical position of the row).
#' @export
plot_reason_proportions_forest <- function(data_a,
                                           data_b,
                                           reasons_list,
                                           labels = c("Group A", "Group B"),
                                           year_col = "retraction_year",
                                           year_limits = NULL,
                                           min_retractions_per_year = 0) {
  summary <- yearly_reason_proportion_differences(
    data_a, data_b, reasons_list, year_col, year_limits,
    min_retractions_per_year
  ) %>%
    select(year, Reason, proportion_a, proportion_b) %>%
    pivot_longer(c(proportion_a, proportion_b), names_to = "group",
                 values_to = "proportion") %>%
    mutate(group = factor(ifelse(group == "proportion_a", labels[1], labels[2]),
                          levels = labels)) %>%
    group_by(Reason, group) %>%
    summarise(
      mean_proportion = mean(proportion),
      q25 = stats::quantile(proportion, 0.25, names = FALSE),
      q75 = stats::quantile(proportion, 0.75, names = FALSE),
      n_years = n(),
      .groups = "drop"
    )

  # One line per reason, the first dataset just above it and the second just
  # below. Positions are set by hand because ggplotly() does not convert a
  # vertical position_dodge()
  reason_order <- summary %>%
    filter(group == labels[1]) %>%
    arrange(mean_proportion) %>%
    pull(Reason)
  summary <- summary %>%
    mutate(y = match(Reason, reason_order) +
             ifelse(group == labels[1], 0.15, -0.15))

  print(summary)

  ggplot(summary, aes(mean_proportion, y, colour = group)) +
    # Points come first because ggplotly() builds the legend from the first
    # layer, and the whiskers would give legend entries without markers.
    # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
    suppressWarnings(geom_point(aes(text = paste0(
      Reason, "<br>", group, ": ", scales::percent(mean_proportion, 0.1),
      " (IQR ", scales::percent(q25, 0.1), " - ", scales::percent(q75, 0.1),
      ", ", n_years, " years)"
    )), size = 2.5)) +
    geom_errorbar(aes(xmin = q25, xmax = q75), width = 0.15,
                  orientation = "y") +
    scale_x_continuous(labels = scales::label_percent()) +
    scale_y_continuous(breaks = seq_along(reason_order), labels = reason_order,
                       minor_breaks = NULL) +
    scale_colour_manual(values = stats::setNames(c("#1b9e77", "#d95f02"), labels)) +
    labs(
      x = "Mean yearly proportion of retractions citing the reason (IQR band)",
      y = NULL,
      colour = NULL,
      title = paste0("Retraction reasons: ", labels[1], " vs ", labels[2]),
      caption = "Points: mean across years. Whiskers: interquartile range across years"
    ) +
    theme_bw() +
    theme(plot.title.position = "plot")
}

#' Plot the average yearly proportion of each reason across groups
#'
#' Summarises the output of [calculate_proportion_of_retractions_per_year()]
#' (one row per country) or
#' [calculate_proportion_of_retractions_per_year_by_publisher()] (one row per
#' publisher): for each year and reason, a line shows the mean proportion
#' across the groups and a band its interquartile range (25th to 75th
#' percentile). Each group counts the same, whatever its number of
#' retractions, and groups without retractions in a year are left out of
#' that year.
#'
#' @param proportions Output of [calculate_proportion_of_retractions_per_year()]
#'   or [calculate_proportion_of_retractions_per_year_by_publisher()].
#' @param group_col Column with the groups: `"Publisher"` or `"Country"`. By
#'   default, whichever of the two is in `proportions`.
#' @param group_label Plural name of the groups used in the title and hover
#'   text. By default `"publishers"` or `"countries"`.
#' @param interactive If `TRUE`, returns a plotly figure in which each band
#'   shares the legend entry of its line, so clicking a reason toggles both.
#' @return A ggplot object, or a plotly object when `interactive = TRUE`. The
#'   ggplot data has one row per year and reason with columns
#'   `mean_proportion`, `q25`, `q75` and `n_groups`.
#' @export
plot_reason_proportions_across_groups_by_year <- function(proportions,
                                                  group_col = NULL,
                                                  group_label = NULL,
                                                  interactive = FALSE) {
  if (is.null(group_col)) {
    group_col <- intersect(c("Publisher", "Country"), names(proportions))[1]
    if (is.na(group_col)) {
      stop("`proportions` needs a `Publisher` or `Country` column, ",
           "or set `group_col`.")
    }
  }
  if (is.null(group_label)) {
    group_label <- switch(group_col, Publisher = "publishers",
                          Country = "countries", tolower(group_col))
  }

  summary <- proportions %>%
    group_by(year, Reason) %>%
    summarise(mean_proportion = mean(proportion),
              q25 = stats::quantile(proportion, 0.25, names = FALSE),
              q75 = stats::quantile(proportion, 0.75, names = FALSE),
              n_groups = n_distinct(.data[[group_col]]),
              .groups = "drop")

  p <- ggplot(summary, aes(year, mean_proportion, colour = Reason, fill = Reason)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, colour = NA) +
    geom_line() +
    # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
    suppressWarnings(geom_point(aes(text = paste0(
      toupper(substr(group_label, 1, 1)), substring(group_label, 2), ": ",
      n_groups
    )))) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
      title = paste("Average proportion of retractions per reason across",
                    group_label),
      x = "Year",
      y = "Mean proportion of retractions (IQR band)",
      colour = "Reason",
      fill = "Reason"
    ) +
    theme_minimal()

  if (!interactive) {
    return(p)
  }

  # ggplotly() adds a legend entry "(Reason,1)" for each band: hide it and
  # group each band with its line, so that clicking a reason toggles both
  widget <- plotly::plotly_build(plotly::ggplotly(p))
  widget$x$data <- lapply(widget$x$data, function(trace) {
    reason <- sub("^\\((.*),1\\)$", "\\1", trace$name)
    trace$legendgroup <- reason
    if (!identical(trace$name, reason)) {
      trace$name <- reason
      trace$showlegend <- FALSE
    }
    trace
  })
  widget
}

#' Bar plot of the number of retractions per year
#'
#' Counts the distinct retracted papers (`Record ID`) of each year, so a
#' paper with authors in several countries counts once.
#'
#' @param retraction_data Output of [load_retraction_data()], or a subset of
#'   it (e.g. without mass retractions).
#' @param year_col Year column to use, `"retraction_year"` or
#'   `"publication_year"`.
#' @param year_limits Optional length-2 vector with the first and last year
#'   (inclusive).
#' @param title Plot title.
#' @param colour Fill colour of the bars.
#' @return A ggplot object. Its data has columns `year` and `retractions`.
#' @export
plot_number_of_total_retractions_per_year <- function(retraction_data,
                                                      year_col = "retraction_year",
                                                      year_limits = NULL,
                                                      title = "Number of retractions per year",
                                                      colour = "#1b9e77") {
  counts <- retraction_data %>%
    distinct(`Record ID`, year = .data[[year_col]]) %>%
    filter(!is.na(year))
  if (!is.null(year_limits)) {
    counts <- counts %>%
      filter(year >= year_limits[1], year <= year_limits[2])
  }
  counts <- count(counts, year, name = "retractions")

  ggplot(counts, aes(year, retractions)) +
    geom_col(fill = colour) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(
      x = if (year_col == "publication_year") "Publication year" else "Retraction year",
      y = "Number of retractions",
      title = title
    ) +
    theme_minimal()
}

#' Scatter plot of the number of retractions per year for each publisher
#'
#' Counts the distinct retracted papers (`Record ID`) of each publisher and
#' year and draws one point per publisher and year, coloured by publisher.
#' Papers without a publisher are left out. With thousands of publishers the
#' legend is hidden by default; in [plotly::ggplotly()] the hover text shows
#' the publisher of each point.
#'
#' @inheritParams plot_number_of_total_retractions_per_year
#' @param top_n If not `NULL`, only the `top_n` publishers with the most
#'   retractions over the period (within `year_limits`) are shown.
#' @param show_legend If `TRUE`, shows the legend of publisher colours,
#'   ordered from the most to the least retractions.
#' @param yscale Scale of the y axis: `"linear"` or `"log"` (base 10), which
#'   keeps publishers with few retractions visible next to the largest ones.
#' @param title Plot title.
#' @return A ggplot object. Its data has columns `Publisher` (a factor
#'   ordered from the most to the least retractions), `year` and
#'   `retractions`.
#' @export
plot_number_of_retractions_per_year_per_publisher <- function(retraction_data,
                                                              year_col = "retraction_year",
                                                              year_limits = NULL,
                                                              top_n = NULL,
                                                              show_legend = FALSE,
                                                              yscale = c("linear", "log"),
                                                              title = "Number of retractions per year and publisher") {
  yscale <- match.arg(yscale)
  papers <- retraction_data %>%
    distinct(`Record ID`, Publisher, year = .data[[year_col]]) %>%
    filter(!is.na(year), !is.na(Publisher))
  if (!is.null(year_limits)) {
    papers <- papers %>%
      filter(year >= year_limits[1], year <= year_limits[2])
  }

  publisher_order <- papers %>%
    count(Publisher, sort = TRUE) %>%
    pull(Publisher)
  if (!is.null(top_n)) {
    publisher_order <- utils::head(publisher_order, top_n)
  }

  counts <- papers %>%
    filter(Publisher %in% publisher_order) %>%
    count(Publisher, year, name = "retractions") %>%
    mutate(Publisher = factor(Publisher, levels = publisher_order))

  y_scale <- if (yscale == "log") {
    scale_y_log10(labels = scales::label_comma())
  } else {
    scale_y_continuous(labels = scales::label_comma())
  }

  # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
  ggplot(counts, aes(year, retractions, colour = Publisher)) +
    suppressWarnings(geom_point(aes(text = Publisher), alpha = 0.7)) +
    y_scale +
    labs(
      x = if (year_col == "publication_year") "Publication year" else "Retraction year",
      y = if (yscale == "log") "Number of retractions (log scale)" else "Number of retractions",
      colour = "Publisher",
      title = title
    ) +
    theme_minimal() +
    theme(legend.position = if (show_legend) "right" else "none")
}

#' Bar plot of the share of retractions of the top publishers
#'
#' Counts the distinct retracted papers (`Record ID`) of each publisher and
#' shows, as one bar each, the proportion of all retractions of the `top_n`
#' publishers with the most retractions, plus one bar for all the other
#' publishers together. Papers without a publisher are left out. A bar chart
#' is used rather than a pie chart because [plotly::ggplotly()] cannot
#' convert pie charts.
#'
#' @inheritParams plot_number_of_retractions_per_year_per_publisher
#' @param top_n Number of publishers shown separately.
#' @param other_label Label of the bar grouping the remaining publishers.
#' @param colour Fill colour of the publisher bars (the other publishers are
#'   grey).
#' @return A ggplot object. Its data has one row per bar with columns
#'   `Publisher` (a factor in plotting order), `retractions` and
#'   `proportion`.
#' @export
plot_proportion_of_total_retractions_per_publisher <- function(retraction_data,
                                                               top_n = 10,
                                                               year_col = "retraction_year",
                                                               year_limits = NULL,
                                                               other_label = "Other publishers",
                                                               colour = "#1b9e77",
                                                               title = "Share of retractions per publisher") {
  papers <- retraction_data %>%
    distinct(`Record ID`, Publisher, year = .data[[year_col]]) %>%
    filter(!is.na(Publisher))
  if (!is.null(year_limits)) {
    papers <- papers %>%
      filter(!is.na(year), year >= year_limits[1], year <= year_limits[2])
  }

  counts <- papers %>%
    count(Publisher, name = "retractions", sort = TRUE)
  top <- utils::head(counts, top_n)
  others <- sum(counts$retractions) - sum(top$retractions)
  if (others > 0) {
    top <- bind_rows(top, tibble(Publisher = other_label, retractions = others))
  }
  shares <- top %>%
    mutate(proportion = retractions / sum(counts$retractions),
           # Largest publisher at the top, other publishers at the bottom
           Publisher = factor(Publisher, levels = rev(Publisher)),
           is_other = Publisher == other_label)

  # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
  ggplot(shares, aes(proportion, Publisher, fill = is_other)) +
    suppressWarnings(geom_col(aes(text = paste0(
      Publisher, "<br>", scales::comma(retractions), " retractions (",
      scales::percent(proportion, accuracy = 0.1), ")"
    )), orientation = "y")) +
    # Labels are centred just right of each bar: ggplotly() ignores hjust, so
    # the gap is part of their x position
    geom_text(aes(x = proportion + 0.05 * max(proportion),
                  label = scales::percent(proportion, accuracy = 0.1)),
              size = 3) +
    scale_x_continuous(labels = scales::label_percent(),
                       expand = expansion(mult = c(0, 0.12))) +
    scale_fill_manual(values = c(`FALSE` = colour, `TRUE` = "grey60"),
                      guide = "none") +
    labs(x = "Proportion of all retractions", y = NULL, title = title) +
    theme_minimal()
}

#' Heatmap of how often retraction reasons are cited in the same paper
#'
#' For every pair of reasons, counts the retracted papers (distinct
#' `Record ID`s) citing both. The diagonal has the number of papers citing
#' each reason. Reasons are split and cleaned as in
#' [compute_top_reasons_over_time()] and sorted from the most cited (top
#' left) to the least cited. Each reason is numbered by this rank: the y axis
#' shows the reason followed by its number and the x axis only the number.
#' The colour scale is logarithmic because pair
#' counts range from one paper to tens of thousands. Pairs never cited
#' together are left blank.
#'
#' With `proportion = TRUE`, each cell shows instead the Jaccard index of the
#' pair: the papers citing both reasons divided by the papers citing either.
#' The diagonal (always 100%) is then left grey, and reasons are sorted (and
#' `top_n` selected) by their highest Jaccard index with any other reason,
#' so the reasons most tied to another one come first. Ties are sorted by
#' the number of papers citing the reason.
#'
#' @param retraction_data Output of [load_retraction_data()].
#' @param year_limits Optional length-2 vector with the first and last year
#'   (inclusive) of the retractions used. All retractions if `NULL`.
#' @param reasons_list Optional character vector (or list) of reasons to
#'   include. All reasons if `NULL`.
#' @param top_n If not `NULL`, only the `top_n` first reasons (among
#'   `reasons_list`, if given) are shown: the most cited ones or, with
#'   `proportion = TRUE`, those with the highest Jaccard index with another
#'   reason.
#' @param proportion If `TRUE`, shows the Jaccard index of each pair instead
#'   of the number of papers.
#' @param year_col Year column used by `year_limits`, `"retraction_year"` or
#'   `"publication_year"`.
#' @param title Plot title.
#' @return A ggplot object. Its data has one row per pair of reasons cited
#'   together, with columns `reason_a`, `reason_b`, `papers` (papers citing
#'   both), `share` (proportion of the papers citing `reason_a` that also
#'   cite `reason_b`) and `jaccard` (papers citing both divided by papers
#'   citing either).
#' @export
plot_reasons_coocurrence_matrix <- function(retraction_data,
                                            year_limits = NULL,
                                            reasons_list = NULL,
                                            top_n = NULL,
                                            proportion = FALSE,
                                            year_col = "retraction_year",
                                            title = "Co-occurrence of retraction reasons in the same paper") {
  papers <- retraction_data %>%
    distinct(`Record ID`, year = .data[[year_col]], Reason)
  if (!is.null(year_limits)) {
    papers <- papers %>%
      filter(!is.na(year), year >= year_limits[1], year <= year_limits[2])
  }
  paper_reasons <- papers %>%
    separate_rows(Reason, sep = ";") %>%
    mutate(Reason = str_trim(Reason)) %>%
    filter(Reason != "") %>%
    distinct(`Record ID`, Reason)
  if (!is.null(reasons_list)) {
    paper_reasons <- paper_reasons %>%
      filter(Reason %in% as.character(unlist(reasons_list)))
  }

  # Every pair of reasons of the same paper, including each reason with itself
  pairs <- inner_join(paper_reasons, paper_reasons, by = "Record ID",
                      suffix = c("_a", "_b"), relationship = "many-to-many") %>%
    count(reason_a = Reason_a, reason_b = Reason_b, name = "papers")
  totals <- filter(pairs, reason_a == reason_b) %>%
    select(reason = reason_a, total = papers)
  pairs <- pairs %>%
    left_join(rename(totals, total_a = total), by = c("reason_a" = "reason")) %>%
    left_join(rename(totals, total_b = total), by = c("reason_b" = "reason")) %>%
    mutate(share = papers / total_a,
           jaccard = papers / (total_a + total_b - papers)) %>%
    select(-total_a, -total_b)

  # Most cited reasons first or, for proportions, those with the highest
  # Jaccard index with another reason
  ranking <- totals
  if (proportion) {
    best_pair <- pairs %>%
      filter(reason_a != reason_b) %>%
      group_by(reason = reason_a) %>%
      summarise(max_jaccard = max(jaccard), .groups = "drop")
    ranking <- ranking %>%
      left_join(best_pair, by = "reason") %>%
      mutate(max_jaccard = coalesce(max_jaccard, 0)) %>%
      arrange(desc(max_jaccard), desc(total), reason)
  } else {
    ranking <- arrange(ranking, desc(total), reason)
  }
  reason_order <- ranking$reason
  if (!is.null(top_n)) {
    reason_order <- utils::head(reason_order, top_n)
  }
  pairs <- pairs %>%
    filter(reason_a %in% reason_order, reason_b %in% reason_order) %>%
    mutate(reason_a = factor(reason_a, levels = rev(reason_order)),
           reason_b = factor(reason_b, levels = reason_order),
           # The diagonal is always 100% for proportions, so it is not coloured
           value = if (proportion) {
             ifelse(reason_a == reason_b, NA_real_, jaccard)
           } else {
             papers
           })

  # Each reason is numbered by its rank: "Reason (1)" on the y axis and only
  # the number on the x axis
  rank <- stats::setNames(seq_along(reason_order), reason_order)
  numbered <- function(reason) paste0(reason, " (", rank[as.character(reason)], ")")

  # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
  fill_scale <- if (proportion) {
    scale_fill_viridis_c(labels = scales::label_percent(), option = "magma",
                         direction = -1, na.value = "grey85")
  } else {
    scale_fill_viridis_c(trans = "log10", labels = scales::label_comma(),
                         option = "magma", direction = -1)
  }

  ggplot(pairs, aes(reason_b, reason_a, fill = value)) +
    suppressWarnings(geom_tile(aes(text = ifelse(
      reason_a == reason_b,
      paste0(numbered(reason_a), "<br>", scales::comma(papers), " papers"),
      paste0(numbered(reason_a), " + ", numbered(reason_b), "<br>",
             scales::comma(papers), " papers (",
             scales::percent(share, accuracy = 0.1), " of the papers citing ",
             reason_a, ")<br>Jaccard: ", scales::percent(jaccard, accuracy = 0.1))
    )))) +
    scale_x_discrete(labels = function(reason) as.character(rank[reason])) +
    scale_y_discrete(labels = numbered) +
    fill_scale +
    labs(x = "Reason number", y = NULL,
         fill = if (proportion) "Jaccard index" else "Papers", title = title) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}

#' Bar plot of the number of distinct retraction reasons per year
#'
#' Counts how many different reasons are cited by the retractions of each
#' year. Reasons are split and cleaned as in [compute_top_reasons_over_time()],
#' and a reason counts once per year however many retractions cite it.
#'
#' @inheritParams plot_number_of_total_retractions_per_year
#' @inheritParams compute_top_reasons_over_time
#' @return A ggplot object. Its data has columns `year` and `reasons`.
#' @export
plot_number_of_distinct_reasons_per_year <- function(retraction_data,
                                                     year_col = "retraction_year",
                                                     year_limits = NULL,
                                                     exclude_reasons = NULL,
                                                     title = "Number of distinct retraction reasons per year",
                                                     colour = "#1b9e77") {
  counts <- yearly_reason_counts(retraction_data, year_col,
                                 min_retractions_per_year = 0, year_limits,
                                 exclude_reasons)$reason_counts %>%
    count(year, name = "reasons")

  ggplot(counts, aes(year, reasons)) +
    geom_col(fill = colour) +
    labs(
      x = if (year_col == "publication_year") "Publication year" else "Retraction year",
      y = "Number of distinct reasons",
      title = title
    ) +
    theme_minimal()
}

#' Timeline of the first year each retraction reason was cited
#'
#' For every reason, finds the first year in which a retraction cited it and
#' plots one row per reason, ordered from the oldest (top) to the newest
#' (bottom): a point at the first year and a line from there to the last
#' year the reason was cited (hollow point) or, with `show_last = FALSE`, to
#' the end of the period. The first year is always computed on all of `retraction_data`;
#' `year_limits` only selects which reasons are shown (those first cited
#' within the limits), so a reason never looks newer than it is. The title
#' says how many reasons were first cited before the period. Reasons are
#' split and cleaned as in [compute_top_reasons_over_time()].
#'
#' With about a hundred reasons the plot needs to be tall, e.g.
#' `ggplotly(p, height = 1400)` or a large `fig-height` chunk option.
#'
#' @inheritParams plot_number_of_total_retractions_per_year
#' @inheritParams compute_top_reasons_over_time
#' @param show_last If `TRUE`, each line ends at the last year the reason was
#'   cited (within `year_limits`), marked with a hollow point. If `FALSE`,
#'   every line runs to the end of the period.
#' @param reason_list Optional character vector (or list) of reasons to plot,
#'   e.g. the output of [find_reasons_not_reported_in_year()]. All reasons
#'   if `NULL`.
#' @return A ggplot object. Its data has one row per reason shown with
#'   columns `Reason`, `first_year` and `n` (first year and number of
#'   retractions citing the reason that year), `last_year` and `last_n` (the
#'   same for the last year it was cited) and `line_end` (end of the line).
#' @export
plot_first_year_occurrence_per_reason <- function(retraction_data,
                                                  year_col = "retraction_year",
                                                  year_limits = NULL,
                                                  exclude_reasons = NULL,
                                                  colour = "#1b9e77",
                                                  show_last = TRUE,
                                                  reason_list = NULL) {
  reason_counts <- yearly_reason_counts(retraction_data, year_col,
                                        min_retractions_per_year = 0,
                                        year_limits = NULL,
                                        exclude_reasons)$reason_counts
  period_end <- max(reason_counts$year)
  if (!is.null(year_limits)) {
    period_end <- min(period_end, year_limits[2])
  }
  first_years <- reason_counts %>%
    group_by(Reason) %>%
    slice_min(year, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(Reason, first_year = year, n)
  # Last year cited up to the end of the period
  last_years <- reason_counts %>%
    filter(year <= period_end) %>%
    group_by(Reason) %>%
    slice_max(year, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(Reason, last_year = year, last_n = n)
  first_years <- first_years %>%
    inner_join(last_years, by = "Reason") %>%
    mutate(line_end = if (show_last) last_year else period_end)
  if (!is.null(reason_list)) {
    first_years <- filter(first_years,
                          Reason %in% as.character(unlist(reason_list)))
  }

  title <- "First year each retraction reason was cited"
  if (!is.null(year_limits)) {
    n_before <- sum(first_years$first_year < year_limits[1])
    first_years <- first_years %>%
      filter(first_year >= year_limits[1], first_year <= year_limits[2])
    if (n_before > 0) {
      title <- paste0(title, "\n(", n_before,
                      if (n_before == 1) " reason" else " reasons",
                      " first cited before ", year_limits[1], " not shown)")
    }
  }
  # Oldest reasons at the top
  first_years <- first_years %>%
    arrange(first_year, Reason) %>%
    mutate(Reason = factor(Reason, levels = rev(Reason)))

  hover <- function(label, year, n) {
    paste0(label, year, " (", n, ifelse(n == 1, " retraction)", " retractions)"))
  }
  p <- ggplot(first_years, aes(y = Reason)) +
    geom_segment(aes(x = first_year, xend = line_end, yend = Reason),
                 colour = colour, alpha = if (show_last) 0.5 else 0.25)
  # `text` is ignored by ggplot2 but shown in the ggplotly() hover text.
  # Last years are drawn first so that, for a reason cited in a single year,
  # the filled first-year point stays visible on top
  if (show_last) {
    p <- p +
      suppressWarnings(geom_point(aes(
        x = last_year,
        text = paste0(Reason, "<br>", hover("Last cited: ", last_year, last_n))
      ), colour = colour, size = 2, shape = 21, fill = "white"))
  }
  p +
    suppressWarnings(geom_point(aes(
      x = first_year,
      text = paste0(Reason, "<br>", hover("First cited: ", first_year, n))
    ), colour = colour, size = 2)) +
    labs(
      x = if (year_col == "publication_year") "Publication year" else "Retraction year",
      y = NULL,
      title = title
    ) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 7))
}

# Yearly proportion of `data_a` retractions citing each reason minus that of
# `data_b`, as in compute_top_reasons_over_time(). Returns columns year,
# Reason, proportion_a, proportion_b and difference, for the years present
# in both datasets.
yearly_reason_proportion_differences <- function(data_a, data_b, reasons_list,
                                                 year_col, year_limits,
                                                 min_retractions_per_year) {
  proportions <- lapply(list(data_a, data_b), function(data) {
    compute_top_reasons_over_time(
      data,
      n_top = NULL,
      year_col = year_col,
      min_retractions_per_year = min_retractions_per_year,
      year_limits = year_limits,
      reasons_list = reasons_list
    ) %>%
      select(year, Reason, proportion)
  })

  # Only years present in both datasets can be compared
  inner_join(proportions[[1]], proportions[[2]],
             by = c("year", "Reason"),
             suffix = c("_a", "_b")) %>%
    mutate(difference = proportion_a - proportion_b)
}

#' ROC curve and area under the curve of a binary classifier
#'
#' Predicts with `model` the probability of `targets[1]` for the rows of
#' `data` whose `column` is one of `targets`, and plots the ROC curve: the
#' true positive rate (share of `targets[1]` rows above the threshold)
#' against the false positive rate (share of `targets[2]` rows above it) for
#' every threshold. The title shows the area under the curve (AUC): 0.5 for
#' a model no better than chance (dashed diagonal) and 1 for a perfect one.
#'
#' @param model A model whose `predict(model, data, type = "response")`
#'   gives the probability of `targets[1]`, e.g. the output of
#'   [train_logistic_regressor()].
#' @param data Data to evaluate, ideally not used for training (e.g. the
#'   `test` element of [divide_by()]).
#' @param targets The positive (`targets[1]`) and negative (`targets[2]`)
#'   classes. By default, those stored in `model` by
#'   [train_logistic_regressor()].
#' @param column Column with the class of each row.
#' @param colour Colour of the curve.
#' @return A ggplot object. Its data has one row per threshold with columns
#'   `threshold`, `fpr` and `tpr`, and its `auc` element has the AUC.
#' @export
plot_auc <- function(model, data, targets = model$targets,
                     column = "country_class", colour = "#1b9e77") {
  data <- data[data[[column]] %in% targets, ]
  probability <- stats::predict(model, newdata = data, type = "response")
  roc <- roc_curve(probability, data[[column]] == targets[1])
  auc <- sum(diff(roc$fpr) * (utils::head(roc$tpr, -1) + utils::tail(roc$tpr, -1)) / 2)

  p <- ggplot(roc, aes(fpr, tpr)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
    geom_path(colour = colour) +
    # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
    suppressWarnings(geom_point(aes(text = paste0(
      "Threshold: ", signif(threshold, 3),
      "<br>True positive rate: ", scales::percent(tpr, accuracy = 0.1),
      "<br>False positive rate: ", scales::percent(fpr, accuracy = 0.1)
    )), colour = colour, size = 0.5)) +
    scale_x_continuous(labels = scales::label_percent()) +
    scale_y_continuous(labels = scales::label_percent()) +
    coord_equal() +
    labs(
      x = paste0("False positive rate (", targets[2], " predicted as ", targets[1], ")"),
      y = paste0("True positive rate (", targets[1], " predicted as ", targets[1], ")"),
      title = paste0("ROC curve: ", targets[1], " vs ", targets[2],
                     " (AUC = ", format(round(auc, 3), nsmall = 3), ")")
    ) +
    theme_minimal()
  p$auc <- auc
  p
}

# False and true positive rates for every threshold of `score`, from the
# highest threshold (no positive predicted) to the lowest (all predicted).
# Tied scores share one threshold, so ties give a diagonal step.
roc_curve <- function(score, is_positive) {
  ordered <- tibble(score = score, is_positive = is_positive) %>%
    group_by(threshold = score) %>%
    summarise(tp = sum(is_positive), fp = sum(!is_positive), .groups = "drop") %>%
    arrange(desc(threshold))
  bind_rows(
    tibble(threshold = Inf, tpr = 0, fpr = 0),
    tibble(threshold = ordered$threshold,
           tpr = cumsum(ordered$tp) / sum(is_positive),
           fpr = cumsum(ordered$fp) / sum(!is_positive))
  )
}

#' Proportion of each publisher's retractions citing a reason
#'
#' For every publisher in `publisher_list`, plots one row with the proportion
#' of its retracted papers (distinct `Record ID`s) that cite `reason`, and
#' optionally its exact (Clopper-Pearson) binomial confidence interval.
#' Rows are sorted from the highest to the lowest proportion. Reasons are
#' split and cleaned as in [compute_top_reasons_over_time()] and matched
#' exactly.
#'
#' @param retraction_data Output of [load_retraction_data()], or a subset of
#'   it (e.g. without mass retractions).
#' @param reason A single retraction reason.
#' @param publisher_list Character vector of publishers, or a data frame with
#'   a `Publisher` column (e.g. the output of
#'   [count_number_of_retractions_per_column()] for `"Publisher"`).
#' @param year_limits Optional length-2 vector with the first and last year
#'   (inclusive) of the retractions used. All retractions if `NULL`.
#' @param year_col Year column used by `year_limits`, `"retraction_year"` or
#'   `"publication_year"`.
#' @param confidence_interval If `TRUE`, draws the binomial confidence
#'   interval of each proportion.
#' @param conf_level Confidence level of the intervals.
#' @param colour Colour of the points and intervals.
#' @return A ggplot object. Its data has one row per publisher with columns
#'   `Publisher`, `papers` (retractions citing the reason), `total`
#'   (retractions of the publisher), `proportion`, `lower` and `upper`.
#' @export
plot_reason_proportion_per_publisher <- function(retraction_data,
                                                 reason,
                                                 publisher_list,
                                                 year_limits = NULL,
                                                 year_col = "retraction_year",
                                                 confidence_interval = TRUE,
                                                 conf_level = 0.95,
                                                 colour = "#1b9e77") {
  if (length(reason) != 1) {
    stop("`reason` must be a single reason, got ", length(reason), ".")
  }
  if (is.data.frame(publisher_list)) {
    publisher_list <- publisher_list$Publisher
  }

  papers <- retraction_data %>%
    filter(Publisher %in% publisher_list) %>%
    distinct(`Record ID`, Publisher, year = .data[[year_col]], Reason)
  if (!is.null(year_limits)) {
    papers <- papers %>%
      filter(!is.na(year), year >= year_limits[1], year <= year_limits[2])
  }
  cites_reason <- papers %>%
    separate_rows(Reason, sep = ";") %>%
    filter(str_trim(Reason) == reason) %>%
    distinct(`Record ID`) %>%
    pull(`Record ID`)

  proportions <- papers %>%
    distinct(`Record ID`, Publisher) %>%
    group_by(Publisher) %>%
    summarise(papers = sum(`Record ID` %in% cites_reason),
              total = n(), .groups = "drop") %>%
    mutate(proportion = papers / total,
           lower = binomial_ci(papers, total, conf_level)$lower,
           upper = binomial_ci(papers, total, conf_level)$upper) %>%
    arrange(proportion, desc(Publisher)) %>%
    mutate(Publisher = factor(Publisher, levels = Publisher))

  p <- ggplot(proportions, aes(proportion, Publisher))
  if (confidence_interval) {
    # Segments rather than error bars, as ggplotly() keeps their height
    p <- p + geom_segment(aes(x = lower, xend = upper, yend = Publisher),
                          colour = colour, alpha = 0.5, linewidth = 0.6)
  }
  p +
    # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
    suppressWarnings(geom_point(aes(text = paste0(
      Publisher, "<br>", scales::comma(papers), " of ", scales::comma(total),
      " retractions (", scales::percent(proportion, accuracy = 0.1), ")",
      if (confidence_interval) {
        paste0("<br>", round(100 * conf_level), "% CI: ",
               scales::percent(lower, accuracy = 0.1), " - ",
               scales::percent(upper, accuracy = 0.1))
      } else ""
    )), colour = colour, size = 2.5)) +
    scale_x_continuous(labels = scales::label_percent()) +
    labs(
      x = paste0("Proportion of the publisher's retractions",
                 if (confidence_interval) paste0(" (", round(100 * conf_level), "% CI)") else ""),
      y = NULL,
      title = reason
    ) +
    theme_minimal()
}

#' Heterogeneity of reason proportions among groups
#'
#' For every reason in `reason_list`, measures how much the proportion of
#' retractions citing it varies among the groups in `group_list` (e.g.
#' publishers or countries) beyond what sampling noise alone would give.
#' Each group's proportion is taken on the log-odds scale, as in a
#' random-effects meta-analysis of proportions, and summarised by:
#'
#' * `I2`: the share of the variation among groups that is real rather than
#'   sampling noise (0 when all groups could share one true proportion,
#'   close to 1 when they clearly differ).
#' * `tau`: the standard deviation of the groups' true log-odds
#'   (Paule-Mandel estimate, which always lies within its Q-profile
#'   confidence interval).
#' * `p_value`: Cochran's Q test of all groups having the same proportion.
#'
#' The plot shows `tau` by default. I2 depends on the group sizes: with
#' hundreds of retractions per group almost any difference exceeds sampling
#' noise, so I2 is close to 100% for every reason and does not tell them
#' apart, while `tau` measures how large the differences are. Both are always
#' in the plot data and in the hover text.
#'
#' Confidence intervals of `tau` and `I2` use the Q-profile method. Groups
#' are counted per distinct `Record ID`, so with `group_col = "Country"` a
#' paper with several countries counts once in each. Groups with no
#' retractions in the period are left out, and a 0.5 continuity correction
#' is added to proportions of exactly 0 or 1.
#'
#' @param retraction_data Output of [load_retraction_data()], or a subset of
#'   it (e.g. without mass retractions).
#' @param group_list Character vector of groups, or a data frame with a
#'   `group_col` column (e.g. the output of
#'   [count_number_of_retractions_per_column()]).
#' @param reason_list Character vector (or list) of reasons.
#' @param group_col Column defining the groups, e.g. `"Publisher"` or
#'   `"Country"`.
#' @param measure Value plotted: `"tau"` or `"I2"`.
#' @param year_limits Optional length-2 vector with the first and last year
#'   (inclusive) of the retractions used. All retractions if `NULL`.
#' @param year_col Year column used by `year_limits`.
#' @param conf_level Confidence level of the intervals.
#' @param colour Colour of the points and intervals.
#' @return A ggplot object. Its data has one row per reason with columns
#'   `Reason`, `groups`, `Q`, `p_value`, `tau`, `tau_lower`, `tau_upper`,
#'   `I2`, `I2_lower`, `I2_upper`, `min_proportion` and `max_proportion`.
#' @export
plot_reason_heterogeneity <- function(retraction_data,
                                      group_list,
                                      reason_list,
                                      group_col = "Publisher",
                                      measure = c("tau", "I2"),
                                      year_limits = NULL,
                                      year_col = "retraction_year",
                                      conf_level = 0.95,
                                      colour = "#1b9e77") {
  measure <- match.arg(measure)
  if (is.data.frame(group_list)) {
    group_list <- group_list[[group_col]]
  }
  reason_list <- as.character(unlist(reason_list))

  papers <- retraction_data %>%
    filter(.data[[group_col]] %in% group_list) %>%
    distinct(`Record ID`, group = .data[[group_col]], year = .data[[year_col]], Reason)
  if (!is.null(year_limits)) {
    papers <- papers %>%
      filter(!is.na(year), year >= year_limits[1], year <= year_limits[2])
  }
  totals <- count(distinct(papers, `Record ID`, group), group, name = "total")
  citing <- papers %>%
    separate_rows(Reason, sep = ";") %>%
    mutate(Reason = str_trim(Reason)) %>%
    filter(Reason %in% reason_list) %>%
    distinct(`Record ID`, group, Reason) %>%
    count(group, Reason, name = "papers")
  counts <- tidyr::crossing(totals, Reason = reason_list) %>%
    left_join(citing, by = c("group", "Reason")) %>%
    mutate(papers = coalesce(papers, 0L))

  heterogeneity <- counts %>%
    group_by(Reason) %>%
    group_modify(function(d, key) {
      as_tibble(c(proportion_heterogeneity(d$papers, d$total, conf_level),
                  list(min_proportion = min(d$papers / d$total),
                       max_proportion = max(d$papers / d$total))))
    }) %>%
    ungroup() %>%
    arrange(.data[[measure]]) %>%
    mutate(Reason = factor(Reason, levels = Reason))

  # "Publisher" -> "publishers", "Country" -> "countries"
  group_name <- sub("y$", "ie", tolower(group_col))
  n_groups <- length(unique(counts$group))
  value <- measure
  lower <- paste0(measure, "_lower")
  upper <- paste0(measure, "_upper")
  ggplot(heterogeneity, aes(.data[[value]], Reason)) +
    # Segments rather than error bars, as ggplotly() keeps their height
    geom_segment(aes(x = .data[[lower]], xend = .data[[upper]], yend = Reason),
                 colour = colour, alpha = 0.5, linewidth = 0.6) +
    # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
    suppressWarnings(geom_point(aes(text = paste0(
      Reason,
      "<br>I2: ", scales::percent(I2, accuracy = 0.1), " (", round(100 * conf_level), "% CI ",
      scales::percent(I2_lower, accuracy = 0.1), " - ", scales::percent(I2_upper, accuracy = 0.1), ")",
      "<br>tau (log-odds): ", formatC(tau, format = "f", digits = 2), " (",
      formatC(tau_lower, format = "f", digits = 2), " - ",
      formatC(tau_upper, format = "f", digits = 2), ")",
      "<br>Q test p-value: ", signif(p_value, 3),
      "<br>Proportions from ", scales::percent(min_proportion, accuracy = 0.1),
      " to ", scales::percent(max_proportion, accuracy = 0.1), " in ", groups, " groups"
    )), colour = colour, size = 2.5)) +
    scale_x_continuous(labels = if (measure == "I2") scales::label_percent() else waiver(),
                       limits = if (measure == "I2") c(0, 1) else c(0, NA)) +
    labs(
      x = if (measure == "I2") {
        paste0("I\u00b2: variation among ", group_name, "s beyond sampling noise (",
               round(100 * conf_level), "% CI)")
      } else {
        paste0("\u03c4: SD of the true log-odds among ", group_name, "s (",
               round(100 * conf_level), "% CI)")
      },
      y = NULL,
      title = paste0("Heterogeneity of reason proportions among ", n_groups, " ",
                     if (n_groups == 1) tolower(group_col) else paste0(group_name, "s"))
    ) +
    theme_minimal()
}

# Random-effects heterogeneity of k proportions papers / total on the
# log-odds scale: Cochran's Q and its p-value, Paule-Mandel tau with a
# Q-profile confidence interval, and I2 (tau^2 over tau^2 plus the typical
# within-group variance) with the interval derived from tau's. Paule-Mandel
# and the Q-profile solve the same equation, so the estimate is always within
# its interval (DerSimonian-Laird can fall outside it).
proportion_heterogeneity <- function(papers, total, conf_level = 0.95) {
  keep <- total > 0
  papers <- papers[keep]
  total <- total[keep]
  k <- length(papers)
  if (k < 2) {
    return(list(groups = k, Q = NA_real_, p_value = NA_real_, tau = NA_real_,
                tau_lower = NA_real_, tau_upper = NA_real_, I2 = NA_real_,
                I2_lower = NA_real_, I2_upper = NA_real_))
  }
  # 0.5 continuity correction for proportions of exactly 0 or 1
  correction <- ifelse(papers == 0 | papers == total, 0.5, 0)
  a <- papers + correction
  b <- total - papers + correction
  y <- log(a / b)
  v <- 1 / a + 1 / b

  generalised_q <- function(tau2) {
    w <- 1 / (v + tau2)
    sum(w * (y - sum(w * y) / sum(w))^2)
  }
  w <- 1 / v
  q <- generalised_q(0)
  df <- k - 1

  # Value of tau^2 where the generalised Q statistic equals `target` (Q
  # decreases as tau^2 grows): `df` gives the Paule-Mandel estimate and the
  # chi-squared quantiles give the Q-profile confidence interval
  alpha <- 1 - conf_level
  solve_tau2 <- function(target) {
    if (generalised_q(0) <= target) {
      return(0)
    }
    upper <- max(1, 10 * stats::var(y))
    while (generalised_q(upper) > target) {
      upper <- upper * 10
    }
    stats::uniroot(function(t) generalised_q(t) - target, c(0, upper),
                   tol = 1e-10)$root
  }
  tau2 <- solve_tau2(df)
  tau2_lower <- solve_tau2(stats::qchisq(1 - alpha / 2, df))
  tau2_upper <- solve_tau2(stats::qchisq(alpha / 2, df))

  typical_variance <- df * sum(w) / (sum(w)^2 - sum(w^2))
  i2 <- function(t2) t2 / (t2 + typical_variance)
  list(groups = k, Q = q, p_value = stats::pchisq(q, df, lower.tail = FALSE),
       tau = sqrt(tau2), tau_lower = sqrt(tau2_lower), tau_upper = sqrt(tau2_upper),
       I2 = i2(tau2), I2_lower = i2(tau2_lower), I2_upper = i2(tau2_upper))
}
