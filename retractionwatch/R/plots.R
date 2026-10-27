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
  several <- length(scenario_name) > 1
  plot_dat <- dat %>%
    filter(scenario %in% scenario_name) %>%
    arrange(.data[[x]]) %>%
    mutate(is_uk = Country == "United Kingdom",
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

  ggplot(plot_dat, aes(x = .data[[x]], y = Country)) +
    geom_point(colour = "grey40", size = 1) +
    geom_point(data = uk_dat, colour = "red", size = 1) +
    geom_text(data = non_uk_dat, aes(label = label), colour = "grey40",
              hjust = 0, vjust = 0.5, size = label_size) +
    geom_text(data = uk_dat, aes(label = label), colour = "red",
              hjust = 0, vjust = 0.5, fontface = "bold", size = label_size) +
    scale_x_log10(expand = expansion(mult = c(0.02, 0.2))) +
    labs(title = if (!several) scenario_name, x = xlab, y = NULL) +
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
#' @param ncol If not `NULL`, combine all the scenarios into one figure with
#'   this number of columns: a ggplot faceted by scenario or, when
#'   `interactive = TRUE`, a [plotly::subplot()] where each row is as tall as
#'   its tallest scenario.
#' @return A named list of ggplot objects, one per scenario, or a tagList of
#'   plotly widgets when `interactive = TRUE`. With `ncol`, a single ggplot or
#'   plotly figure.
#' @export
plot_all_scenarios <- function(dat, ..., label_size = 2.5, interactive = FALSE,
                               ncol = NULL) {
  scenarios <- unique(dat$scenario)
  if (!interactive && !is.null(ncol)) {
    return(plot_scenario(dat, scenarios, ..., label_size = label_size) +
             facet_wrap(~scenario, ncol = ncol, scales = "free_y"))
  }
  plots <- stats::setNames(
    lapply(scenarios, function(s) plot_scenario(dat, s, ..., label_size = label_size)),
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
#' @param y Column to plot on the (log) y axis.
#' @param ylab Y axis label.
#' @param year_limits Range of publication years shown.
#' @return A ggplot object.
#' @export
plot_ai_ts <- function(data, y = "ai_retractions",
                       ylab = "AI Retractions (counts)",
                       year_limits = c(2000, 2025)) {
  ggplot(data, aes(publication_year, .data[[y]], colour = country_class)) +
    geom_line() +
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

#' Plot the average difference in the yearly proportions of retraction reasons
#'
#' Summarises [plot_reason_proportion_differences()] with one bar per reason
#' in `reasons_list`: the bar is the mean, across years, of the proportion of
#' `data_a` retractions citing the reason minus the proportion of `data_b`
#' retractions citing it. Every year counts the same, whatever its number of
#' retractions. Error bars show the year-to-year variation of the
#' difference. Positive values mean the reason is cited more often in
#' `data_a`. Only years present in both datasets are used.
#'
#' @inheritParams plot_reason_proportion_differences
#' @param error Error bars to draw: `"se"` (standard error of the mean across
#'   years) or `"sd"` (standard deviation across years).
#' @return A ggplot object. Its data has one row per reason with columns
#'   `Reason`, `mean_difference`, `error` and `n_years`.
#' @export
plot_mean_reason_proportion_differences <- function(data_a,
                                                    data_b,
                                                    reasons_list,
                                                    labels = c("Group A", "Group B"),
                                                    year_col = "retraction_year",
                                                    year_limits = NULL,
                                                    min_retractions_per_year = 0,
                                                    error = c("se", "sd")) {
  error <- match.arg(error)
  mean_differences <- yearly_reason_proportion_differences(
    data_a, data_b, reasons_list, year_col, year_limits,
    min_retractions_per_year
  ) %>%
    group_by(Reason) %>%
    summarise(
      mean_difference = mean(difference),
      sd = stats::sd(difference),
      n_years = n(),
      .groups = "drop"
    ) %>%
    mutate(error = if (error == "se") sd / sqrt(n_years) else sd) %>%
    select(-sd)

  # Horizontal bars are drawn with orientation = "y" rather than coord_flip(),
  # which ggplotly() converts into whisker caps far taller than the bars
  ggplot(mean_differences,
         aes(mean_difference, stats::reorder(Reason, mean_difference),
             fill = mean_difference > 0)) +
    geom_vline(xintercept = 0, colour = "grey50") +
    geom_col(width = 0.7, orientation = "y") +
    geom_errorbar(aes(xmin = mean_difference - error,
                      xmax = mean_difference + error),
                  width = 0.25, orientation = "y") +
    scale_x_continuous(labels = scales::label_percent()) +
    scale_fill_manual(values = c(`TRUE` = "#1b9e77", `FALSE` = "#d95f02"),
                      guide = "none") +
    labs(
      x = paste0("Mean yearly proportion difference (", labels[1], " - ",
                 labels[2], ")"),
      y = NULL,
      title = paste0("Mean difference in retraction reasons: ",
                     labels[1], " vs ", labels[2]),
      caption = paste0("Error bars: ",
                       if (error == "se") "standard error" else "standard deviation",
                       " across years")
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
