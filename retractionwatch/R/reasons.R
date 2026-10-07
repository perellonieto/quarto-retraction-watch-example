#' One row per retraction with a logical column per retraction reason
#'
#' Mass retractions are excluded. Each reason becomes an `is_<reason>` column
#' (names cleaned with [janitor::clean_names()]) and a `period` column splits
#' papers published before and after `split_year`.
#'
#' @param data Cleaned retraction data.
#' @param group_col Name of the column defining the groups to compare
#'   (e.g. `"country_class"` or `"international"`).
#' @param split_year Publication year separating the two periods.
#' @return A wide tibble of reason flags.
#' @export
reason_flags <- function(data, group_col = "country_class", split_year = 2017) {
  data %>%
    filter(Mass_Retraction == "Other") %>%
    select(RetractionID, publication_year, all_of(group_col), Reason) %>%
    separate_rows(Reason, sep = ";") %>%
    mutate(Reason = str_trim(Reason)) %>%
    stats::na.omit() %>%
    mutate(value = TRUE) %>%
    distinct(RetractionID, publication_year, Reason, .keep_all = TRUE) %>%
    pivot_wider(
      id_cols = c(RetractionID, publication_year, all_of(group_col)),
      names_from = Reason,
      values_from = value,
      values_fill = FALSE,
      names_prefix = "is_"
    ) %>%
    clean_names() %>%
    mutate(
      period = if_else(
        publication_year < split_year,
        paste0("Pre-", split_year),
        paste0("Post-", split_year)
      )
    )
}

#' Compare the proportion of each retraction reason between two groups
#'
#' For each period and reason computes the proportion and count of
#' retractions in every group, the difference in proportions between
#' `group_a` and `group_b`, a two-sample proportion test p-value and its
#' Benjamini-Hochberg adjustment.
#'
#' Output columns are named `proportion_<group>`, `count_<group>` and
#' `<group>` (number of retractions in the group).
#'
#' @param flags Output of [reason_flags()].
#' @param group_col Column defining the groups.
#' @param group_a,group_b Values of `group_col` to compare.
#' @return A tibble with one row per period and reason, sorted by the
#'   absolute difference in proportions.
#' @export
compare_reason_groups <- function(flags, group_col = "country_class",
                                  group_a = "United Kingdom",
                                  group_b = "Other OECD") {
  reasons_compare_wide <- flags %>%
    group_by(period, .data[[group_col]]) %>%
    summarise(
      across(
        starts_with("is_"),
        list(
          proportion = ~mean(.x, na.rm = TRUE),
          count = ~sum(.x, na.rm = TRUE)
        )
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      -c(period, all_of(group_col)),
      names_to = c("flag", ".value"),
      names_pattern = "(is_.*)_(proportion|count)"
    ) %>%
    select(period, all_of(group_col), flag, proportion, count) %>%
    pivot_wider(
      names_from = all_of(group_col),
      values_from = c(proportion, count)
    )

  group_sizes <- flags %>%
    count(period, .data[[group_col]], name = "n") %>%
    pivot_wider(names_from = all_of(group_col), values_from = n)

  results <- reasons_compare_wide %>%
    left_join(group_sizes, by = "period")

  count_a <- results[[paste0("count_", group_a)]]
  count_b <- results[[paste0("count_", group_b)]]
  n_a <- results[[as.character(group_a)]]
  n_b <- results[[as.character(group_b)]]

  results %>%
    mutate(
      diff = .data[[paste0("proportion_", group_a)]] -
        .data[[paste0("proportion_", group_b)]],
      p_value = mapply(
        function(xa, xb, na, nb) stats::prop.test(x = c(xa, xb), n = c(na, nb))$p.value,
        count_a, count_b, n_a, n_b
      ),
      p_adj = stats::p.adjust(p_value, method = "BH")
    ) %>%
    arrange(desc(abs(diff)))
}

#' Ratio between the reason counts of two groups in each period
#'
#' Fits `count_<group_a> ~ count_<group_b> - 1` per period and returns the
#' inverse of the slope, i.e. how many `group_b` retractions there are per
#' `group_a` retraction.
#'
#' @param results Output of [compare_reason_groups()].
#' @inheritParams compare_reason_groups
#' @return A named numeric vector with one ratio per period.
#' @export
group_count_ratios <- function(results, group_a = "United Kingdom",
                               group_b = "Other OECD") {
  formula <- stats::as.formula(
    sprintf("`count_%s` ~ `count_%s` - 1", group_a, group_b)
  )
  periods <- unique(results$period)
  ratios <- vapply(periods, function(p) {
    fit <- stats::lm(formula, data = filter(results, period == p))
    as.numeric(1 / stats::coef(fit))
  }, numeric(1))
  stats::setNames(ratios, periods)
}

#' Flag reasons with low counts and recompute adjusted p-values without them
#'
#' A reason is low count when `ratio * count_<group_a> + count_<group_b>` is
#' below `threshold`, with `ratio` from [group_count_ratios()]. Adds the
#' columns `significant`, `low_count`, `p_adj_filtered` (BH adjustment over
#' non low-count reasons only) and `significant_filtered`.
#'
#' @param results Output of [compare_reason_groups()].
#' @param ratios Output of [group_count_ratios()].
#' @inheritParams compare_reason_groups
#' @param threshold Minimum adjusted total count.
#' @param alpha Significance level.
#' @return `results` with the added columns.
#' @export
flag_low_counts <- function(results, ratios, group_a = "United Kingdom",
                            group_b = "Other OECD", threshold = 500,
                            alpha = 0.05) {
  results %>%
    mutate(
      significant = p_adj < alpha,
      low_count = unname(ratios[period]) * .data[[paste0("count_", group_a)]] +
        .data[[paste0("count_", group_b)]] < threshold,
      p_adj_filtered = NA_real_,
      p_adj_filtered = replace(
        p_adj_filtered,
        !low_count,
        stats::p.adjust(p_value[!low_count], method = "BH")
      ),
      significant_filtered = p_adj_filtered < alpha
    )
}

#' Significant reasons in long format, ready for [plot_significant_reasons()]
#'
#' @param results Output of [compare_reason_groups()] (optionally passed
#'   through [flag_low_counts()]).
#' @inheritParams compare_reason_groups
#' @param labels Names for the two groups in the plot legend.
#' @param p_col Column with the adjusted p-values to use.
#' @param alpha Significance level.
#' @param exclude_low_count Drop reasons flagged by [flag_low_counts()].
#' @param min_total_count If not `NULL`, keep only reasons whose combined
#'   count across both groups is greater than this value.
#' @return A long tibble with `period`, `flag`, `group` and `proportion`.
#' @export
significant_reasons_long <- function(results, group_a = "United Kingdom",
                                     group_b = "Other OECD",
                                     labels = c(group_a, group_b),
                                     p_col = "p_adj", alpha = 0.05,
                                     exclude_low_count = FALSE,
                                     min_total_count = NULL) {
  prop_a <- paste0("proportion_", group_a)
  prop_b <- paste0("proportion_", group_b)

  sig <- results %>%
    filter(!is.na(p_adj), .data[[p_col]] < alpha)
  if (exclude_low_count) {
    sig <- filter(sig, !low_count)
  }
  if (!is.null(min_total_count)) {
    sig <- filter(
      sig,
      .data[[paste0("count_", group_a)]] + .data[[paste0("count_", group_b)]] >
        min_total_count
    )
  }

  sig %>%
    mutate(flag = reorder_within(flag, abs(diff), period)) %>%
    select(period, flag, !!labels[1] := all_of(prop_a),
           !!labels[2] := all_of(prop_b)) %>%
    pivot_longer(
      all_of(labels),
      names_to = "group",
      values_to = "proportion"
    )
}

#' Yearly proportion of retractions citing each of the top reasons
#'
#' Computes, for each year, the proportion of retractions that cite each
#' retraction reason. A retraction citing several reasons counts once for
#' each of them, so proportions within a year can add up to more than one.
#' The reasons kept are those ranked in the top `n_top` of at least one
#' year; their proportions are then returned for every year.
#'
#' @param retraction_data Output of [load_retraction_data()]. Rows are
#'   de-duplicated by `Record ID`, so the one-row-per-country format is fine.
#' @param n_top Number of top reasons selected in each year.
#' @param year_col Year column to use, `"retraction_year"` or
#'   `"publication_year"`.
#' @param min_retractions_per_year Years with fewer retractions are dropped,
#'   as their top reasons are dominated by noise.
#' @param year_limits Optional length-2 vector with the first and last year.
#' @param exclude_reasons Optional character vector of reasons to ignore
#'   (e.g. administrative notices).
#' @return A tibble with columns `year`, `Reason`, `n` (retractions citing
#'   the reason), `total` (retractions that year) and `proportion`.
#' @export
compute_top_reasons_over_time <- function(retraction_data,
                                          n_top = 3,
                                          year_col = "retraction_year",
                                          min_retractions_per_year = 100,
                                          year_limits = NULL,
                                          exclude_reasons = NULL) {
  retractions <- retraction_data %>%
    distinct(`Record ID`, year = .data[[year_col]], Reason) %>%
    filter(!is.na(year))
  if (!is.null(year_limits)) {
    retractions <- retractions %>%
      filter(year >= year_limits[1], year <= year_limits[2])
  }

  totals <- retractions %>%
    count(year, name = "total") %>%
    filter(total >= min_retractions_per_year)

  reason_props <- retractions %>%
    separate_rows(Reason, sep = ";") %>%
    mutate(Reason = str_trim(Reason)) %>%
    filter(Reason != "", !Reason %in% exclude_reasons) %>%
    distinct() %>%
    count(year, Reason) %>%
    inner_join(totals, by = "year")

  top_reasons <- reason_props %>%
    group_by(year) %>%
    slice_max(n, n = n_top, with_ties = FALSE) %>%
    pull(Reason) %>%
    unique()

  # Years in which a top reason does not appear get a proportion of 0
  reason_props %>%
    filter(Reason %in% top_reasons) %>%
    tidyr::complete(year = totals$year, Reason, fill = list(n = 0L)) %>%
    select(-total) %>%
    left_join(totals, by = "year") %>%
    mutate(proportion = n / total) %>%
    arrange(year, desc(proportion))
}
