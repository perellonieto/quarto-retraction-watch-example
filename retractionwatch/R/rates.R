#' Total publications per year and country class
#'
#' SCImago only gives totals per country, in which a paper with authors in
#' several countries is counted once for each of them. Summing countries is
#' exact for the United Kingdom, but for "Other OECD" and "Rest of World" it
#' counts collaborations within the class more than once and includes papers
#' that the per-paper retraction classes assign to a higher class, so rates
#' for those two classes are biased low.
#'
#' @param sjr_country_counts SCImago counts, see [build_sjr_country_counts()].
#' @param oecd_countries Data frame with a `Name` column of OECD countries.
#' @return A tibble with `publication_year`, `country_class` and `total_papers`.
#' @export
publication_counts_by_class <- function(sjr_country_counts, oecd_countries) {
  sjr_country_counts %>%
    add_country_class(oecd_countries) %>%
    group_by(publication_year = Year, country_class) %>%
    summarise(total_papers = sum(Documents, na.rm = TRUE), .groups = "drop")
}

#' Total publications per country (all years)
#'
#' @param sjr_country_counts SCImago counts, see [build_sjr_country_counts()].
#' @return A tibble with `Country` and `total_papers`.
#' @export
publication_counts_by_country <- function(sjr_country_counts) {
  sjr_country_counts %>%
    group_by(Country) %>%
    summarise(total_papers = sum(Documents, na.rm = TRUE), .groups = "drop")
}

#' Retraction rates per year, country class and scenario
#'
#' @param retraction_data Cleaned retraction data with `country_class`.
#' @param publication_counts Output of [publication_counts_by_class()].
#' @param scenarios Scenarios table, see [default_scenarios()].
#' @return A tibble with retraction counts, rates and rates per 100k papers.
#' @export
compute_retraction_rates <- function(retraction_data, publication_counts,
                                     scenarios = default_scenarios()) {
  retraction_data %>%
    count_by_scenario(publication_year, country_class, scenario,
                      scenarios = scenarios) %>%
    stats::na.omit() %>%
    left_join(publication_counts, by = c("publication_year", "country_class")) %>%
    mutate(
      retraction_rate = retractions / total_papers,
      retraction_rate_per_100k = 100000 * retraction_rate
    )
}

#' AI retraction shares and rates per country and scenario
#'
#' Pass the full retraction data (with mass retractions): the scenarios
#' decide whether mass retractions are excluded. Every country with a
#' retraction gets a row in every scenario, with zero counts where it has
#' none, so countries without AI retractions are not silently dropped.
#'
#' The rate per published paper is `NA` in domestic-only scenarios: SCImago
#' counts all of a country's papers, not only its domestic ones, so the
#' rate would compare domestic retractions with all papers. Use the share of
#' retractions that mention AI there instead, whose numerator and
#' denominator are both domestic.
#'
#' @param retraction_data Cleaned retraction data.
#' @param sjr_country_counts SCImago counts, see [build_sjr_country_counts()].
#' @param scenarios Scenarios table, see [default_scenarios()].
#' @param publication_years Optional length-2 vector with the first and last
#'   publication year, applied to both the retractions and the SCImago
#'   counts so that both cover the same years.
#' @param conf_level Confidence level of the intervals.
#' @return A tibble with AI retraction counts, their share of all retractions
#'   (with exact binomial intervals) and their rate per published paper
#'   (with exact Poisson intervals). Interval columns end in `_lower` and
#'   `_upper`.
#' @export
compute_ai_rates_by_country <- function(retraction_data, sjr_country_counts,
                                        scenarios = default_scenarios(),
                                        publication_years = NULL,
                                        conf_level = 0.95) {
  retraction_data <- filter(retraction_data, !is.na(.data$Country))
  if (!is.null(publication_years)) {
    retraction_data <- filter(retraction_data,
                              publication_year >= publication_years[1],
                              publication_year <= publication_years[2])
    sjr_country_counts <- filter(sjr_country_counts,
                                 Year >= publication_years[1],
                                 Year <= publication_years[2])
  }
  ai_counts <- retraction_data %>%
    filter_ai_retractions() %>%
    count_by_scenario(Country, scenario, scenarios = scenarios,
                      name = "ai_retractions")

  retraction_data %>%
    count_by_scenario(Country, scenario, scenarios = scenarios) %>%
    tidyr::complete(Country, scenario = scenarios$scenario,
                    fill = list(retractions = 0L)) %>%
    left_join(ai_counts, by = c("Country", "scenario")) %>%
    mutate(ai_retractions = coalesce(ai_retractions, 0L)) %>%
    left_join(publication_counts_by_country(sjr_country_counts), by = "Country") %>%
    left_join(select(scenarios, scenario, domestic_only), by = "scenario") %>%
    mutate(
      ai_retraction_share = if_else(retractions > 0, ai_retractions / retractions, NA_real_),
      ai_retraction_share_pct = 100 * ai_retraction_share,
      ai_retraction_share_pct_lower = 100 * binomial_ci(ai_retractions, retractions, conf_level)$lower,
      ai_retraction_share_pct_upper = 100 * binomial_ci(ai_retractions, retractions, conf_level)$upper,
      ai_retraction_rate = if_else(domestic_only, NA_real_, ai_retractions / total_papers),
      ai_retraction_rate_per_100k = 100000 * ai_retraction_rate,
      ai_retraction_rate_per_100k_lower = if_else(
        domestic_only, NA_real_,
        100000 * poisson_ci(ai_retractions, conf_level)$lower / total_papers),
      ai_retraction_rate_per_100k_upper = if_else(
        domestic_only, NA_real_,
        100000 * poisson_ci(ai_retractions, conf_level)$upper / total_papers)
    ) %>%
    select(-domestic_only)
}

# Exact (Garwood) confidence interval for a Poisson count
poisson_ci <- function(count, conf_level = 0.95) {
  alpha <- 1 - conf_level
  list(lower = if_else(count == 0, 0, stats::qgamma(alpha / 2, count)),
       upper = stats::qgamma(1 - alpha / 2, count + 1))
}

# Exact (Clopper-Pearson) confidence interval for a proportion; NA when the
# number of trials is 0
binomial_ci <- function(successes, trials, conf_level = 0.95) {
  alpha <- 1 - conf_level
  lower <- if_else(successes == 0, 0,
                   suppressWarnings(stats::qbeta(alpha / 2, successes, trials - successes + 1)))
  upper <- if_else(successes == trials, 1,
                   suppressWarnings(stats::qbeta(1 - alpha / 2, successes + 1, trials - successes)))
  list(lower = if_else(trials > 0, lower, NA_real_),
       upper = if_else(trials > 0, upper, NA_real_))
}

#' AI retraction counts per year, country class and scenario
#'
#' Papers with no known country (no `country_class`) are left out.
#'
#' @inheritParams compute_retraction_rates
#' @param conf_level Confidence level of the exact Poisson intervals.
#' @return A tibble with `ai_retractions` counts and their intervals
#'   `ai_retractions_lower` and `ai_retractions_upper`.
#' @export
compute_ai_counts_ts <- function(retraction_data, scenarios = default_scenarios(),
                                 conf_level = 0.95) {
  retraction_data %>%
    filter(!is.na(country_class)) %>%
    filter_ai_retractions() %>%
    count_by_scenario(publication_year, country_class, scenario,
                      scenarios = scenarios, name = "ai_retractions") %>%
    mutate(ai_retractions_lower = poisson_ci(ai_retractions, conf_level)$lower,
           ai_retractions_upper = poisson_ci(ai_retractions, conf_level)$upper)
}

#' Proportion of retractions mentioning AI per year, country class and scenario
#'
#' Papers with no known country (no `country_class`) are left out. Years in
#' which a class has retractions but none mentioning AI get a proportion of 0.
#'
#' @inheritParams compute_ai_counts_ts
#' @return A tibble with `ai_retractions`, `total_retractions`, `prop_ai` and
#'   its exact binomial interval `prop_ai_lower` and `prop_ai_upper`.
#' @export
compute_ai_proportion_ts <- function(retraction_data, scenarios = default_scenarios(),
                                     conf_level = 0.95) {
  # Each paper counts once, in its per-paper country class, both here and in
  # the AI counts, so `prop_ai` compares like with like
  retraction_data <- filter(retraction_data, !is.na(country_class))
  ai_counts <- compute_ai_counts_ts(retraction_data, scenarios, conf_level) %>%
    select(publication_year, country_class, scenario, ai_retractions)

  retraction_data %>%
    count_by_scenario(publication_year, country_class, scenario,
                      scenarios = scenarios, name = "total_retractions") %>%
    left_join(ai_counts, by = c("publication_year", "country_class", "scenario")) %>%
    mutate(ai_retractions = coalesce(ai_retractions, 0L),
           prop_ai = ai_retractions / total_retractions,
           prop_ai_lower = binomial_ci(ai_retractions, total_retractions, conf_level)$lower,
           prop_ai_upper = binomial_ci(ai_retractions, total_retractions, conf_level)$upper)
}

#' Table of AI retraction rates for selected countries in one scenario
#'
#' @param rates Output of [compute_ai_rates_by_country()].
#' @param countries Character vector of countries, or a data frame with a
#'   `Country` column.
#' @param scenario_name Scenario to show.
#' @param digits Number of decimals for the rates and percentages.
#' @return A tibble with one row per country, sorted by the AI retraction
#'   rate, with counts and formatted estimates and 95% intervals.
#' @export
ai_rates_table <- function(rates, countries, scenario_name = "All", digits = 2) {
  if (is.data.frame(countries)) {
    countries <- countries$Country
  }
  estimate <- function(x, lower, upper) {
    fmt <- function(v) formatC(v, format = "f", digits = digits)
    if_else(is.na(x), NA_character_,
            paste0(fmt(x), " (", fmt(lower), "–", fmt(upper), ")"))
  }
  rates %>%
    filter(scenario == scenario_name, Country %in% countries) %>%
    arrange(desc(ai_retraction_rate_per_100k)) %>%
    transmute(
      Country,
      `Retractions` = retractions,
      `AI retractions` = ai_retractions,
      `Papers published` = total_papers,
      `% of retractions mentioning AI (95% CI)` = estimate(
        ai_retraction_share_pct, ai_retraction_share_pct_lower, ai_retraction_share_pct_upper),
      `AI retractions per 100,000 papers (95% CI)` = estimate(
        ai_retraction_rate_per_100k, ai_retraction_rate_per_100k_lower,
        ai_retraction_rate_per_100k_upper)
    )
}
