#' Total publications per year and country class
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
#' @param retraction_data Cleaned retraction data.
#' @param sjr_country_counts SCImago counts, see [build_sjr_country_counts()].
#' @param scenarios Scenarios table, see [default_scenarios()].
#' @return A tibble with AI retraction counts, their share of all retractions
#'   and their rate per published paper.
#' @export
compute_ai_rates_by_country <- function(retraction_data, sjr_country_counts,
                                        scenarios = default_scenarios()) {
  retraction_counts <- retraction_data %>%
    count_by_scenario(Country, scenario, scenarios = scenarios)

  retraction_data %>%
    filter_ai_retractions() %>%
    count_by_scenario(Country, scenario, scenarios = scenarios,
                      name = "ai_retractions") %>%
    left_join(retraction_counts, by = c("Country", "scenario")) %>%
    left_join(publication_counts_by_country(sjr_country_counts), by = "Country") %>%
    mutate(
      ai_retraction_share = ai_retractions / retractions,
      ai_retraction_share_pct = 100 * ai_retraction_share,
      ai_retraction_rate = ai_retractions / total_papers,
      ai_retraction_rate_per_100k = 100000 * ai_retraction_rate
    )
}

#' AI retraction counts per year, country class and scenario
#'
#' @inheritParams compute_retraction_rates
#' @return A tibble with `ai_retractions` counts.
#' @export
compute_ai_counts_ts <- function(retraction_data, scenarios = default_scenarios()) {
  retraction_data %>%
    filter_ai_retractions() %>%
    count_by_scenario(publication_year, country_class, scenario,
                      scenarios = scenarios, name = "ai_retractions")
}

#' Proportion of retractions mentioning AI per year, country class and scenario
#'
#' @inheritParams compute_retraction_rates
#' @return A tibble with `ai_retractions`, `total_retractions` and `prop_ai`.
#' @export
compute_ai_proportion_ts <- function(retraction_data, scenarios = default_scenarios()) {
  total_counts <- retraction_data %>%
    count_by_scenario(publication_year, country_class, scenario,
                      scenarios = scenarios, name = "total_retractions")

  compute_ai_counts_ts(retraction_data, scenarios) %>%
    left_join(total_counts, by = c("publication_year", "country_class", "scenario")) %>%
    mutate(prop_ai = ai_retractions / total_retractions)
}
