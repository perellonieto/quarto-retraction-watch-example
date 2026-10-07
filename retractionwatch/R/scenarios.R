#' Default filtering scenarios
#'
#' Each scenario says whether to exclude mass retractions and whether to keep
#' only domestic (non-international) papers.
#'
#' @return A tibble with columns `scenario`, `exclude_mass` and `domestic_only`.
#' @export
default_scenarios <- function() {
  tribble(
    ~scenario,                            ~exclude_mass, ~domestic_only,
    "All",                                FALSE,         FALSE,
    "All without mass retractions",       TRUE,          FALSE,
    "Domestic",                           FALSE,         TRUE,
    "Domestic without mass retractions",  TRUE,          TRUE
  )
}

#' Count unique retractions under each scenario
#'
#' Keeps one row per `RetractionID`, replicates the data for every scenario,
#' applies each scenario's filters and counts rows by the grouping columns.
#'
#' @param data Cleaned retraction data.
#' @param ... Columns to count by (in addition to `scenario`, which should
#'   be listed explicitly where wanted).
#' @param scenarios Scenarios table, see [default_scenarios()].
#' @param name Name of the count column.
#' @return A tibble of counts.
#' @export
count_by_scenario <- function(data, ..., scenarios = default_scenarios(),
                              name = "retractions") {
  data %>%
    distinct(RetractionID, .keep_all = TRUE) %>%
    crossing(scenarios) %>%
    filter(
      !exclude_mass | Mass_Retraction == "Other",
      !domestic_only | !international
    ) %>%
    count(..., name = name)
}

#' Keep only retractions whose reasons mention AI-generated content
#'
#' @param data Retraction data with a `Reason` column.
#' @param pattern Regular expression identifying AI-related reasons.
#' @return The filtered data.
#' @export
filter_ai_retractions <- function(data,
                                  pattern = "Computer-Aided Content|Computer-Generated Content") {
  data %>% filter(str_detect(Reason, pattern))
}
