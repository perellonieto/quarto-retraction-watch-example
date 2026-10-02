#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr separate_rows crossing pivot_wider pivot_longer
#' @importFrom rlang .data :=
#' @importFrom lubridate mdy
#' @importFrom stringr str_extract str_trim str_detect
#' @importFrom tibble tibble tribble as_tibble
#' @importFrom janitor clean_names
#' @importFrom tidytext reorder_within scale_y_reordered
"_PACKAGE"

# Column names used with non-standard evaluation
utils::globalVariables(c(
  "Country", "Documents", "Year", "Reason", "Publisher", "Record ID",
  "RetractionDate", "OriginalPaperDate", "RetractionID", "Mass_Retraction",
  "international", "exclude_mass", "domestic_only", "publication_year",
  "country_class", "scenario", "retractions", "total_papers",
  "retraction_rate", "ai_retractions", "total_retractions", "prop_ai",
  "ai_retraction_share", "ai_retraction_rate", "ai_retraction_share_pct",
  "ai_retraction_rate_per_100k", "retraction_rate_per_100k", "period",
  "value", "flag", "proportion", "count", "p_value", "p_adj", "p_adj_filtered",
  "low_count", "sig_class", "diff", "group", "x", "y", "label", "n", "."
))
