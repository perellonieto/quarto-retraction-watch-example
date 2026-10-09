# Package-level setup for retractionwatch. This file contains no analysis
# functions; it holds settings that apply to the whole package:
#
# 1. The roxygen block above "_PACKAGE" documents the package itself
#    (?retractionwatch, using the Title and Description from DESCRIPTION)
#    and lists the functions imported from other packages. Running
#    devtools::document() turns the @import/@importFrom tags into the
#    import() lines of NAMESPACE, so add a tag here whenever package code
#    uses a function from another package without the pkg:: prefix.
# 2. utils::globalVariables() declares the column names used unquoted in
#    dplyr/ggplot2 code (e.g. filter(Country == ...)). Without it,
#    R CMD check reports "no visible binding for global variable" notes.
#    Add new column names here when they are used that way.

#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr separate_rows crossing pivot_wider pivot_longer
#' @importFrom rlang .data :=
#' @importFrom lubridate mdy
#' @importFrom stringr str_extract str_trim str_detect
#' @importFrom tibble tibble tribble as_tibble
#' @importFrom janitor clean_names
#' @importFrom readr cols col_character col_integer
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
  "low_count", "sig_class", "diff", "group", "x", "y", "label", "n", ".",
  "year", "total"
))
