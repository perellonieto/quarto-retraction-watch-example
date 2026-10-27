# Compares, step by step, the data frames saved by retraction-analysis.qmd
# ("original") and retraction-analysis-library.qmd ("library").
#
# 1. Render both pages so that they write their checkpoints:
#      quarto render retraction-analysis.qmd
#      quarto render retraction-analysis-library.qmd
# 2. Run this script from the project root:
#      Rscript compare_checkpoints.R
#    or source() it in an R session to inspect the returned differences, e.g.
#      View(differences[["04_retraction_rates"]])
#
# The first step with differing rows is where the library code departs from
# the original one.

source("checkpoints.R")

# Labels of the original page mapped to those of the library
scenario_names <- c(
  "Raw"                 = "All",
  "No mass retractions" = "All without mass retractions",
  "Domestic only"       = "Domestic",
  "Domestic + no mass"  = "Domestic without mass retractions"
)
original_scenarios <- list(recode = list(scenario = scenario_names))

# The original page names reasons like janitor::clean_names() of
# "is_<Reason>" (e.g. "is_author_unresponsive"); the library keeps the
# reason text (e.g. "Author Unresponsive")
library_flags <- list(transform = function(df) {
  reasons <- unique(df$flag)
  clean <- janitor::make_clean_names(paste0("is_", reasons))
  mutate(df, flag = clean[match(flag, reasons)])
})

differences <- list(
  # One row per paper and country; the columns that the analysis relies on
  "01_retraction_data" = compare_step(
    "01_retraction_data",
    keys = c("Record ID", "Country"),
    original = list(keep = c("Record ID", "Country", "country_class",
                             "international", "Mass_Retraction",
                             "publication_year", "retraction_year")),
    library = list(keep = c("Record ID", "Country", "country_class",
                            "international", "Mass_Retraction",
                            "publication_year", "retraction_year"))
  ),
  "02_sjr_country_counts" = compare_step(
    "02_sjr_country_counts", keys = c("Country", "Year")
  ),
  "03_publication_counts" = compare_step(
    "03_publication_counts", keys = c("publication_year", "country_class")
  ),
  "04_retraction_rates" = compare_step(
    "04_retraction_rates",
    keys = c("publication_year", "country_class", "scenario"),
    original = original_scenarios
  ),
  "05_reasons_compare_results" = compare_step(
    "05_reasons_compare_results", keys = c("period", "flag"),
    library = library_flags
  ),
  "06_retraction_ai_rates" = compare_step(
    "06_retraction_ai_rates", keys = c("Country", "scenario"),
    original = original_scenarios
  ),
  "07_retraction_ai_counts_ts" = compare_step(
    "07_retraction_ai_counts_ts",
    keys = c("publication_year", "country_class", "scenario"),
    original = original_scenarios
  ),
  "08_retraction_ai_prop_ts" = compare_step(
    "08_retraction_ai_prop_ts",
    keys = c("publication_year", "country_class", "scenario"),
    original = original_scenarios
  ),
  "09_reasons_compare_results_uk" = compare_step(
    "09_reasons_compare_results_uk", keys = c("period", "flag"),
    library = library_flags
  )
)

cat("\n== Summary: rows that differ per step ==\n")
print(vapply(differences, nrow, integer(1)))
