# Small hand-made inputs in tests/testthat/fixtures/, so the tests do not
# depend on the full (untracked) files in data/.
#
# Retractions in fixtures/retraction_watch.csv:
#   1: United Kingdom, IEEE 2010 (IEEE mass retraction), retracted in 3 months
#   2: United Kingdom;China (international), retracted after 3.4 years
#   3: China, Hindawi 2023 (Hindawi mass retraction)
#   4: France, retracted after 1.4 years

fixture_path <- function(...) test_path("fixtures", ...)

read_fixture_retractions <- function() {
  readr::read_csv(fixture_path("retraction_watch.csv"),
                  col_types = RETRACTION_DATA_TYPES,
                  name_repair = "unique_quiet")
}

read_fixture_reason_groups <- function() {
  readr::read_csv(fixture_path("retraction_reason_groupings.csv"),
                  show_col_types = FALSE)
}

read_fixture_oecd <- function() {
  load_oecd_countries(fixture_path("oecd_countries.csv"))
}
