#' Combine yearly SCImago country rankings into a single table
#'
#' Reads every `scimagojr country rank <YEAR>.xlsx` file in `dir`, adds a
#' `Year` column taken from the file name, writes the combined table to
#' `output_csv` and returns it read back from that CSV.
#'
#' @param dir Directory with the yearly SCImago Excel files.
#' @param output_csv Path of the combined CSV file to write.
#' @return A tibble with one row per country and year.
#' @export
build_sjr_country_counts <- function(dir = "data/sjr_rankings",
                                     output_csv = "data/scimagojr_country_rank_all_years.csv") {
  files <- list.files(
    dir,
    pattern = "^scimagojr country rank .*\\.xlsx$",
    full.names = TRUE
  )

  sjr_country_counts <- purrr::map_dfr(files, function(f) {
    year <- str_extract(basename(f), "\\d{4}")
    readxl::read_excel(f) %>%
      mutate(Year = as.integer(year))
  })

  readr::write_csv(sjr_country_counts, output_csv)
  readr::read_csv(output_csv, show_col_types = FALSE)
}

#' Load the list of OECD countries
#'
#' Country names are recoded to match the names used by Retraction Watch.
#'
#' @param path Path to the OECD countries CSV file.
#' @return A tibble with (at least) a `Name` column.
#' @export
load_oecd_countries <- function(path = "data/oecd_countries.csv") {
  readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(
      Name = recode(
        .data$Name,
        "Korea" = "South Korea",
        "Slovak Republic" = "Slovakia"
      )
    )
}

#' Load and clean the Retraction Watch dataset
#'
#' @param path Path to the Retraction Watch CSV file.
#' @param oecd_countries Output of [load_oecd_countries()].
#' @param reason_groupings_path Path to the CSV mapping each retraction
#'   reason to a reason group.
#' @return The cleaned data (one row per paper and country) with a
#'   `country_class` column added by [add_country_class()].
#' @export
load_retraction_data <- function(path = "data/retraction_watch.csv",
                                 oecd_countries = load_oecd_countries(),
                                 reason_groupings_path = "data/retraction_reason_groupings.csv") {
  retraction_data <- readr::read_csv(path, show_col_types = FALSE, col_types = RETRACTION_DATA_TYPES)
  retraction_reason_conversion <- readr::read_csv(reason_groupings_path,
                                                  show_col_types = FALSE, col_types = RETRACTION_REASON_GROUPINGS_TYPES)
  retraction_data %>%
    clean_retraction_data(oecd_countries, retraction_reason_conversion) %>%
    add_country_class(oecd_countries)
}
