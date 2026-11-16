#' Combine yearly SCImago country rankings into a single table
#'
#' Reads every `scimagojr country rank <YEAR>.xlsx` file in `dir`, adds a
#' `Year` column taken from the file name, writes the combined table to
#' `output_csv` and returns it read back from that CSV. Country names are
#' recoded to the names used by Retraction Watch (e.g. "Russian Federation"
#' becomes "Russia").
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
  }) %>%
    recode_sjr_countries()

  readr::write_csv(sjr_country_counts, output_csv)
  readr::read_csv(output_csv, show_col_types = FALSE)
}

# Recodes SCImago country names to the names used by Retraction Watch
recode_sjr_countries <- function(sjr_country_counts) {
  mutate(sjr_country_counts,
         Country = recode(.data$Country, !!!SJR_COUNTRY_RECODES))
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
#' @param publication_years Length-2 vector with the first and last
#'   publication year kept. Defaults to the years covered by the SCImago
#'   publication counts, so that every retraction has a denominator.
#' @param retractions_only If `TRUE`, keeps only entries whose
#'   `RetractionNature` is "Retraction" (dropping corrections, expressions of
#'   concern and reinstatements).
#' @param verbose If `TRUE`, prints a summary of the raw file that identifies
#'   its version before any row is removed, see
#'   [describe_retraction_data()].
#' @return The cleaned and anonymized data (one row per paper and country,
#'   with dates coarsened to years, see [anonymize_retraction_data()] and
#'   [coarsen_retraction_dates()]) with a per-paper `country_class` column,
#'   see [add_paper_country_class()].
#' @export
load_retraction_data <- function(path = "data/retraction_watch.csv",
                                 oecd_countries = load_oecd_countries(),
                                 reason_groupings_path = "data/retraction_reason_groupings.csv",
                                 publication_years = c(1996, 2025),
                                 retractions_only = FALSE,
                                 verbose = TRUE) {
  retraction_data <- readr::read_csv(path, show_col_types = FALSE, col_types = RETRACTION_DATA_TYPES)
  if (verbose) {
    describe_retraction_data(retraction_data, path)
  }
  # Stops the execution if titles, authors, journals or identifiers cannot be
  # removed
  retraction_data <- anonymize_retraction_data(retraction_data)
  retraction_reason_conversion <- readr::read_csv(reason_groupings_path,
                                                  show_col_types = FALSE, col_types = RETRACTION_REASON_GROUPINGS_TYPES)
  # Cleaning needs the exact dates (for the retraction delay), so they are
  # coarsened to years afterwards; this also stops the execution if it fails
  clean_retraction_data(retraction_data, oecd_countries, retraction_reason_conversion,
                        publication_years = publication_years,
                        retractions_only = retractions_only) %>%
    coarsen_retraction_dates()
}

#' Describe the version of a raw Retraction Watch file
#'
#' Prints aggregate statistics of the raw data, before it is anonymized or
#' cleaned, which tell different downloads of the dataset apart: the MD5
#' checksum and modification time of the file, its numbers of rows, columns
#' and distinct records, the rows without a `Record ID` (usually lines the
#' CSV parser could not read), the first and last original paper and retraction
#' dates, and the number of entries of each `RetractionNature`. The last
#' retraction date shows roughly when the data was exported. Nothing that
#' identifies a paper is printed.
#'
#' @param retraction_data Raw Retraction Watch data, as read from the CSV.
#' @param path Optional path of the CSV file, for its checksum and
#'   modification time.
#' @return Invisibly, a list with the statistics printed.
#' @export
describe_retraction_data <- function(retraction_data, path = NULL) {
  # Raw dates look like "1/31/2024 0:00"
  parse_dates <- function(x) {
    suppressWarnings(mdy(sub(" .*$", "", x)))
  }
  date_range <- function(column) {
    if (!column %in% names(retraction_data)) {
      return(list(first = NA, last = NA, missing = NA_integer_))
    }
    dates <- parse_dates(retraction_data[[column]])
    list(first = suppressWarnings(min(dates, na.rm = TRUE)),
         last = suppressWarnings(max(dates, na.rm = TRUE)),
         missing = sum(is.na(dates)))
  }

  info <- list(
    file = path,
    md5 = if (!is.null(path) && file.exists(path)) unname(tools::md5sum(path)) else NA_character_,
    modified = if (!is.null(path) && file.exists(path)) file.mtime(path) else NA,
    rows = nrow(retraction_data),
    columns = ncol(retraction_data),
    records = if ("Record ID" %in% names(retraction_data)) {
      length(unique(retraction_data$`Record ID`))
    } else {
      NA_integer_
    },
    rows_without_id = if ("Record ID" %in% names(retraction_data)) {
      sum(is.na(retraction_data$`Record ID`))
    } else {
      NA_integer_
    },
    publication_dates = date_range("OriginalPaperDate"),
    retraction_dates = date_range("RetractionDate"),
    nature = if ("RetractionNature" %in% names(retraction_data)) {
      sort(table(retraction_data$RetractionNature, useNA = "ifany"), decreasing = TRUE)
    } else {
      NULL
    }
  )

  format_range <- function(range) {
    paste0(format(range$first), " to ", format(range$last),
           " (", format(range$missing, big.mark = ","), " missing or unparsed)")
  }
  cat("Retraction Watch data", if (!is.null(path)) paste0("(", path, ")"), "\n")
  if (!is.na(info$md5)) {
    cat("  MD5 checksum:     ", info$md5, "\n")
    cat("  File modified:    ", format(info$modified, "%Y-%m-%d %H:%M"), "\n")
  }
  cat("  Rows:             ", format(info$rows, big.mark = ","), "\n")
  cat("  Columns:          ", info$columns, "\n")
  cat("  Distinct records: ", format(info$records, big.mark = ","), "\n")
  cat("  Rows without ID:  ", format(info$rows_without_id, big.mark = ","), "\n")
  cat("  Publication dates:", format_range(info$publication_dates), "\n")
  cat("  Retraction dates: ", format_range(info$retraction_dates), "\n")
  if (!is.null(info$nature)) {
    cat("  Retraction nature:",
        # prettyNum() formats each count on its own, without padding
        paste0(names(info$nature), " ", prettyNum(as.integer(info$nature), big.mark = ","),
               collapse = "; "), "\n")
  }
  invisible(info)
}

#' Describe the cleaned Retraction Watch data
#'
#' The counterpart of [describe_retraction_data()] for the output of
#' [load_retraction_data()] (or a subset of it), to see what the cleaning and
#' filtering kept. Prints its numbers of rows (one per paper and country),
#' columns and distinct records, the first and last publication and
#' retraction years (the dates are coarsened to years by then) and the number
#' of distinct records of each `RetractionNature`. Nothing that identifies a
#' paper is printed.
#'
#' @param retraction_data Output of [load_retraction_data()], or a subset of
#'   it.
#' @return Invisibly, a list with the statistics printed.
#' @export
describe_cleaned_retraction_data <- function(retraction_data) {
  year_range <- function(column) {
    if (!column %in% names(retraction_data)) {
      return(list(first = NA, last = NA, missing = NA_integer_))
    }
    years <- suppressWarnings(as.numeric(retraction_data[[column]]))
    list(first = suppressWarnings(min(years, na.rm = TRUE)),
         last = suppressWarnings(max(years, na.rm = TRUE)),
         missing = sum(is.na(years)))
  }
  has_id <- "Record ID" %in% names(retraction_data)
  papers <- if (has_id) {
    distinct(retraction_data, `Record ID`, .keep_all = TRUE)
  } else {
    retraction_data
  }

  info <- list(
    rows = nrow(retraction_data),
    columns = ncol(retraction_data),
    records = if (has_id) nrow(papers) else NA_integer_,
    publication_years = year_range("publication_year"),
    retraction_years = year_range("retraction_year"),
    # Per distinct record, as papers with several countries have several rows
    nature = if ("RetractionNature" %in% names(papers)) {
      sort(table(papers$RetractionNature, useNA = "ifany"), decreasing = TRUE)
    } else {
      NULL
    }
  )

  format_range <- function(range) {
    paste0(range$first, " to ", range$last, " (",
           prettyNum(range$missing, big.mark = ","), " rows missing)")
  }
  cat("Cleaned retraction data\n")
  cat("  Rows:             ", prettyNum(info$rows, big.mark = ","),
      "(one per paper and country)\n")
  cat("  Columns:          ", info$columns, "\n")
  cat("  Distinct records: ", prettyNum(info$records, big.mark = ","), "\n")
  cat("  Publication years:", format_range(info$publication_years), "\n")
  cat("  Retraction years: ", format_range(info$retraction_years), "\n")
  if (!is.null(info$nature)) {
    cat("  Retraction nature:",
        paste0(names(info$nature), " ", prettyNum(as.integer(info$nature), big.mark = ","),
               collapse = "; "), "(distinct records)\n")
  }
  invisible(info)
}
