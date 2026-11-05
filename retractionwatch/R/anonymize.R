#' Columns of the Retraction Watch data that identify papers or people
#'
#' Removed by [anonymize_retraction_data()]: titles, author names, their
#' institutions, links, free-text notes (which can name people), the DOIs
#' and PubMed IDs of the retraction notices and the original papers, and
#' the journal, which together with the dates singles out most papers.
#' @export
IDENTIFYING_COLUMNS <- c(
  "Title", "Author", "Institution", "URLS", "Notes", "Journal",
  "RetractionDOI", "RetractionPubMedID", "OriginalPaperDOI", "OriginalPaperPubMedID"
)

#' Date columns of the Retraction Watch data, coarsened to years by
#' [coarsen_retraction_dates()]
#' @export
DATE_COLUMNS <- c("RetractionDate", "OriginalPaperDate")

#' Remove author names, identifiers, titles and journals from the data
#'
#' Drops the [IDENTIFYING_COLUMNS] and replaces `Record ID`, the public
#' Retraction Watch identifier of each paper, by a random permutation of
#' 1 to the number of papers. Rows of the same paper keep sharing one
#' number, so the analyses that count each paper once still work. Without a
#' seed the permutation is different every time; R's random number generator
#' is left as it was either way.
#'
#' The result is checked, and the execution stops with an error if any
#' identifying column remains, the identifiers were not replaced, or any
#' remaining text column contains a DOI, a URL, a PubMed identifier or an
#' email address.
#'
#' Exact dates are still needed to compute the retraction delay, so they
#' are coarsened to years after cleaning, by [coarsen_retraction_dates()]
#' (as done by [load_retraction_data()]).
#'
#' @param retraction_data Raw Retraction Watch data, as read from its CSV.
#' @param seed Optional seed of the permutation of the identifiers, only
#'   needed to get the same identifiers in several runs (e.g. to compare the
#'   checkpoints of two pages, see `make run-comparison`). It defaults to the
#'   `RETRACTIONWATCH_ID_SEED` environment variable or, if it is not set, to
#'   `NULL`: a new random permutation every time. Anyone knowing the seed and
#'   the order of the public data can undo the permutation, so do not publish
#'   a seed used for shared outputs.
#' @return `retraction_data` without the identifying columns and with a new
#'   `Record ID`.
#' @export
anonymize_retraction_data <- function(retraction_data, seed = anonymization_seed()) {
  if (!"Record ID" %in% names(retraction_data)) {
    stop("Cannot anonymize: the data has no `Record ID` column.")
  }
  original_ids <- retraction_data$`Record ID`
  papers <- unique(original_ids)
  new_ids <- with_seed(seed, sample.int(length(papers)))

  anonymized <- retraction_data[setdiff(names(retraction_data), IDENTIFYING_COLUMNS)]
  anonymized$`Record ID` <- new_ids[match(original_ids, papers)]

  check_anonymized(anonymized, original_ids)
  anonymized
}

#' Coarsen the exact retraction and publication dates to years
#'
#' Replaces each of the [DATE_COLUMNS] by its year (an integer). Run after
#' [clean_retraction_data()], which needs the exact dates for the retraction
#' delay. The execution stops with an error if any date column, or any other
#' column holding dates, remains more precise than a year.
#'
#' @param retraction_data Cleaned Retraction Watch data.
#' @return `retraction_data` with years instead of dates.
#' @export
coarsen_retraction_dates <- function(retraction_data) {
  for (column in intersect(DATE_COLUMNS, names(retraction_data))) {
    values <- retraction_data[[column]]
    if (!inherits(values, "Date")) {
      # Raw "M/D/YYYY H:MM" text: keep the year after the last "/"
      values <- as.integer(sub("^.*/(\\d{4}).*$", "\\1", values))
    } else {
      values <- as.integer(format(values, "%Y"))
    }
    retraction_data[[column]] <- values
  }
  check_coarsened(retraction_data)
  retraction_data
}

# Seed of the identifier permutation: the RETRACTIONWATCH_ID_SEED environment
# variable, or NULL (a new random permutation every time) if it is not set
anonymization_seed <- function() {
  seed <- Sys.getenv("RETRACTIONWATCH_ID_SEED", "")
  if (seed == "") NULL else as.integer(seed)
}

# Evaluates `expr` with the random number generator set to `seed`, and then
# restores its previous state. With `seed = NULL`, the seed is taken from the
# clock and the process ID, so it does not depend on any set.seed() call
with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    seed <- as.integer((as.numeric(Sys.time()) * 1e6 + Sys.getpid()) %%
                         .Machine$integer.max)
  }
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit(if (had_seed) {
    assign(".Random.seed", old_seed, envir = globalenv())
  } else {
    rm(".Random.seed", envir = globalenv())
  })
  set.seed(seed)
  expr
}

# Stops with an error if `data` still has identifying columns, its
# `Record ID` is not a consistent new numbering of `original_ids`, or a text
# column contains a DOI, URL, PubMed identifier or email address
check_anonymized <- function(data, original_ids) {
  left <- intersect(IDENTIFYING_COLUMNS, names(data))
  if (length(left) > 0) {
    stop("Anonymization failed: identifying columns remain: ",
         paste(left, collapse = ", "), ".")
  }
  new_ids <- data$`Record ID`
  papers <- unique(original_ids)
  # One new ID per paper, all different, and not the original ones
  consistent <- length(new_ids) == length(original_ids) && !anyNA(new_ids) &&
    length(unique(new_ids)) == length(papers) &&
    nrow(unique(data.frame(original_ids, new_ids))) == length(papers)
  if (!consistent || (length(papers) > 1 && identical(as.numeric(new_ids), as.numeric(original_ids)))) {
    stop("Anonymization failed: `Record ID` was not replaced consistently.")
  }

  patterns <- c(
    `a DOI` = "\\b10\\.\\d{4,9}/\\S+",
    `a URL` = "https?://|www\\.",
    `a PubMed identifier` = "pubmed|\\bPMID\\b",
    `an email address` = "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}"
  )
  for (column in names(data)[vapply(data, is.character, logical(1))]) {
    for (kind in names(patterns)) {
      found <- grepl(patterns[[kind]], data[[column]], ignore.case = TRUE, perl = TRUE)
      if (any(found)) {
        stop("Anonymization failed: column `", column, "` contains ", sum(found),
             " value(s) that look like ", kind, ".")
      }
    }
  }
  invisible(TRUE)
}

# Stops with an error if a column holds dates, the date columns are not
# years, or the data has row numbers of the raw file (which identify papers)
check_coarsened <- function(data) {
  row_numbers <- intersect(c("paper_index", "row"), names(data))
  if (length(row_numbers) > 0) {
    stop("Anonymization failed: row numbers of the raw data remain: ",
         paste(row_numbers, collapse = ", "), ".")
  }
  dated <- names(data)[vapply(data, function(x) inherits(x, c("Date", "POSIXt")), logical(1))]
  if (length(dated) > 0) {
    stop("Anonymization failed: columns with exact dates remain: ",
         paste(dated, collapse = ", "), ".")
  }
  for (column in intersect(DATE_COLUMNS, names(data))) {
    values <- data[[column]]
    if (!is.numeric(values) || any(!is.na(values) & (values != round(values) |
                                                      values < 1000 | values > 9999))) {
      stop("Anonymization failed: `", column, "` is not coarsened to years.")
    }
  }
  invisible(TRUE)
}
