# Helpers to compare the intermediate data frames of retraction-analysis.qmd
# ("original") and retraction-analysis-library.qmd ("library").
#
# Both pages source this file, set `checkpoint_version` and call
# `checkpoint(df, "step_name")` after each step. Rendering the pages writes
# checkpoints/<step>_<version>.rds. Then run compare_checkpoints.R to see,
# step by step, where the two pages start to differ.

library(dplyr)

checkpoint_dir <- "checkpoints"

# Saves `df` as checkpoints/<step>_<version>.rds. RDS keeps column types
# (factors, integers, dates), which a CSV would lose.
checkpoint <- function(df, step, version = checkpoint_version) {
  dir.create(checkpoint_dir, showWarnings = FALSE)
  saveRDS(df, file.path(checkpoint_dir, paste0(step, "_", version, ".rds")))
  invisible(df)
}

# Makes a data frame comparable with its counterpart from the other page:
# * keep:      columns to keep (all if NULL); duplicated rows are then dropped.
# * rename:    c(new_name = "old_name") to align column names.
# * recode:    list(column = c(old_value = "new_value")) to align labels.
# * transform: any other function applied to the data frame.
# Data frame columns (e.g. RetractionID) are unpacked, factors become
# characters, doubles are rounded to ignore floating point noise and columns
# and rows are sorted.
normalise <- function(df, keep = NULL, rename = NULL, recode = NULL,
                      transform = identity) {
  df <- as_tibble(df)
  for (col in names(df)) {
    if (is.data.frame(df[[col]])) df[[col]] <- df[[col]][[1]]
  }
  if (!is.null(rename)) df <- dplyr::rename(df, any_of(rename))
  for (col in names(recode)) {
    df[[col]] <- dplyr::recode(df[[col]], !!!as.list(recode[[col]]))
  }
  df <- transform(df)
  if (!is.null(keep)) df <- distinct(select(df, any_of(keep)))
  df %>%
    mutate(across(where(is.factor), as.character),
           across(where(is.double), ~ round(.x, 10))) %>%
    select(sort(names(.))) %>%
    arrange(across(everything()))
}

# Compares the original and library checkpoints of `step`. Rows are matched by
# `keys`, so their order does not matter. Prints the number of rows, the
# columns present in only one page and the number of differing rows, and
# returns (invisibly) the differing rows side by side, with suffixes .orig and
# .lib. Arguments in `original` and `library` are passed to normalise() for
# each page.
compare_step <- function(step, keys, original = list(), library = list()) {
  read_step <- function(version, args) {
    path <- file.path(checkpoint_dir, paste0(step, "_", version, ".rds"))
    do.call(normalise, c(list(readRDS(path)), args))
  }
  a <- read_step("original", original)
  b <- read_step("library", library)

  cat("\n==", step, "==\n")
  cat("Rows:", nrow(a), "(original) vs", nrow(b), "(library)\n")
  only_a <- setdiff(names(a), names(b))
  only_b <- setdiff(names(b), names(a))
  if (length(only_a)) cat("Columns only in original:", only_a, "\n")
  if (length(only_b)) cat("Columns only in library: ", only_b, "\n")

  common <- intersect(names(a), names(b))
  values <- setdiff(common, keys)
  joined <- full_join(a[common], b[common], by = keys,
                      suffix = c(".orig", ".lib"), relationship = "many-to-many")
  differ_by_column <- lapply(stats::setNames(values, values), function(v) {
    x <- joined[[paste0(v, ".orig")]]
    y <- joined[[paste0(v, ".lib")]]
    is.na(x) != is.na(y) | (!is.na(x) & !is.na(y) & x != y)
  })
  differ <- Reduce(`|`, differ_by_column, rep(FALSE, nrow(joined)))
  differing_values <- values[vapply(differ_by_column, any, logical(1))]
  differing_columns <- as.vector(rbind(sprintf("%s.orig", differing_values),
                                       sprintf("%s.lib", differing_values)))
  differences <- joined[differ, c(keys, differing_columns)]

  cat("Rows that differ:", nrow(differences), "of", nrow(joined), "\n")
  if (nrow(differences) > 0) {
    cat("Columns that differ:", differing_values, "\n")
    print(differences, n = 10)
  }
  invisible(differences)
}
