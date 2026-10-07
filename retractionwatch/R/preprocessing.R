
#' Clean the raw Retraction Watch data
#'
#' Cleans reasons and dates, adds one indicator column per reason group,
#' retraction delay categories, OECD/UK flags, an `international` flag and a
#' `Mass_Retraction` factor (IEEE 2009-2011 and Hindawi mass retractions).
#' Each step can be switched off with its corresponding argument.
#'
#' @param retraction_data Raw Retraction Watch data.
#' @param oecd_countries Data frame with a `Name` column of OECD countries.
#'   Only needed when `long_df_by_countries = TRUE`.
#' @param retraction_reason_conversion Data frame with `Reason` and `Group`
#'   columns mapping each reason to a reason group. Only needed when
#'   `add_reason_groups = TRUE`.
#' @param long_df_by_countries If `TRUE`, papers with several countries are
#'   split into one row per country and `oecd`/`UK` flags are added.
#' @param clean_reasons If `TRUE`, removes `+` characters from `Reason`.
#' @param parse_dates If `TRUE`, parses `OriginalPaperDate` and
#'   `RetractionDate` and adds `publication_year` and `retraction_year`.
#' @param add_reason_groups If `TRUE`, adds one 0/1 column per reason group.
#' @param add_retraction_delay If `TRUE`, adds `retracted_within_year` and
#'   `retracted_within_two_years`. Requires parsed dates.
#' @param add_international If `TRUE`, adds the `international` flag.
#' @param add_mass_retraction If `TRUE`, adds the `Mass_Retraction` factor.
#'   Requires `publication_year` and `retraction_year`.
#' @return The cleaned data frame.
#' @export
clean_retraction_data <- function(retraction_data,
                                  oecd_countries = NULL,
                                  retraction_reason_conversion = NULL,
                                  long_df_by_countries = TRUE,
                                  clean_reasons = TRUE,
                                  parse_dates = TRUE,
                                  add_reason_groups = TRUE,
                                  add_retraction_delay = TRUE,
                                  add_international = TRUE,
                                  add_mass_retraction = TRUE) {
  if (clean_reasons) {
    retraction_data <- clean_reason_strings(retraction_data)
  }
  retraction_data <- add_number_of_countries(retraction_data)
  if (parse_dates) {
    retraction_data <- parse_retraction_dates(retraction_data)
  }
  if (add_reason_groups) {
    if (is.null(retraction_reason_conversion)) {
      stop("`retraction_reason_conversion` is required when `add_reason_groups = TRUE`.")
    }
    retraction_data <- add_reason_group_flags(retraction_data,
                                              retraction_reason_conversion)
  }
  if (add_retraction_delay) {
    retraction_data <- add_retraction_delay_flags(retraction_data)
  }
  # Note: this duplicates retractions for papers with more than one country
  if (long_df_by_countries) {
    if (is.null(oecd_countries)) {
      stop("`oecd_countries` is required when `long_df_by_countries = TRUE`.")
    }
    retraction_data <- split_by_country(retraction_data, oecd_countries)
  }
  if (add_international) {
    retraction_data <- add_international_flag(retraction_data)
  }
  if (add_mass_retraction) {
    retraction_data <- add_mass_retraction_factor(retraction_data)
  }
  if (parse_dates) {
    retraction_data$publication_year <- as.numeric(retraction_data$publication_year)
    retraction_data$retraction_year <- as.numeric(retraction_data$retraction_year)
  }
  retraction_data$RetractionID = retraction_data["Record ID"]
  retraction_data
}

# Stops with an informative error if `data` lacks any of `columns`
require_columns <- function(data, columns, step) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop(step, " requires columns: ", paste(missing, collapse = ", "),
         ". Enable the step that creates them (e.g. `parse_dates = TRUE`).")
  }
}

# Removes '+' characters from the reasons for retraction
clean_reason_strings <- function(retraction_data) {
  retraction_data$Reason <- gsub('+', '', retraction_data$Reason, fixed = TRUE)
  # Remove trailing semicolon
  retraction_data$Reason <- gsub(';$', ';', retraction_data$Reason)
  retraction_data
}

# Number of countries involved in each article
add_number_of_countries <- function(retraction_data) {
  retraction_data$number_of_countries <- lengths(strsplit(retraction_data$Country, ';'))
  retraction_data
}

# Parses the "m/d/Y 0:00" date columns and adds publication/retraction years
parse_retraction_dates <- function(retraction_data) {
  parse_date <- function(x) mdy(gsub(' 0:00', '', x))
  retraction_data$OriginalPaperDate <- parse_date(retraction_data$OriginalPaperDate)
  retraction_data$RetractionDate <- parse_date(retraction_data$RetractionDate)
  retraction_data %>%
    mutate(retraction_year = format(RetractionDate, "%Y"),
           publication_year = format(OriginalPaperDate, "%Y"))
}

# Adds one 0/1 column per reason group, set to 1 if any of the paper's
# reasons belongs to that group
add_reason_group_flags <- function(retraction_data, retraction_reason_conversion) {
  reasons_per_paper <- strsplit(retraction_data$Reason, ';')
  grouped_reasons <- sapply(unique(retraction_reason_conversion$Group), function(group) {
    group_reasons <- unique(retraction_reason_conversion$Reason[retraction_reason_conversion$Group == group])
    vapply(reasons_per_paper, function(reasons) any(reasons %in% group_reasons), logical(1))
  }, simplify = FALSE) %>%
    as_tibble() %>%
    mutate_all(as.numeric)
  bind_cols(retraction_data, grouped_reasons)
}

# Categorises the time between publication and retraction
add_retraction_delay_flags <- function(retraction_data) {
  if (!inherits(retraction_data$RetractionDate, "Date") ||
      !inherits(retraction_data$OriginalPaperDate, "Date")) {
    stop("add_retraction_delay requires parsed dates. Use `parse_dates = TRUE`.")
  }
  delay_years <- (retraction_data$RetractionDate - retraction_data$OriginalPaperDate) / 365.25
  retraction_data$retracted_within_year <- case_when(
    delay_years <= 1 ~ 'Within 1 year',
    delay_years > 1 ~ 'After 1 year')
  retraction_data$retracted_within_two_years <- case_when(
    delay_years <= 2 ~ 'Within 2 years',
    delay_years > 2 ~ 'After 2 years')
  retraction_data
}

# One row per paper and country, with OECD and UK flags
split_by_country <- function(retraction_data, oecd_countries) {
  retraction_data$paper_index <- seq_len(nrow(retraction_data))
  retraction_data <- retraction_data %>%
    mutate(row = row_number()) %>%
    separate_rows(Country, sep = ';')
  retraction_data$oecd <- case_when(
    retraction_data$Country %in% oecd_countries$Name ~ 'OECD',
    TRUE ~ 'Non-OECD')
  retraction_data$UK <- case_when(
    retraction_data$Country == "United Kingdom" ~ 'UK',
    TRUE ~ 'Non-UK')
  retraction_data
}

# TRUE if the paper involves more than one country. Only meaningful after
# `split_by_country()`, otherwise every paper has a single `Country` row
add_international_flag <- function(retraction_data) {
  retraction_data %>%
    group_by(`Record ID`) %>%
    mutate(international = if_any(Country, ~n_distinct(.) > 1)) %>%
    ungroup()
}

# Labels the IEEE (2009-2011) and Hindawi mass retractions
# MPN: Changed 2014 to 2024 for Hindawi mass retractions
add_mass_retraction_factor <- function(retraction_data) {
  require_columns(retraction_data, c("publication_year", "retraction_year"),
                  "add_mass_retraction")
  retraction_data$Mass_Retraction <- case_when(
    retraction_data$Publisher == 'IEEE: Institute of Electrical and Electronics Engineers' &
      (retraction_data$publication_year %in% c(2009, 2010, 2011) |
         retraction_data$retraction_year %in% c(2009, 2010, 2011)) ~ 'IEEE Mass Retraction',
    retraction_data$Publisher == 'Hindawi' &
      (retraction_data$retraction_year %in% c(2022, 2023, 2024)) ~ 'Hindawi Mass Retraction',
    TRUE ~ 'Other')
  retraction_data$Mass_Retraction <- factor(
    retraction_data$Mass_Retraction,
    levels = c('IEEE Mass Retraction', 'Hindawi Mass Retraction', 'Other'))
  retraction_data
}

#' Classify countries into UK, other OECD and rest of world
#'
#' @param country Character vector of country names.
#' @param oecd_names Character vector of OECD country names.
#' @return Character vector with values `"United Kingdom"`, `"Other OECD"`
#'   or `"Rest of World"`.
#' @export
classify_country <- function(country, oecd_names) {
  case_when(
    country == "United Kingdom" ~ "United Kingdom",
    country %in% oecd_names ~ "Other OECD",
    TRUE ~ "Rest of World"
  )
}

#' Add a `country_class` column based on the `Country` column
#'
#' @param data Data frame with a `Country` column.
#' @param oecd_countries Data frame with a `Name` column of OECD countries.
#' @return `data` with a `country_class` column (see [classify_country()]).
#' @export
add_country_class <- function(data, oecd_countries) {
  data %>%
    mutate(country_class = classify_country(.data$Country, oecd_countries$Name))
}
