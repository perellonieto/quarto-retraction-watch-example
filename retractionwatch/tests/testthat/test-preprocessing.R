clean_fixture <- function(...) {
  clean_retraction_data(read_fixture_retractions(),
                        oecd_countries = read_fixture_oecd(),
                        retraction_reason_conversion = read_fixture_reason_groups(),
                        ...)
}

# One row per paper, ordered by Record ID
by_paper <- function(data) {
  data[!duplicated(data$`Record ID`), ][order(unique(data$`Record ID`)), ]
}

test_that("reasons are cleaned of '+' characters", {
  data <- clean_fixture()

  expect_false(any(grepl("+", data$Reason, fixed = TRUE)))
  expect_equal(by_paper(data)$Reason[1], "Plagiarism of Text;")
})

test_that("the number of countries is counted per paper", {
  expect_equal(by_paper(clean_fixture())$number_of_countries, c(1, 2, 1, 1))
})

test_that("dates are parsed and years added as numbers", {
  papers <- by_paper(clean_fixture())

  expect_s3_class(papers$RetractionDate, "Date")
  expect_s3_class(papers$OriginalPaperDate, "Date")
  expect_equal(papers$RetractionDate[1], as.Date("2010-06-01"))
  expect_equal(papers$publication_year, c(2010, 2015, 2021, 2019))
  expect_equal(papers$retraction_year, c(2010, 2018, 2023, 2020))
})

test_that("one 0/1 column is added per reason group", {
  papers <- by_paper(clean_fixture())

  expect_equal(papers$Mechanism, c(0, 1, 1, 0))
  expect_equal(papers$Plagiarism, c(1, 0, 0, 0))
  expect_equal(papers$Data, c(0, 1, 0, 1))
})

test_that("ReasonGroups lists the groups of each paper's reasons", {
  papers <- by_paper(clean_fixture())

  # Groups follow the order of the groupings table: Mechanism, Plagiarism, Data
  expect_equal(papers$ReasonGroups,
               c("Plagiarism", "Mechanism;Data", "Mechanism", "Data"))
})

test_that("ReasonGroups is NA when no reason belongs to a group", {
  raw <- read_fixture_retractions()
  raw$Reason[1] <- "+Unlisted Reason;"

  data <- clean_retraction_data(raw,
                                retraction_reason_conversion = read_fixture_reason_groups(),
                                long_df_by_countries = FALSE)

  expect_true(is.na(data$ReasonGroups[1]))
  expect_equal(data$ReasonGroups[2], "Mechanism;Data")
})

test_that("retraction delay is categorised", {
  papers <- by_paper(clean_fixture())

  expect_equal(papers$retracted_within_year,
               c("Within 1 year", "After 1 year", "After 1 year", "After 1 year"))
  expect_equal(papers$retracted_within_two_years,
               c("Within 2 years", "After 2 years", "After 2 years", "Within 2 years"))
})

test_that("papers are split into one row per country with a per-paper class", {
  data <- clean_fixture()
  paper_2 <- data[data$`Record ID` == 2, ]

  expect_equal(nrow(data), 5)
  expect_setequal(paper_2$Country, c("United Kingdom", "China"))
  # Both rows of the UK-China paper are in the UK class
  expect_equal(paper_2$country_class, c("United Kingdom", "United Kingdom"))
  expect_equal(by_paper(data)$country_class,
               c("United Kingdom", "United Kingdom", "Rest of World", "Other OECD"))
})

test_that("country classes follow UK > Other OECD > Rest of World per paper", {
  raw <- read_fixture_retractions()
  raw$Country <- c("France;United Kingdom", "China;France", "China;Brazil",
                   "South Korea;Slovakia")
  papers <- by_paper(clean_retraction_data(
    raw, oecd_countries = read_fixture_oecd(),
    retraction_reason_conversion = read_fixture_reason_groups()))

  expect_equal(papers$country_class,
               c("United Kingdom", "Other OECD", "Rest of World", "Other OECD"))
})

test_that("unknown countries are treated as missing", {
  raw <- read_fixture_retractions()
  raw$Country[1] <- "Unknown"
  raw$Country[2] <- "Unknown;China"
  data <- clean_retraction_data(raw, oecd_countries = read_fixture_oecd(),
                                retraction_reason_conversion = read_fixture_reason_groups())

  # A paper with no known country keeps one row, with no country or class
  expect_equal(nrow(data[data$`Record ID` == 1, ]), 1)
  expect_true(is.na(data$Country[data$`Record ID` == 1]))
  expect_true(is.na(data$country_class[data$`Record ID` == 1]))
  # Unknown is dropped from papers that also list a known country
  expect_equal(data$Country[data$`Record ID` == 2], "China")
  expect_equal(data$country_class[data$`Record ID` == 2], "Rest of World")
  expect_false(data$international[data$`Record ID` == 2])
  expect_equal(by_paper(data)$number_of_countries, c(0, 1, 1, 1))
})

test_that("retractions_only keeps only retractions", {
  raw <- read_fixture_retractions()
  raw$RetractionNature[2:3] <- c("Correction", "Expression of concern")

  kept <- clean_fixture()
  dropped <- clean_retraction_data(raw, oecd_countries = read_fixture_oecd(),
                                   retraction_reason_conversion = read_fixture_reason_groups(),
                                   retractions_only = TRUE)

  expect_setequal(unique(kept$`Record ID`), 1:4)
  expect_setequal(unique(dropped$`Record ID`), c(1, 4))
})

test_that("publication_years keeps papers published in that range", {
  data <- clean_fixture(publication_years = c(2015, 2020))

  expect_setequal(unique(data$`Record ID`), c(2, 4))
})

test_that("only papers with several countries are international", {
  data <- clean_fixture()

  expect_true(all(data$international[data$`Record ID` == 2]))
  expect_false(any(data$international[data$`Record ID` != 2]))
})

test_that("IEEE and Hindawi mass retractions are labelled", {
  papers <- by_paper(clean_fixture())

  expect_s3_class(papers$Mass_Retraction, "factor")
  expect_equal(levels(papers$Mass_Retraction),
               c("IEEE Mass Retraction", "Hindawi Mass Retraction", "Other"))
  expect_equal(as.character(papers$Mass_Retraction),
               c("IEEE Mass Retraction", "Other", "Hindawi Mass Retraction", "Other"))
})

test_that("without splitting by country there is one row per paper", {
  data <- clean_fixture(long_df_by_countries = FALSE)

  expect_equal(nrow(data), 4)
  expect_false(any(c("oecd", "UK", "paper_index") %in% names(data)))
  expect_equal(data$Country[2], "United Kingdom;China")
})

test_that("disabled steps do not add their columns", {
  data <- clean_retraction_data(read_fixture_retractions(),
                                long_df_by_countries = FALSE,
                                add_reason_groups = FALSE,
                                add_retraction_delay = FALSE,
                                add_international = FALSE,
                                add_mass_retraction = FALSE)

  expect_false(any(c("Mechanism", "retracted_within_year", "international",
                     "Mass_Retraction") %in% names(data)))
  expect_true(all(c("number_of_countries", "retraction_year") %in% names(data)))
})

test_that("reasons are left untouched when clean_reasons = FALSE", {
  data <- clean_fixture(clean_reasons = FALSE, long_df_by_countries = FALSE)

  expect_equal(data$Reason[1], "+Plagiarism of Text;")
})

test_that("missing inputs and unmet step dependencies give clear errors", {
  raw <- read_fixture_retractions()

  expect_error(clean_retraction_data(raw, oecd_countries = read_fixture_oecd()),
               "retraction_reason_conversion")
  expect_error(clean_retraction_data(raw, add_reason_groups = FALSE),
               "oecd_countries")
  expect_error(clean_retraction_data(raw, long_df_by_countries = FALSE,
                                     add_reason_groups = FALSE,
                                     parse_dates = FALSE),
               "parse_dates")
  expect_error(clean_retraction_data(raw, long_df_by_countries = FALSE,
                                     add_reason_groups = FALSE,
                                     add_retraction_delay = FALSE,
                                     parse_dates = FALSE),
               "add_mass_retraction")
})

test_that("classify_country() and add_country_class() assign country classes", {
  expect_equal(classify_country(c("United Kingdom", "France", "China"),
                                c("United Kingdom", "France")),
               c("United Kingdom", "Other OECD", "Rest of World"))
  expect_true(is.na(classify_country(NA_character_, "France")))

  data <- add_country_class(data.frame(Country = c("France", "Brazil")),
                            read_fixture_oecd())
  expect_equal(data$country_class, c("Other OECD", "Rest of World"))
})
