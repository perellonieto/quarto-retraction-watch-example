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

test_that("papers are split into one row per country with OECD and UK flags", {
  data <- clean_fixture()
  paper_2 <- data[data$`Record ID` == 2, ]

  expect_equal(nrow(data), 5)
  expect_setequal(paper_2$Country, c("United Kingdom", "China"))
  expect_equal(paper_2$oecd[paper_2$Country == "United Kingdom"], "OECD")
  expect_equal(paper_2$oecd[paper_2$Country == "China"], "Non-OECD")
  expect_equal(data$UK[data$Country == "France"], "Non-UK")
  expect_true(all(data$UK[data$Country == "United Kingdom"] == "UK"))
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

  data <- add_country_class(data.frame(Country = c("France", "Brazil")),
                            read_fixture_oecd())
  expect_equal(data$country_class, c("Other OECD", "Rest of World"))
})
