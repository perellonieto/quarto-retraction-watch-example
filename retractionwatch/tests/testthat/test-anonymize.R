test_that("anonymize_retraction_data() removes identifying columns and permutes IDs", {
  raw <- read_fixture_retractions()
  raw$`Record ID` <- c(5012, 77, 5012, 300)

  result <- anonymize_retraction_data(raw, seed = 1)

  expect_false(any(IDENTIFYING_COLUMNS %in% names(result)))
  expect_false("Journal" %in% names(result))
  expect_true(all(c("Publisher", "Country", "Reason", "RetractionDate") %in% names(result)))
  # A permutation of 1 to the number of papers; rows of a paper share one ID
  expect_setequal(unique(result$`Record ID`), 1:3)
  expect_equal(result$`Record ID`[1], result$`Record ID`[3])
  expect_false(any(result$`Record ID` %in% c(5012, 77, 300)))
})

test_that("the ID permutation is random unless a seed is given", {
  raw <- read_fixture_retractions()[rep(1:4, 25), ]
  raw$`Record ID` <- 1:100
  old_seed <- Sys.getenv("RETRACTIONWATCH_ID_SEED", NA)
  on.exit(if (is.na(old_seed)) Sys.unsetenv("RETRACTIONWATCH_ID_SEED")
          else Sys.setenv(RETRACTIONWATCH_ID_SEED = old_seed))

  # No seed: a different permutation every time, whatever set.seed() was used,
  # and R's random state is left as it was
  Sys.unsetenv("RETRACTIONWATCH_ID_SEED")
  set.seed(99)
  first <- anonymize_retraction_data(raw)$`Record ID`
  after_first <- stats::runif(1)
  set.seed(99)
  second <- anonymize_retraction_data(raw)$`Record ID`
  expect_equal(stats::runif(1), after_first)
  expect_false(identical(first, second))
  expect_setequal(first, 1:100)

  # A seed gives the same permutation, from the argument or the environment
  seeded <- anonymize_retraction_data(raw, seed = 7)$`Record ID`
  expect_equal(anonymize_retraction_data(raw, seed = 7)$`Record ID`, seeded)
  Sys.setenv(RETRACTIONWATCH_ID_SEED = "7")
  expect_equal(anonymize_retraction_data(raw)$`Record ID`, seeded)
})

test_that("anonymize_retraction_data() halts when identifying data remains", {
  raw <- read_fixture_retractions()

  with_doi <- raw
  with_doi$Subject[2] <- "See 10.1234/abcd.5678"
  expect_error(anonymize_retraction_data(with_doi),
               "`Subject` contains 1 value\\(s\\) that look like a DOI")

  with_url <- raw
  with_url$Reason[1] <- "Error in Data; see https://example.org"
  expect_error(anonymize_retraction_data(with_url), "look like a URL")

  with_email <- raw
  with_email$Subject[3] <- "contact author@example.ac.uk"
  expect_error(anonymize_retraction_data(with_email), "look like an email address")

  expect_error(anonymize_retraction_data(raw[setdiff(names(raw), "Record ID")]),
               "no `Record ID` column")
})

test_that("check_anonymized() halts if identifying columns or IDs remain", {
  raw <- read_fixture_retractions()
  expect_error(check_anonymized(raw, raw$`Record ID`), "identifying columns remain: Title")

  kept <- raw[setdiff(names(raw), IDENTIFYING_COLUMNS)]
  # The original IDs were kept
  expect_error(check_anonymized(kept, raw$`Record ID`), "not replaced consistently")
  # Two different papers got the same ID
  kept$`Record ID` <- c(4, 4, 2, 1)
  expect_error(check_anonymized(kept, raw$`Record ID`), "not replaced consistently")
})

test_that("coarsen_retraction_dates() keeps only the years", {
  raw <- read_fixture_retractions()
  from_text <- coarsen_retraction_dates(raw[c("Record ID", DATE_COLUMNS)])
  expect_equal(from_text$RetractionDate, c(2010L, 2018L, 2023L, 2020L))

  dates <- tibble::tibble(RetractionDate = as.Date(c("2020-12-31", NA)),
                          OriginalPaperDate = as.Date(c("2019-01-02", "2001-05-06")))
  coarse <- coarsen_retraction_dates(dates)
  expect_equal(coarse$RetractionDate, c(2020L, NA))
  expect_equal(coarse$OriginalPaperDate, c(2019L, 2001L))

  expect_error(check_coarsened(dates), "exact dates remain: RetractionDate, OriginalPaperDate")
  expect_error(check_coarsened(tibble::tibble(RetractionDate = 20201231)),
               "not coarsened to years")
  expect_error(check_coarsened(tibble::tibble(RetractionDate = 2020, paper_index = 1)),
               "row numbers of the raw data remain: paper_index")
})

test_that("load_retraction_data() returns anonymized data with years", {
  data <- load_retraction_data(
    fixture_path("retraction_watch.csv"),
    oecd_countries = read_fixture_oecd(),
    reason_groupings_path = fixture_path("retraction_reason_groupings.csv")
  )
  expect_false(any(IDENTIFYING_COLUMNS %in% names(data)))
  expect_type(data$RetractionDate, "integer")
  # The delay flags still come from the exact dates
  expect_true("retracted_within_year" %in% names(data))
  expect_equal(data$retraction_year, data$RetractionDate)
})

test_that("anonymize_retraction_data() never keeps all the original IDs", {
  # With two papers, half of the permutations would keep the original IDs
  data <- tibble::tibble(`Record ID` = 1:2, Reason = "R1;")
  for (seed in 1:20) {
    expect_false(identical(anonymize_retraction_data(data, seed = seed)$`Record ID`, 1:2))
  }
})
