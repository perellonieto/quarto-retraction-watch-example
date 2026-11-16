test_that("build_sjr_country_counts() combines the yearly files with a Year column", {
  output_csv <- withr::local_tempfile(fileext = ".csv")

  counts <- build_sjr_country_counts(fixture_path("sjr_rankings"), output_csv)

  expect_true(file.exists(output_csv))
  expect_setequal(unique(counts$Year), c(1996, 1997))
  expect_true(all(c("Country", "Documents", "Year") %in% names(counts)))
  expect_equal(
    nrow(counts),
    sum(vapply(list.files(fixture_path("sjr_rankings"), full.names = TRUE),
               function(f) nrow(readxl::read_excel(f)), integer(1)))
  )
})

test_that("build_sjr_country_counts() ignores files that are not SJR rankings", {
  dir <- withr::local_tempdir()
  file.copy(list.files(fixture_path("sjr_rankings"), full.names = TRUE), dir)
  writeLines("not a ranking", file.path(dir, "notes.txt"))

  counts <- build_sjr_country_counts(dir, file.path(dir, "all_years.csv"))

  expect_setequal(unique(counts$Year), c(1996, 1997))
})

test_that("load_oecd_countries() recodes names to match Retraction Watch", {
  oecd <- read_fixture_oecd()

  expect_true(all(c("South Korea", "Slovakia") %in% oecd$Name))
  expect_false(any(c("Korea", "Slovak Republic") %in% oecd$Name))
  expect_equal(nrow(oecd), 4)
})

test_that("RETRACTION_DATA_TYPES reads Record ID as integer and dates as text", {
  raw <- read_fixture_retractions()

  expect_type(raw$`Record ID`, "integer")
  expect_type(raw$RetractionDate, "character")
  expect_type(raw$OriginalPaperDate, "character")
})

test_that("load_retraction_data() returns cleaned data with a paper country class", {
  data <- load_retraction_data(
    fixture_path("retraction_watch.csv"),
    oecd_countries = read_fixture_oecd(),
    reason_groupings_path = fixture_path("retraction_reason_groupings.csv")
  )

  # Paper 2 has two countries, so it is split into two rows
  expect_equal(nrow(data), 5)
  expect_setequal(unique(data$`Record ID`), 1:4)
  expect_equal(
    unique(data$country_class[data$Country == "United Kingdom"]), "United Kingdom"
  )
  expect_equal(unique(data$country_class[data$Country == "France"]), "Other OECD")
  # China appears alone (Rest of World) and with the UK (United Kingdom)
  expect_setequal(data$country_class[data$Country == "China"],
                  c("United Kingdom", "Rest of World"))
})

test_that("SCImago country names are recoded to Retraction Watch names", {
  counts <- recode_sjr_countries(
    data.frame(Country = c("Russian Federation", "Viet Nam", "France")))

  expect_equal(counts$Country, c("Russia", "Vietnam", "France"))
})

test_that("describe_retraction_data() summarises the raw file without identifying papers", {
  raw <- tibble::tibble(
    `Record ID` = c(1L, 2L, 2L, 3L),
    Title = c("Secret title A", "Secret title B", "Secret title B", "Secret title C"),
    OriginalPaperDate = c("1/15/2001 0:00", "3/2/2010 0:00", "3/2/2010 0:00", "not a date"),
    RetractionDate = c("6/30/2005 0:00", "12/31/2024 0:00", "12/31/2024 0:00", "2/1/2020 0:00"),
    RetractionNature = c("Retraction", "Retraction", "Retraction", "Correction")
  )

  output <- capture.output(info <- describe_retraction_data(raw))
  expect_equal(info$rows, 4)
  expect_equal(info$columns, 5)
  expect_equal(info$records, 3)
  expect_equal(info$rows_without_id, 0)
  expect_equal(info$publication_dates$first, as.Date("2001-01-15"))
  expect_equal(info$publication_dates$last, as.Date("2010-03-02"))
  expect_equal(info$publication_dates$missing, 1)
  expect_equal(info$retraction_dates$last, as.Date("2024-12-31"))
  expect_equal(as.integer(info$nature[["Retraction"]]), 3L)
  expect_true(any(grepl("Retraction 3; Correction 1", output)))
  expect_false(any(grepl("Secret", output)))

  # With a file, its checksum is printed
  path <- tempfile(fileext = ".csv")
  readr::write_csv(raw, path)
  output <- capture.output(info <- describe_retraction_data(raw, path))
  expect_equal(info$md5, unname(tools::md5sum(path)))
  expect_true(any(grepl(info$md5, output, fixed = TRUE)))
})

test_that("load_retraction_data() prints the description only when verbose", {
  load <- function(verbose) {
    load_retraction_data(fixture_path("retraction_watch.csv"),
                         oecd_countries = read_fixture_oecd(),
                         reason_groupings_path = fixture_path("retraction_reason_groupings.csv"),
                         verbose = verbose)
  }
  expect_output(load(TRUE), "MD5 checksum")
  expect_output(suppressMessages(load(FALSE)), NA)
})

test_that("describe_cleaned_retraction_data() summarises the cleaned data", {
  cleaned <- tibble::tibble(
    `Record ID` = c(1L, 2L, 2L, 3L),
    Country = c("A", "A", "B", NA),
    publication_year = c(2001, 2010, 2010, NA),
    retraction_year = c(2005, 2024, 2024, 2020),
    RetractionNature = c("Retraction", "Retraction", "Retraction", "Correction")
  )

  output <- capture.output(info <- describe_cleaned_retraction_data(cleaned))
  expect_equal(info$rows, 4)
  expect_equal(info$columns, 5)
  expect_equal(info$records, 3)
  expect_equal(unlist(info$publication_years), c(first = 2001, last = 2010, missing = 1))
  expect_equal(unlist(info$retraction_years), c(first = 2005, last = 2024, missing = 0))
  # Paper 2 has two rows but counts once
  expect_equal(as.integer(info$nature[["Retraction"]]), 2L)
  expect_true(any(grepl("Retraction 2; Correction 1", output)))
})
