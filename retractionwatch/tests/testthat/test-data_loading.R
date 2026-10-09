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

test_that("load_retraction_data() returns cleaned data with a country class", {
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
  expect_equal(unique(data$country_class[data$Country == "China"]), "Rest of World")
})
