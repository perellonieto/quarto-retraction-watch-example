# Three papers: 1 UK-only AI retraction, 2 UK-France non-AI, 3 France-only
# non-AI published before the year window used below
ai_fixture <- function() {
  tibble::tibble(
    `Record ID` = c(1, 2, 2, 3),
    RetractionID = c(1, 2, 2, 3),
    Country = c("United Kingdom", "United Kingdom", "France", "France"),
    international = c(FALSE, TRUE, TRUE, FALSE),
    Mass_Retraction = "Other",
    Reason = c("Computer-Generated Content;", "Error in Data;", "Error in Data;",
               "Error in Data;"),
    publication_year = c(2020, 2020, 2020, 2010)
  )
}

sjr_fixture <- function() {
  tibble::tibble(Country = c("United Kingdom", "France", "United Kingdom", "France"),
                 Year = c(2010, 2010, 2020, 2020),
                 Documents = c(1e5, 1e5, 2e5, 4e5))
}

test_that("countries without AI retractions get a zero row in every scenario", {
  rates <- compute_ai_rates_by_country(ai_fixture(), sjr_fixture())

  expect_equal(nrow(rates), 2 * nrow(default_scenarios()))
  france_all <- dplyr::filter(rates, Country == "France", scenario == "All")
  expect_equal(france_all$ai_retractions, 0)
  expect_equal(france_all$retractions, 2)
  expect_equal(france_all$ai_retraction_rate_per_100k, 0)
  expect_equal(france_all$ai_retraction_rate_per_100k_lower, 0)
  expect_gt(france_all$ai_retraction_rate_per_100k_upper, 0)
})

test_that("rates are NA in domestic scenarios, shares are not", {
  rates <- compute_ai_rates_by_country(ai_fixture(), sjr_fixture())
  uk_domestic <- dplyr::filter(rates, Country == "United Kingdom", scenario == "Domestic")

  expect_true(is.na(uk_domestic$ai_retraction_rate_per_100k))
  expect_true(is.na(uk_domestic$ai_retraction_rate_per_100k_upper))
  # Only paper 1 is domestic in the UK, and it mentions AI
  expect_equal(uk_domestic$ai_retraction_share_pct, 100)
})

test_that("confidence intervals are exact Poisson and binomial intervals", {
  rates <- compute_ai_rates_by_country(ai_fixture(), sjr_fixture())
  uk_all <- dplyr::filter(rates, Country == "United Kingdom", scenario == "All")

  poisson <- stats::poisson.test(1)$conf.int
  expect_equal(c(uk_all$ai_retraction_rate_per_100k_lower,
                 uk_all$ai_retraction_rate_per_100k_upper),
               1e5 * as.numeric(poisson) / 3e5)
  binomial <- stats::binom.test(1, 2)$conf.int
  expect_equal(c(uk_all$ai_retraction_share_pct_lower,
                 uk_all$ai_retraction_share_pct_upper),
               100 * as.numeric(binomial))
})

test_that("publication_years restricts retractions and publications alike", {
  rates <- compute_ai_rates_by_country(ai_fixture(), sjr_fixture(),
                                       publication_years = c(2015, 2025))
  france_all <- dplyr::filter(rates, Country == "France", scenario == "All")
  uk_all <- dplyr::filter(rates, Country == "United Kingdom", scenario == "All")

  expect_equal(france_all$retractions, 1)
  expect_equal(france_all$total_papers, 4e5)
  expect_equal(uk_all$ai_retraction_rate_per_100k, 1e5 * 1 / 2e5)
})

# Two years of UK and Other OECD papers, plus one AI paper with no country
ts_fixture <- function() {
  tibble::tibble(
    `Record ID` = 1:6,
    RetractionID = 1:6,
    country_class = c("United Kingdom", "United Kingdom", "United Kingdom",
                      "Other OECD", "Other OECD", NA),
    international = FALSE,
    Mass_Retraction = "Other",
    Reason = c("Computer-Generated Content;", "Error in Data;", "Error in Data;",
               "Error in Data;", "Error in Data;", "Computer-Generated Content;"),
    publication_year = c(2020, 2020, 2021, 2020, 2021, 2020)
  )
}

test_that("AI time series drop papers without a country class", {
  counts <- compute_ai_counts_ts(ts_fixture())
  props <- compute_ai_proportion_ts(ts_fixture())

  expect_false(anyNA(counts$country_class))
  expect_false(anyNA(props$country_class))
})

test_that("AI proportions include zero years and exact binomial intervals", {
  props <- compute_ai_proportion_ts(ts_fixture()) %>%
    dplyr::filter(scenario == "All") %>%
    dplyr::arrange(country_class, publication_year)

  expect_equal(props$country_class, c("Other OECD", "Other OECD",
                                      "United Kingdom", "United Kingdom"))
  expect_equal(props$prop_ai, c(0, 0, 1 / 2, 0))
  uk_2020 <- props[3, ]
  expect_equal(c(uk_2020$prop_ai_lower, uk_2020$prop_ai_upper),
               as.numeric(stats::binom.test(1, 2)$conf.int))
  expect_equal(props$prop_ai_lower[1], 0)
})

test_that("ai_rates_table() formats one scenario for the selected countries", {
  rates <- compute_ai_rates_by_country(ai_fixture(), sjr_fixture())
  table <- ai_rates_table(rates, c("United Kingdom", "France"))

  expect_equal(table$Country, c("United Kingdom", "France"))
  expect_equal(table$`AI retractions`, c(1, 0))
  expect_match(table$`AI retractions per 100,000 papers (95% CI)`[2], "^0\\.00 \\(0\\.00–")
  expect_equal(ncol(table), 6)
})
