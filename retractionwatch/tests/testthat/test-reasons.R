# One retraction per element of `reasons`, all retracted in `year`
make_retractions <- function(reasons, year = 2020) {
  tibble::tibble(`Record ID` = seq_along(reasons),
                 retraction_year = year,
                 Reason = reasons)
}

# Top 2 reasons: R1 and R2 (3 retractions each), R3 has only 1
retractions_a <- make_retractions(c(rep("R1;R2;", 3), "R3;"))
# Top 2 reasons: R1 (3 retractions) and R4 (2), R2 has only 1
retractions_b <- make_retractions(c(rep("R1;", 3), rep("R4;", 2), "R2;"))

test_that("find_top_overlapping_reasons() splits top reasons into shared and per dataset", {
  result <- find_top_overlapping_reasons(retractions_a, retractions_b,
                                         n_top = 2, min_retractions_per_year = 1)

  expect_named(result, c("overlapping", "only_in_a", "only_in_b"))
  expect_equal(result$overlapping, "R1")
  expect_equal(result$only_in_a, "R2")
  expect_equal(result$only_in_b, "R4")
})

test_that("find_top_overlapping_reasons() uses the top reasons of every year", {
  a <- rbind(make_retractions(c("R1;", "R1;"), 2019),
             make_retractions(c("R2;", "R2;"), 2020))
  b <- make_retractions(c("R2;", "R2;", "R3;"), 2020)

  result <- find_top_overlapping_reasons(a, b, n_top = 1,
                                         min_retractions_per_year = 1)

  expect_equal(result$overlapping, "R2")
  expect_equal(result$only_in_a, "R1")
  expect_length(result$only_in_b, 0)
})

test_that("find_top_overlapping_reasons() applies year limits and exclusions", {
  a <- rbind(make_retractions(c("R1;", "R1;"), 2010),
             make_retractions(c("R2;", "R2;", "R5;"), 2020))
  b <- make_retractions(c("R2;", "R2;", "R5;"), 2020)

  result <- find_top_overlapping_reasons(a, b, n_top = 1,
                                         min_retractions_per_year = 1,
                                         year_limits = c(2015, 2025),
                                         exclude_reasons = "R2")

  expect_equal(result$overlapping, "R5")
  expect_length(result$only_in_a, 0)
  expect_length(result$only_in_b, 0)
})

test_that("years with too few retractions are ignored", {
  result <- find_top_overlapping_reasons(retractions_b, retractions_b,
                                         n_top = 2, min_retractions_per_year = 100)

  expect_length(result$overlapping, 0)
  expect_length(result$only_in_a, 0)
  expect_length(result$only_in_b, 0)
})

test_that("compute_top_reasons_over_time() gives proportions of the top reasons", {
  result <- compute_top_reasons_over_time(retractions_b, n_top = 2,
                                          min_retractions_per_year = 1)

  expect_equal(result$Reason, c("R1", "R4"))
  expect_equal(result$proportion, c(3, 2) / 6)
})

test_that("n_top = NULL keeps every reason", {
  result <- compute_top_reasons_over_time(retractions_b, n_top = NULL,
                                          min_retractions_per_year = 1)

  expect_setequal(result$Reason, c("R1", "R4", "R2"))
})

test_that("reasons_list selects reasons without changing their proportions", {
  all_reasons <- compute_top_reasons_over_time(retractions_b, n_top = NULL,
                                               min_retractions_per_year = 1)
  selected <- compute_top_reasons_over_time(retractions_b, n_top = NULL,
                                            min_retractions_per_year = 1,
                                            reasons_list = c("R2", "R4"))

  expect_setequal(selected$Reason, c("R2", "R4"))
  # Proportions are relative to all 6 retractions of the year
  expect_equal(selected$proportion[selected$Reason == "R4"], 2 / 6)
  expect_equal(selected$proportion[selected$Reason == "R2"],
               all_reasons$proportion[all_reasons$Reason == "R2"])
})

test_that("reasons_list is combined with n_top, and unseen reasons get 0", {
  top_and_listed <- compute_top_reasons_over_time(retractions_b, n_top = 2,
                                                  min_retractions_per_year = 1,
                                                  reasons_list = c("R2", "R4"))
  expect_equal(top_and_listed$Reason, "R4")

  unseen <- compute_top_reasons_over_time(retractions_b, n_top = NULL,
                                          min_retractions_per_year = 1,
                                          reasons_list = "R9")
  expect_equal(unseen$proportion, 0)
})

test_that("reasons_list can be given as a list", {
  from_vector <- compute_top_reasons_over_time(retractions_b, n_top = NULL,
                                               min_retractions_per_year = 1,
                                               reasons_list = c("R2", "R4"))
  from_list <- compute_top_reasons_over_time(retractions_b, n_top = NULL,
                                             min_retractions_per_year = 1,
                                             reasons_list = list("R2", "R4"))
  expect_equal(from_list, from_vector)
})

test_that("plot_retraction_reasons_over_time() plots only the listed reasons", {
  p <- plot_retraction_reasons_over_time(retractions_b, n_top = NULL,
                                         min_retractions_per_year = 1,
                                         reasons_list = c("R1", "R2"))

  expect_s3_class(p, "ggplot")
  expect_setequal(unique(p$data$Reason), c("R1", "R2"))
  expect_equal(p$labels$title, "Selected retraction reasons")
})

test_that("find_top_overlapping_reasons() with n_top = NULL compares all reasons", {
  result <- find_top_overlapping_reasons(retractions_a, retractions_b,
                                         n_top = NULL, min_retractions_per_year = 1)

  expect_equal(result$overlapping, c("R1", "R2"))
  expect_equal(result$only_in_a, "R3")
  expect_equal(result$only_in_b, "R4")
})

test_that("compare_reason_groups() keeps the original reason names in flag", {
  data <- tibble::tibble(
    RetractionID = 1:4,
    publication_year = c(2010, 2010, 2020, 2020),
    country_class = c("A", "B", "A", "B"),
    Reason = c("Error in Data;", "Error in Data;Removed;",
               "Concerns/Issues About Data;", "Removed;"),
    Mass_Retraction = "Other"
  )

  # prop.test() warns about the small counts of this toy example
  results <- suppressWarnings(
    data %>%
      reason_flags("country_class") %>%
      compare_reason_groups(group_a = "A", group_b = "B")
  )

  expect_setequal(unique(results$flag),
                  c("Error in Data", "Concerns/Issues About Data", "Removed"))
})

test_that("calculate_proportion_of_retractions_per_year() splits by country and year", {
  data <- rbind(
    cbind(make_retractions(c("R1;R2;", "R1;", "R3;"), 2020), Country = "A"),
    cbind(make_retractions(c("R1;", "R3;"), 2021), Country = "A"),
    cbind(make_retractions(c("R2;", "R2;"), 2020), Country = "B"),
    cbind(make_retractions("R1;", 2010), Country = "B"),
    cbind(make_retractions("R1;", 2020), Country = "C")
  )
  data$`Record ID` <- seq_len(nrow(data))

  result <- calculate_proportion_of_retractions_per_year(
    data, country_list = tibble::tibble(Country = c("A", "B")),
    reasons = c("R1", "R2"), yearmin = 2015, yearmax = 2021
  ) %>%
    dplyr::arrange(Country, year, Reason)

  expect_equal(result$Country, c("A", "A", "A", "A", "B", "B"))
  expect_equal(result$year, c(2020, 2020, 2021, 2021, 2020, 2020))
  expect_equal(result$Reason, c("R1", "R2", "R1", "R2", "R1", "R2"))
  expect_equal(result$proportion, c(2 / 3, 1 / 3, 1 / 2, 0, 0, 1))
})

test_that("calculate_proportion_of_retractions_per_year_by_publisher() splits by publisher and year", {
  data <- rbind(
    cbind(make_retractions(c("R1;R2;", "R1;", "R3;"), 2020), Publisher = "P1"),
    cbind(make_retractions(c("R1;", "R3;"), 2021), Publisher = "P1"),
    cbind(make_retractions(c("R2;", "R2;"), 2020), Publisher = "P2"),
    cbind(make_retractions("R1;", 2010), Publisher = "P2"),
    cbind(make_retractions("R1;", 2020), Publisher = "P3")
  )
  data$`Record ID` <- seq_len(nrow(data))

  result <- calculate_proportion_of_retractions_per_year_by_publisher(
    data, publisher_list = tibble::tibble(Publisher = c("P1", "P2")),
    reasons = c("R1", "R2"), yearmin = 2015, yearmax = 2021
  ) %>%
    dplyr::arrange(Publisher, year, Reason)

  expect_equal(result$Publisher, c("P1", "P1", "P1", "P1", "P2", "P2"))
  expect_equal(result$year, c(2020, 2020, 2021, 2021, 2020, 2020))
  expect_equal(result$Reason, c("R1", "R2", "R1", "R2", "R1", "R2"))
  expect_equal(result$proportion, c(2 / 3, 1 / 3, 1 / 2, 0, 0, 1))
})

test_that("count_number_of_retractions_per_column() counts distinct papers per value", {
  # Paper 1 has two author countries, so it appears twice
  data <- tibble::tibble(`Record ID` = c(1, 1, 2, 3, 4),
                         Country = c("A", "B", "A", "A", "B"),
                         Publisher = c("P1", "P1", "P2", "P2", "P2"))

  result <- count_number_of_retractions_per_column(data, "Publisher")

  expect_equal(names(result), c("Publisher", "retractions"))
  expect_equal(result$Publisher, c("P2", "P1"))
  expect_equal(result$retractions, c(3, 1))
})

test_that("count_number_of_retractions_per_column() can count only some years", {
  data <- tibble::tibble(`Record ID` = c(1, 1, 2, 3, 4, 5),
                         Publisher = c("P1", "P1", "P1", "P2", "P2", "P2"),
                         retraction_year = c(2015, 2015, 2018, 2018, 2020, NA),
                         publication_year = c(2010, 2010, 2016, 2017, 2019, 2019))

  # All years: P2 has 3 papers, P1 has 2
  expect_equal(count_number_of_retractions_per_column(data, "Publisher")$retractions,
               c(3, 2))

  # 2017-2020 leaves out paper 1 (2015) and paper 5 (no year)
  result <- count_number_of_retractions_per_column(data, "Publisher",
                                                   year_limits = c(2017, 2020))
  expect_equal(result$Publisher, c("P2", "P1"))
  expect_equal(result$retractions, c(2, 1))

  by_publication <- count_number_of_retractions_per_column(
    data, "Publisher", year_limits = c(2016, 2019), year_col = "publication_year"
  )
  expect_equal(by_publication$Publisher, c("P2", "P1"))
  expect_equal(by_publication$retractions, c(3, 1))
})

test_that("select_top_retraction_reasons() ranks reasons over the whole period", {
  data <- rbind(
    make_retractions(c("R1;", "R1;", "R1;"), 2019),
    make_retractions(c("R2;R3;", "R2;", "R3;"), 2020),
    make_retractions(c("R2;", "R4;"), 2021)
  )
  data$`Record ID` <- seq_len(nrow(data))

  # 2020-2021: R2 in 3/5 retractions, R3 in 2/5 and R4 in 1/5
  expect_equal(select_top_retraction_reasons(data, n_top = 2,
                                             year_limits = c(2020, 2021)),
               c("R2", "R3"))
  expect_equal(select_top_retraction_reasons(data, n_top = 10,
                                             year_limits = c(2020, 2021)),
               c("R2", "R3", "R4"))
  # All years: R1 and R2 tie at 3/8, ahead of R3 at 2/8
  expect_equal(select_top_retraction_reasons(data, n_top = 3),
               c("R1", "R2", "R3"))
  expect_equal(select_top_retraction_reasons(data, n_top = 1,
                                             exclude_reasons = "R1"),
               "R2")
})

test_that("find_reasons_not_reported_in_year() lists the reasons absent that year", {
  data <- rbind(
    make_retractions(c("R1;R2;", "R3;"), 2019),
    make_retractions(c("R1;", "R2;"), 2020),
    make_retractions("R4;", 2021)
  )
  data$`Record ID` <- seq_len(nrow(data))

  expect_equal(find_reasons_not_reported_in_year(data, 2020), c("R3", "R4"))
  expect_equal(find_reasons_not_reported_in_year(data, 2021), c("R1", "R2", "R3"))
  expect_equal(find_reasons_not_reported_in_year(data, 2020, exclude_reasons = "R4"), "R3")
  # A year without retractions lacks every reason
  expect_equal(find_reasons_not_reported_in_year(data, 2030), c("R1", "R2", "R3", "R4"))
})

test_that("one_hot_encoding_reasons_and_country_type() has one row per paper", {
  # Paper 1 has two country rows; paper 3 has no reason
  data <- tibble::tibble(
    `Record ID` = c(1, 1, 2, 3),
    Country = c("United Kingdom", "France", "France", "China"),
    country_class = c("United Kingdom", "United Kingdom", "Other OECD", "Rest of World"),
    Reason = c("Paper Mill;Error in Data;", "Paper Mill;Error in Data;", "Error in Data;", "")
  )

  result <- one_hot_encoding_reasons_and_country_type(data)

  expect_equal(names(result), c("Record ID", "country_class", "error_in_data", "paper_mill"))
  expect_equal(result$`Record ID`, c(1, 2, 3))
  expect_equal(result$country_class, c("United Kingdom", "Other OECD", "Rest of World"))
  expect_equal(result$error_in_data, c(1L, 1L, 0L))
  expect_equal(result$paper_mill, c(1L, 0L, 0L))
  expect_equal(attr(result, "reasons")[["paper_mill"]], "Paper Mill")

  expect_error(one_hot_encoding_reasons_and_country_type(data, country_col = "Country"),
               "several values")
})
