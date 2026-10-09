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
