test_that("plot_reason_proportion_differences() plots proportion_a - proportion_b", {
  # 2020: R1 in 2/4 of `a` and 1/4 of `b`; 2021 only exists in `a`
  a <- tibble::tibble(`Record ID` = 1:5,
                      retraction_year = c(2020, 2020, 2020, 2020, 2021),
                      Reason = c("R1;", "R1;R2;", "R2;", "R3;", "R1;"))
  b <- tibble::tibble(`Record ID` = 1:4,
                      retraction_year = 2020,
                      Reason = c("R1;", "R3;", "R3;", "R3;"))

  p <- plot_reason_proportion_differences(a, b, reasons_list = c("R1", "R2"),
                                          labels = c("A", "B"))

  expect_s3_class(p, "ggplot")
  data <- dplyr::arrange(p$data, Reason)
  expect_equal(data$year, c(2020, 2020))
  expect_equal(data$Reason, c("R1", "R2"))
  expect_equal(data$difference, c(2 / 4 - 1 / 4, 2 / 4 - 0))
  expect_equal(p$labels$y, "Proportion difference (A - B)")
})
