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

test_that("plot_mean_reason_proportion_differences() averages the yearly differences", {
  # R1 differences: 2020 = 2/4 - 1/4, 2021 = 1/2 - 0; R2: 2020 = 1/4, 2021 = 0
  a <- tibble::tibble(`Record ID` = 1:6,
                      retraction_year = c(2020, 2020, 2020, 2020, 2021, 2021),
                      Reason = c("R1;", "R1;R2;", "R3;", "R3;", "R1;", "R3;"))
  b <- tibble::tibble(`Record ID` = 1:6,
                      retraction_year = c(2020, 2020, 2020, 2020, 2021, 2021),
                      Reason = c("R1;", "R3;", "R3;", "R3;", "R3;", "R3;"))

  p <- plot_mean_reason_proportion_differences(a, b, reasons_list = c("R1", "R2"),
                                               labels = c("A", "B"))

  expect_s3_class(p, "ggplot")
  data <- dplyr::arrange(p$data, Reason)
  expect_equal(data$Reason, c("R1", "R2"))
  expect_equal(data$n_years, c(2, 2))
  expect_equal(data$mean_difference, c(mean(c(1 / 4, 1 / 2)), mean(c(1 / 4, 0))))
  expect_equal(data$error, c(sd(c(1 / 4, 1 / 2)), sd(c(1 / 4, 0))) / sqrt(2))

  p_sd <- plot_mean_reason_proportion_differences(a, b, reasons_list = c("R1", "R2"),
                                                  error = "sd")
  expect_equal(dplyr::arrange(p_sd$data, Reason)$error,
               c(sd(c(1 / 4, 1 / 2)), sd(c(1 / 4, 0))))
})

test_that("plot_all_scenarios() combines the scenarios when ncol is given", {
  dat <- tibble::tibble(
    scenario = rep(c("S1", "S2", "S3"), times = c(3, 3, 2)),
    Country = c("United Kingdom", "A", "B", "United Kingdom", "A", "B", "A", "B"),
    ai_retraction_rate_per_100k = c(2, 1, 3, 1, 3, 2, 5, 4)
  )

  p <- plot_all_scenarios(dat, ncol = 2)
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
  layout <- ggplot2::ggplot_build(p)$layout$layout
  expect_equal(nrow(layout), 3)
  expect_equal(max(layout$COL), 2)
  # Countries are ordered by value within each scenario
  s2 <- dplyr::arrange(dplyr::filter(p$data, scenario == "S2"), Country)
  expect_equal(as.character(s2$label), paste0("  ", c("United Kingdom", "B", "A")))

  fig <- plot_all_scenarios(dat, ncol = 2, interactive = TRUE)
  expect_s3_class(fig, "plotly")
  titles <- vapply(plotly::plotly_build(fig)$x$layout$annotations,
                   function(a) a$text, character(1))
  expect_true(all(c("S1", "S2", "S3") %in% titles))

  # Without ncol, one plot per scenario as before
  expect_length(plot_all_scenarios(dat), 3)
  expect_equal(plot_scenario(dat, "S1")$labels$title, "S1")
})

test_that("plot_all_scenarios() skips all-NA scenarios and draws intervals and zeros", {
  dat <- tibble::tibble(
    scenario = rep(c("S1", "S2"), each = 2),
    Country = rep(c("United Kingdom", "A"), 2),
    ai_retraction_rate_per_100k = c(0, 2, NA, NA),
    ai_retraction_rate_per_100k_lower = c(0, 1, NA, NA),
    ai_retraction_rate_per_100k_upper = c(1, 4, NA, NA)
  )

  plots <- plot_all_scenarios(dat)
  expect_named(plots, "S1")
  layers <- vapply(plots$S1$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% layers)
  # The zero is kept and labels start after the interval
  expect_equal(nrow(plots$S1$data), 2)
  expect_equal(sort(plots$S1$data$label_x), c(1, 4))
  expect_s3_class(plot_all_scenarios(dat, interactive = TRUE), "shiny.tag.list")
})

test_that("plot_ai_ts() draws confidence bands when interval columns exist", {
  props <- tibble::tibble(publication_year = c(2020, 2021), country_class = "United Kingdom",
                          scenario = "All", prop_ai = c(0, 0.5),
                          prop_ai_lower = c(0, 0.1), prop_ai_upper = c(0.4, 0.9))

  p <- plot_ai_ts(props, "prop_ai", labels = scales::label_percent())
  layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRibbon" %in% layers)
  expect_false("GeomRibbon" %in% vapply(
    plot_ai_ts(props[c("publication_year", "country_class", "scenario", "prop_ai")], "prop_ai")$layers,
    function(l) class(l$geom)[1], character(1)))
})

test_that("plot_reasons_over_time_by_group() stacks one panel per group", {
  uk <- tibble::tibble(`Record ID` = 1:2, retraction_year = 2020, Reason = c("R1;", "R2;"))
  oecd <- tibble::tibble(`Record ID` = 1:3, retraction_year = 2020, Reason = c("R1;", "R1;", "R3;"))

  p <- plot_reasons_over_time_by_group(list(UK = uk, OECD = oecd), c("R1", "R2"))
  expect_equal(levels(p$data$group), c("UK", "OECD"))
  expect_equal(levels(p$data$Reason), c("R1", "R2"))
  oecd_r1 <- dplyr::filter(p$data, group == "OECD", Reason == "R1")
  expect_equal(oecd_r1$proportion, 2 / 3)
  expect_equal(ggplot2::ggplot_build(p)$layout$layout$ROW, c(1, 2))
})
