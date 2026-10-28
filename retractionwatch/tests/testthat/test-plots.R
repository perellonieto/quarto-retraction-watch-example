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

test_that("plot_reason_proportions_forest() summarises each dataset's yearly proportions", {
  # Yearly proportions (2020, 2021): A has R1 = (2/4, 1/2) and R2 = (1/4, 0);
  # B has R1 = (1/4, 0) and R2 = (0, 0)
  a <- tibble::tibble(`Record ID` = 1:6,
                      retraction_year = c(2020, 2020, 2020, 2020, 2021, 2021),
                      Reason = c("R1;", "R1;R2;", "R3;", "R3;", "R1;", "R3;"))
  b <- tibble::tibble(`Record ID` = 1:6,
                      retraction_year = c(2020, 2020, 2020, 2020, 2021, 2021),
                      Reason = c("R1;", "R3;", "R3;", "R3;", "R3;", "R3;"))

  p <- plot_reason_proportions_forest(a, b, reasons_list = c("R1", "R2"),
                                      labels = c("A", "B"))

  expect_s3_class(p, "ggplot")
  data <- dplyr::arrange(p$data, Reason, group)
  expect_equal(data$Reason, c("R1", "R1", "R2", "R2"))
  expect_equal(as.character(data$group), c("A", "B", "A", "B"))
  expect_equal(data$n_years, c(2, 2, 2, 2))
  expect_equal(data$mean_proportion, c(1 / 2, 1 / 8, 1 / 8, 0))
  expect_equal(data$q25, c(1 / 2, 1 / 16, 1 / 16, 0))
  expect_equal(data$q75, c(1 / 2, 3 / 16, 3 / 16, 0))
  # Reasons sorted by their mean in A (R2 < R1), with A just above B
  expect_equal(data$y, c(2.15, 1.85, 1.15, 0.85))
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

test_that("ggplotly_free_space() sizes the panel rows like space = 'free_y'", {
  # 6 reasons in "Post" and 2 in "Pre"
  data <- tibble::tibble(
    flag = c(paste0("r", 1:6), "r1", "r2"),
    period = rep(c("Post", "Pre"), times = c(6, 2)),
    proportion = seq(0.1, 0.8, by = 0.1),
    group = "A"
  )
  p <- ggplot2::ggplot(data, ggplot2::aes(proportion, flag)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(period ~ ., scales = "free_y", space = "free_y")

  layout <- ggplotly_free_space(p)$x$layout

  top <- diff(layout$yaxis$domain)
  bottom <- diff(layout$yaxis2$domain)
  # A discrete y range spans the categories (n - 1) plus 0.6 on each side
  expect_equal(top / bottom, (6 + 0.2) / (2 + 0.2))
  # The strip boxes follow the panels
  expect_equal(c(layout$shapes[[2]]$y0, layout$shapes[[2]]$y1),
               layout$yaxis2$domain)
})

test_that("plot_reason_proportions_across_groups_by_year() summarises across publishers or countries", {
  proportions <- tibble::tibble(
    Publisher = rep(c("P1", "P2", "P3"), each = 2),
    year = rep(c(2020, 2021), times = 3),
    Reason = "R1",
    proportion = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  )

  p <- plot_reason_proportions_across_groups_by_year(proportions)

  expect_s3_class(p, "ggplot")
  data <- dplyr::arrange(p$data, year)
  expect_equal(data$mean_proportion, c(0.3, 0.4))
  expect_equal(data$q25, c(0.2, 0.3))
  expect_equal(data$q75, c(0.4, 0.5))
  expect_equal(data$n_groups, c(3, 3))
  expect_match(p$labels$title, "across publishers$")

  by_country <- dplyr::rename(proportions, Country = Publisher)
  expect_match(plot_reason_proportions_across_groups_by_year(by_country)$labels$title,
               "across countries$")
  expect_error(plot_reason_proportions_across_groups_by_year(proportions[-1]),
               "Publisher` or `Country")
})

test_that("plot_reason_proportions_across_groups_by_year() gives each band its line's legend entry", {
  proportions <- tibble::tibble(
    Publisher = rep(c("P1", "P2"), each = 4),
    year = rep(c(2020, 2020, 2021, 2021), times = 2),
    Reason = rep(c("R1", "R2"), times = 4),
    proportion = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
  )

  widget <- plot_reason_proportions_across_groups_by_year(proportions, interactive = TRUE)

  traces <- widget$x$data
  names <- vapply(traces, function(t) t$name, character(1))
  in_legend <- vapply(traces, function(t) !isFALSE(t$showlegend), logical(1))
  expect_setequal(names, c("R1", "R2"))
  expect_equal(sort(names[in_legend]), c("R1", "R2"))
  expect_equal(vapply(traces, function(t) t$legendgroup, character(1)), names)
})

test_that("plot_number_of_total_retractions_per_year() counts distinct papers per year", {
  # Paper 1 has two countries, so it has two rows; paper 4 has no year
  data <- tibble::tibble(
    `Record ID` = c(1, 1, 2, 3, 4, 5),
    Country = c("A", "B", "A", "A", "A", "B"),
    retraction_year = c(2019, 2019, 2019, 2020, NA, 2021),
    publication_year = c(2018, 2018, 2018, 2018, 2020, 2020)
  )

  p <- plot_number_of_total_retractions_per_year(data)
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$year, c(2019, 2020, 2021))
  expect_equal(p$data$retractions, c(2, 1, 1))
  expect_equal(p$labels$x, "Retraction year")

  limited <- plot_number_of_total_retractions_per_year(data, year_limits = c(2020, 2021))
  expect_equal(limited$data$year, c(2020, 2021))

  by_publication <- plot_number_of_total_retractions_per_year(data, year_col = "publication_year")
  expect_equal(by_publication$data$retractions, c(3, 2))
  expect_equal(by_publication$labels$x, "Publication year")
})

test_that("plot_number_of_distinct_reasons_per_year() counts each reason once per year", {
  # 2019: R1, R2 (R1 cited twice, paper 1 has two country rows); 2020: R1, R2, R3
  data <- tibble::tibble(
    `Record ID` = c(1, 1, 2, 3, 4),
    Country = c("A", "B", "A", "A", "B"),
    retraction_year = c(2019, 2019, 2019, 2020, 2020),
    Reason = c("R1;R2;", "R1;R2;", "R1;", "R1;R2;", "R3;")
  )

  p <- plot_number_of_distinct_reasons_per_year(data)
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$year, c(2019, 2020))
  expect_equal(p$data$reasons, c(2, 3))

  expect_equal(plot_number_of_distinct_reasons_per_year(data, year_limits = c(2020, 2020))$data$reasons, 3)
  expect_equal(plot_number_of_distinct_reasons_per_year(data, exclude_reasons = "R1")$data$reasons, c(1, 2))
})

test_that("plot_first_year_occurrence_per_reason() finds each reason's first year", {
  # R1 cited in 1975, 1990 and 2000; R2 in 1990 (twice); R3 in 2000 and 2010
  data <- tibble::tibble(
    `Record ID` = 1:5,
    retraction_year = c(1975, 1990, 1990, 2000, 2010),
    Reason = c("R1;", "R2;", "R1;R2;", "R3;R1;", "R3;")
  )

  p <- plot_first_year_occurrence_per_reason(data)
  expect_s3_class(p, "ggplot")
  data_shown <- dplyr::arrange(p$data, first_year)
  expect_equal(as.character(data_shown$Reason), c("R1", "R2", "R3"))
  expect_equal(data_shown$first_year, c(1975, 1990, 2000))
  expect_equal(data_shown$n, c(1, 2, 1))
  expect_equal(data_shown$last_year, c(2000, 1990, 2010))
  expect_equal(data_shown$last_n, c(1, 2, 1))
  expect_equal(data_shown$line_end, data_shown$last_year)
  expect_length(p$layers, 3)
  # Oldest reason at the top of the y axis
  expect_equal(levels(p$data$Reason), c("R3", "R2", "R1"))

  # The first year is computed on all the data, not only within the limits
  limited <- plot_first_year_occurrence_per_reason(data, year_limits = c(1980, 2005))
  expect_equal(as.character(dplyr::arrange(limited$data, first_year)$Reason),
               c("R2", "R3"))
  # R3 is last cited in 2010, after the period, so its last year is 2000
  expect_equal(dplyr::arrange(limited$data, first_year)$last_year, c(1990, 2000))

  # Without show_last, every line runs to the end of the period
  to_end <- plot_first_year_occurrence_per_reason(data, year_limits = c(1980, 2005),
                                                  show_last = FALSE)
  expect_equal(unique(to_end$data$line_end), 2005)
  expect_length(to_end$layers, 2)

  # reason_list keeps only the listed reasons
  listed <- plot_first_year_occurrence_per_reason(data, reason_list = list("R1", "R3"))
  expect_setequal(as.character(listed$data$Reason), c("R1", "R3"))
  expect_match(limited$labels$title, "1 reason first cited before 1980 not shown")
})
