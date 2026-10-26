test_that("count_by_scenario() counts international papers for each country class", {
  # Paper 1 is UK-only, paper 2 is China-UK (UK listed second), paper 3 is a
  # mass retraction from China
  data <- tibble::tibble(
    RetractionID = c(1, 2, 2, 3),
    country_class = c("United Kingdom", "Rest of World", "United Kingdom",
                      "Rest of World"),
    international = c(FALSE, TRUE, TRUE, FALSE),
    Mass_Retraction = c("Other", "Other", "Other", "Hindawi Mass Retraction")
  )

  result <- count_by_scenario(data, country_class, scenario) %>%
    dplyr::arrange(scenario, country_class)

  expect_equal(result$scenario, rep(default_scenarios()$scenario,
                                    times = c(2, 2, 2, 1)))
  expect_equal(result$country_class, c("Rest of World", "United Kingdom",
                                       "Rest of World", "United Kingdom",
                                       "Rest of World", "United Kingdom",
                                       "United Kingdom"))
  expect_equal(result$retractions, c(
    2, 2, # All: Rest of World (papers 2, 3), UK (papers 1, 2)
    1, 2, # All without mass retractions: Rest of World (2), UK (1, 2)
    1, 1, # Domestic: Rest of World (3), UK (1)
    1     # Domestic without mass retractions: UK (1)
  ))
})
