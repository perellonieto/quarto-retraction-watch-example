# 50 UK, 100 OECD and 20 Rest of World papers; reason_a is more common in
# the UK and reason_c is never cited
make_encoded <- function() {
  set.seed(1)
  country_class <- rep(c("United Kingdom", "Other OECD", "Rest of World"),
                       times = c(50, 100, 20))
  tibble::tibble(
    `Record ID` = seq_along(country_class),
    country_class = country_class,
    reason_a = as.integer(stats::runif(170) < ifelse(country_class == "United Kingdom", 0.7, 0.2)),
    reason_b = as.integer(stats::runif(170) < 0.5),
    reason_c = 0L
  )
}

test_that("divide_by() keeps the proportion of each target in both sets", {
  data <- make_encoded()

  split <- divide_by(data, column = "country_class",
                     targets = c("United Kingdom", "Other OECD"),
                     proportions = 0.8, seed = 1)

  expect_named(split, c("train", "test"))
  expect_equal(as.vector(table(split$train$country_class)[c("United Kingdom", "Other OECD")]),
               c(40, 80))
  expect_equal(as.vector(table(split$test$country_class)[c("United Kingdom", "Other OECD")]),
               c(10, 20))
  # Rows of other targets are dropped, and no row is in both sets
  expect_false(any(c(split$train$country_class, split$test$country_class) == "Rest of World"))
  expect_length(intersect(split$train$`Record ID`, split$test$`Record ID`), 0)

  # The same seed gives the same split
  again <- divide_by(data, "country_class", c("United Kingdom", "Other OECD"), 0.8, seed = 1)
  expect_equal(again$train$`Record ID`, split$train$`Record ID`)
})

test_that("train_logistic_regressor() predicts the first target", {
  data <- make_encoded()

  model <- train_logistic_regressor(data, targets = c("United Kingdom", "Other OECD"))

  expect_s3_class(model, "glm")
  expect_equal(model$targets, c("United Kingdom", "Other OECD"))
  # Record ID and the constant reason_c are not predictors
  expect_setequal(names(stats::coef(model)), c("(Intercept)", "reason_a", "reason_b"))
  # reason_a makes the UK more likely
  expect_gt(stats::coef(model)[["reason_a"]], 0)
  # Only UK and OECD rows are used
  expect_equal(stats::nobs(model), 150)
})

test_that("prune_one_hot_columns() removes the columns with few ones", {
  data <- tibble::tibble(
    `Record ID` = c(1, 0, 1, 0),
    country_class = c("United Kingdom", "Other OECD", "Other OECD", "Other OECD"),
    reason_a = c(1L, 1L, 1L, 0L),
    reason_b = c(1L, 0L, 0L, 0L),
    reason_c = c(0L, 0L, 0L, 0L)
  )
  attr(data, "reasons") <- c(reason_a = "A", reason_b = "B", reason_c = "C")

  result <- prune_one_hot_columns(data, mincount = 2)

  # Record ID only holds 0 and 1 here, but it is excluded
  expect_equal(names(result), c("Record ID", "country_class", "reason_a"))
  expect_equal(attr(result, "reasons"), c(reason_a = "A"))
  expect_equal(names(prune_one_hot_columns(data, mincount = 1)),
               c("Record ID", "country_class", "reason_a", "reason_b"))
})

test_that("summarise_logistic_regressor_findings() describes the coefficients", {
  data <- make_encoded()
  # reason_d is only cited by UK papers, so its coefficient cannot be estimated
  data$reason_d <- as.integer(data$country_class == "United Kingdom" & data$`Record ID` <= 5)
  model <- suppressWarnings(train_logistic_regressor(data, c("United Kingdom", "Other OECD")))

  findings <- summarise_logistic_regressor_findings(
    model, reason_names = c(reason_a = "Reason A", reason_b = "Reason B")
  )

  expect_s3_class(findings, "logistic_regressor_findings")
  table <- findings$table
  # Reason names from reason_names, or from the column names otherwise
  expect_setequal(table$reason, c("Reason A", "Reason B", "reason d"))
  row_a <- table[table$column == "reason_a", ]
  expect_equal(row_a$odds_ratio, exp(stats::coef(model)[["reason_a"]]))
  expect_equal(row_a$favours, "United Kingdom")
  expect_true(row_a$significant)
  expect_equal(row_a$papers_a + row_a$papers_b, sum(data$reason_a[data$country_class != "Rest of World"]))
  expect_false(table$reliable[table$column == "reason_d"])
  # Unreliable coefficients are left out of the plot
  expect_false("reason d" %in% as.character(findings$plot$data$reason))
  expect_match(findings$text[1], "United Kingdom \\(50 papers\\) vs Other OECD \\(100 papers\\)")
  expect_match(findings$text[4], "Reason A \\(odds of United Kingdom x")

  expect_output(print(findings), "significant")
})
