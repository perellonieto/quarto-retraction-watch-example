#' Stratified split of a data frame into training and test sets
#'
#' Keeps the rows whose `column` is one of `targets` and splits them at
#' random into a training and a test set. The split is done separately for
#' each target, so both sets keep (up to rounding) the proportion of each
#' target of the original data.
#'
#' @param data A data frame, e.g. the output of
#'   [one_hot_encoding_reasons_and_country_type()].
#' @param column Column with the target of each row.
#' @param targets Values of `column` to keep.
#' @param proportions Proportion of the rows of each target that go to the
#'   training set; the rest go to the test set.
#' @param seed Optional random seed, to get the same split every time.
#' @return A list with two data frames, `train` and `test`.
#' @export
divide_by <- function(data, column, targets, proportions = 0.8, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  data <- data[data[[column]] %in% targets, ]
  in_train <- logical(nrow(data))
  for (target in targets) {
    rows <- which(data[[column]] == target)
    n_train <- round(length(rows) * proportions)
    in_train[rows[sample.int(length(rows), n_train)]] <- TRUE
  }
  list(train = data[in_train, ], test = data[!in_train, ])
}

#' Train a logistic regression to tell two targets apart
#'
#' Fits a logistic regression ([stats::glm()] with a binomial family) that
#' predicts whether a row belongs to `targets[1]` rather than `targets[2]`,
#' using every other column as a predictor except those in `exclude`.
#' Predictors that are constant in `data` (e.g. a reason never cited in the
#' training set) are dropped, as their coefficients cannot be estimated.
#'
#' @param data Training data, e.g. the `train` element of [divide_by()].
#' @param targets Two values of `column`: the positive class (`targets[1]`)
#'   and the negative class (`targets[2]`). Other rows are ignored.
#' @param column Column with the target of each row.
#' @param exclude Columns not used as predictors (e.g. identifiers).
#' @return A `glm` object. Its predictions with `type = "response"` are the
#'   probabilities of `targets[1]`. The targets are stored in its `targets`
#'   element.
#' @export
train_logistic_regressor <- function(data, targets, column = "country_class",
                                     exclude = "Record ID") {
  data <- data[data[[column]] %in% targets, ]
  predictors <- setdiff(names(data), c(column, exclude))
  constant <- vapply(data[predictors], function(x) length(unique(x)) < 2,
                     logical(1))
  predictors <- predictors[!constant]

  model_data <- data[predictors]
  model_data$target <- as.integer(data[[column]] == targets[1])
  model <- stats::glm(target ~ ., data = model_data,
                      family = stats::binomial())
  model$targets <- targets
  model
}

#' Remove one-hot columns with few ones
#'
#' Drops the 0/1 columns (e.g. the reason columns of
#' [one_hot_encoding_reasons_and_country_type()]) with fewer than `mincount`
#' ones, i.e. reasons cited by fewer than `mincount` papers. Rare columns
#' carry little information and make models such as
#' [train_logistic_regressor()] unstable. Other columns are kept. If `data`
#' has a `"reasons"` attribute, it is updated to the columns kept.
#'
#' @param data A data frame with one-hot encoded columns.
#' @param mincount Minimum number of ones a column needs to be kept.
#' @param exclude Columns never removed, even if they only hold 0 and 1.
#' @return `data` without the rare one-hot columns.
#' @export
prune_one_hot_columns <- function(data, mincount, exclude = "Record ID") {
  is_one_hot <- vapply(data, function(x) {
    is.numeric(x) && all(x %in% c(0, 1))
  }, logical(1))
  is_one_hot[names(data) %in% exclude] <- FALSE
  rare <- names(data)[is_one_hot][colSums(data[is_one_hot]) < mincount]

  reasons <- attr(data, "reasons")
  data <- data[setdiff(names(data), rare)]
  if (!is.null(reasons)) {
    attr(data, "reasons") <- reasons[setdiff(names(reasons), rare)]
  }
  data
}

#' Summarise the coefficients learned by a logistic regression
#'
#' Turns the coefficients of a model from [train_logistic_regressor()] into a
#' table of reasons, a short text and a forest plot. For a one-hot reason
#' column, the odds ratio (`exp(coefficient)`) is how many times the odds of
#' `targets[1]` (rather than `targets[2]`) multiply when a paper cites the
#' reason, the other reasons being equal. P-values are adjusted with
#' Benjamini-Hochberg. Coefficients with a very large standard error come
#' from reasons (almost) only cited in one of the targets, which the model
#' cannot estimate; they are flagged as unreliable and left out of the text
#' and the plot.
#'
#' @param model Output of [train_logistic_regressor()].
#' @param reason_names Optional named character vector mapping column names
#'   to reason text, e.g. the `"reasons"` attribute of
#'   [one_hot_encoding_reasons_and_country_type()]. By default the column
#'   names are used, with underscores replaced by spaces.
#' @param top_n Number of reasons shown in the plot and listed in the text
#'   for each direction.
#' @param alpha Significance level for the adjusted p-values.
#' @param max_std_error Coefficients with a larger standard error are
#'   flagged as unreliable.
#' @return An object of class `logistic_regressor_findings`, a list with
#'   `table` (one row per reason, sorted by coefficient, with the papers
#'   citing it in each target in `papers_a` and `papers_b`), `plot` (a
#'   ggplot), `text` (a character vector) and `targets`. Printing it shows
#'   the text, the significant reasons and the plot.
#' @export
summarise_logistic_regressor_findings <- function(model, reason_names = NULL,
                                                  top_n = 15, alpha = 0.05,
                                                  max_std_error = 5) {
  targets <- model$targets
  coefficients <- summary(model)$coefficients
  model_data <- model$model
  is_target <- model_data$target == 1

  table <- tibble(
    column = rownames(coefficients),
    coefficient = unname(coefficients[, "Estimate"]),
    std_error = unname(coefficients[, "Std. Error"]),
    p_value = unname(coefficients[, "Pr(>|z|)"])
  ) %>%
    filter(column != "(Intercept)") %>%
    mutate(
      column = gsub("`", "", column),
      reason = if (is.null(reason_names)) {
        gsub("_", " ", column)
      } else {
        coalesce(unname(reason_names[column]), gsub("_", " ", column))
      },
      odds_ratio = exp(coefficient),
      conf_low = exp(coefficient - stats::qnorm(0.975) * std_error),
      conf_high = exp(coefficient + stats::qnorm(0.975) * std_error),
      p_adj = stats::p.adjust(p_value, method = "BH"),
      reliable = std_error <= max_std_error,
      significant = reliable & p_adj < alpha,
      papers_a = vapply(column, function(x) sum(model_data[[x]][is_target]),
                        numeric(1), USE.NAMES = FALSE),
      papers_b = vapply(column, function(x) sum(model_data[[x]][!is_target]),
                        numeric(1), USE.NAMES = FALSE),
      favours = ifelse(coefficient > 0, targets[1], targets[2])
    ) %>%
    arrange(desc(coefficient)) %>%
    select(reason, column, coefficient, odds_ratio, conf_low, conf_high,
           p_value, p_adj, significant, reliable, favours, papers_a, papers_b)

  # McFadden's pseudo R-squared: 1 - log-likelihood of the model / of the
  # intercept-only model
  pseudo_r2 <- 1 - model$deviance / model$null.deviance
  describe <- function(rows) {
    if (nrow(rows) == 0) {
      return("none")
    }
    paste0(rows$reason, " (odds of ", targets[1], " x",
           formatC(rows$odds_ratio, format = "f", digits = 2), ")", collapse = "; ")
  }
  significant <- filter(table, significant)
  text <- c(
    paste0("Logistic regression predicting ", targets[1], " (", sum(is_target),
           " papers) vs ", targets[2], " (", sum(!is_target), " papers) from ",
           nrow(table), " reasons."),
    paste0("McFadden pseudo R-squared: ", formatC(pseudo_r2, format = "f", digits = 3),
           " (0 = no better than the overall proportion of ", targets[1], ")."),
    paste0(nrow(significant), " reasons are significant (adjusted p < ", alpha, "); ",
           sum(!table$reliable), " coefficients are unreliable and ignored."),
    paste0("Reasons that most increase the odds of ", targets[1], ": ",
           describe(utils::head(filter(significant, coefficient > 0), top_n)), "."),
    paste0("Reasons that most increase the odds of ", targets[2], ": ",
           describe(utils::head(arrange(filter(significant, coefficient < 0), coefficient), top_n)), ".")
  )

  plot_rows <- table %>%
    filter(reliable) %>%
    arrange(p_adj) %>%
    utils::head(top_n) %>%
    mutate(reason = stats::reorder(reason, coefficient))
  plot <- ggplot(plot_rows, aes(odds_ratio, reason, colour = favours)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    # `text` is ignored by ggplot2 but shown in the ggplotly() hover text
    suppressWarnings(geom_point(aes(
      shape = significant,
      text = paste0(reason, "<br>Odds ratio: ", signif(odds_ratio, 3),
                    " (95% CI ", signif(conf_low, 3), " - ", signif(conf_high, 3), ")",
                    "<br>Adjusted p-value: ", signif(p_adj, 3),
                    "<br>Papers: ", papers_a, " ", targets[1], ", ", papers_b, " ", targets[2])
    ), size = 2.5)) +
    geom_errorbar(aes(xmin = conf_low, xmax = conf_high), width = 0.2,
                  orientation = "y") +
    scale_x_log10() +
    scale_colour_manual(values = stats::setNames(c("#1b9e77", "#d95f02"), targets)) +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                       labels = c(`TRUE` = paste("Adjusted p <", alpha),
                                  `FALSE` = "Not significant")) +
    labs(x = paste0("Odds ratio of ", targets[1], " vs ", targets[2], " (log scale)"),
         y = NULL, colour = "More likely in", shape = NULL,
         title = paste0("Reasons with the smallest adjusted p-values (top ", nrow(plot_rows), ")")) +
    theme_minimal()

  structure(list(table = table, plot = plot, text = text, targets = targets),
            class = "logistic_regressor_findings")
}

#' @export
print.logistic_regressor_findings <- function(x, ...) {
  cat(paste(x$text, collapse = "\n\n"), "\n\n")
  significant <- x$table %>%
    filter(significant) %>%
    select(reason, odds_ratio, conf_low, conf_high, p_adj, papers_a, papers_b)
  names(significant)[6:7] <- paste("papers", x$targets)
  print(significant, n = Inf)
  print(x$plot)
  invisible(x)
}
