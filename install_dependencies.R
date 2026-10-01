#!/usr/bin/env Rscript

required_packages <- c(
  "dplyr",
  "forcats",
  "ggplot2",
  "httr",
  "janitor",
  "knitr",
  "lubridate",
  "magrittr",
  "readr",
  "readxl",
  "rmarkdown",
  "tibble",
  "tidyr",
  "tidytext",
  "tidyverse",
  "pandoc",
  "plotly"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  message(
    "Installing missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

still_missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(still_missing) > 0) {
  stop(
    "The following packages could not be installed: ",
    paste(still_missing, collapse = ", ")
  )
}

message("All required R packages are installed.")
