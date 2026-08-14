# Install the R packages used across the exercise scripts.
packages <- c(
  "fBasics", "fGarch", "forecast", "ggplot2", "dplyr",
  "kableExtra", "openxlsx", "rugarch", "tseries"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
