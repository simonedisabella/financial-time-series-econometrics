# Shared path helpers for the portfolio version of the assignment.
# Run exercise scripts from the repository root.

project_paths <- function(chapter) {
  if (!file.exists(file.path("R", "utils.R"))) {
    stop("Run this script from the repository root (the directory containing README.md).")
  }
  fig_dir <- file.path("figures", chapter)
  res_dir <- file.path("results", chapter)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)
  list(
    data = function(name) file.path("data", "raw", name),
    figure = function(name) file.path(fig_dir, name),
    result = function(name) file.path(res_dir, name)
  )
}
