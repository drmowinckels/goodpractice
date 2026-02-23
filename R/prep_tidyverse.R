
#' @include lists.R
#' @importFrom lintr lint_package linters_with_defaults

PREPS$tidyverse_lintr <- function(state, path = state$path, quiet) {
  linters <- lintr::linters_with_defaults()

  path <- normalizePath(path)
  suppressMessages(
    state$tidyverse_lintr <- try(
      lint_package(path, linters = linters),
      silent = TRUE
    )
  )
  if (inherits(state$tidyverse_lintr, "try-error")) {
    warning("Prep step for tidyverse_lintr failed.")
  }
  state
}
