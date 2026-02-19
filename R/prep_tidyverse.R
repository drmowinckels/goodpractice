
#' @include lists.R
#' @importFrom lintr lint_package linters_with_defaults

tidyverse_linters_to_lint <- lintr::linters_with_defaults(
  assignment_linter = NULL,
  line_length_linter = NULL,
  semicolon_linter = NULL,
  seq_linter = NULL,
  T_and_F_symbol_linter = NULL
)

PREPS$tidyverse_lintr <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  suppressMessages(
    state$tidyverse_lintr <- try(
      lint_package(path, linters = tidyverse_linters_to_lint),
      silent = TRUE
    )
  )
  if (inherits(state$tidyverse_lintr, "try-error")) {
    warning("Prep step for tidyverse_lintr failed.")
  }
  state
}
