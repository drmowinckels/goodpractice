
PREPS <- list()
CHECKS <- list()

#' List the names of all checks
#'
#' @return Character vector of checks
#' @export

all_checks <- function() {
  names(CHECKS)
}

#' List the names of default checks (excludes optional check sets)
#'
#' @return Character vector of default check names
#' @export

default_checks <- function() {
  setdiff(all_checks(), c(tidyverse_checks(), opt_in_checks()))
}

#' List the names of opt-in checks
#'
#' These checks are included in \code{\link{all_checks}} but excluded from
#' \code{\link{default_checks}}. Include them individually by name or use
#' \code{checks = c(default_checks(), opt_in_checks())} to add all opt-in
#' checks to the default set.
#'
#' @return Character vector of opt-in check names
#' @export

opt_in_checks <- function() {
  opt_in <- vapply(CHECKS, function(chk) "opt-in" %in% chk$tags, logical(1))
  names(CHECKS)[opt_in]
}

#' List the names of tidyverse style checks
#'
#' These checks are optional and not included in the default set.
#' They are powered by \code{\link[lintr]{lint_package}} using lintr's
#' default linter set and respect any \code{.lintr} configuration file
#' in the package root (e.g. to disable specific linters or add exclusions).
#' Add them via \code{checks = c(default_checks(), tidyverse_checks())}.
#'
#' @return Character vector of tidyverse check names
#' @export

tidyverse_checks <- function() {
  grep("^tidyverse_", all_checks(), value = TRUE)
}

#' Describe one or more checks
#'
#' @param check_name Names of checks to be described.
#' @return List of character descriptions for each \code{check_name}
#' @export
#' @examples 
#' describe_check("rcmdcheck_non_portable_makevars")
#' check_name <- c("no_description_depends",
#'                 "lintr_assignment_linter",
#'                 "no_import_package_as_a_whole",
#'                 "rcmdcheck_missing_docs")
#' describe_check(check_name)
#' # Or to see all checks:
#' \dontrun{
#'   describe_check(all_checks())
#' }

describe_check <- function(check_name = NULL) {
    check_name <- intersect(check_name, names(CHECKS))
    lapply(CHECKS[check_name], function(i) i$description)
} 
