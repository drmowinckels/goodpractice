
#' @include lists.R

query_reverse_deps <- function(pkg_name) {
  if (!curl::has_internet()) {
    warning("Skipping reverse dependency check: no internet connection.")
    return(NA)
  }

  repos <- getOption("repos")["CRAN"]
  if (is.na(repos) || repos == "@CRAN@") {
    repos <- "https://cloud.r-project.org"
  }
  db <- utils::available.packages(repos = repos)
  deps <- tools::package_dependencies(pkg_name, db = db, reverse = TRUE)
  deps[[pkg_name]]
}

revdep_info_message <- function(revdeps) {
  n <- length(revdeps)
  dep_list <- paste(utils::head(revdeps, 10), collapse = ", ")
  suffix <- if (n > 10) paste0(", ... and ", n - 10, " more") else ""
  cli::cli_alert_info(paste0(
    "This package has ", n, " reverse ",
    if (n == 1) "dependency" else "dependencies",
    " on CRAN: ", dep_list, suffix,
    ". Consider running {.code revdepcheck::revdep_check()} before submission."
  ))
}

CHECKS$reverse_dependencies <- make_check(

  description = "Check for reverse dependencies on CRAN",
  tags = c("info", "CRAN"),
  preps = "description",

  gp = "run reverse dependency checks before CRAN submission.",

  check = function(state) {
    if (inherits(state$description, "try-error")) return(NA)

    pkg_name <- state$description$get_field("Package", default = NA_character_)
    if (is.na(pkg_name)) return(NA)

    revdeps <- tryCatch(
      query_reverse_deps(pkg_name),
      error = function(e) NA
    )

    if (identical(revdeps, NA)) return(NA)
    if (is.null(revdeps) || length(revdeps) == 0) return(TRUE)

    revdep_info_message(revdeps)
    TRUE
  }
)
