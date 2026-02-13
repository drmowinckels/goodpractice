#' @include lists.R
#' @importFrom roxygen2 parse_package

find_function_defs <- function(path) {
  rfiles <- r_package_files(path)
  fn_re <- "^([A-Za-z0-9_.]+)\\s*(?:<-|=)\\s*function\\b"
  defs <- list()

  for (f in rfiles) {
    if (!file.exists(f)) next
    lines <- readLines(f, warn = FALSE)
    for (i in seq_along(lines)) {
      m <- regmatches(lines[i], regexec(fn_re, lines[i]))[[1]]
      if (length(m) >= 2) {
        defs[[length(defs) + 1]] <- data.frame(
          name = m[2],
          file = f,
          line = i,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(defs) == 0) {
    data.frame(name = character(), file = character(),
               line = integer(), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, defs)
  }
}

PREPS$roxygen2 <- function(state, path = state$path, quiet) {
  state$roxygen2 <- try({
    blocks <- roxygen2::parse_package(path, env = NULL)

    ns <- tryCatch(
      parseNamespaceFile(basename(path), dirname(path)),
      error = function(e) {
        list(exports = character(),
             S3methods = matrix(character(), ncol = 3, nrow = 0))
      }
    )

    s3m <- ns$S3methods
    s3methods <- if (nrow(s3m) > 0) {
      paste0(s3m[, 1], ".", s3m[, 2])
    } else {
      character()
    }

    list(
      blocks = blocks,
      namespace_exports = ns$exports,
      namespace_s3methods = s3methods,
      function_defs = find_function_defs(path)
    )
  }, silent = quiet)

  if (inherits(state$roxygen2, "try-error")) {
    warning("Prep step for roxygen2 failed.")
  }
  state
}
