#' @include lists.R

block_is_function <- function(block) {
  cl <- block$call
  if (is.null(cl) || !is.call(cl) || length(cl) < 3) return(FALSE)
  op <- as.character(cl[[1]])
  if (!op %in% c("<-", "=", "<<-")) return(FALSE)
  rhs <- cl[[3]]
  is.call(rhs) && identical(rhs[[1]], quote(`function`))
}

block_function_name <- function(block) {
  as.character(block$call[[2]])
}

make_block_position <- function(block) {
  list(
    filename = file.path("R", basename(block$file)),
    line_number = as.integer(block$line),
    column_number = NA_integer_,
    ranges = list(),
    line = deparse(block$call, nlines = 1)
  )
}

roxygen2_na_result <- function() {
  list(status = NA, positions = list())
}

# --- Checks ---

CHECKS$roxygen2_has_examples <- make_check(

  description = "Exported functions have @examples or @example",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",

  gp = "Add examples to all exported functions using @examples or @example.",

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    problems <- list()

    for (block in rox$blocks) {
      if (!block_is_function(block)) next
      name <- block_function_name(block)

      exported <- roxygen2::block_has_tags(block, "export") ||
        name %in% rox$namespace_exports
      if (!exported) next

      skip <- roxygen2::block_has_tags(block, c("rdname", "describeIn")) ||
        name %in% rox$namespace_s3methods
      if (skip) next

      if (!roxygen2::block_has_tags(block, c("examples", "example"))) {
        problems[[length(problems) + 1]] <- make_block_position(block)
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

CHECKS$roxygen2_has_return <- make_check(

  description = "Exported functions have a @return tag",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",

  gp = paste(
    "Document return values for exported (non-method)",
    "functions using @return."
  ),

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    problems <- list()

    for (block in rox$blocks) {
      if (!block_is_function(block)) next
      name <- block_function_name(block)

      exported <- roxygen2::block_has_tags(block, "export") ||
        name %in% rox$namespace_exports
      if (!exported) next

      skip <- roxygen2::block_has_tags(block, c("rdname", "describeIn")) ||
        name %in% rox$namespace_s3methods
      if (skip) next

      if (!roxygen2::block_has_tags(block, "return")) {
        problems[[length(problems) + 1]] <- make_block_position(block)
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

CHECKS$roxygen2_has_export_or_nord <- make_check(

  description = "Functions have roxygen @export or @noRd tags",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",

  gp = "Tag every function with either @export or @noRd.",

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    problems <- list()

    documented_names <- character()
    for (block in rox$blocks) {
      if (!block_is_function(block)) next
      name <- block_function_name(block)
      documented_names <- c(documented_names, name)

      has_tag <- roxygen2::block_has_tags(block, c("export", "noRd", "rdname"))
      in_ns <- name %in% rox$namespace_exports ||
        name %in% rox$namespace_s3methods
      if (!has_tag && !in_ns) {
        problems[[length(problems) + 1]] <- make_block_position(block)
      }
    }

    for (i in seq_len(nrow(rox$function_defs))) {
      fn <- rox$function_defs[i, ]
      if (fn$name %in% documented_names) next
      if (fn$name %in% rox$namespace_exports) next
      if (fn$name %in% rox$namespace_s3methods) next
      problems[[length(problems) + 1]] <- list(
        filename = file.path("R", basename(fn$file)),
        line_number = as.integer(fn$line),
        column_number = NA_integer_,
        ranges = list(),
        line = fn$name
      )
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

CHECKS$roxygen2_nord_has_keywords_internal <- make_check(

  description = "Functions tagged @noRd also have @keywords internal",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",

  gp = "Add @keywords internal alongside @noRd for internal functions.",

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    problems <- list()

    for (block in rox$blocks) {
      if (!block_is_function(block)) next
      if (!roxygen2::block_has_tags(block, "noRd")) next

      kw_tags <- roxygen2::block_get_tags(block, "keywords")
      has_internal <- any(vapply(kw_tags, function(t) {
        "internal" %in% strsplit(t$val, "\\s+")[[1]]
      }, logical(1)))

      if (!has_internal) {
        problems[[length(problems) + 1]] <- make_block_position(block)
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

CHECKS$roxygen2_no_export_and_keywords_internal <- make_check(

  description = "@export and @keywords internal should not co-exist",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",

  gp = paste(
    "Remove @keywords internal from exported functions, or remove @export",
    "if the function is meant to be internal."
  ),

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    problems <- list()

    for (block in rox$blocks) {
      if (!block_is_function(block)) next
      if (!roxygen2::block_has_tags(block, "export")) next

      kw_tags <- roxygen2::block_get_tags(block, "keywords")
      has_internal <- any(vapply(kw_tags, function(t) {
        "internal" %in% strsplit(t$val, "\\s+")[[1]]
      }, logical(1)))

      if (has_internal) {
        problems[[length(problems) + 1]] <- make_block_position(block)
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

CHECKS$roxygen2_examples_dontrun <- make_check(

  description = "Examples do not use \\dontrun",
  tags = c("documentation", "roxygen2", "CRAN"),
  preps = "roxygen2",

  gp = paste(
    "Replace \\dontrun{} with \\donttest{} in examples.",
    "\\dontrun{} should only be used if the example truly cannot be",
    "executed by the user (e.g. missing API keys or external software)."
  ),

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    problems <- list()

    for (block in rox$blocks) {
      ex_tags <- roxygen2::block_get_tags(block, "examples")
      for (tag in ex_tags) {
        if (grepl("\\\\dontrun\\s*\\{", tag$val)) {
          problems[[length(problems) + 1]] <- make_block_position(block)
          break
        }
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

CHECKS$roxygen2_examples_runnable <- make_check(

  description = "Examples have runnable code outside \\dontrun/\\donttest",
  tags = c("documentation", "roxygen2", "CRAN"),
  preps = "roxygen2",

  gp = paste(
    "Include some directly runnable example code.",
    "Wrapping all example code in \\dontrun{} or \\donttest{} means",
    "nothing is checked by R CMD check. At least some examples should",
    "run unconditionally."
  ),

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    problems <- list()

    for (block in rox$blocks) {
      ex_tags <- roxygen2::block_get_tags(block, "examples")
      if (length(ex_tags) == 0) next

      code <- paste(vapply(ex_tags, function(t) t$val, ""), collapse = "\n")
      stripped <- gsub("\\\\dont(run|test)\\s*\\{[^}]*\\}", "", code)
      stripped <- gsub("\\s", "", stripped)

      if (nzchar(code) && !nzchar(stripped)) {
        problems[[length(problems) + 1]] <- make_block_position(block)
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

CHECKS$roxygen2_deprecated_tags <- make_check(

  description = "No deprecated roxygen2 tags are used",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",

  gp = paste(
    "Replace deprecated roxygen2 tags.",
    "@S3method was removed in roxygen2 7.0.0;",
    "use @export instead (S3 methods are detected automatically)."
  ),

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    problems <- list()

    deprecated_re <- "^#'\\s*@(S3method)\\b"

    files <- unique(c(
      vapply(rox$blocks, function(b) b$file, ""),
      rox$function_defs$file
    ))

    for (f in files) {
      if (!file.exists(f)) next
      src <- readLines(f, warn = FALSE)
      for (i in seq_along(src)) {
        if (grepl(deprecated_re, src[i])) {
          problems[[length(problems) + 1]] <- list(
            filename = file.path("R", basename(f)),
            line_number = as.integer(i),
            column_number = NA_integer_,
            ranges = list(),
            line = trimws(src[i])
          )
        }
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

CHECKS$roxygen2_valid_inherit <- make_check(

  description = "@inheritParams/@inheritDotParams reference known functions",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",

  gp = paste(
    "Ensure functions referenced by @inheritParams and @inheritDotParams",
    "exist within the package. Use pkg::func syntax for external functions."
  ),

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    pkg_fns <- rox$function_defs$name
    problems <- list()

    for (block in rox$blocks) {
      inherit_tags <- c(
        roxygen2::block_get_tags(block, "inheritParams"),
        roxygen2::block_get_tags(block, "inheritDotParams")
      )
      if (length(inherit_tags) == 0) next

      for (tag in inherit_tags) {
        ref <- trimws(strsplit(trimws(tag$val), "\\s+")[[1]][1])
        if (grepl("::", ref, fixed = TRUE)) next
        if (!ref %in% pkg_fns) {
          problems[[length(problems) + 1]] <- make_block_position(block)
          break
        }
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)
