#' @include lists.R
#' @importFrom utils getParseData

vignette_files <- function(path) {
  vigdir <- file.path(path, "vignettes")
  if (!dir.exists(vigdir)) return(character())
  list.files(vigdir, pattern = "\\.(Rmd|Rnw|qmd)$",
             full.names = TRUE, recursive = TRUE)
}

skip_eval_false <- function(options) {
  if (isFALSE(options$eval)) options$purl <- FALSE
  options
}

purl_vignette <- function(f) {
  r_file <- tempfile(fileext = ".R")
  old_hooks <- knitr::opts_hooks$get()
  on.exit(knitr::opts_hooks$restore(old_hooks))

  knitr::opts_hooks$set(eval = skip_eval_false)

  tryCatch({
    knitr::purl(f, output = r_file, quiet = TRUE, documentation = 2L)
    r_file
  }, error = function(e) { unlink(r_file); NULL })
}

vignette_parse_data <- function(f) {
  r_file <- purl_vignette(f)
  if (is.null(r_file)) return(NULL)
  on.exit(unlink(r_file))

  parsed <- tryCatch(
    parse(r_file, keep.source = TRUE),
    error = function(e) NULL
  )
  if (is.null(parsed) || length(parsed) == 0) return(NULL)

  getParseData(parsed)
}

call_descendants <- function(pd, fn_call_id) {
  name_expr <- pd$parent[pd$id == fn_call_id]
  call_expr <- pd$parent[pd$id == name_expr]

  ids <- call_expr
  queue <- call_expr
  while (length(queue) > 0) {
    children <- pd$id[pd$parent %in% queue]
    if (length(children) == 0) break
    ids <- c(ids, children)
    queue <- children
  }
  ids
}

check_vignette_calls <- function(state, fn_name, nested_fn = NULL) {
  vfiles <- vignette_files(state$path)
  problems <- list()

  for (f in vfiles) {
    pd <- vignette_parse_data(f)
    if (is.null(pd)) next

    fn_rows <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == fn_name, ,
                  drop = FALSE]
    if (nrow(fn_rows) == 0) next

    orig_lines <- readLines(f, warn = FALSE)

    for (i in seq_len(nrow(fn_rows))) {
      if (!is.null(nested_fn)) {
        desc_ids <- call_descendants(pd, fn_rows$id[i])
        desc <- pd[pd$id %in% desc_ids, , drop = FALSE]
        if (!any(desc$token == "SYMBOL_FUNCTION_CALL" &
                 desc$text == nested_fn)) {
          next
        }
      }

      ln <- fn_rows$line1[i]
      line_text <- if (ln <= length(orig_lines)) orig_lines[ln] else ""

      problems[[length(problems) + 1]] <- list(
        filename = file.path("vignettes", basename(f)),
        line_number = ln,
        column_number = fn_rows$col1[i],
        ranges = list(),
        line = trimws(line_text)
      )
    }
  }

  if (length(problems) == 0) {
    list(status = TRUE, positions = list())
  } else {
    list(status = FALSE, positions = problems)
  }
}

CHECKS$vignette_no_rm_list <- make_check(

  description = "Vignettes do not use rm(list = ls())",
  tags = c("best practice", "warning"),
  preps = character(),

  gp = paste(
    "do not use rm(list = ls()) in vignettes.",
    "Vignettes run in their own environment;",
    "clearing the workspace is unnecessary and confusing for users."
  ),

  check = function(state) {
    check_vignette_calls(state, "rm", nested_fn = "ls")
  }
)

CHECKS$vignette_no_setwd <- make_check(

  description = "Vignettes do not use setwd()",
  tags = c("best practice", "warning"),
  preps = character(),

  gp = paste(
    "do not use setwd() in vignettes.",
    "Changing the working directory makes vignettes fragile",
    "and non-reproducible on other machines."
  ),

  check = function(state) {
    check_vignette_calls(state, "setwd")
  }
)
