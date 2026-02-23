#' @include lists.R

vignette_files <- function(path) {
  vigdir <- file.path(path, "vignettes")
  if (!dir.exists(vigdir)) return(character())
  list.files(vigdir, pattern = "\\.(Rmd|Rnw|qmd)$",
             full.names = TRUE, recursive = TRUE)
}

extract_evaluated_code <- function(lines) {
  in_chunk <- FALSE
  is_eval <- FALSE
  result <- data.frame(
    line_number = integer(),
    text = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(lines)) {
    line <- lines[i]

    if (!in_chunk && grepl("^\\s*```\\s*\\{\\s*r", line)) {
      in_chunk <- TRUE
      is_eval <- !grepl("eval\\s*=\\s*FALSE", line)
      next
    }

    if (in_chunk && grepl("^\\s*```\\s*$", line)) {
      in_chunk <- FALSE
      is_eval <- FALSE
      next
    }

    if (in_chunk && is_eval) {
      result <- rbind(result, data.frame(
        line_number = i,
        text = line,
        stringsAsFactors = FALSE
      ))
    }
  }
  result
}

check_vignette_pattern <- function(state, pattern) {
  path <- state$path
  vfiles <- vignette_files(path)
  problems <- list()

  for (f in vfiles) {
    lines <- readLines(f, warn = FALSE)
    code <- extract_evaluated_code(lines)
    if (nrow(code) == 0) next

    hits <- grepl(pattern, code$text)
    for (j in which(hits)) {
      problems[[length(problems) + 1]] <- list(
        filename = file.path("vignettes", basename(f)),
        line_number = code$line_number[j],
        column_number = NA_integer_,
        ranges = list(),
        line = trimws(code$text[j])
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
    check_vignette_pattern(state, "rm\\s*\\(\\s*list\\s*=\\s*ls\\s*\\(")
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
    check_vignette_pattern(state, "setwd\\s*\\(")
  }
)
