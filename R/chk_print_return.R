#' @include lists.R

has_invisible_call <- function(expr) {
  if (is.call(expr)) {
    fn <- deparse(expr[[1]])
    if (fn == "invisible") return(TRUE)
    for (i in seq_along(expr)) {
      if (has_invisible_call(expr[[i]])) return(TRUE)
    }
  }
  FALSE
}

CHECKS$print_return_invisible <- make_check(

  description = "Print methods return the object invisibly",
  tags = c("warning", "best practice"),
  preps = character(),

  gp = paste(
    "print methods should return the input object invisibly,",
    "e.g. invisible(x). This allows chaining and consistent behaviour",
    "with base R print methods."
  ),

  check = function(state) {
    path <- state$path
    rdir <- file.path(path, "R")

    if (!dir.exists(rdir)) {
      return(list(status = TRUE, positions = list()))
    }

    enc <- tryCatch(
      desc::desc_get_field("Encoding", default = "UTF-8", file = path),
      error = function(e) "UTF-8"
    )

    rfiles <- list.files(rdir, pattern = "\\.[rR]$", full.names = TRUE)
    problems <- list()

    for (f in rfiles) {
      exprs <- tryCatch(
        parse(f, keep.source = TRUE, encoding = enc),
        error = function(e) {
          tryCatch(
            parse(f, keep.source = FALSE, encoding = enc),
            error = function(e) NULL
          )
        }
      )
      if (is.null(exprs) || length(exprs) == 0) next

      srcrefs <- attr(exprs, "srcref")

      for (i in seq_along(exprs)) {
        e <- exprs[[i]]
        if (!is.call(e)) next

        op <- deparse(e[[1]])
        if (!(op %in% c("<-", "=")) || length(e) != 3) next
        if (!is.call(e[[3]])) next
        if (!identical(deparse(e[[3]][[1]]), "function")) next

        name <- deparse(e[[2]])
        if (!grepl("^print\\.", name)) next

        body_expr <- e[[3]][[3]]
        if (!has_invisible_call(body_expr)) {
          line <- if (!is.null(srcrefs) && !is.null(srcrefs[[i]])) {
            srcrefs[[i]][1]
          } else {
            NA_integer_
          }
          problems[[length(problems) + 1]] <- list(
            filename = file.path("R", basename(f)),
            line_number = line,
            column_number = NA_integer_,
            ranges = list(),
            line = name
          )
        }
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)
