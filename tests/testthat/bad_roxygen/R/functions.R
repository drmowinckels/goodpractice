#' Exported but no return
#'
#' @param x A value.
#' @export
#' @examples
#' exported_no_return(1)
exported_no_return <- function(x) {
  x + 1
}

#' Exported but no examples
#'
#' @param x A value.
#' @return The input plus one.
#' @export
exported_no_examples <- function(x) {
  x + 1
}

untagged_func <- function(x) {
  x
}

#' Internal but no keywords
#' @noRd
nord_no_keywords <- function() {
  NULL
}

#' Conflicting tags
#' @export
#' @keywords internal
#' @return Nothing.
#' @examples
#' confused_func()
confused_func <- function() {
  NULL
}

#' Has dontrun in examples
#'
#' @return Nothing.
#' @export
#' @examples
#' \dontrun{
#'   dontrun_example()
#' }
dontrun_example <- function() {
  NULL
}

#' All code wrapped
#'
#' @return Nothing.
#' @export
#' @examples
#' \donttest{
#'   all_wrapped()
#' }
all_wrapped <- function() {
  NULL
}
