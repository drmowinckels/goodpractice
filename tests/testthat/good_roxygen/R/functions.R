#' A well documented function
#'
#' @param x A value.
#' @return The input plus one.
#' @export
#' @examples
#' good_func(1)
good_func <- function(x) {
  x + 1
}

#' Internal helper
#' @noRd
#' @keywords internal
internal_helper <- function() {
  NULL
}

#' Wrapper that inherits params
#'
#' @inheritParams good_func
#' @return The input plus two.
#' @export
#' @examples
#' wrapper_func(1)
wrapper_func <- function(x) {
  good_func(x) + 1
}
