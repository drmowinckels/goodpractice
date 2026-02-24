get_result <- function(res, check) res$passed[res$check == check]

test_that("print_return_invisible fails when print method lacks invisible()", {
  gp_res <- gp("bad_print", checks = "print_return_invisible")
  res <- results(gp_res)
  expect_false(get_result(res, "print_return_invisible"))

  pos <- failed_positions(gp_res)$print_return_invisible
  names <- vapply(pos, `[[`, "", "line")
  expect_true("print.myclass" %in% names)
  expect_true("print.another" %in% names)
})

test_that("print_return_invisible passes with correct invisible() return", {
  gp_res <- gp("good_print", checks = "print_return_invisible")
  res <- results(gp_res)
  expect_true(get_result(res, "print_return_invisible"))
})

test_that("print_return_invisible passes when no print methods exist", {
  gp_res <- gp("good", checks = "print_return_invisible")
  res <- results(gp_res)
  expect_true(get_result(res, "print_return_invisible"))
})

test_that("print_return_invisible passes when no R directory exists", {
  state <- list(path = tempdir())
  result <- CHECKS$print_return_invisible$check(state)
  expect_true(result$status)
  expect_length(result$positions, 0)
})

test_that("has_invisible_call detects invisible in various expression forms", {
  expect_true(has_invisible_call(quote(invisible(x))))
  expect_true(has_invisible_call(quote({
    cat("hi\n")
    invisible(x)
  })))
  expect_true(has_invisible_call(quote(
    if (TRUE) invisible(x) else invisible(x)
  )))
  expect_false(has_invisible_call(quote(cat("hi\n"))))
  expect_false(has_invisible_call(quote(x + 1)))
  expect_false(has_invisible_call(quote(42)))
  expect_false(has_invisible_call(quote("text")))
})
