get_result <- function(res, check) res$passed[res$check == check]

# -- vignette_no_rm_list ------------------------------------------------------

test_that("vignette_no_rm_list fails when vignette has rm(list = ls())", {
  gp_res <- gp("bad_vignettes", checks = "vignette_no_rm_list")
  res <- results(gp_res)
  expect_false(get_result(res, "vignette_no_rm_list"))

  pos <- failed_positions(gp_res)$vignette_no_rm_list
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("rm\\(list = ls\\(\\)\\)", lines)))
})

test_that("vignette_no_rm_list passes when no vignettes directory", {
  gp_res <- gp("good", checks = "vignette_no_rm_list")
  res <- results(gp_res)
  expect_true(get_result(res, "vignette_no_rm_list"))
})

test_that("vignette_no_rm_list ignores rm() without ls()", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r}",
    "rm(x)",
    "```"
  ), file.path(pkg, "vignettes", "demo.Rmd"))

  state <- list(path = pkg)
  expect_true(CHECKS$vignette_no_rm_list$check(state)$status)
})

# -- vignette_no_setwd --------------------------------------------------------

test_that("vignette_no_setwd fails when vignette has setwd()", {
  gp_res <- gp("bad_vignettes", checks = "vignette_no_setwd")
  res <- results(gp_res)
  expect_false(get_result(res, "vignette_no_setwd"))

  pos <- failed_positions(gp_res)$vignette_no_setwd
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("setwd", lines)))
})

test_that("vignette_no_setwd passes when no vignettes directory", {
  gp_res <- gp("good", checks = "vignette_no_setwd")
  res <- results(gp_res)
  expect_true(get_result(res, "vignette_no_setwd"))
})

# -- shared behaviour ---------------------------------------------------------

test_that("vignette checks ignore non-evaluated chunks", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "```{r eval=FALSE}",
    "rm(list = ls())",
    "setwd('/tmp')",
    "```"
  ), file.path(pkg, "vignettes", "demo.Rmd"))

  state <- list(path = pkg)
  expect_true(CHECKS$vignette_no_rm_list$check(state)$status)
  expect_true(CHECKS$vignette_no_setwd$check(state)$status)
})

test_that("skip_eval_false sets purl=FALSE for eval=FALSE chunks", {
  skip_fn <- goodpractice:::skip_eval_false
  result <- skip_fn(list(eval = FALSE, purl = TRUE))
  expect_false(result$purl)

  result2 <- skip_fn(list(eval = TRUE, purl = TRUE))
  expect_true(result2$purl)
})

test_that("vignette_parse_data returns NULL for empty code chunks", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "vignettes"), showWarnings = FALSE)
  writeLines(c(
    "---",
    "title: test",
    "---",
    "",
    "Just prose, no code."
  ), file.path(pkg, "vignettes", "empty.Rmd"))

  result <- goodpractice:::vignette_parse_data(
    file.path(pkg, "vignettes", "empty.Rmd")
  )
  expect_null(result)
})

test_that("vignette_parse_data returns NULL when purl fails", {
  expect_warning(
    result <- goodpractice:::vignette_parse_data(tempfile(fileext = ".Rmd"))
  )
  expect_null(result)
})

test_that("vignette checks report correct positions", {
  gp_res <- gp("bad_vignettes",
                checks = c("vignette_no_rm_list", "vignette_no_setwd"))

  rm_pos <- failed_positions(gp_res)$vignette_no_rm_list
  expect_true(all(vapply(rm_pos, function(p) {
    grepl("^vignettes/", p$filename)
  }, logical(1))))
  expect_equal(rm_pos[[1]]$line_number, 10L)

  setwd_pos <- failed_positions(gp_res)$vignette_no_setwd
  expect_true(all(vapply(setwd_pos, function(p) {
    grepl("^vignettes/", p$filename)
  }, logical(1))))
  expect_equal(setwd_pos[[1]]$line_number, 11L)
})
