make_desc_state <- function(pkg_name = "testpkg") {
  d <- desc::desc("!new")
  d$set(Package = pkg_name)
  list(description = d)
}

describe("reverse_dependencies check", {

  it("passes when package has no reverse deps", {
    local_mocked_bindings(query_reverse_deps = function(pkg_name) NULL)
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(result)
  })

  it("passes and prints info when package has reverse deps", {
    local_mocked_bindings(
      query_reverse_deps = function(pkg_name) c("pkgA", "pkgB")
    )
    state <- make_desc_state()
    withr::local_options(cli.default_handler = NULL)
    expect_message(
      result <- CHECKS$reverse_dependencies$check(state),
      "2 reverse dependencies"
    )
    expect_true(result)
  })

  it("returns NA on query error", {
    local_mocked_bindings(
      query_reverse_deps = function(pkg_name) stop("no internet")
    )
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("returns NA when description is a try-error", {
    state <- list(description = try(stop("fail"), silent = TRUE))
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("returns NA when package name is missing", {
    d <- desc::desc("!new")
    d$del("Package")
    state <- list(description = d)
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("passes when package has zero reverse deps (empty character)", {
    local_mocked_bindings(query_reverse_deps = function(pkg_name) character(0))
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(result)
  })

  it("returns NA when query_reverse_deps returns NA (no internet)", {
    local_mocked_bindings(query_reverse_deps = function(pkg_name) NA)
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })
})

describe("revdep_info_message", {

  it("lists reverse deps", {
    withr::local_options(cli.default_handler = NULL)
    expect_message(
      revdep_info_message(c("pkgA", "pkgB")),
      "pkgA, pkgB"
    )
  })

  it("uses singular for 1 reverse dep", {
    withr::local_options(cli.default_handler = NULL)
    expect_message(
      revdep_info_message("pkgA"),
      "1 reverse dependency"
    )
  })

  it("truncates when more than 10 deps", {
    withr::local_options(cli.default_handler = NULL)
    deps <- paste0("pkg", seq_len(12))
    expect_message(
      revdep_info_message(deps),
      "and 2 more"
    )
  })

  it("mentions revdepcheck", {
    withr::local_options(cli.default_handler = NULL)
    expect_message(
      revdep_info_message("pkgA"),
      "revdep_check"
    )
  })
})

describe("query_reverse_deps", {

  it("returns NA with warning when no internet", {
    local_mocked_bindings(has_internet = function() FALSE, .package = "curl")
    expect_warning(
      result <- query_reverse_deps("testpkg"),
      "no internet connection"
    )
    expect_true(is.na(result))
  })

  it("queries CRAN and returns reverse deps", {
    fake_db <- matrix(
      c("pkgA", "1.0", "testpkg"), nrow = 1,
      dimnames = list("pkgA", c("Package", "Version", "Depends"))
    )
    local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
    local_mocked_bindings(
      available.packages = function(...) fake_db,
      .package = "utils"
    )
    local_mocked_bindings(
      package_dependencies = function(pkg, db, reverse) list(testpkg = c("pkgA")),
      .package = "tools"
    )
    result <- query_reverse_deps("testpkg")
    expect_equal(result, "pkgA")
  })

  it("falls back to cloud.r-project.org when CRAN repo unset", {
    fake_db <- matrix(
      character(0), nrow = 0, ncol = 3,
      dimnames = list(NULL, c("Package", "Version", "Depends"))
    )
    local_mocked_bindings(has_internet = function() TRUE, .package = "curl")
    called_with <- NULL
    local_mocked_bindings(
      available.packages = function(repos, ...) {
        called_with <<- repos
        fake_db
      },
      .package = "utils"
    )
    local_mocked_bindings(
      package_dependencies = function(pkg, db, reverse) list(testpkg = NULL),
      .package = "tools"
    )
    withr::with_options(list(repos = c(CRAN = "@CRAN@")), {
      query_reverse_deps("testpkg")
    })
    expect_equal(called_with, "https://cloud.r-project.org")
  })
})
