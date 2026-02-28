
describe("opt_in_checks()", {

  it("returns a character vector", {
    oi <- opt_in_checks()
    expect_type(oi, "character")
  })

  it("only contains registered check names", {
    oi <- opt_in_checks()
    expect_true(all(oi %in% all_checks()))
  })
})

describe("check tiers", {

  it("all_checks() includes opt-in checks", {
    oi <- opt_in_checks()
    if (length(oi) == 0) skip("no opt-in checks registered yet")
    expect_true(all(oi %in% all_checks()))
  })

  it("default_checks() excludes opt-in checks", {
    oi <- opt_in_checks()
    if (length(oi) == 0) skip("no opt-in checks registered yet")
    expect_false(any(oi %in% default_checks()))
  })

  it("default + tidyverse + opt-in = all", {
    expect_equal(
      sort(c(default_checks(), tidyverse_checks(), opt_in_checks())),
      sort(all_checks())
    )
  })

  it("tiers are mutually exclusive", {
    dc <- default_checks()
    tv <- tidyverse_checks()
    oi <- opt_in_checks()
    expect_length(intersect(dc, tv), 0)
    expect_length(intersect(dc, oi), 0)
    expect_length(intersect(tv, oi), 0)
  })
})

describe("make_check() with opt-in tag", {

  it("flows through to opt_in_checks()", {
    chk <- make_check(
      description = "test opt-in check",
      tags = c("info", "opt-in"),
      preps = character(),
      gp = "test",
      check = function(state) TRUE
    )
    pkg_env <- environment(opt_in_checks)
    unlockBinding("CHECKS", pkg_env)
    pkg_env$CHECKS[["__test_opt_in"]] <- chk
    withr::defer({
      pkg_env$CHECKS[["__test_opt_in"]] <- NULL
      lockBinding("CHECKS", pkg_env)
    })

    expect_true("__test_opt_in" %in% opt_in_checks())
    expect_true("__test_opt_in" %in% all_checks())
    expect_false("__test_opt_in" %in% default_checks())
  })
})
