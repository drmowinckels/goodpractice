describe("roxygen2 prep", {
  it("parses blocks from fixture package", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    expect_false(inherits(state$roxygen2, "try-error"))
    expect_true(length(state$roxygen2$blocks) > 0)
    expect_s3_class(state$roxygen2$function_defs, "data.frame")
  })

  it("collects namespace exports", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    expect_true("good_func" %in% state$roxygen2$namespace_exports)
  })

  it("finds all function definitions", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    fn_names <- state$roxygen2$function_defs$name
    expect_true("untagged_func" %in% fn_names)
    expect_true("exported_no_return" %in% fn_names)
  })
})

describe("roxygen2 checks return NA on prep failure", {
  it("returns NA status for all checks", {
    state <- list(roxygen2 = structure("error", class = "try-error"))
    check_names <- grep("^roxygen2_", names(CHECKS), value = TRUE)
    for (nm in check_names) {
      result <- CHECKS[[nm]]$check(state)
      expect_true(is.na(result$status), info = nm)
      expect_length(result$positions, 0)
    }
  })
})

describe("roxygen2_has_examples", {
  it("detects missing examples on bad fixture", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_has_examples$check(state)
    expect_false(result$status)
    lines <- vapply(result$positions, `[[`, "", "line")
    expect_true(any(grepl("exported_no_examples", lines)))
  })

  it("passes on good fixture", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_has_examples$check(state)
    expect_true(result$status)
  })
})

describe("roxygen2_has_return", {
  it("detects missing return on bad fixture", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_has_return$check(state)
    expect_false(result$status)
    lines <- vapply(result$positions, `[[`, "", "line")
    expect_true(any(grepl("exported_no_return", lines)))
  })

  it("passes on good fixture", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_has_return$check(state)
    expect_true(result$status)
  })
})

describe("roxygen2_has_export_or_nord", {
  it("detects untagged functions on bad fixture", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_has_export_or_nord$check(state)
    expect_false(result$status)
    lines <- vapply(result$positions, `[[`, "", "line")
    expect_true(any(grepl("untagged_func", lines)))
  })

  it("passes on good fixture", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_has_export_or_nord$check(state)
    expect_true(result$status)
  })
})

describe("roxygen2_nord_has_keywords_internal", {
  it("detects noRd without keywords internal", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_nord_has_keywords_internal$check(state)
    expect_false(result$status)
    lines <- vapply(result$positions, `[[`, "", "line")
    expect_true(any(grepl("nord_no_keywords", lines)))
  })

  it("passes on good fixture", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_nord_has_keywords_internal$check(state)
    expect_true(result$status)
  })
})

describe("roxygen2_no_export_and_keywords_internal", {
  it("detects conflicting export and keywords internal", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_no_export_and_keywords_internal$check(state)
    expect_false(result$status)
    lines <- vapply(result$positions, `[[`, "", "line")
    expect_true(any(grepl("confused_func", lines)))
  })

  it("passes on good fixture", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_no_export_and_keywords_internal$check(state)
    expect_true(result$status)
  })
})

describe("roxygen2_examples_dontrun", {
  it("detects dontrun in examples", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_examples_dontrun$check(state)
    expect_false(result$status)
    lines <- vapply(result$positions, `[[`, "", "line")
    expect_true(any(grepl("dontrun_example", lines)))
  })

  it("passes on good fixture", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_examples_dontrun$check(state)
    expect_true(result$status)
  })
})

describe("roxygen2_examples_runnable", {
  it("detects all-wrapped examples", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_examples_runnable$check(state)
    expect_false(result$status)
    lines <- vapply(result$positions, `[[`, "", "line")
    expect_true(any(grepl("all_wrapped", lines)))
  })

  it("passes on good fixture", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_examples_runnable$check(state)
    expect_true(result$status)
  })
})

describe("roxygen2_unknown_tags", {
  it("detects unknown @S3method tag via parse warnings", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    expect_true(any(grepl("is not a known tag", state$roxygen2$parse_messages)))
    result <- CHECKS$roxygen2_unknown_tags$check(state)
    expect_false(result$status)
    lines <- vapply(result$positions, `[[`, "", "line")
    expect_true(any(grepl("S3method", lines)))
  })

  it("passes on good fixture", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_unknown_tags$check(state)
    expect_true(result$status)
  })
})

describe("roxygen2_valid_inherit", {
  it("detects @inheritParams referencing nonexistent function", {
    state <- list(path = "bad_roxygen", package = "badroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_valid_inherit$check(state)
    expect_false(result$status)
    lines <- vapply(result$positions, `[[`, "", "line")
    expect_true(any(grepl("bad_inherit", lines)))
  })

  it("passes on good fixture with valid @inheritParams", {
    state <- list(path = "good_roxygen", package = "goodroxygen")
    state <- PREPS$roxygen2(state, quiet = TRUE)
    result <- CHECKS$roxygen2_valid_inherit$check(state)
    expect_true(result$status)
  })
})
