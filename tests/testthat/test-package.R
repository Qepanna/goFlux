# ---------------------------------------------------------------------------
# Package-level sanity checks: the package loads, the aquatic API is present,
# the shipped example data are readable, and the published defaults have not
# silently changed.
# ---------------------------------------------------------------------------

test_that("the package loads and the aquatic API is exported", {

  expect_true(requireNamespace("goFlux", quietly = TRUE))

  exported <- c("goAquaFlux", "goAquaFlux.diagnostics", "flux.plot.aqua")
  for (fn in exported) {
    expect_true(fn %in% getNamespaceExports("goFlux"),
                info = paste(fn, "should be exported"))
    expect_true(is.function(get(fn, envir = asNamespace("goFlux"))))
  }
})


test_that("the internal aquatic workers are available in the namespace", {

  # These are called by goAquaFlux() and are documented in the paper, so they
  # must at least exist with the expected names.
  internal <- c("find.bubbles", "goAquaFlux.diffusive",
                "goAquaFlux.ebullition", "goAquaFlux.total")
  for (fn in internal) {
    expect_true(is.function(get(fn, envir = asNamespace("goFlux"))),
                info = paste(fn, "should exist in the namespace"))
  }
})


test_that("shipped example datasets load and have the expected structure", {

  datasets <- c("manID.UGGA", "manID.G2201i", "imp.UGGA", "imp.G2508")

  for (d in datasets) {
    obj <- get(d, envir = asNamespace("goFlux"))
    expect_s3_class(obj, "data.frame")
    expect_gt(nrow(obj), 0)
  }

  # The aquatic workflow needs an elapsed-time column and a CH4 column to
  # detect bubbles on.
  expect_true("Etime" %in% names(goFlux::manID.UGGA))
  expect_true("CH4dry_ppb" %in% names(goFlux::manID.UGGA))
})


test_that("published defaults are unchanged", {

  # The paper documents these defaults. A change to any of them alters
  # published behaviour, so it should break a test rather than pass silently.
  fb <- formals(get("find.bubbles", envir = asNamespace("goFlux")))

  expect_equal(eval(fb$window.size),    15)
  expect_equal(eval(fb$dt),              1)
  expect_equal(eval(fb$var.quantile),  0.7)
  expect_equal(eval(fb$k),               4)
  expect_equal(eval(fb$min_ratio),       3)
  expect_equal(eval(fb$min_gap),        10)
  expect_equal(eval(fb$min_length),      5)
  expect_equal(eval(fb$max_reg_window), 120)
  expect_equal(eval(fb$reg.min.obs),    10)
  expect_equal(eval(fb$min_magnitude),   5)
  expect_equal(eval(fb$method)[1],          "diff")
  expect_equal(eval(fb$magnitude.model)[1], "step_reequil")

  ga <- formals(goFlux::goAquaFlux)
  expect_true(eval(ga$use_bubble_detection))
  expect_equal(eval(ga$bubble_gas), "CH4dry_ppb")
  expect_equal(eval(ga$bubble.method), "diff")
  expect_equal(eval(ga$bubble.window.size), 15)
  expect_equal(eval(ga$ebullition.final_window_min), 30)
  expect_equal(eval(ga$diffusion.minimum_window), 30)
})
