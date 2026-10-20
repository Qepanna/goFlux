# ---------------------------------------------------------------------------
# Diagnostic plots.
#
# The plot is a quality-control device, so the contract tested here is that it
# returns one named ggplot per incubation, whether or not bubbles were found,
# and that it rejects malformed input rather than drawing something wrong.
# ---------------------------------------------------------------------------


test_that("one named ggplot is returned per incubation", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 401)
  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))

  plots <- run_quiet(flux.plot.aqua(res, dataframe = df, gastype = "CH4dry_ppb"))

  expect_type(plots, "list")
  expect_length(plots, 1L)
  expect_named(plots, "sim_01")
  expect_s3_class(plots[[1]], "ggplot")
})


test_that("the plot builds without error when bubbles are present", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 402)
  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))

  plots <- run_quiet(flux.plot.aqua(res, dataframe = df, gastype = "CH4dry_ppb"))

  # ggplot is lazy: only building it actually exercises the layers.
  #
  # Silence matters here. The shape and alpha scales share a NULL title and
  # the same breaks, so ggplot2 merges their guides into one legend; if both
  # carry an override.aes, one is silently discarded and the legend keys are
  # not the ones intended. See SETUP-TESTS.md for the fix in flux.plot.aqua().
  expect_silent(invisible(ggplot2::ggplot_build(plots[[1]])))
})


test_that("the plot builds without error when no bubbles are present", {

  df  <- sim_incubation(bubbles = NULL, seed = 403)
  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))

  plots <- run_quiet(flux.plot.aqua(res, dataframe = df, gastype = "CH4dry_ppb"))

  expect_s3_class(plots[[1]], "ggplot")
  expect_silent(invisible(ggplot2::ggplot_build(plots[[1]])))
})


test_that("the plot builds when flux separation was disabled", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 404)
  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb", use_bubble_detection = FALSE))

  plots <- run_quiet(flux.plot.aqua(res, dataframe = df, gastype = "CH4dry_ppb"))

  expect_s3_class(plots[[1]], "ggplot")
  expect_silent(invisible(ggplot2::ggplot_build(plots[[1]])))
})


test_that("a batch returns one plot per incubation, named by UniqueID", {

  df <- sim_batch(list(
    list(id = "lake_a", bubbles = default_bubbles(), seed = 411),
    list(id = "lake_b", bubbles = NULL,              seed = 412)
  ))

  res   <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))
  plots <- run_quiet(flux.plot.aqua(res, dataframe = df, gastype = "CH4dry_ppb"))

  expect_length(plots, 2L)
  expect_setequal(names(plots), c("lake_a", "lake_b"))
  expect_true(all(vapply(plots, inherits, logical(1), "ggplot")))
})


test_that("each plot.display option is accepted on its own", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 405)
  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))

  for (opt in c("diffusive.window", "ebullition.events", "bubble.fits")) {
    plots <- run_quiet(flux.plot.aqua(res, dataframe = df, gastype = "CH4dry_ppb",
                                  plot.display = opt))
    expect_s3_class(plots[[1]], "ggplot")
  }
})


test_that("invalid plotting input is rejected", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 406)
  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))

  expect_error(flux.plot.aqua(res, gastype = "CH4dry_ppb"),
               "'dataframe' is required")
  expect_error(flux.plot.aqua(res, dataframe = df),
               "'gastype' is required")
  expect_error(flux.plot.aqua(res, dataframe = df, gastype = "not_a_gas"),
               "'gastype' must be one of")
  expect_error(flux.plot.aqua(res, dataframe = df, gastype = "CH4dry_ppb",
                              shoulder = -1),
               "non-negative")
  expect_error(flux.plot.aqua(res, dataframe = df, gastype = "CH4dry_ppb",
                              plot.display = "nonsense"),
               "unknown 'plot.display'")
  expect_error(flux.plot.aqua(res, dataframe = df, gastype = "CH4dry_ppb",
                              conversion.factor = 0),
               "conversion.factor")
})


test_that("a flux_summary missing required columns is refused", {

  df  <- sim_incubation(bubbles = NULL, seed = 407)
  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))

  broken <- res
  broken$flux_summary$flux_ebullition <- NULL

  expect_error(flux.plot.aqua(broken, dataframe = df, gastype = "CH4dry_ppb"),
               "missing columns")
})


test_that("a bare data.frame falls back to flux.plot with a message", {

  df  <- sim_incubation(bubbles = NULL, seed = 408)
  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))

  expect_message(
    try(flux.plot.aqua(res$diffusive, dataframe = df, gastype = "CH4dry_ppb"),
        silent = TRUE),
    "delegating to flux.plot")
})
