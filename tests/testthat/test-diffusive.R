# ---------------------------------------------------------------------------
# Diffusive flux on a bubble-free window.
#
# Two things are tested: that the fitting window is truncated where the paper
# says it is, and that the flux returned on that window recovers the known
# diffusive slope through goFlux's conversion.
# ---------------------------------------------------------------------------

find.bubbles         <- get("find.bubbles",         envir = asNamespace("goFlux"))
goAquaFlux.diffusive <- get("goAquaFlux.diffusive", envir = asNamespace("goFlux"))


test_that("the full record is used when no bubbles are present", {

  df  <- sim_incubation(bubbles = NULL, seed = 101)
  res <- run_quiet(goAquaFlux.diffusive(df, gastype = "CH4dry_ppb", bubbles = NULL))

  expect_equal(res$n_used, nrow(df))
  expect_true(is.na(res$first_bubble_time))
  expect_true(is.finite(res$flux))
})


test_that("the window is truncated at the first bubble for the bubble gas", {

  bub <- data.frame(time = 300, magnitude = 200)
  df  <- sim_incubation(bubbles = bub, seed = 102)
  bb  <- find.bubbles(df, "CH4dry_ppb")

  res <- run_quiet(goAquaFlux.diffusive(df, gastype = "CH4dry_ppb",
                                    bubble_gas = "CH4dry_ppb", bubbles = bb))

  # Only the pre-bubble segment may be used.
  expect_lt(res$n_used, nrow(df))
  expect_lte(res$n_used, bb$start[1] + 2)
  expect_equal(res$first_bubble_time, bb$start[1])
})


test_that("the known diffusive slope is recovered", {

  slope <- 0.5                              # ppb / s, by construction
  df    <- sim_incubation(bubbles = default_bubbles(), CH4_slope = slope,
                          seed = 103)
  bb    <- find.bubbles(df, "CH4dry_ppb")

  res <- run_quiet(goAquaFlux.diffusive(df, gastype = "CH4dry_ppb",
                                    bubble_gas = "CH4dry_ppb", bubbles = bb))

  expected <- slope * sim_flux.term()
  expect_equal(res$flux, expected, tolerance = 0.10)

  # The reported SE must be finite and positive, because it propagates into
  # SE(F_total).
  expect_true(is.finite(res$SE))
  expect_gt(res$SE, 0)
})


test_that("a bubble gas flux is NOT biased by the post-bubble steps", {

  # The point of truncating: fitting the whole record would absorb the steps
  # into the slope and inflate the diffusive flux.
  slope <- 0.5
  bub   <- default_bubbles()
  df    <- sim_incubation(bubbles = bub, CH4_slope = slope, seed = 104)
  bb    <- find.bubbles(df, "CH4dry_ppb")

  res_trunc <- run_quiet(goAquaFlux.diffusive(df, gastype = "CH4dry_ppb",
                                          bubble_gas = "CH4dry_ppb",
                                          bubbles = bb))
  res_full  <- run_quiet(goAquaFlux.diffusive(df, gastype = "CH4dry_ppb",
                                          bubble_gas = "CH4dry_ppb",
                                          bubbles = NULL))

  expect_lt(res_trunc$flux, res_full$flux)
  expect_equal(res_trunc$flux, slope * sim_flux.term(), tolerance = 0.10)
})


test_that("an insufficient pre-bubble window is flagged, not fitted", {

  # A bubble almost immediately after closure leaves too few observations.
  bub <- data.frame(time = 20, magnitude = 300)
  df  <- sim_incubation(bubbles = bub, seed = 105)
  bb  <- find.bubbles(df, "CH4dry_ppb")

  skip_if(is.null(bb), "no bubble detected in this configuration")

  res <- run_quiet(goAquaFlux.diffusive(df, gastype = "CH4dry_ppb",
                                    bubble_gas = "CH4dry_ppb", bubbles = bb,
                                    minimum_window = 60))

  expect_true(is.na(res$flux))
  expect_match(res$message, "Insufficient", ignore.case = TRUE)
})


test_that("a non-bubble gas keeps its full record when it is unperturbed", {

  # CO2 partitions weakly into bubbles: a CH4 event alone must not discard
  # CO2 observations.
  df <- sim_incubation(bubbles = default_bubbles(), co2_bubbles = FALSE,
                       seed = 106)
  bb <- find.bubbles(df, "CH4dry_ppb")

  res <- run_quiet(goAquaFlux.diffusive(df, gastype = "CO2dry_ppm",
                                    bubble_gas = "CH4dry_ppb", bubbles = bb))

  expect_equal(res$n_used, nrow(df))
})


test_that("a non-bubble gas is truncated when it does change abruptly", {

  df <- sim_incubation(bubbles = data.frame(time = 300, magnitude = 2000),
                       co2_bubbles = TRUE, noise_CO2 = 0.01, seed = 107)
  bb <- find.bubbles(df, "CH4dry_ppb")

  res <- run_quiet(goAquaFlux.diffusive(df, gastype = "CO2dry_ppm",
                                    bubble_gas = "CH4dry_ppb", bubbles = bb))

  expect_lt(res$n_used, nrow(df))
})


test_that("the model selected by goFlux is reported", {

  df  <- sim_incubation(bubbles = NULL, seed = 108)
  res <- run_quiet(goAquaFlux.diffusive(df, gastype = "CH4dry_ppb", bubbles = NULL))

  expect_false(is.null(res$best.flux.output))
  expect_true(res$best.flux.output$model[1] %in% c("LM", "HM"))
})
