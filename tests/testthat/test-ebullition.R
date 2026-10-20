# ---------------------------------------------------------------------------
# Ebullitive flux and its uncertainty.
#
# F_ebull = (sum_k dC_k / t_inc) * flux.term, so with known step magnitudes
# the expected value is known exactly and can be asserted rather than merely
# smoke-tested.
# ---------------------------------------------------------------------------

find.bubbles          <- get("find.bubbles",          envir = asNamespace("goFlux"))
goAquaFlux.ebullition <- get("goAquaFlux.ebullition", envir = asNamespace("goFlux"))


test_that("no bubbles gives a zero ebullitive flux, not NA", {

  df <- sim_incubation(bubbles = NULL, seed = 201)
  ft <- sim_flux.term()

  res <- goAquaFlux.ebullition(df, gastype = "CH4dry_ppb", bubbles = NULL,
                               flux.term = ft)

  expect_equal(res$flux, 0)
  expect_equal(res$SE, 0)
  expect_equal(res$n_bubbles, 0)
  expect_false(res$flag_inconsistent)
  expect_match(res$message, "No bubbling")
})


test_that("the ebullitive flux equals the summed steps over the incubation", {

  bub <- default_bubbles()
  df  <- sim_incubation(bubbles = bub, seed = 202)
  bb  <- find.bubbles(df, "CH4dry_ppb")
  ft  <- sim_flux.term()

  res <- goAquaFlux.ebullition(df, gastype = "CH4dry_ppb", bubbles = bb,
                               flux.term = ft)

  # Exact relation to the detected magnitudes (not to the injected ones):
  # this isolates the conversion step from the detection step.
  t_inc    <- max(df$Etime)
  expected <- sum(bb$magnitude) / t_inc * ft
  expect_equal(res$flux, expected, tolerance = 1e-8)

  # And the detected magnitudes themselves recover the injected ones.
  expect_equal(res$deltaC_bubbles, sum(bub$magnitude), tolerance = 0.10)
  expect_equal(res$n_bubbles, 3L)
})


test_that("the flux scales linearly with the chamber conversion term", {

  bub <- default_bubbles()
  df  <- sim_incubation(bubbles = bub, seed = 203)
  bb  <- find.bubbles(df, "CH4dry_ppb")

  r1 <- goAquaFlux.ebullition(df, "CH4dry_ppb", bb, flux.term = 1)
  r2 <- goAquaFlux.ebullition(df, "CH4dry_ppb", bb, flux.term = 2)

  expect_equal(r2$flux, 2 * r1$flux, tolerance = 1e-10)
  expect_equal(r2$SE,   2 * r1$SE,   tolerance = 1e-10)
})


test_that("the standard error is the propagated quadrature of the step SEs", {

  bub <- default_bubbles()
  df  <- sim_incubation(bubbles = bub, seed = 204)
  bb  <- find.bubbles(df, "CH4dry_ppb")
  ft  <- sim_flux.term()

  res <- goAquaFlux.ebullition(df, "CH4dry_ppb", bb, flux.term = ft)

  t_inc    <- max(df$Etime)
  expected <- (ft / t_inc) * sqrt(sum(bb$SE^2))
  expect_equal(res$SE, expected, tolerance = 1e-8)
  expect_gt(res$SE, 0)
})


test_that("an implausible partition raises the inconsistency flag", {

  # Steps summing to far more than the observed endpoint change cannot be
  # real; the function must say so rather than return a silent number.
  df <- sim_incubation(bubbles = NULL, CH4_slope = 0.01, seed = 205)

  fake_bubbles <- data.frame(start = 100, end = 110, magnitude = 1e5,
                             SE = 10, slope = 0, n_used = 30)

  res <- goAquaFlux.ebullition(df, "CH4dry_ppb", fake_bubbles,
                               flux.term = sim_flux.term())

  expect_true(res$flag_inconsistent)
  expect_match(res$message, "exceeds")
})


test_that("bubbles with unusable magnitudes are reported, not silently dropped", {

  df <- sim_incubation(bubbles = NULL, seed = 206)

  na_bubbles <- data.frame(start = 100, end = 110, magnitude = NA_real_,
                           SE = NA_real_, slope = 0, n_used = 30)

  res <- goAquaFlux.ebullition(df, "CH4dry_ppb", na_bubbles,
                               flux.term = sim_flux.term())

  expect_true(is.na(res$flux))
  expect_match(res$message, "unavailable")
})


test_that("the incubation time is truncated when a bubble ends too late", {

  # A bubble close to the end leaves less than final_window.min seconds of
  # trailing record, so t_inc is cut back to that bubble's start and the flux
  # is correspondingly larger.
  bub <- data.frame(time = 580, magnitude = 300)
  df  <- sim_incubation(bubbles = bub, duration = 600, seed = 207)
  bb  <- find.bubbles(df, "CH4dry_ppb")

  skip_if(is.null(bb), "no bubble detected in this configuration")

  res_cut  <- goAquaFlux.ebullition(df, "CH4dry_ppb", bb, flux.term = 1,
                                    final_window.min = 30)
  res_keep <- goAquaFlux.ebullition(df, "CH4dry_ppb", bb, flux.term = 1,
                                    final_window.min = 0)

  expect_gt(res_cut$flux, res_keep$flux)
})


test_that("the two-point endpoint estimate is returned for comparison", {

  df <- sim_incubation(bubbles = default_bubbles(), seed = 208)
  bb <- find.bubbles(df, "CH4dry_ppb")

  res <- goAquaFlux.ebullition(df, "CH4dry_ppb", bb, flux.term = sim_flux.term())

  expect_true(is.finite(res$F_tot2pts))
  expect_true(is.finite(res$deltaC_total))
  expect_gt(res$F_tot2pts, 0)
})
