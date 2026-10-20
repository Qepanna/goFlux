# ---------------------------------------------------------------------------
# End-to-end workflow.
#
# Covers the user-facing entry point with and without flux separation, on one
# and on several incubations, for the bubble gas and for another gas, plus the
# argument checks.
# ---------------------------------------------------------------------------


# ---- Output structure ------------------------------------------------------

test_that("goAquaFlux returns the documented structure as a data.frame", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 301)
  res <- run_quiet(goAquaFlux(df, gastype = "CH4dry_ppb"))

  expect_type(res, "list")
  expect_named(res, c("flux_summary", "bubbles", "diffusive", "diagnostics"),
               ignore.order = TRUE)

  expected_cols <- c("UniqueID", "gastype",
                     "flux_total", "SE_total",
                     "flux_diffusive", "SE_diffusive", "n_obs.diffusion",
                     "flux_ebullition", "SE_ebullition",
                     "first_bubble_time", "model", "quality.check",
                     "ebullition.check", "closure")
  expect_true(all(expected_cols %in% names(res$flux_summary)),
              info = paste("missing:",
                           paste(setdiff(expected_cols, names(res$flux_summary)),
                                 collapse = ", ")))

  expect_equal(nrow(res$flux_summary), 1L)
  expect_equal(res$flux_summary$gastype[1], "CH4dry_ppb")
})


test_that("return_df = FALSE gives the per-incubation list", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 302)
  res <- run_quiet(goAquaFlux(df, gastype = "CH4dry_ppb", return_df = FALSE))

  expect_type(res, "list")
  expect_length(res, 1L)
  expect_named(res[[1]],
               c("flux_summary", "bubbles", "best.diffusive.flux", "diagnostics"),
               ignore.order = TRUE)
})


# ---- With flux separation --------------------------------------------------

test_that("with separation, the three flux terms are consistent", {

  bub <- default_bubbles()
  df  <- sim_incubation(bubbles = bub, CH4_slope = 0.5, seed = 303)

  res <- run_quiet(goAquaFlux(df, gastype = "CH4dry_ppb",
                              use_bubble_detection = TRUE))
  fs  <- res$flux_summary

  expect_true(is.finite(fs$flux_diffusive))
  expect_true(is.finite(fs$flux_ebullition))
  expect_gt(fs$flux_ebullition, 0)

  # Additivity, exactly.
  expect_equal(fs$flux_total, fs$flux_diffusive + fs$flux_ebullition,
               tolerance = 1e-8)

  # Error propagation, exactly.
  expect_equal(fs$SE_total,
               sqrt(fs$SE_diffusive^2 + fs$SE_ebullition^2),
               tolerance = 1e-8)

  # The known diffusive slope is recovered despite the three steps.
  expect_equal(fs$flux_diffusive, 0.5 * sim_flux.term(), tolerance = 0.10)

  # All three bubbles are reported in the bubbles table.
  expect_equal(nrow(res$bubbles), 3L)
  expect_true("UniqueID" %in% names(res$bubbles))
})


test_that("the partition recovers the known ebullitive contribution", {

  bub <- default_bubbles()
  df  <- sim_incubation(bubbles = bub, CH4_slope = 0.5, seed = 304)

  res <- run_quiet(goAquaFlux(df, gastype = "CH4dry_ppb"))
  fs  <- res$flux_summary

  expected_ebull <- sum(bub$magnitude) / max(df$Etime) * sim_flux.term()
  expect_equal(fs$flux_ebullition, expected_ebull, tolerance = 0.15)
})


# ---- Without flux separation -----------------------------------------------

test_that("use_bubble_detection = FALSE disables the partition", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 305)
  res <- run_quiet(goAquaFlux(df, gastype = "CH4dry_ppb",
                              use_bubble_detection = FALSE))
  fs  <- res$flux_summary

  expect_null(res$bubbles)

  # Current behaviour: detection is skipped, bubbles is NULL, and the
  # ebullition routine is still called with bubbles = NULL, so it reports a
  # flux of exactly 0 rather than NA.
  #
  # This is worth a second look. A 0 asserts "ebullition was assessed and
  # found absent", whereas the user asked for it not to be assessed at all;
  # NA would be the honest value. If goAquaFlux is changed to return NA here,
  # change this expectation to expect_true(is.na(fs$flux_ebullition)).
  expect_equal(fs$flux_ebullition, 0)

  # Without separation the whole record is fitted.
  expect_equal(fs$n_obs.diffusion, nrow(df))
  expect_equal(fs$flux_total, fs$flux_diffusive)
})


test_that("a bubble-free incubation gives a zero ebullitive flux", {

  df  <- sim_incubation(bubbles = NULL, CH4_slope = 0.5, seed = 306)
  res <- run_quiet(goAquaFlux(df, gastype = "CH4dry_ppb"))
  fs  <- res$flux_summary

  expect_equal(fs$flux_ebullition, 0)
  expect_equal(fs$flux_total, fs$flux_diffusive, tolerance = 1e-8)
  expect_equal(fs$flux_diffusive, 0.5 * sim_flux.term(), tolerance = 0.10)
  expect_match(fs$ebullition.check, "no bubble")
})


test_that("separation and no separation agree when there are no bubbles", {

  df <- sim_incubation(bubbles = NULL, seed = 307)

  with_sep <- run_quiet(goAquaFlux(df, "CH4dry_ppb", use_bubble_detection = TRUE))
  no_sep   <- run_quiet(goAquaFlux(df, "CH4dry_ppb", use_bubble_detection = FALSE))

  expect_equal(with_sep$flux_summary$flux_diffusive,
               no_sep$flux_summary$flux_diffusive, tolerance = 1e-8)
})


# ---- Other gases -----------------------------------------------------------

test_that("a non-bubble gas is processed as diffusion only", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 308)
  res <- run_quiet(goAquaFlux(df, gastype = "CO2dry_ppm"))
  fs  <- res$flux_summary

  expect_equal(fs$gastype[1], "CO2dry_ppm")
  expect_true(is.na(fs$flux_ebullition))
  expect_equal(fs$flux_total, fs$flux_diffusive)
  expect_true(is.finite(fs$flux_diffusive))
})


# ---- Several incubations ---------------------------------------------------

test_that("a batch of incubations is processed independently", {

  df <- sim_batch(list(
    list(id = "lake_a", bubbles = default_bubbles(), seed = 311),
    list(id = "lake_b", bubbles = NULL,              seed = 312),
    list(id = "lake_c", bubbles = data.frame(time = 400, magnitude = 300),
         seed = 313)
  ))

  res <- run_quiet(goAquaFlux(df, gastype = "CH4dry_ppb"))
  fs  <- res$flux_summary

  expect_equal(nrow(fs), 3L)
  expect_setequal(fs$UniqueID, c("lake_a", "lake_b", "lake_c"))

  # The bubble-free incubation must not inherit bubbles from its neighbours.
  expect_equal(fs$flux_ebullition[fs$UniqueID == "lake_b"], 0)
  expect_gt(fs$flux_ebullition[fs$UniqueID == "lake_a"], 0)

  expect_true(all(fs$UniqueID %in% unique(res$bubbles$UniqueID) |
                    fs$flux_ebullition == 0))
})


test_that("results do not depend on the order of the input rows", {

  df <- sim_batch(list(
    list(id = "lake_a", bubbles = default_bubbles(), seed = 321),
    list(id = "lake_b", bubbles = NULL,              seed = 322)
  ))

  res1 <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))
  res2 <- run_quiet(goAquaFlux(df[sample(nrow(df)), ], "CH4dry_ppb"))

  expect_equal(res1$flux_summary$flux_total,
               res2$flux_summary$flux_total, tolerance = 1e-8)
})


test_that("the workflow is deterministic", {

  df <- sim_incubation(bubbles = default_bubbles(), seed = 331)

  r1 <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))
  r2 <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))

  expect_equal(r1$flux_summary, r2$flux_summary)
})


# ---- Tuning pass-through ---------------------------------------------------

test_that("bubble.args reaches find.bubbles", {

  df <- sim_incubation(bubbles = default_bubbles(), seed = 341)

  res_default <- run_quiet(goAquaFlux(df, "CH4dry_ppb"))
  res_strict  <- run_quiet(goAquaFlux(df, "CH4dry_ppb",
                                      bubble.args = list(min_magnitude = 1e4)))

  expect_equal(nrow(res_default$bubbles), 3L)
  expect_null(res_strict$bubbles)
  expect_equal(res_strict$flux_summary$flux_ebullition, 0)
})


test_that("both bubble.method values run end to end", {

  df <- sim_incubation(bubbles = default_bubbles(), seed = 342)

  r_diff <- run_quiet(goAquaFlux(df, "CH4dry_ppb", bubble.method = "diff"))
  r_var  <- run_quiet(goAquaFlux(df, "CH4dry_ppb", bubble.method = "variance"))

  expect_equal(nrow(r_diff$bubbles), 3L)
  expect_equal(nrow(r_var$bubbles),  3L)
})


test_that("chamber geometry can be passed as arguments instead of columns", {

  df <- sim_incubation(bubbles = NULL, seed = 343)
  df$Area <- df$Vtot <- df$Pcham <- df$Tcham <- NULL

  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb",
                              Area = 0.09, Vtot = 15,
                              Pcham = 101.325, Tcham = 15))

  expect_true(is.finite(res$flux_summary$flux_diffusive))
})


# ---- Argument checks -------------------------------------------------------

test_that("invalid input is rejected with an informative error", {

  df <- sim_incubation(bubbles = NULL, seed = 351)

  expect_error(goAquaFlux(gastype = "CH4dry_ppb"), "'dataframe' is required")
  expect_error(goAquaFlux(df[0, ], "CH4dry_ppb"), "empty")
  expect_error(goAquaFlux(df), "'gastype' is required")
  expect_error(goAquaFlux(df, gastype = "not_a_gas"), "'gastype' must be one of")
  expect_error(goAquaFlux(df, gastype = "N2Odry_ppb"),
               "must contain a column that matches")

  expect_error(goAquaFlux(df[, setdiff(names(df), "Etime")], "CH4dry_ppb"),
               "'Etime'")
  expect_error(goAquaFlux(df[, setdiff(names(df), "flag")], "CH4dry_ppb"),
               "'flag'")
  expect_error(goAquaFlux(df[, setdiff(names(df), "UniqueID")], "CH4dry_ppb"),
               "UniqueID")
  expect_error(goAquaFlux(df[, setdiff(names(df), "CH4_prec")], "CH4dry_ppb"),
               "CH4_prec")
  expect_error(goAquaFlux(df[, setdiff(names(df), "Area")], "CH4dry_ppb"),
               "Area")
})


test_that("rows with flag != 1 are excluded", {

  df <- sim_incubation(bubbles = NULL, seed = 352)
  df$flag[df$Etime > 400] <- 0

  res <- run_quiet(goAquaFlux(df, "CH4dry_ppb", use_bubble_detection = FALSE))

  expect_equal(res$flux_summary$n_obs.diffusion, sum(df$flag == 1))
})


test_that("an absent water-vapour column is handled, with a warning", {

  df <- sim_incubation(bubbles = NULL, seed = 353)
  df$H2O_ppm <- NULL

  # Disabling the dilution correction must warn, not fail.
  expect_warning(
    utils::capture.output(res <- goAquaFlux(df, "CH4dry_ppb", H2O_col = NULL)),
    "H2O_col is NULL")

  expect_true(is.finite(res$flux_summary$flux_diffusive))
})
