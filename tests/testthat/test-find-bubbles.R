# ---------------------------------------------------------------------------
# Ebullition detection.
#
# These are the core scientific tests of the paper: on series where the step
# times and magnitudes are known by construction, the detector must find them,
# and on series with no steps it must find nothing.
# ---------------------------------------------------------------------------

find.bubbles <- get("find.bubbles", envir = asNamespace("goFlux"))


# ---- True negatives --------------------------------------------------------

test_that("no events are returned on a purely diffusive series", {

  df <- sim_incubation(bubbles = NULL, CH4_slope = 0.5, seed = 11)
  expect_null(find.bubbles(df, bubble_source = "CH4dry_ppb"))
})


test_that("no events are returned on a steep but smooth series", {

  # The relative guard (min_ratio) must stop a strong diffusive trend from
  # being mistaken for ebullition.
  df <- sim_incubation(bubbles = NULL, CH4_slope = 5, seed = 12)
  expect_null(find.bubbles(df, bubble_source = "CH4dry_ppb"))
})


test_that("no events are returned on a perfectly flat series", {

  # MAD = 0: standardisation is undefined and detection must bail out.
  df <- sim_incubation(bubbles = NULL, CH4_slope = 0, noise_CH4 = 0, seed = 13)
  expect_null(find.bubbles(df, bubble_source = "CH4dry_ppb"))
})


test_that("a downward step is never reported as ebullition", {

  df <- sim_incubation(bubbles = data.frame(time = 300, magnitude = -200),
                       seed = 14)
  expect_null(find.bubbles(df, bubble_source = "CH4dry_ppb"))
})


# ---- True positives --------------------------------------------------------

test_that("a single known step is detected at the right time and size", {

  bub <- data.frame(time = 300, magnitude = 200)
  df  <- sim_incubation(bubbles = bub, seed = 21)

  res <- find.bubbles(df, bubble_source = "CH4dry_ppb")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)

  # Step time recovered within one detection window.
  expect_lt(abs(res$t.step[1] - 300), 15)

  # Magnitude recovered within 10 %.
  expect_equal(res$magnitude[1], 200, tolerance = 0.10)

  # A positive, finite standard error must accompany every magnitude, since
  # the uncertainty of F_ebull is built from it.
  expect_true(is.finite(res$SE[1]))
  expect_gt(res$SE[1], 0)
})


test_that("three known steps are all detected, in order", {

  bub <- default_bubbles()
  df  <- sim_incubation(bubbles = bub, seed = 22)

  res <- find.bubbles(df, bubble_source = "CH4dry_ppb")

  expect_equal(nrow(res), 3L)
  expect_true(all(diff(res$start) > 0))

  for (i in 1:3) {
    expect_lt(abs(res$t.step[i] - bub$time[i]), 15)
  }

  # The summed magnitude is what becomes the ebullitive flux, so it is the
  # quantity that matters most.
  expect_equal(sum(res$magnitude), sum(bub$magnitude), tolerance = 0.10)
})


test_that("the returned table carries every column the workflow consumes", {

  df  <- sim_incubation(bubbles = default_bubbles(), seed = 23)
  res <- find.bubbles(df, bubble_source = "CH4dry_ppb")

  required <- c("start", "end", "magnitude", "SE", "slope", "n_used",
                "t.step", "t.peak", "intercept", "fit.start", "fit.end")
  expect_true(all(required %in% names(res)),
              info = paste("missing:",
                           paste(setdiff(required, names(res)), collapse = ", ")))

  expect_true(all(res$n_used >= 10))        # reg.min.obs
  expect_true(all(res$end >= res$start))
})


# ---- Robustness of the two dispersion modes --------------------------------

test_that("both dispersion modes detect a clear step", {

  df <- sim_incubation(bubbles = data.frame(time = 300, magnitude = 200),
                       seed = 31)

  res_diff <- find.bubbles(df, bubble_source = "CH4dry_ppb", method = "diff")
  res_var  <- find.bubbles(df, bubble_source = "CH4dry_ppb", method = "variance")

  expect_equal(nrow(res_diff), 1L)
  expect_equal(nrow(res_var),  1L)
})


test_that("the diff mode stays sensitive under a steep diffusive trend", {

  # This is the claim made in the paper for taking diff as the default: a
  # strong trend inflates the rolling variance but not the differenced
  # dispersion, so detection should survive.
  df <- sim_incubation(bubbles = data.frame(time = 300, magnitude = 200),
                       CH4_slope = 5, seed = 32)

  res <- find.bubbles(df, bubble_source = "CH4dry_ppb", method = "diff")

  expect_equal(nrow(res), 1L)
  expect_equal(res$magnitude[1], 200, tolerance = 0.15)
})


# ---- Gates and tuning ------------------------------------------------------

test_that("min_magnitude filters steps below the stated size", {

  df <- sim_incubation(bubbles = data.frame(time = 300, magnitude = 200),
                       seed = 41)

  expect_equal(nrow(find.bubbles(df, "CH4dry_ppb", min_magnitude = 50)), 1L)
  expect_null(find.bubbles(df, "CH4dry_ppb", min_magnitude = 1000))
})


test_that("k never increases the number of detections", {

  # k enters the threshold as median(D) + k * MAD(D). A clear bubble produces a
  # dispersion spike orders of magnitude above median(D), so k has to be very
  # large before it bites: raising it can only ever remove events, never add
  # them, and that monotonicity is what is asserted here.
  df <- sim_incubation(bubbles = data.frame(time = 300, magnitude = 200),
                       seed = 42)

  n_events <- function(k) {
    res <- find.bubbles(df, "CH4dry_ppb", k = k)
    if (is.null(res)) 0L else nrow(res)
  }

  counts <- vapply(c(2, 4, 20, 100, 500), n_events, integer(1))

  expect_equal(counts[2], 1L)                 # the documented default
  expect_true(all(diff(counts) <= 0))
})


test_that("a k large enough to dominate the threshold suppresses detection", {

  df <- sim_incubation(bubbles = data.frame(time = 300, magnitude = 200),
                       seed = 42)

  expect_null(find.bubbles(df, "CH4dry_ppb", k = 1e6))
})


test_that("min_ratio suppresses detection when contrast is insufficient", {

  df <- sim_incubation(bubbles = data.frame(time = 300, magnitude = 200),
                       seed = 43)

  expect_null(find.bubbles(df, "CH4dry_ppb", min_ratio = 1e6))
})


test_that("both magnitude models run and agree on a clean step", {

  # With no overshoot in the simulated signal, the settled step and the plain
  # step should coincide.
  df <- sim_incubation(bubbles = data.frame(time = 300, magnitude = 200),
                       seed = 44)

  res_re <- find.bubbles(df, "CH4dry_ppb", magnitude.model = "step_reequil")
  res_st <- find.bubbles(df, "CH4dry_ppb", magnitude.model = "step")

  expect_equal(nrow(res_re), 1L)
  expect_equal(nrow(res_st), 1L)
  expect_equal(res_re$magnitude[1], res_st$magnitude[1], tolerance = 0.10)
})


# ---- Input validation ------------------------------------------------------

test_that("invalid input is rejected with an informative error", {

  df <- sim_incubation(seed = 51)

  expect_error(find.bubbles(df[1:20, ], "CH4dry_ppb"),
               "At least 30 observations")
  expect_error(find.bubbles(df, "CH4dry_ppb", window.size = 2),
               "window.size")
  expect_error(find.bubbles(df, "CH4dry_ppb", var.quantile = 0),
               "var.quantile")
  expect_error(find.bubbles(df, "CH4dry_ppb", var.quantile = 1),
               "var.quantile")
})


# ---- Reproducibility -------------------------------------------------------

test_that("detection is deterministic", {

  df <- sim_incubation(bubbles = default_bubbles(), seed = 61)

  expect_identical(find.bubbles(df, "CH4dry_ppb"),
                   find.bubbles(df, "CH4dry_ppb"))
})


test_that("detection runs without warnings on an ordinary incubation", {

  # The tau profiling in the step_reequil model calls optimize() on an
  # objective that returns Inf when a candidate fit fails, which makes
  # optimize() warn "Inf replaced by maximum positive value" once per failed
  # evaluation. Users should not see that; returning a large finite value
  # instead of Inf fixes it. This test guards the fix.
  df <- sim_incubation(bubbles = default_bubbles(), seed = 62)

  expect_no_warning(find.bubbles(df, "CH4dry_ppb",
                                 magnitude.model = "step_reequil"))
})
