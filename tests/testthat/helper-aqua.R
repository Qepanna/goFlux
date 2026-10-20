# ---------------------------------------------------------------------------
# Helpers shared by all goAquaFlux test files.
#
# testthat sources every helper-*.R file before running the tests, so the
# functions below are available everywhere without being exported by the
# package.
#
# The central idea of this test suite is that goAquaFlux is validated against
# incubations whose diffusive slope and ebullition magnitudes are known by
# construction. Recovering those known values is what the tests assert.
# ---------------------------------------------------------------------------


#' Simulate one floating-chamber incubation with a known partition
#'
#' Builds a data.frame with the columns goAquaFlux() requires, containing a
#' linear diffusive accumulation plus a set of Heaviside ebullition steps and
#' Gaussian noise, i.e. exactly the conceptual model of the paper:
#'   C(t) = C0 + slope * t + sum_k dC_k * H(t - tau_k) + eps(t)
#'
#' @param id          UniqueID of the incubation.
#' @param duration    Incubation length (s).
#' @param dt          Sampling interval (s).
#' @param CH4_0       Initial CH4 (ppb); CH4_slope its diffusive rate (ppb/s).
#' @param CO2_0       Initial CO2 (ppm); CO2_slope its diffusive rate (ppm/s).
#' @param bubbles     NULL, or data.frame(time =, magnitude =) giving the step
#'                    times (s) and sizes (ppb) injected into CH4.
#' @param co2_bubbles Logical; if TRUE the same steps are also injected into
#'                    CO2 (to test the abrupt-change branch of the diffusive
#'                    window selection for a non-bubble gas).
#' @param seed        RNG seed, so every test is exactly reproducible.
#'
#' @return A data.frame ready for goAquaFlux().
sim_incubation <- function(id          = "sim_01",
                           duration    = 600,
                           dt          = 1,
                           CH4_0       = 2000,
                           CH4_slope   = 0.5,
                           CO2_0       = 420,
                           CO2_slope   = 0.02,
                           bubbles     = NULL,
                           co2_bubbles = FALSE,
                           noise_CH4   = 1,
                           noise_CO2   = 0.05,
                           H2O_ppm     = 10000,
                           CH4_prec    = 2,
                           CO2_prec    = 0.5,
                           Area        = 0.09,
                           Vtot        = 15,
                           Pcham       = 101.325,
                           Tcham       = 15,
                           seed        = 1L) {

  set.seed(seed)

  Etime <- seq(0, duration, by = dt)
  n     <- length(Etime)

  CH4 <- CH4_0 + CH4_slope * Etime
  CO2 <- CO2_0 + CO2_slope * Etime

  if (!is.null(bubbles) && nrow(bubbles) > 0) {
    for (i in seq_len(nrow(bubbles))) {
      step <- as.numeric(Etime >= bubbles$time[i])
      CH4  <- CH4 + bubbles$magnitude[i] * step
      # CO2 is enriched far less than CH4 in a sediment bubble; the factor is
      # arbitrary and only used to exercise the abrupt-change detection.
      if (isTRUE(co2_bubbles)) CO2 <- CO2 + bubbles$magnitude[i] * 0.01 * step
    }
  }

  CH4 <- CH4 + stats::rnorm(n, 0, noise_CH4)
  CO2 <- CO2 + stats::rnorm(n, 0, noise_CO2)

  data.frame(
    UniqueID   = id,
    Etime      = Etime,
    flag       = 1,
    CH4dry_ppb = CH4,
    CO2dry_ppm = CO2,
    H2O_ppm    = H2O_ppm,
    CH4_prec   = CH4_prec,
    CO2_prec   = CO2_prec,
    Area       = Area,
    Vtot       = Vtot,
    Pcham      = Pcham,
    Tcham      = Tcham,
    stringsAsFactors = FALSE
  )
}


#' Bind several simulated incubations into one multi-incubation data.frame
sim_batch <- function(specs) {
  do.call(rbind, lapply(seq_along(specs), function(i) {
    do.call(sim_incubation, specs[[i]])
  }))
}


#' The chamber conversion term for the default simulated geometry
#'
#' Lets a test predict the expected flux analytically from a known
#' concentration rate, using the same conversion goFlux applies.
sim_flux.term <- function(Vtot = 15, Pcham = 101.325, Area = 0.09,
                          Tcham = 15, H2O_mol = 10000 / 1e6) {
  flux.term(Vtot, Pcham, Area, Tcham, H2O_mol)
}


#' Run an expression without the progress bar or messages
#'
#' goAquaFlux() prints a txtProgressBar, which is noise in test output.
#'
#' NOTE: deliberately named run_quiet(), not quiet(). goFlux imports SimDesign,
#' which exports a function called quiet(); because testthat evaluates tests
#' with the package namespace on the search path, a helper called quiet() is
#' ambiguous and SimDesign's version can win.
#'
#' NOTE: warnings are NOT suppressed. A warning raised during a test is a
#' finding, and hiding it here would defeat the purpose of the suite.
run_quiet <- function(expr) {
  out <- NULL
  utils::capture.output(
    out <- suppressMessages(expr)
  )
  out
}


#' Default set of bubbles used across several test files
default_bubbles <- function() {
  data.frame(time      = c(150, 300, 450),
             magnitude = c(200, 150, 250))
}
