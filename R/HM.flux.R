#' Non-linear model for flux calculation: the Hutchinson and Mosier model
#'
#' Estimates a gas flux with a non-linear model, and then extracts values from
#' the model fit, calculates standard error, R2, p-value and RMSE.
#'
#' @param gas.meas numerical vector containing gas measurements (ppm or ppb)
#' @param time.meas numerical vector containing time stamps (seconds)
#' @param flux.term numerical value; flux term calculated with the function flux.term
#' @param Ci numerical; gas concentration after i seconds (ppm or ppb). For forest
#'           soil conditions, use these estimates for the following gastypes:
#'           "CO2dry_ppm": Ci = 1000 ppm; "CH4dry_ppb": Ci = 1800 ppb;
#'           "N2Odry_ppb": Ci = 500 ppb; "H2O_ppm": Ci = 20000 ppm.
#' @param C0 numerical; initial gas concentration at time 0 second (ppm or ppb)
#'           For forest soil conditions, use these estimates for the following
#'           gastypes: "CO2dry_ppm": C0 = 380 ppm; "CH4dry_ppb": C0 = 2200 ppb;
#'           "N2Odry_ppb": C0 = 250 ppb; "H2O_ppm": C0 = 10000 ppm.
#' @param k numerical; kappa gives the curvature of the regression line (s-1).
#'          The default parameter for kappa is k = 0.005 s-1. k = 0.005 is the
#'          minimal value of kappa if LM.flux = MDF and t = 180 secs.
#' @param k.max numerical; kappa-max is the maximal curvature allowed in the
#'              Hutchinson and Mosier model. Calculated with the k.max function.
#' @param k.ratio numerical; a multiplier for the allowed k.max. Default is
#'                k.ratio = 1.
#'
#' @return a data.frame
#'
#' @include GoFluxYourself-package.R
#'
#' @keywords internal
#'
HM.flux <- function(gas.meas, time.meas, flux.term, k.max,
                    Ci = NULL, C0 = NULL, k.ratio = 1) {

  # Root Mean Squared Error (RMSE)
  RMSE <- function(gas.meas, fit.val){
    sqrt(sum((na.omit(gas.meas - fit.val))^2) / length(na.omit(gas.meas)))
  }

  # kappa limits
  if (k.max < 0) {
    kappa.max <- 1
    kappa.min <- k.max*k.ratio
  } else {
    kappa.max <- k.max*k.ratio
    kappa.min <- -1
  }

  # Define the Hutchinson and Mosier model
  HMmod <- conc ~ Ci+(C0-Ci)*exp(-k*t)

  # Define the initial parameters for the fitting of the model
  start <- list(Ci=Ci, C0=C0, k=0)

  # Run the model using the nlsLM function from the minpack.lm package
  HM <- try(nlsLM(HMmod,
                  data = cbind.data.frame(conc = gas.meas, t = time.meas),
                  lower = c(Ci=0, C0=0, k=kappa.min),
                  upper = c(Ci=Inf, C0=Inf, k=kappa.max),
                  start = start,
                  na.action = na.exclude,
                  control = nls.lm.control(
                    ftol = sqrt(.Machine$double.eps),
                    ptol = sqrt(.Machine$double.eps),
                    gtol = 0, diag = list(), epsfcn = 0, factor = 100,
                    maxfev = integer(), maxiter = 1000, nprint = 0)))

  # If no error encountered,
  if(!inherits(HM, "try-error")){

    # then extract values from the HM model
    HM.Ci <- coef(summary(HM))[1,1]
    HM.C0 <- coef(summary(HM))[2,1]
    HM.k <- coef(summary(HM))[3,1]

    # Calculate the flux from slope at t = 0
    HM.slope <- (HM.Ci - HM.C0)*HM.k

    # Multiply the slope of the model by the flux term calculated previously.
    HM.flux <- HM.slope * flux.term

    # Use the delta method to propagate total error to the flux calculation.
    form <- sprintf("~ (x1 - x2) * x3 * %f", flux.term)
    HM.se <- deltamethod(as.formula(form), coef(HM), vcov(HM))

    # Indices of the model fit
    # Relative flux standard error, R2 and RMSE
    HM.se.rel <- (HM.se / HM.flux) * 100
    HM.R2 <- as.numeric(summary(lm(fitted(HM) ~ gas.meas))[9])[1]
    HM.RMSE <- RMSE(gas.meas, fitted(HM))

    # Store results in new data table
    HM_results <- cbind.data.frame(HM.Ci, HM.C0, HM.k, HM.slope, HM.flux,
                                   HM.se, HM.se.rel, HM.R2, HM.RMSE)

  } else {
    HM_results <- cbind.data.frame(HM.Ci = NA, HM.C0 = NA, HM.k = NA,
                                   HM.slope = NA, HM.flux = NA, HM.se = NA,
                                   HM.se.rel = NA, HM.R2 = NA, HM.RMSE = NA)
  }
}