#' Non-linear model for flux calculation
#'
#' Estimates a gas flux with a non-linear regression, the Hutchinson and Mosier
#' model, and then extracts values from the model fit, calculates standard error,
#' r2, p-value and Root Mean Squared Error (RMSE).
#'
#' @param gas.meas numerical vector containing gas measurements (ppm or ppb)
#' @param time.meas numerical vector containing time stamps (seconds)
#' @param flux.term numerical value; flux term calculated with the function `flux.term()`
#' @param Ci numerical; gas concentration after i seconds (ppm or ppb). For typical
#'           forest soil conditions, use these estimates for the following gastypes:
#'           "CO2dry_ppm": Ci = 1000 ppm; "CH4dry_ppb": Ci = 1800 ppb;
#'           "N2Odry_ppb": Ci = 500 ppb; "H2O_ppm": Ci = 20000 ppm.
#' @param C0 numerical; initial gas concentration at time 0 second (ppm or ppb)
#'           For typical forest soil conditions, use these estimates for the following
#'           gastypes: "CO2dry_ppm": C0 = 380 ppm; "CH4dry_ppb": C0 = 2200 ppb;
#'           "N2Odry_ppb": C0 = 250 ppb; "H2O_ppm": C0 = 10000 ppm.
#' @param k numerical; kappa gives the curvature of the regression line (s-1).
#'          The default parameter for kappa is k = 0.005 s-1. k = 0.005 is the
#'          minimal value of kappa if LM.flux = MDF and t = 180 secs.
#' @param k.max numerical; kappa-max is the maximal curvature allowed in the
#'              Hutchinson and Mosier model. Calculated with the `k.max()` function.
#' @param k.mult numerical; a multiplier for the allowed k.max. Default is
#'               k.mult = 1.
#'
#' @seealso Look up the functions \code{\link[GoFluxYourself]{flux.term}} and
#'          \code{\link[GoFluxYourself]{k.max}} of this package for more
#'          information about these parameters.
#' @seealso See also the function \code{\link[GoFluxYourself]{LM.flux}} for
#'          information about the linear regression model used in this package.
#'
#' @references Hüppi et al. (2018). Restricting the nonlinearity parameter in soil greenhouse gas flux calculation for more reliable flux estimates. *PloS one*, 13(7), e0200876.
#'
#' @return a data.frame
#'
#' @include GoFluxYourself-package.R
#'
#' @keywords internal
#'
HM.flux <- function(gas.meas, time.meas, flux.term, k.max,
                    Ci = NULL, C0 = NULL, k.mult = 1) {

  # Root Mean Squared Error (RMSE)
  RMSE <- function(gas.meas, fit.val){
    sqrt(sum((na.omit(gas.meas - fit.val))^2) / length(na.omit(gas.meas)))
  }

  # Mean Absolute Deviation (MAD)
  MAD <- function(gas.meas, fit.val){
    sum(na.omit(gas.meas - fit.val)) / length(na.omit(gas.meas))
  }

  # kappa limits
  if (k.max < 0) {
    kappa.max <- 1
    kappa.min <- k.max*k.mult
  } else {
    kappa.max <- k.max*k.mult
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
    # Relative flux standard error, r2, MAD and RMSE
    HM.se.rel <- (HM.se / HM.flux) * 100
    HM.r2 <- as.numeric(summary(lm(fitted(HM) ~ gas.meas))[9])[1]
    HM.RMSE <- RMSE(gas.meas, fitted(HM))
    HM.MAD <- MAD(gas.meas, fitted(HM))

    # Store results in new data table
    HM_results <- cbind.data.frame(HM.flux, HM.C0, HM.Ci, HM.slope, HM.k,
                                   HM.se, HM.se.rel, HM.MAD, HM.RMSE, HM.r2)

  } else {
    HM_results <- cbind.data.frame(HM.Ci = NA, HM.C0 = NA, HM.k = NA,
                                   HM.slope = NA, HM.flux = NA, HM.se = NA,
                                   HM.se.rel = NA, HM.r2 = NA, HM.RMSE = NA)
  }
}
