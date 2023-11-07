#' Non-linear model for flux calculation
#'
#' Estimates a gas flux with a non-linear regression, the Hutchinson and Mosier
#' model, and then extracts values from the model fit.
#'
#' @param gas.meas numerical vector containing gas measurements (ppm or ppb)
#' @param time.meas numerical vector containing time stamps (seconds)
#' @param flux.term numerical value; flux term calculated with the function
#'                  \code{\link[GoFluxYourself]{flux.term}}
#' @param Ct numerical; gas concentration after \emph{t} seconds (ppm or ppb).
#'           For typical forest soil conditions, use these estimates for the
#'           following \code{gastypes}:
#'           \itemize{
#'               \item "CO2dry_ppm": \code{Ct = 1000 ppm}
#'               \item "CH4dry_ppb": \code{Ct = 1800 ppb}
#'               \item "N2Odry_ppb": \code{Ct = 500 ppb}
#'               \item "H2O_ppm": \code{Ct = 20000 ppm}
#'             }
#' @param C0 numerical; initial gas concentration at time 0 second (ppm or ppb)
#'           For typical forest soil conditions, use these estimates for the
#'           following \code{gastypes}:
#'           \itemize{
#'               \item "CO2dry_ppm": \code{C0 = 380 ppm}
#'               \item "CH4dry_ppb": \code{C0 = 2200 ppb}
#'               \item "N2Odry_ppb": \code{C0 = 250 ppb}
#'               \item "H2O_ppm": \code{C0 = 10000 ppm}
#'             }
#' @param k.max numerical; kappa-max is the maximal curvature allowed in the
#'              Hutchinson and Mosier model. Calculated with the
#'              \code{\link[GoFluxYourself]{k.max}} function.
#' @param k.mult numerical; a multiplier for the allowed k.max. Default is no
#'               multiplier (\code{k.mult = 1}).
#'
#' @seealso Look up the functions \code{\link[GoFluxYourself]{flux.term}} and
#'          \code{\link[GoFluxYourself]{k.max}} of this package for more
#'          information about these parameters.
#' @seealso See also the function \code{\link[GoFluxYourself]{LM.flux}} for
#'          information about the linear regression model used in this package.
#'
#' @references Hüppi et al. (2018). Restricting the nonlinearity parameter in
#' soil greenhouse gas flux calculation for more reliable flux estimates.
#' \emph{PloS one}, 13(7), e0200876.
#'
#' @returns Returns a data frame with 10 columns: non-linear flux estimate,
#'          initial gas concentration (C0), final gas concentration (Ct), slope
#'          at \code{t=0}, mean absolute error (MAE), root mean square error
#'          (RMSE), standard error (se), relative se (se.rel),
#'          \ifelse{html}{\out{r<sup>2</sup>}}{\eqn{r^2}{ASCII}},
#'          and curvature (kappa).
#'
#' @details
#' Flux estimate units are
#' \ifelse{html}{\out{µmol/m<sup>2</sup>s}}{\eqn{µmol/m^{2}s}{ASCII}}
#' (if initial concentration is ppm, e.g. CO2dry_ppm) and
#' \ifelse{html}{\out{nmol/m<sup>2</sup>s}}{\eqn{nmol/m^{2}s}{ASCII}}
#' (if initial concentration is ppb, e.g. CH4dry_ppb).
#'
#' @include GoFluxYourself-package.R
#'
#' @keywords internal
#'
HM.flux <- function(gas.meas, time.meas, flux.term, k.max,
                    Ct = NULL, C0 = NULL, k.mult = 1) {

  # Root Mean Squared Error (RMSE)
  RMSE <- function(gas.meas, fit.val){
    sqrt(sum((na.omit(gas.meas - fit.val))^2) / length(na.omit(gas.meas)))
  }

  # Mean Absolute Error (MAE)
  MAE <- function(gas.meas, fit.val){
    sum(abs(na.omit(gas.meas - fit.val))) / length(na.omit(gas.meas))
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
  start <- list(Ci=Ct, C0=C0, k=0)

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
    # Relative flux standard error, r2, MAE and RMSE
    HM.se.rel <- (HM.se / HM.flux) * 100
    HM.r2 <- as.numeric(summary(lm(fitted(HM) ~ gas.meas))[9])[1]
    HM.RMSE <- RMSE(gas.meas, fitted(HM))
    HM.MAE <- MAE(gas.meas, fitted(HM))

    # Store results in new data table
    HM_results <- cbind.data.frame(HM.flux, HM.C0, HM.Ci, HM.slope, HM.se,
                                   HM.se.rel, HM.MAE, HM.RMSE, HM.r2, HM.k)

  } else {
    HM_results <- cbind.data.frame(HM.Ci = NA, HM.C0 = NA, HM.k = NA,
                                   HM.slope = NA, HM.flux = NA, HM.se = NA,
                                   HM.se.rel = NA, HM.r2 = NA, HM.RMSE = NA)
  }
}
