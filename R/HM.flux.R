#' Non-linear model for flux calculation
#'
#' Estimates a gas flux with a non-linear regression, the Hutchinson and Mosier
#' model, and then extracts values from the model fit.
#'
#' @param gas.meas numerical vector containing gas measurements (ppm or ppb)
#' @param time.meas numerical vector containing time stamps (seconds)
#' @param flux.term numerical value; flux term calculated with the function
#'                  \code{\link[goFlux]{flux.term}}
#' @param Ct numerical value; gas concentration after \emph{t} seconds (ppm or ppb).
#' @param C0 numerical value; initial gas concentration at time 0 second (ppm or ppb).
#' @param k.max numerical value; kappa-max is the maximal curvature allowed in the
#'              Hutchinson and Mosier model. Calculated with the
#'              \code{\link[goFlux]{k.max}} function.
#' @param k.mult numerical value; a multiplier for the allowed k.max. Default is no
#'               multiplier (\code{k.mult = 1}).
#'
#' @seealso Look up the functions \code{\link[goFlux]{flux.term}} and
#'          \code{\link[goFlux]{k.max}} of this package for more
#'          information about these parameters.
#' @seealso See also the function \code{\link[goFlux]{LM.flux}} for
#'          information about the linear regression model used in this package.
#'
#' @references Hüppi et al. (2018). Restricting the nonlinearity parameter in
#' soil greenhouse gas flux calculation for more reliable flux estimates.
#' \emph{PloS one}, 13(7), e0200876.
#'
#' @references Hutchinson and Mosier (1981). Improved soil cover method for
#' field measurement of nitrous oxide fluxes.
#' \emph{Soil Science Society of America Journal}, 45(2), 311-316.
#'
#' @returns Returns a data frame with 11 columns: non-linear flux estimate,
#'          initial gas concentration (\code{HM.C0}), the assumed concentration
#'          of constant gas source below the surface (\code{HM.Ct}), slope at
#'          \code{t=0} (\code{HM.slope}), mean absolute error (\code{HM.MAE}),
#'          root mean square error (\code{HM.RMSE}), Akaike's information
#'          criterion corrected for small sample size (\code{LM.AICc}),
#'          standard error (\code{HM.SE}), relative standard error
#'          (\code{HM.se.rel}), coefficiend of determination (\code{HM.r2}),
#'          and curvature (kappa; (\code{HM.k})).
#'
#' @details
#' Flux estimate units are
#' \ifelse{html}{\out{µmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{µmol m^{-2}s^{-1}}{ASCII}}
#' (if initial concentration is ppm, e.g. CO2dry_ppm) and
#' \ifelse{html}{\out{nmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{nmol m^{-2}s^{-1}}{ASCII}}
#' (if initial concentration is ppb, e.g. CH4dry_ppb).
#'
#' The Hutchinson and Mosier model (HM) is a non-linear model, whose curvature
#' is controlled by the parameter kappa. A large kappa returns a strong
#' curvature. A maximum threshold for this parameter, kappa-max
#' (\code{\link[goFlux]{k.max}}), can be calculated from the linear
#' flux estimate (\code{\link[goFlux]{LM.flux}}), the minimal detectable
#' flux (\code{\link[goFlux]{MDF}}) and the time of chamber closure.
#' This limit of kappa-max is included in the
#' \code{\link[goFlux]{goFlux}} function, so that the non-linear flux
#' estimate cannot exceed this maximum curvature.
#'
#' The HM slope is multiplied by a \code{\link[goFlux]{flux.term}} which
#' is used to correct for water vapor inside the chamber, as well as convert the
#' units of the slope to obtain a flux estimate in nmol or
#' \ifelse{html}{\out{µmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{µmol m^{-2}s^{-1}}{ASCII}}.
#'
#' In \code{Ct} and \code{C0}, typical values for soil respiration and
#' 180 seconds of chamber closure are as follows for each \code{gastype}:
#' \itemize{
#'   \item CO2dry_ppm:  \code{Ct=1000} ; \code{C0=380}
#'   \item CH4dry_ppb:  \code{Ct=1800} ; \code{C0=2200}
#'   \item N2Odry_ppb:  \code{Ct=500} ; \code{C0=250}
#'   \item H2O_ppm:     \code{Ct=20000} ; \code{C0=10000}
#' }
#' Values of \code{Ct} and \code{C0} will vary depending on ecosystem type and
#' chamber application scheme.
#'
#' @include goFlux-package.R
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

  # Define the Hutchinson and Mosier model
  HMmod <- conc ~ Ci+(C0-Ci)*exp(-k*t)

  # Define the initial parameters for the fitting of the model
  start <- list(Ci=Ct, C0=C0, k=0)

  # Run the model using the nlsLM function from the minpack.lm package
  HM <- try(nlsLM(HMmod,
                  data = cbind.data.frame(conc = gas.meas, t = time.meas),
                  lower = c(Ci=0, C0=0, k=-1),
                  upper = c(Ci=Inf, C0=Inf, k=k.max*k.mult),
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
    HM.SE <- deltamethod(as.formula(form), coef(HM), vcov(HM))

    # Indices of model fit
    HM.AICc <- AICc(HM)
    HM.se.rel <- (HM.SE / HM.flux) * 100
    HM.r2 <- as.numeric(summary(lm(fitted(HM) ~ gas.meas))[9])[1]
    HM.RMSE <- RMSE(gas.meas, fitted(HM))
    HM.MAE <- MAE(gas.meas, fitted(HM))

    # Store results in new data table
    HM_results <- cbind.data.frame(HM.flux, HM.C0, HM.Ci, HM.slope, HM.MAE,
                                   HM.RMSE, HM.AICc, HM.SE, HM.se.rel, HM.r2, HM.k)

  } else {
    HM_results <- cbind.data.frame(HM.flux = NA, HM.C0 = NA, HM.Ci = NA,
                                   HM.slope = NA, HM.MAE = NA, HM.RMSE = NA,
                                   HM.AICc = NA, HM.SE = NA, HM.se.rel = NA,
                                   HM.r2 = NA, HM.k = NA)
  }
}
