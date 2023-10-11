#' Linear model for flux calculation
#'
#' Estimates a gas flux with a linear regression, and then extracts values from
#' the linear fit, calculates standard error, r~2~, p-value and Root Mean Squared
#' Error (RMSE).
#'
#' @param gas.meas numerical vector containing gas measurements (ppm or ppb)
#' @param time.meas numerical vector containing time stamps (seconds)
#' @param flux.term numerical value; flux term calculated with the function `flux.term()`
#'
#' @seealso Look up the function \code{\link[GoFluxYourself]{flux.term}} of this
#'          package for more information about this parameter.
#' @seealso See also the function \code{\link[GoFluxYourself]{HM.flux}}
#'          for information about the non-linear regression model used in this package.
#'
#' @return a data.frame
#'
#' @include GoFluxYourself-package.R
#'
#' @keywords internal
#'
LM.flux <- function(gas.meas, time.meas, flux.term) {

  # Root Mean Squared Error (RMSE)
  RMSE <- function(gas.meas, fit.val){
    sqrt(sum((na.omit(gas.meas - fit.val))^2) / length(na.omit(gas.meas)))
  }

  # Mean Absolute Deviation (MAD)
  MAD <- function(gas.meas, fit.val){
    sum(na.omit(gas.meas - fit.val)) / length(na.omit(gas.meas))
  }

  # Linear model
  LM <- lm(gas.meas ~ time.meas)

  # Extract values from the linear fit
  LM.C0 <- summary(LM)[[4]][1,1]
  LM.slope <- summary(LM)[[4]][2,1]

  # Calculate LM.Ci
  LM.Ci <- LM.slope * last(time.meas) + LM.C0

  # Multiply the slope of the model by the flux term. The flux term corrects
  # for water vapor at the start of the measurement, as well as total volume,
  # pressure, area, and temperature. Unique flux term per measurement.
  LM.flux <- LM.slope * flux.term

  # Use the delta method to propagate total error to the flux calculation.
  form <- sprintf("~ x2 * %f", flux.term)
  LM.se <- deltamethod(as.formula(form), coef(LM), vcov(LM))

  # Indices of the model fit
  # Relative flux standard error, r2, p-value, MAD and RMSE
  LM.se.rel <- (LM.se / LM.flux) * 100
  LM.r2 <- as.numeric(summary(lm(fitted(LM) ~ gas.meas))[9])[1]
  LM.p.val <- summary(LM)[[4]][2,4]
  LM.RMSE <- RMSE(gas.meas, fitted(LM))
  LM.MAD <- MAD(gas.meas, fitted(LM))

  # Store results in new data table
  LM_results <- cbind.data.frame(LM.flux, LM.C0, LM.Ci, LM.slope, LM.se,
                                 LM.se.rel, LM.MAD, LM.RMSE, LM.r2, LM.p.val)

  return(LM_results)
}
