#' Linear model for flux calculation
#'
#' Estimates a gas flux with a linear regression, and then extracts values from
#' the linear fit, calculates standard error, R2, p-value and RMSE.
#'
#' @param gas.meas numerical vector containing gas measurements (ppm or ppb)
#' @param time.meas numerical vector containing time stamps (seconds)
#' @param flux.term numerical value; flux term calculated with the function flux.term
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

  # Linear model
  LM <- lm(gas.meas ~ time.meas)

  # Extract values from the linear fit
  LM.intercept <- summary(LM)[[4]][1,1]
  LM.slope <- summary(LM)[[4]][2,1]

  # Multiply the slope of the model by the flux term. The flux term corrects
  # for water vapor at the start of the measurement, as well as total volume,
  # pressure, area, and temperature. Unique flux term per measurement.
  LM.flux <- LM.slope * flux.term

  # Use the delta method to propagate total error to the flux calculation.
  form <- sprintf("~ x2 * %f", flux.term)
  LM.se <- deltamethod(as.formula(form), coef(LM), vcov(LM))

  # Indices of the model fit
  # Relative flux standard error, R2, p-value, RMSE and nRMSE
  LM.se.rel <- (LM.se / LM.flux) * 100
  LM.R2 <- as.numeric(summary(lm(fitted(LM) ~ gas.meas))[9])[1]
  LM.p.val <- summary(LM)[[4]][2,4]
  LM.RMSE <- RMSE(gas.meas, fitted(LM))

  # Store results in new data table
  LM_results <- cbind.data.frame(LM.slope, LM.intercept, LM.flux, LM.RMSE,
                                 LM.se, LM.se.rel, LM.R2, LM.p.val)

  return(LM_results)
}
