#' Linear model for flux calculation
#'
#' Estimates a gas flux with a linear regression, and then extracts values from
#' the linear fit.
#'
#' @param gas.meas numerical vector containing gas measurements (ppm or ppb)
#' @param time.meas numerical vector containing time stamps (seconds)
#' @param flux.term numerical value; flux term calculated with the function
#'                  \code{\link[GoFluxYourself]{flux.term}}
#'
#' @seealso Look up the function \code{\link[GoFluxYourself]{flux.term}} of this
#'          package for more information about this parameter.
#' @seealso See also the function \code{\link[GoFluxYourself]{HM.flux}}
#'          for information about the non-linear regression model used in this package.
#'
#' @returns Returns a data frame with 11 columns: linear flux estimate, initial
#'          gas concentration (\code{LM.C0}), final gas concentration
#'          (\code{LM.Ct}), slope of linear regression (\code{LM.slope}), mean
#'          absolute error (\code{LM.MAE}), root mean square error
#'          (\code{LM.RMSE}), Akaike information criterion corrected for small
#'          sample size (\code{LM.AICc}), standard error (\code{LM.SE}),
#'          relative standard error (\code{LM.se.rel}),
#'          \ifelse{html}{\out{r<sup>2</sup>}}{\eqn{r^2}{ASCII}} (\code{LM.r2}),
#'          and p-value (\code{LM.p.val}).
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
LM.flux <- function(gas.meas, time.meas, flux.term) {

  # Root Mean Squared Error (RMSE)
  RMSE <- function(gas.meas, fit.val){
    sqrt(sum((na.omit(gas.meas - fit.val))^2) / length(na.omit(gas.meas)))
  }

  # Mean Absolute Error (MAE)
  MAE <- function(gas.meas, fit.val){
    sum(abs(na.omit(gas.meas - fit.val))) / length(na.omit(gas.meas))
  }

  # Linear model
  LM <- lm(gas.meas ~ time.meas)

  # Extract values from the linear fit
  LM.C0 <- summary(LM)[[4]][1,1]
  LM.slope <- summary(LM)[[4]][2,1]

  # Calculate LM.Ct
  LM.Ct <- LM.slope * last(time.meas) + LM.C0

  # Multiply the slope of the model by the flux term. The flux term corrects
  # for water vapor at the start of the measurement, as well as total volume,
  # pressure, area, and temperature. Unique flux term per measurement.
  LM.flux <- LM.slope * flux.term

  # Use the delta method to propagate total error to the flux calculation.
  form <- sprintf("~ x2 * %f", flux.term)
  LM.SE <- deltamethod(as.formula(form), coef(LM), vcov(LM))

  # Indices of model fit
  LM.AICc <- AICc(LM)
  LM.se.rel <- (LM.SE / LM.flux) * 100
  LM.r2 <- as.numeric(summary(lm(fitted(LM) ~ gas.meas))[9])[1]
  LM.p.val <- summary(LM)[[4]][2,4]
  LM.RMSE <- RMSE(gas.meas, fitted(LM))
  LM.MAE <- MAE(gas.meas, fitted(LM))

  # Store results in new data table
  LM_results <- cbind.data.frame(LM.flux, LM.C0, LM.Ct, LM.slope, LM.MAE, LM.RMSE,
                                 LM.AICc, LM.SE, LM.se.rel, LM.r2, LM.p.val)

  return(LM_results)
}
