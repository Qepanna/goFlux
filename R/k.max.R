#' KAPPA MAX
#'
#' The parameter kappa determines the curvature of the non-linear regression in the
#' Hutchinson and Mosier model. A maximum threshold for this parameter, kappa-max,
#' can be calculated based on the minimal detectable flux (MDF), the linear
#' flux estimate and the measurement time. The unit of the kappa-max is s-1.
#'
#' @param MDF numerical; minimal detectable flux, calculated with the function `MDF()`
#' @param LM.flux numerical; flux estimate from a linear flux calculation model,
#'                calculated with the function `LM.flux()`.
#' @param t numerical; measurement time (enclosure time) (seconds)
#'
#' @return a numerical value
#'
#' @seealso Look up the functions \code{\link[GoFluxYourself]{MDF}} and
#'          \code{\link[GoFluxYourself]{LM.flux}} of this package
#'          for more information about these parameters.
#'
#' @references Hüppi et al. (2018). Restricting the nonlinearity parameter in soil greenhouse gas flux calculation for more reliable flux estimates. *PloS one*, 13(7), e0200876.
#'
#' @keywords internal
#'
k.max <- function(MDF, LM.flux, t) {
  LM.flux / (MDF * t)
}
