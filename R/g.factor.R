#' G-factor
#'
#' Calculates the g-factor: the ratio between the flux estimates of a non-linear
#' regression model (e.g. Hutchinson and Mosier) and a linear regression model.
#' The g-factor should be <4 (flexible threshold), <2 (medium), or <1.25 (conservative).
#'
#' @param HM.flux a numerical value; flux estimate from a non-linear
#'                regression model (e.g. Hutchinson and Mosier). Calculated with
#'                the `HM.flux()` function of this package.
#' @param LM.flux a numerical value; flux estimate from a linear regression
#'                model. Calculated with the `LM.flux()` function of this package.
#'
#' @seealso Look up the functions [HM.flux()] and [LM.flux()] of this package
#'          for more information about these parameters.
#'
#' @references Hüppi et al. (2018). Restricting the nonlinearity parameter in soil greenhouse gas flux calculation for more reliable flux estimates. *PloS one*, 13(7), e0200876.

#' @return a numerical value
#'
#' @keywords internal
#'
g.factor <- function(HM.flux, LM.flux){
  HM.flux/LM.flux
}
