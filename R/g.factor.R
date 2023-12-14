#' G-factor
#'
#' Calculates the g-factor: the ratio between the flux estimates of a non-linear
#' regression model (e.g. Hutchinson and Mosier) and a linear regression model.
#' The recommended g-factor thresholds are <4 (flexible threshold), <2 (medium),
#' or <1.25 (conservative).
#'
#' @param HM.flux a numerical value; flux estimate from a non-linear
#'                regression model (e.g. Hutchinson and Mosier). Calculated with
#'                the \code{\link[GoFluxYourself]{HM.flux}} function of this package.
#' @param LM.flux a numerical value; flux estimate from a linear regression
#'                model. Calculated with the \code{\link[GoFluxYourself]{LM.flux}}
#'                function of this package.
#'
#' @seealso Look up the functions \code{\link[GoFluxYourself]{HM.flux}} and
#'          \code{\link[GoFluxYourself]{LM.flux}} of this package for more
#'          information about these parameters.
#' @seealso The g-factor is used in the function \code{\link[GoFluxYourself]{best.flux}}
#'          to select the best flux estimate.
#'
#' @references Hüppi et al. (2018). Restricting the nonlinearity parameter in
#' soil greenhouse gas flux calculation for more reliable flux estimates.
#' \emph{PloS one}, 13(7), e0200876.
#'
#' @return A numerical value
#'
#' @keywords internal
#'
g.factor <- function(HM.flux, LM.flux){
  HM.flux/LM.flux
}
