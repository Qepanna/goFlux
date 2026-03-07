#' G-factor
#'
#' Calculates the g-factor: the ratio between the flux estimates of a non-linear
#' regression model (e.g. Hutchinson and Mosier) and a linear regression model.
#' The recommended g-factor thresholds are <4 (flexible threshold), <2 (medium),
#' or <1.25 (conservative).
#'
#' @param HM.flux a numerical value; flux estimate from a non-linear
#'                regression model (e.g. Hutchinson and Mosier). Calculated with
#'                the \code{\link[goFlux]{HM.flux}} function of this package.
#' @param LM.flux a numerical value; flux estimate from a linear regression
#'                model. Calculated with the \code{\link[goFlux]{LM.flux}}
#'                function of this package.
#'
#' @seealso Look up the functions \code{\link[goFlux]{HM.flux}} and
#'          \code{\link[goFlux]{LM.flux}} of this package for more
#'          information about these parameters.
#' @seealso The g-factor is used in the function \code{\link[goFlux]{best.flux}}
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
g.factor <- function(HM.flux, LM.flux, eps = .Machine$double.eps^0.5) {
  if (is.na(HM.flux) || is.na(LM.flux)) return(NA_real_)
  if (!is.finite(HM.flux) || !is.finite(LM.flux)) return(NA_real_)
  if (abs(LM.flux) < eps) return(NA_real_)
  HM.flux / LM.flux
}
