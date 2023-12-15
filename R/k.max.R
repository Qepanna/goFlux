#' KAPPA MAX
#'
#' The parameter kappa determines the curvature of the non-linear regression in the
#' Hutchinson and Mosier model. A maximum threshold for this parameter, kappa-max,
#' can be calculated based on the minimal detectable flux (MDF), the linear
#' flux estimate and the measurement time. The unit of the kappa-max is
#' \ifelse{html}{\out{s<sup>-1</sup>}}{\eqn{s^{-1}}{ASCII}}.
#'
#' @param MDF numerical value; minimal detectable flux, calculated with the function
#'            \code{\link[goFlux]{MDF}}
#' @param LM.flux numerical value; flux estimate from a linear flux calculation model,
#'                calculated with the function \code{\link[goFlux]{LM.flux}}
#' @param t numerical value; measurement time (enclosure time) (seconds)
#'
#' @return A numerical value: the kappa-max.
#'
#' @seealso Look up the functions \code{\link[goFlux]{MDF}} and
#'          \code{\link[goFlux]{LM.flux}} of this package
#'          for more information about these parameters.
#' @seealso Kappa max is used in the function \code{\link[goFlux]{HM.flux}}
#'          to restrict the maximum allowed curvature in the Hutchinson and
#'          Mosier model.
#'
#' @references Hüppi et al. (2018). Restricting the nonlinearity parameter in
#' soil greenhouse gas flux calculation for more reliable flux estimates.
#' \emph{PloS one}, 13(7), e0200876.
#'
#' @keywords internal
#'
k.max <- function(MDF, LM.flux, t) {
  LM.flux / (MDF * t)
}
