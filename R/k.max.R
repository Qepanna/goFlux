#' KAPPA max
#'
#' The parameter kappa determines the curvature of the non-linear flux in the
#' Hutchinson and Mosier model. A maximum threshold for this parameter, kappa-max
#' can be calculated based on the minimal detectable flux (MDF), the linear
#' flux estimate and the measurement time. The units of the kappa-max is s-1
#'
#' @param MDF numerical; minimal detectable flux, calculated with the function MDF
#' @param LM.flux numerical; flux estimate from a linear flux calculation model
#' @param t measurement time (enclosure time) (seconds)
#'
#' @return a numerical value
#'
#' @keywords internal
#'
k.max <- function(MDF, LM.flux, t) {
  LM.flux / (MDF * t)
}
