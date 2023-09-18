#' G-Factor
#'
#' Calculates the G-factor: the ratio between the result of a non-linear flux
#' calculation model (e.g. Hutchinson and Mosier; HM) and the result of a
#' linear flux calculation model. The g-factor should be <4 (flexible threshold),
#' <2 (medium), or <1.25 (conservative).
#'
#' @param HM.flux a numerical value; flux estimate from a non-linear flux
#'                calculation model (e.g. Hutchinson and Mosier; HM)
#' @param LM.flux a numerical value; flux estimate from a linear flux
#'                calculation model
#'
#' @return a numerical value
#'
#' @keywords internal
#'
g.factor <- function(HM.flux, LM.flux){
  HM.flux/LM.flux
}
