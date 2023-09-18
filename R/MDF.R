#' Minimal Detectable Flux (MDF)
#'
#' The minimal detectable flux based on instrument precision, measurements time,
#' and the number of measurement points.
#'
#' @param p numerical; precision of the instrument (same units as measured gas; ex. ppm)
#' @param t numerical; measurement time (enclosure time; seconds)
#' @param n numerical; number of measurements during measurement time
#' @param flux.term numerical; flux term calculated with the function flux.term
#'
#' @return a numerical value
#'
#' @keywords internal
#'
MDF <- function(p, t, n, flux.term) {
  (p / (t * sqrt(n))) * flux.term
}
