#' Minimal Detectable Flux (MDF)
#'
#' The minimal detectable flux based on instrument precision and the
#' measurement time.
#'
#' @param p numerical; precision of the instrument (same units as measured gas; ex. ppm)
#' @param t numerical; measurement time (enclosure time; seconds)
#' @param flux.term numerical; flux term calculated with the function `flux.term()`
#'
#' @return a numerical value
#'
#' @seealso \code{\link[GoFluxYourself]{flux.term}}
#'
#' @references Christiansen et al. (2015). Comparison of CO<sub>2</sub>, CH<sub>4</sub> and N<sub>2</sub>O soil-atmosphere exchange measured in static chambers with cavity ring-down spectroscopy and gas chromatography. *Agricultural and Forest Meteorology*, 211, 48-57.
#'
#' @keywords internal
#'
MDF <- function(p, t, flux.term) {
  (p / t) * flux.term
}
