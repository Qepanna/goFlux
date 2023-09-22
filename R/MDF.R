#' Minimal Detectable Flux (MDF)
#'
#' The minimal detectable flux based on instrument precision, measurements time,
#' and the number of measurement points.
#'
#' @param p numerical; precision of the instrument (same units as measured gas; ex. ppm)
#' @param t numerical; measurement time (enclosure time; seconds)
#' @param n numerical; number of measurements during measurement time
#' @param flux.term numerical; flux term calculated with the function `flux.term()`
#'
#' @return a numerical value
#'
#' @references Brechet et al. (2021). Simultaneous tree stem and soil greenhouse gas (CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O) flux measurements: a novel design for continuous monitoring towards improving flux estimates and temporal resolution. *New Phytologist*, 230(6), 2487-2500.
#' @references Christiansen et al. (2015). Comparison of CO<sub>2</sub>, CH<sub>4</sub> and N<sub>2</sub>O soil-atmosphere exchange measured in static chambers with cavity ring-down spectroscopy and gas chromatography. *Agricultural and Forest Meteorology*, 211, 48-57.
#'
#' @keywords internal
#'
MDF <- function(p, t, n, flux.term) {
  (p / (t * sqrt(n))) * flux.term
}
