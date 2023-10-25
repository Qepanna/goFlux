#' Flux term
#'
#' The "flux term" corrects the flux estimate for atmospheric pressure, air
#' temperature, the effect of water vapor, the volume of the chamber, and
#' the surface area covered by the chamber. This corrections changes the units
#' of the flux estimate from a concentration (ppm or ppb) per time (seconds)
#' into a molarity (µmol or nmol) per area (m2) per time (seconds).
#'
#' @param V_L numerical; total volume inside the chamber, tubes, instruments, etc. (L)
#' @param P_kPa numerical; atmospheric pressure (kPa)
#' @param A_cm2 numerical; area of the soil surface inside the chamber
#'              \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}}
#' @param T_C numerical; air temperature before chamber closure (Celsius)
#' @param H2O_mol numerical; water vapor concentration in the air before chamber
#'                closure (mol/mol)
#'
#' @details
#' Flux term units are
#' \ifelse{html}{\out{mol/m<sup>2</sup>}}{\eqn{mol/m^{2}}{ASCII}}
#'
#' @return a numerical value
#'
#' @keywords internal
#'
flux.term <- function(V_L, P_kPa, A_cm2, T_C, H2O_mol) {
  (V_L * P_kPa * (1 - H2O_mol)) / (8.314 * (A_cm2/10000) * (T_C + 273.15))
}
