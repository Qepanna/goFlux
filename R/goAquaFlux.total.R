#' Estimate total gas flux from incubation endpoint concentrations
#'
#' Computes the total gas flux from a floating chamber incubation using
#' the endpoint method, defined as the difference between the mean gas
#' concentration at the end and the beginning of the incubation divided
#' by the effective incubation time. Concentrations are averaged within
#' fixed observation windows at the start and end of the incubation to
#' reduce the influence of short-term variability.
#'
#' If bubbling events are detected, the final concentration window is
#' positioned immediately before the start of the last bubbling event
#' to avoid contamination of the endpoint estimate by ebullition-driven
#' concentration spikes.
#'
#' Flux uncertainty is estimated using standard error propagation from
#' the variance of concentration measurements within the initial and
#' final windows.
#'
#' @param dataframe Data frame containing incubation time series data.
#'   Must include a `POSIX.time` column and a column corresponding to
#'   the gas concentration specified by `gastype`. If available, the
#'   column `H2O_ppm` is used to correct for water vapor dilution.
#'
#' @param gastype Character string specifying the column name of the gas
#'   concentration to analyse (e.g., `"CH4dry_ppb"` or `"CO2dry_ppm"`).
#'
#' @param auxfile Data frame or list containing chamber and environmental
#'   parameters required for flux conversion:
#'   \describe{
#'     \item{Vtot}{Chamber volume (L)}
#'     \item{Pcham}{Chamber pressure (kPa)}
#'     \item{Area}{Chamber surface area (cm²)}
#'     \item{Tcham}{Chamber temperature (°C)}
#'   }
#'
#' @param bubbles Optional data frame describing bubbling events detected
#'   with \code{\link{find.bubbles}}. Must contain columns `start` and `end`
#'   defining bubbling periods in seconds since incubation start.
#'   If provided, the final concentration window is placed immediately
#'   before the last bubbling event.
#'
#' @param t.window Numeric. Duration (in seconds) of the observation windows
#'   used to estimate initial and final concentrations (default = 30 s).
#'
#' @param minimum_window Integer. Minimum number of observations required
#'   within a window to compute concentration statistics (default = 10).
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{flux}{Estimated total gas flux (nmol m⁻² s⁻¹)}
#'   \item{SE}{Standard error of the flux estimate derived from
#'   concentration variability within endpoint windows}
#'   \item{C0}{Mean concentration in the initial window}
#'   \item{Cf}{Mean concentration in the final window}
#'   \item{incubation_time}{Effective incubation duration used for the
#'   flux calculation (seconds)}
#' }
#'
#' If suitable endpoint windows cannot be identified (e.g., insufficient
#' observations or excessive bubbling), the function returns `NA` values
#' for flux and standard error.
#'
#' @details
#' The total flux is computed as:
#'
#' \deqn{
#' F = \frac{(C_f - C_0)}{t} \times K
#' }
#'
#' where \eqn{C_0} and \eqn{C_f} are the mean concentrations in the
#' initial and final observation windows, \eqn{t} is the effective
#' incubation duration, and \eqn{K} is a conversion factor derived
#' from chamber geometry and environmental conditions.
#'
#' The standard error of the flux estimate is obtained via error
#' propagation from the concentration variances in the two windows.
#'
#' @examples
#' total_flux <- goAquaFlux.total(
#'   dataframe = incubation_data,
#'   gastype = "CH4dry_ppb",
#'   auxfile = chamber_metadata,
#'   bubbles = bubbles
#' )
#'
#' total_flux$flux
#'
#' @export


flux_conversion <- function(V_L, P_kPa, A_cm2, T_C, H2O_mol) {
  (V_L * P_kPa * (1 - H2O_mol)) /
    (8.314 * (A_cm2 / 10000) * (T_C + 273.15))
}

goAquaFlux.total <- function(dataframe,
                             gastype,
                             auxfile,
                             bubbles = NULL,
                             t.window = 30,
                             minimum_window = 10) {

  # --- Build time vector
  time0 <- dataframe$POSIX.time[1]

  df <- data.frame(
    time = as.numeric(dataframe$POSIX.time - time0),
    conc = dataframe[[gastype]]
  )

  df <- df[!duplicated(df$time), ]

  T_total <- max(df$time)

  # ----------------------------
  # Determine final stable window
  # ----------------------------

  if (is.null(bubbles) || nrow(bubbles) == 0) {

    # No bubbling → use end window
    end_limit <- T_total

  } else {

    last_bubble_start <- bubbles$start[nrow(bubbles)]

    # If last bubble occurs near the end,
    # define Cf before that bubble
    end_limit <- last_bubble_start

  }

  # Define final window
  idxf <- df$time >= (end_limit - t.window) & df$time < end_limit

  if (sum(idxf) < minimum_window) {
    return(list(
      flux = NA,
      SE = NA,
      message = "No stable final window available",
      incubation_time = T_total
    ))
  }

  # ----------------------------
  # Initial window (always at start)
  # ----------------------------

  idx0 <- df$time <= t.window

  if (sum(idx0) < minimum_window) {
    return(list(
      flux = NA,
      SE = NA,
      message = "No stable initial window available",
      incubation_time = T_total
    ))
  }

  # Compute means
  C0_vals <- df$conc[idx0]
  Cf_vals <- df$conc[idxf]

  C0 <- mean(C0_vals, na.rm = TRUE)
  Cf <- mean(Cf_vals, na.rm = TRUE)

  # Variances
  s0 <- var(C0_vals, na.rm = TRUE)
  sf <- var(Cf_vals, na.rm = TRUE)

  n0 <- sum(idx0)
  nf <- sum(idxf)

  # ----------------------------
  # Conversion term
  # ----------------------------

  H2O_mol <- mean(dataframe$H2O_ppm[1:30], na.rm = TRUE) / 1e6

  K <- flux_conversion(
    V_L = auxfile$Vtot,
    P_kPa = auxfile$Pcham,
    A_cm2 = auxfile$Area,
    T_C = auxfile$Tcham,
    H2O_mol = H2O_mol
  )

  # ----------------------------
  # Flux and SE
  # ----------------------------

  effective_time <- end_limit  # time span used

  flux <- (Cf - C0) / effective_time * K

  flux_se <- (K / effective_time) *
    sqrt((s0 / n0) + (sf / nf))

  return(list(
    flux = flux,
    SE = flux_se,
    C0 = C0,
    Cf = Cf,
    incubation_time = effective_time
  ))
}
