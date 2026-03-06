#' Estimate ebullitive gas flux from detected bubbling events
#'
#' Computes the ebullitive component of gas flux during an aquatic chamber
#' incubation using bubbling events detected by \code{find.bubbles()}.
#' Ebullition flux is estimated from the summed concentration increase
#' associated with individual bubbling events and scaled by incubation time
#' and a flux conversion factor.
#'
#' The function also computes an independent total flux estimate using the
#' endpoint (two-point) concentration method for diagnostic comparison.
#' Several consistency checks are performed to detect situations where the
#' summed bubble magnitudes exceed the observed overall concentration change.
#'
#' @param df A data.frame containing the incubation time series. Must include
#'   an \code{Etime} column representing elapsed incubation time and the gas
#'   concentration variable specified by \code{gastype}.
#'
#' @param gastype Character string specifying the gas concentration variable
#'   to analyze (e.g. \code{"CH4dry_ppb"}, \code{"CO2dry_ppm"}).
#'
#' @param bubbles Optional data.frame containing bubbling events detected by
#'   \code{find.bubbles()}. Must include at least a \code{magnitude} column
#'   representing the estimated concentration step associated with each
#'   bubbling event and optionally a \code{SE} column containing the standard
#'   error of the magnitude estimate.
#'
#' @param flux.term Numeric conversion factor transforming concentration change
#'   per unit time into flux units. Typically derived from chamber geometry and
#'   environmental conditions (e.g., using a gas law conversion).
#'
#' @param final_window.min Numeric. Minimum time (in seconds) required after the
#'   last detected bubbling event to consider the full incubation length for
#'   flux calculation. If the remaining time after the last bubble is shorter
#'   than this threshold, the incubation time is truncated to the start of the
#'   last bubbling event.
#'
#' @param window_C0Cf Numeric. Duration (in seconds) of the initial and final
#'   windows used to compute mean starting and ending concentrations for the
#'   endpoint (two-point) flux estimate.
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{flux}{Estimated ebullitive flux.}
#'   \item{SE}{Standard error of the ebullitive flux estimate.}
#'   \item{F_tot2pts}{Total flux estimated using the endpoint (two-point) method.}
#'   \item{F_tot2pts.SE}{Standard error of the endpoint total flux estimate.}
#'   \item{n_bubbles}{Number of bubbling events used in the calculation.}
#'   \item{deltaC_bubbles}{Total concentration increase attributed to bubbling events.}
#'   \item{deltaC_total}{Observed total concentration change over the incubation.}
#'   \item{bubble_ratio}{Ratio between bubbling-induced concentration change and total change.}
#'   \item{inconsistent}{Logical flag indicating that summed bubble magnitudes exceed the observed endpoint concentration change.}
#'   \item{message}{Optional message returned when no bubbling events are detected
#'   or when bubbling magnitudes are unavailable.}
#' }
#'
#' @details
#' The ebullitive flux is computed as:
#'
#' \deqn{F_E = \frac{\sum \Delta C_{bubble}}{t_{inc}} \times K}
#'
#' where:
#' \itemize{
#'   \item \eqn{\sum \Delta C_{bubble}} is the summed concentration increase
#'   associated with bubbling events,
#'   \item \eqn{t_{inc}} is the effective incubation time,
#'   \item \eqn{K} is the flux conversion factor (\code{flux.term}).
#' }
#'
#' Standard errors are propagated assuming independence of individual bubble
#' magnitude estimates:
#'
#' \deqn{SE(F_E) = \frac{K}{t_{inc}} \sqrt{\sum SE_{bubble}^2}}
#'
#' Diagnostic checks are implemented to identify inconsistencies between the
#' cumulative bubble magnitude and the observed endpoint concentration change.
#' When the sum of bubble magnitudes exceeds the observed concentration change,
#' a warning is issued and the \code{inconsistent} flag is set to \code{TRUE}.
#'
#' @examples
#' ebullition_flux <- goAquaFlux.ebullition(
#'   df = incubation_data,
#'   gastype = "CH4dry_ppb",
#'   bubbles = bubbles,
#'   flux.term = K
#' )
#'
#' @seealso
#' \code{\link{find.bubbles}},
#' \code{\link{goAquaFlux.diffusive}},
#' \code{\link{goAquaFlux.total}}
#'
#' @include goFlux-package.R
#'
#' @keywords internal
goAquaFlux.ebullition <- function(df,
                                  gastype,
                                  bubbles,
                                  flux.term,
                                  final_window.min = 30,
                                  window_C0Cf = 10) {
  end_limit <- df$Etime[nrow(df)]


  # ----------- Retrieve initial and final concentrations Co and Cf
  # Define initial window
  idx0 <- df$Etime <= window_C0Cf

  # Define final window
  idxf <- df$Etime >= (end_limit - window_C0Cf) & df$Etime < end_limit

  # Compute means
  C0_vals <- df[[gastype]][idx0]
  Cf_vals <- df[[gastype]][idxf]

  C0 <- mean(C0_vals, na.rm = TRUE)
  Cf <- mean(Cf_vals, na.rm = TRUE)

  # Variances
  s0 <- var(C0_vals, na.rm = TRUE)
  sf <- var(Cf_vals, na.rm = TRUE)

  n0 <- sum(idx0)
  nf <- sum(idxf)



  deltaC_total <- Cf - C0

  # ----------------------------
  # Total flux and SE with 2-points method
  # ----------------------------

  F_tot2pts <- deltaC_total / end_limit * flux.term

  F_tot2pts.SE <- (flux.term / end_limit) *
    sqrt((s0 / n0) + (sf / nf))


  if (is.null(bubbles) || nrow(bubbles) == 0) {
    return(list(
      flux = 0,
      SE = 0,
      F_tot2pts = F_tot2pts,
      F_tot2pts.SE = F_tot2pts.SE,
      n_bubbles = 0,
      deltaC_bubbles = 0,
      deltaC_total = deltaC_total,
      bubble_ratio = 0,
      inconsistent = FALSE,
      message = "No bubbling events detected"
    ))

  }

  # Remove bubbles with invalid magnitude
  bubbles <- bubbles[!is.na(bubbles$magnitude), ]

  if (nrow(bubbles) == 0) {
    return(list(
      flux = NA,
      SE = NA,
      F_tot2pts = F_tot2pts,
      F_tot2pts.SE = F_tot2pts.SE,
      n_bubbles = NA,
      deltaC_bubbles = NA,
      deltaC_total = deltaC_total,
      bubble_ratio = NA,
      inconsistent = FALSE,
      message = "Bubble magnitudes unavailable"
    ))
  } else {
    # If not enough observations after last bubble, incubation_time is
    # set to the start of the last bubble.
    t.after_bubble <- end_limit - bubbles$end[nrow(bubbles)]
    if (t.after_bubble >= final_window.min){
      incubation_time = end_limit
    } else {
      incubation_time = bubbles$start[nrow(bubbles)]
    }
  }

  # Sum concentration increase
  deltaC_bubbles <- sum(bubbles$magnitude, na.rm = TRUE)



  ratio <- deltaC_bubbles / deltaC_total

  # issue a warning when deltaC_bubbles > deltaC_total
  flag_inconsistent <- FALSE
  if (!is.na(deltaC_total) && deltaC_bubbles > deltaC_total) {
    flag_inconsistent <- TRUE
    message = "Sum of bubble magnitudes exceeds endpoint concentration change"
  }


  # Variance propagation
  if ("SE" %in% colnames(bubbles)) {
    var_sum <- sum(bubbles$SE^2, na.rm = TRUE)
  } else {
    var_sum <- NA
  }


  # ----------------------------
  # Ebullition flux and SE estimates
  # ----------------------------

  flux <- deltaC_bubbles / incubation_time * flux.term

  if (!is.na(var_sum)) {
    flux_se <- (flux.term / incubation_time) * sqrt(var_sum)
  } else {
    flux_se <- NA
  }

  # Saving results in a list
  return(list(
    flux = flux,
    SE = flux_se,
    F_tot2pts = F_tot2pts,
    F_tot2pts.SE = F_tot2pts.SE,
    n_bubbles = nrow(bubbles),
    deltaC_bubbles = deltaC_bubbles,
    deltaC_total = deltaC_total,
    bubble_ratio = ratio,
    inconsistent = flag_inconsistent
  ))
}

