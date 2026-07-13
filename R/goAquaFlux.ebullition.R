#' Estimate ebullitive gas flux from detected bubbling events
#'
#' Computes the ebullitive component of a gas flux during an aquatic chamber
#' incubation, using bubbling events detected by \code{\link{find.bubbles}}.
#' The ebullitive flux is estimated from the summed concentration steps
#' attributed to individual bubbling events, divided by the effective incubation
#' time and scaled by the flux conversion term. An independent two-point
#' (endpoint) estimate of the total flux is also returned for diagnostic
#' comparison, together with several consistency checks.
#'
#' @param df A data.frame for a single incubation. Must contain an \code{Etime}
#'   column (elapsed time, seconds, starting at 0) and the gas concentration
#'   column named by \code{gastype}. Rows are assumed to be sorted by
#'   \code{Etime}.
#'
#' @param gastype Character; name of the gas concentration column
#'   (e.g. \code{"CH4dry_ppb"}).
#'
#' @param bubbles Data.frame of bubbling events from \code{\link{find.bubbles}}
#'   (or \code{NULL}). Must contain \code{start}, \code{end} and \code{magnitude};
#'   an optional \code{SE} column enables error propagation.
#'
#' @param flux.term Numeric; conversion factor turning a concentration change
#'   per unit time into flux units (from \code{\link[goFlux]{flux.term}}).
#'
#' @param final_window.min Numeric; minimum time (seconds) that must remain
#'   after the last bubble for the full deployment length to be used as the
#'   incubation time. If less time remains, the incubation time is truncated to
#'   the start of the last bubbling event. Default \code{30}.
#'
#' @param window_C0Cf Numeric; duration (seconds) of the initial and final
#'   windows used to compute mean start and end concentrations for the two-point
#'   endpoint estimate. Default \code{10}.
#'
#' @return A named list, always with the same fields:
#' \describe{
#'   \item{flux}{Ebullitive flux (0 when no bubbles, \code{NA} when magnitudes
#'     are unavailable).}
#'   \item{SE}{Standard error of the ebullitive flux (\code{NA} if \code{bubbles}
#'     has no \code{SE} column).}
#'   \item{F_tot2pts}{Two-point endpoint estimate of the total flux.}
#'   \item{F_tot2pts.SE}{Standard error of the endpoint estimate.}
#'   \item{n_bubbles}{Number of bubbling events used.}
#'   \item{deltaC_bubbles}{Summed concentration increase attributed to bubbles.}
#'   \item{deltaC_total}{Observed endpoint concentration change (Cf - C0).}
#'   \item{bubble_ratio}{\code{deltaC_bubbles / deltaC_total}.}
#'   \item{flag_inconsistent}{Logical; \code{TRUE} when summed bubble magnitudes
#'     exceed the observed endpoint change.}
#'   \item{message}{Diagnostic message (\code{NA} when nothing to report).}
#' }
#'
#' @details
#' Ebullitive flux:
#' \deqn{F_E = \frac{\sum \Delta C_{bubble}}{t_{inc}} \times K}
#' with \eqn{K} the flux conversion term and \eqn{t_{inc}} the effective
#' incubation time. Standard errors are propagated assuming independent bubble
#' magnitude estimates:
#' \deqn{SE(F_E) = \frac{K}{t_{inc}} \sqrt{\sum SE_{bubble}^2}}
#'
#' @seealso \code{\link{find.bubbles}}, \code{\link{goAquaFlux.diffusive}},
#'   \code{\link{goAquaFlux.total}}
#'
#' @importFrom stats var
#' @include goFlux-package.R
#'
#' @keywords internal
#'
goAquaFlux.ebullition <- function(df,
                                  gastype,
                                  bubbles,
                                  flux.term,
                                  final_window.min = 30,
                                  window_C0Cf = 10) {

  ## Initialise the message so we never accidentally return base::message.
  msg <- NA_character_

  ## NOTE: a small helper guarantees every exit path returns the SAME set of
  ## fields (the original returned `inconsistent` in some branches and
  ## `flag_inconsistent` in others, and omitted `message` on the main path).
  .out <- function(flux, SE, n_bubbles, deltaC_bubbles, bubble_ratio,
                   flag_inconsistent, message) {
    list(flux = flux, SE = SE,
         F_tot2pts = F_tot2pts, F_tot2pts.SE = F_tot2pts.SE,
         n_bubbles = n_bubbles,
         deltaC_bubbles = deltaC_bubbles,
         deltaC_total = deltaC_total,
         bubble_ratio = bubble_ratio,
         flag_inconsistent = flag_inconsistent,
         message = message)
  }

  end_limit <- df$Etime[nrow(df)]

  # ---- Endpoint (two-point) total-flux estimate ----------------------------
  # Mean concentration over the first and last `window_C0Cf` seconds.
  idx0 <- df$Etime <= window_C0Cf
  idxf <- df$Etime >= (end_limit - window_C0Cf) & df$Etime < end_limit

  C0_vals <- df[[gastype]][idx0]
  Cf_vals <- df[[gastype]][idxf]

  C0 <- mean(C0_vals, na.rm = TRUE)
  Cf <- mean(Cf_vals, na.rm = TRUE)

  s0 <- var(C0_vals, na.rm = TRUE)
  sf <- var(Cf_vals, na.rm = TRUE)

  ## Count only non-missing values, so the SE below matches the mean.
  n0 <- sum(!is.na(C0_vals))
  nf <- sum(!is.na(Cf_vals))

  deltaC_total <- Cf - C0

  F_tot2pts <- deltaC_total / end_limit * flux.term
  ## NOTE: SE requires >= 2 obs in each window; otherwise var() is NA -> SE NA.
  F_tot2pts.SE <- (flux.term / end_limit) * sqrt((s0 / n0) + (sf / nf))

  # ---- No bubbles ----------------------------------------------------------
  if (is.null(bubbles) || nrow(bubbles) == 0) {
    return(.out(flux = 0, SE = 0, n_bubbles = 0, deltaC_bubbles = 0,
                bubble_ratio = 0, flag_inconsistent = FALSE,
                message = "No bubbling events detected"))
  }

  # Drop bubbles with an unusable magnitude.
  bubbles <- bubbles[!is.na(bubbles$magnitude), ]

  if (nrow(bubbles) == 0) {
    return(.out(flux = NA_real_, SE = NA_real_, n_bubbles = 0L,
                deltaC_bubbles = NA_real_, bubble_ratio = NA_real_,
                flag_inconsistent = FALSE,
                message = "Bubble magnitudes unavailable"))
  }

  # ---- Effective incubation time -------------------------------------------
  # If too little time remains after the last bubble, the trailing window is
  # not representative of ebullition; truncate to the last bubble's start.
  t.after_bubble <- end_limit - bubbles$end[nrow(bubbles)]
  incubation_time <- if (t.after_bubble >= final_window.min) {
    end_limit
  } else {
    bubbles$start[nrow(bubbles)]
  }

  # ---- Summed bubble magnitude and consistency check -----------------------
  deltaC_bubbles <- sum(bubbles$magnitude, na.rm = TRUE)
  ratio <- deltaC_bubbles / deltaC_total

  flag_inconsistent <- FALSE
  if (!is.na(deltaC_total) && deltaC_bubbles > deltaC_total) {
    flag_inconsistent <- TRUE
    msg <- "Sum of bubble magnitudes exceeds endpoint concentration change"
  }

  # ---- Error propagation ----------------------------------------------------
  var_sum <- if ("SE" %in% colnames(bubbles)) {
    sum(bubbles$SE^2, na.rm = TRUE)
  } else NA_real_

  # ---- Ebullitive flux ------------------------------------------------------
  flux <- deltaC_bubbles / incubation_time * flux.term
  flux_se <- if (!is.na(var_sum)) (flux.term / incubation_time) * sqrt(var_sum) else NA_real_

  .out(flux = flux, SE = flux_se, n_bubbles = nrow(bubbles),
       deltaC_bubbles = deltaC_bubbles, bubble_ratio = ratio,
       flag_inconsistent = flag_inconsistent, message = msg)
}
