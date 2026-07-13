#' Combine diffusive and ebullitive flux into a total chamber flux
#'
#' Combines the diffusive and ebullitive components of a gas flux into a single
#' total flux estimate. The total flux is the sum of the two components and the
#' associated uncertainty is propagated assuming the two errors are independent.
#' As a consistency check, the reconstructed total flux is compared against a
#' simple two-point (endpoint) estimate of the total flux over the whole
#' deployment (\code{F_tot2pts}, produced by
#' \code{\link{goAquaFlux.ebullition}}). If the reconstructed flux exceeds the
#' endpoint estimate by more than \code{tolerance}, the result is flagged as
#' potentially suspicious and a warning is issued. This typically indicates that
#' bubble magnitudes were over-estimated (e.g. a diffusive ramp mistaken for a
#' step).
#'
#' @param ebullition_flux A list returned by \code{\link{goAquaFlux.ebullition}}.
#'   Must contain at least \code{flux} (ebullitive flux), \code{SE} (its standard
#'   error) and \code{F_tot2pts} (the endpoint total-flux estimate).
#'
#' @param diffusive_flux A list returned by \code{\link{goAquaFlux.diffusive}}.
#'   Must contain at least \code{flux} (diffusive flux) and \code{SE}.
#'
#' @param tolerance Numeric > 1. Ratio above which the reconstructed total flux
#'   is flagged as suspiciously larger than the endpoint estimate. Default
#'   \code{1.2} (i.e. 20\% larger).
#'
#' @return A named list with:
#' \describe{
#'   \item{flux}{Total flux (diffusion + ebullition).}
#'   \item{SE}{Propagated standard error of the total flux.}
#'   \item{ratio}{Ratio of the reconstructed total flux to the two-point
#'     endpoint estimate (\code{NA} if the endpoint estimate is unavailable).}
#'   \item{flag_suspicious}{Logical; \code{TRUE} when
#'     \code{flux > F_tot2pts * tolerance}.}
#'   \item{message}{Diagnostic message (\code{NA} when nothing to report).}
#' }
#'
#' @details
#' Total flux:  \deqn{F_T = F_E + F_D}
#' Error propagation (independent errors):  \deqn{SE_T = \sqrt{SE_E^2 + SE_D^2}}
#'
#' @seealso \code{\link{goAquaFlux.diffusive}},
#'   \code{\link{goAquaFlux.ebullition}}, \code{\link{find.bubbles}}
#'
#' @keywords internal
#'
goAquaFlux.total <- function(ebullition_flux,
                             diffusive_flux,
                             tolerance = 1.2) {

  ## Initialise the diagnostic message up front
  msg <- NA_character_

  # ---- Structural checks ----
  if (is.null(ebullition_flux) || is.null(diffusive_flux)) {
    return(list(flux = NA_real_, SE = NA_real_, ratio = NA_real_,
                flag_suspicious = FALSE,
                message = "Ebullition or diffusive flux object is NULL"))
  }

  # ---- Extract components ----
  F_E  <- ebullition_flux$flux
  SE_E <- ebullition_flux$SE
  F_D  <- diffusive_flux$flux
  SE_D <- diffusive_flux$SE

  # ---- Availability check ----
  if (is.na(F_E) || is.na(F_D)) {
    return(list(flux = NA_real_, SE = NA_real_, ratio = NA_real_,
                flag_suspicious = FALSE,
                message = "Ebullition or diffusive flux could not be computed"))
  }

  # ---- Total flux ----
  F_T <- F_E + F_D

  # ---- Error propagation (assumes independent errors) ----
  SE_T <- if (!is.na(SE_E) && !is.na(SE_D)) sqrt(SE_E^2 + SE_D^2) else NA_real_

  # ---- Consistency check against the two-point endpoint estimate ----
  F_T.2pts <- ebullition_flux$F_tot2pts
  ratio <- if (!is.null(F_T.2pts) && !is.na(F_T.2pts) && F_T.2pts != 0) {
    F_T / F_T.2pts
  } else NA_real_

  flag_suspicious <- FALSE
  if (!is.na(ratio) && F_T > F_T.2pts * tolerance) {
    flag_suspicious <- TRUE
    msg <- paste0(
      "Total flux (", round(F_T, 2), ") reconstructed as diffusion + ebullition ",
      "is ", round(ratio, 1), " times the two-point total flux estimate (",
      round(F_T.2pts, 2), "). Bubble magnitudes may be over-estimated.")
    warning(msg, call. = FALSE)
  }

  # ---- Return ----
  list(flux = F_T,
       SE = SE_T,
       ratio = ratio,
       flag_suspicious = flag_suspicious,
       message = msg)
}
