#' Combine diffusive and ebullitive flux into a total chamber flux
#'
#' Combines the diffusive and ebullitive components of a gas flux into a single
#' total flux estimate. The total flux is the sum of the two components and the
#' associated uncertainty is propagated assuming the two errors are independent.
#' The ratio of the total flux to the two-point (endpoint) estimate is returned
#' for information only. It is not a quality check: when \code{best.flux}
#' selects the Hutchinson-Mosier model, the diffusive flux is an initial slope
#' whereas the two-point estimate is a mean rate, so the ratio exceeds 1 even
#' without any bubble. Use \code{\link{goAquaFlux.diagnostics}} (mass-balance
#' closure) to assess the flux separation.
#'
#' @param ebullition_flux A list returned by \code{\link{goAquaFlux.ebullition}}.
#'   Must contain at least \code{flux} (ebullitive flux), \code{SE} (its standard
#'   error) and \code{F_tot2pts} (the endpoint total-flux estimate).
#'
#' @param diffusive_flux A list returned by \code{\link{goAquaFlux.diffusive}}.
#'   Must contain at least \code{flux} (diffusive flux) and \code{SE}.
#'
#' @return A named list with:
#' \describe{
#'   \item{flux}{Total flux (diffusion + ebullition).}
#'   \item{SE}{Propagated standard error of the total flux.}
#'   \item{ratio}{Ratio of the reconstructed total flux to the two-point
#'     endpoint estimate (\code{NA} if the endpoint estimate is unavailable).}
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
                             diffusive_flux) {

  ## Initialise the diagnostic message up front
  msg <- NA_character_

  # ---- Structural checks ----
  if (is.null(ebullition_flux) || is.null(diffusive_flux)) {
    return(list(flux = NA_real_, SE = NA_real_, ratio = NA_real_,
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
                message = "Ebullition or diffusive flux could not be computed"))
  }

  # ---- Total flux ----
  F_T <- F_E + F_D

  # ---- Error propagation (assumes independent errors) ----
  SE_T <- if (!is.na(SE_E) && !is.na(SE_D)) sqrt(SE_E^2 + SE_D^2) else NA_real_

  # ---- Ratio to the two-point endpoint estimate (information only) ----
  F_T.2pts <- ebullition_flux$F_tot2pts
  ratio <- if (!is.null(F_T.2pts) && !is.na(F_T.2pts) && F_T.2pts != 0) {
    F_T / F_T.2pts
  } else NA_real_

  # ---- Return ----
  list(flux = F_T,
       SE = SE_T,
       ratio = ratio,
       message = msg)
}
