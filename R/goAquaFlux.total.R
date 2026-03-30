#' Compute total chamber flux from diffusive and ebullitive components
#'
#' Combines diffusive and ebullitive methane flux estimates into a total
#' chamber flux. The total flux is calculated as the sum of both components,
#' and the associated uncertainty is propagated assuming independence of
#' errors. The function also compares the reconstructed total flux against a
#' simple two-point endpoint flux estimate to detect potential overestimation
#' of ebullition.
#'
#' If the reconstructed flux exceeds the endpoint flux by more than a defined
#' tolerance (default = 1.2), the function issues a warning and flags the
#' result as potentially suspicious.
#'
#' @param ebullition_flux A list returned by \code{goAquaFlux.ebullition}
#'   containing at least:
#'   \itemize{
#'     \item \code{flux} ebullitive flux estimate
#'     \item \code{SE} standard error of the ebullitive flux
#'     \item \code{F_tot2pts} total flux estimated using a two-point endpoint method
#'   }
#'
#' @param diffusive_flux A list returned by \code{goAquaFlux.diffusive}
#'   containing at least:
#'   \itemize{
#'     \item \code{flux} diffusive flux estimate
#'     \item \code{SE} standard error of the diffusive flux
#'   }
#'
#' @param flux.term Character string describing the flux component
#'   being calculated (currently not used internally but kept for
#'   compatibility with the main \code{goAquaFlux} workflow).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{flux} total methane flux (diffusion + ebullition)
#'   \item \code{SE} propagated standard error of the total flux
#'   \item \code{ratio} ratio between reconstructed total flux and the
#'   two-point endpoint flux estimate
#'   \item \code{flag_suspicious} logical flag indicating whether the
#'   reconstructed flux substantially exceeds the endpoint estimate
#'   \item \code{diagnostic} diagnostic message explaining the warning
#'   when the tolerance threshold is exceeded
#' }
#'
#' @details
#' Total flux is computed as:
#' \deqn{F_T = F_E + F_D}
#'
#' where:
#' \itemize{
#'   \item \eqn{F_E} is the ebullition flux
#'   \item \eqn{F_D} is the diffusive flux
#' }
#'
#' Error propagation assumes independent uncertainties:
#'
#' \deqn{SE_T = \sqrt{SE_E^2 + SE_D^2}}
#'
#' The resulting flux is compared to a two-point endpoint estimate
#' (\code{F_tot2pts}) obtained from the full chamber deployment to
#' detect cases where bubble magnitude estimation may be excessive.
#'
#' @seealso
#' \code{\link{goAquaFlux.diffusive}},
#' \code{\link{goAquaFlux.ebullition}},
#' \code{\link{find.bubbles}}
#'
#' @internal
#'
goAquaFlux.total <- function(ebullition_flux,
                             diffusive_flux,
                             flux.term) {

  # -----------------------------
  # Check structure
  # -----------------------------
  if (is.null(ebullition_flux) || is.null(diffusive_flux)) {
    return(list(
      flux = NA,
      SE = NA,
      message = "Ebullition or Diffusive flux object is NULL"
    ))
  }

  # -----------------------------
  # Extract values
  # -----------------------------
  F_E  <- ebullition_flux$flux
  SE_E <- ebullition_flux$SE

  F_D  <- diffusive_flux$flux
  SE_D <- diffusive_flux$SE

  # -----------------------------
  # Check availability
  # -----------------------------
  if (is.na(F_E) || is.na(F_D)) {
    return(list(
      flux = NA,
      SE = NA,
      message = "Ebullition or Diffusive flux could not be computed"
    ))
  }

  # -----------------------------
  # Compute total flux as sum of diffusion and ebullition
  # -----------------------------
  F_T <- F_E + F_D


  # Error propagation
  if (!is.na(SE_E) && !is.na(SE_D)) {
    SE_T <- sqrt(SE_E^2 + SE_D^2)
  } else {
    SE_T <- NA
  }


  # Compare F_T with total flux calcuated with endpoint approach
  F_T.2pts <- ebullition_flux$F_tot2pts
  ratio <- F_T / F_T.2pts

  # issue a warning when ratio > tolerance
  tolerance = 1.2
  flag_suspicious <- FALSE
  if (!is.na(F_T) && !is.na(F_T.2pts) && F_T > F_T.2pts * tolerance) {
    flag_suspicious <- TRUE
    message <- paste0(
      "Total flux (", round(F_T,2),") as a sum of Diffusion and Ebullition is ",
      round(ratio,1),
      " times the 2-points total flux estimate (", round(F_T.2pts,2),")")
    warning(message)

  }


  # -----------------------------
  # Return results
  # -----------------------------
  return(list(
    flux = F_T,
    SE = SE_T,
    ratio = ratio,
    flag_suspicious = flag_suspicious,
    message = message
  ))
}
