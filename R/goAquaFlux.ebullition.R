#' Compute Ebullition Flux from Total and Diffusive Flux Estimates
#'
#' Calculates ebullition flux as the difference between total chamber flux
#' and diffusive flux. Ebullition is defined as:
#'
#' \deqn{F_E = F_T - F_D}
#'
#' where \eqn{F_E} is ebullition flux, \eqn{F_T} is total flux, and
#' \eqn{F_D} is diffusive flux. Uncertainty of the ebullition estimate is
#' calculated by standard error propagation assuming independence of
#' the two flux estimates:
#'
#' \deqn{SE_E = \sqrt{SE_T^2 + SE_D^2}}
#'
#' If either total or diffusive flux cannot be computed (i.e. \code{NA}
#' or \code{NULL}), the function returns \code{NA} for ebullition flux.
#'
#' Because ebullition represents bubble-mediated gas release, negative
#' ebullition flux values are physically impossible. If the computed
#' ebullition flux is negative (i.e. diffusive flux exceeds total flux),
#' a warning is issued and diagnostic information is returned to help
#' identify potential issues in flux estimation.
#'
#' @param total_flux A list containing the output of
#'   \code{goAquaFlux.total()}, including elements \code{flux} and
#'   \code{SE}.
#'
#' @param diffusive_flux A list containing the output of
#'   \code{goAquaFlux.diffusive()}, including elements \code{flux} and
#'   \code{SE}.
#'
#' @param tol Numeric tolerance threshold for detecting negative
#'   ebullition flux. Small negative values within this tolerance are
#'   ignored to account for numerical noise. Default is \code{0}.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{flux}{Estimated ebullition flux.}
#'   \item{SE}{Standard error of the ebullition flux estimated by
#'   uncertainty propagation.}
#'   \item{total_flux}{Total flux used in the calculation.}
#'   \item{diffusive_flux}{Diffusive flux used in the calculation.}
#'   \item{diagnostic}{Diagnostic message returned when negative
#'   ebullition is detected, otherwise \code{NULL}.}
#' }
#'
#' @details
#' Ebullition flux represents the component of gas emission driven by
#' bubble release from sediments. It is obtained indirectly by
#' subtracting the diffusive flux component from the total chamber
#' flux. This approach assumes that total flux is the sum of diffusive
#' and ebullitive transport.
#'
#' Negative ebullition values may arise when:
#' \itemize{
#'   \item Diffusive flux is overestimated (e.g. unstable linear fit).
#'   \item Total flux is underestimated due to late bubbling events.
#'   \item Bubble detection incorrectly truncates the diffusive window.
#' }
#'
#' Such cases should be interpreted cautiously and may require manual
#' inspection of the concentration time series.
#'
#' @examples
#' total <- list(flux = 0.45, SE = 0.05)
#' diff  <- list(flux = 0.30, SE = 0.04)
#'
#' goAquaFlux.ebullition(total, diff)
#'
#' @include goFlux-package.R
#'
#' @export
#'
goAquaFlux.ebullition <- function(total_flux,
                                  diffusive_flux,
                                  tol = 0) {

  # -----------------------------
  # Check structure
  # -----------------------------
  if (is.null(total_flux) || is.null(diffusive_flux)) {
    return(list(
      flux = NA,
      SE = NA,
      message = "Total or Diffusive flux object is NULL"
    ))
  }

  # -----------------------------
  # Extract values
  # -----------------------------
  F_T  <- total_flux$flux
  SE_T <- total_flux$SE

  F_D  <- diffusive_flux$flux
  SE_D <- diffusive_flux$SE

  # -----------------------------
  # Check availability
  # -----------------------------
  if (is.na(F_T) || is.na(F_D)) {
    return(list(
      flux = NA,
      SE = NA,
      message = "Total or Diffusive flux could not be computed"
    ))
  }

  # -----------------------------
  # Compute ebullition
  # -----------------------------
  F_E <- F_T - F_D

  # Error propagation
  if (!is.na(SE_T) && !is.na(SE_D)) {
    SE_E <- sqrt(SE_T^2 + SE_D^2)
  } else {
    SE_E <- NA
  }

  # -----------------------------
  # Diagnostic check
  # -----------------------------
  diagnostic <- NULL

  if (F_E < -tol) {

    diagnostic <- paste0(
      "Negative ebullition detected: Diffusive flux (",
      round(F_D, 4),
      ") exceeds Total flux (",
      round(F_T, 4),
      ")."
    )

    warning(diagnostic)

  }

  # -----------------------------
  # Return results
  # -----------------------------
  return(list(
    flux = F_E,
    SE = SE_E,
    total_flux = F_T,
    diffusive_flux = F_D,
    diagnostic = diagnostic
  ))
}
