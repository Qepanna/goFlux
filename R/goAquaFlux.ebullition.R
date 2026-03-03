


goAquaFlux.ebullition <- function(total_flux,
                                  diffusive_flux) {

  # Check structure
  if (is.null(total_flux) || is.null(diffusive_flux)) {
    return(list(
      flux = NA,
      SE = NA,
      message = "Total or Diffusive flux object is NULL"
    ))
  }

  # Extract values
  F_T  <- total_flux$flux
  SE_T <- total_flux$SE

  F_D  <- diffusive_flux$flux
  SE_D <- diffusive_flux$SE

  # Check availability
  if (is.na(F_T) || is.na(F_D)) {
    return(list(
      flux = NA,
      SE = NA,
      message = "Total or Diffusive flux could not be computed"
    ))
  }

  # Compute ebullition flux
  F_E <- F_T - F_D

  # Error propagation (if SE available)
  if (!is.na(SE_T) && !is.na(SE_D)) {
    SE_E <- sqrt(SE_T^2 + SE_D^2)
  } else {
    SE_E <- NA
  }

  return(list(
    flux = F_E,
    SE = SE_E
  ))
}
