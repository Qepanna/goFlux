#' Estimate diffusive gas flux from chamber time series
#'
#' Calculates the diffusive component of gas flux during an aquatic chamber
#' incubation using concentration time series data. If bubbling events are
#' detected, only the portion of the time series prior to the first bubbling
#' event is used to estimate the diffusive flux. When no bubbling is detected,
#' the entire time series is used.
#'
#' Flux estimation is performed by calling \code{autoID()} and \code{goFlux()},
#' after which the best model is selected using \code{best.flux()}. The standard
#' error of the flux is taken from the selected model (linear or HMR).
#'
#' @param dataframe A data.frame containing the incubation time series,
#' including a \code{POSIX.time} column and gas concentration measurements.
#'
#' @param gastype Character string specifying the gas concentration variable
#' to analyze (e.g., \code{"CH4dry_ppb"}, \code{"CO2dry_ppm"}).
#'
#' @param auxfile A list or data structure containing chamber metadata required
#' by \code{autoID()} and \code{goFlux()}, such as chamber volume, pressure,
#' temperature, and surface area.
#'
#' @param criteria Optional parameter passed to flux model selection routines.
#' Typically used to define model evaluation criteria.
#'
#' @param bubbles Optional data.frame containing bubbling events detected by
#' \code{find.bubbles()}, with at least a \code{start} column indicating the
#' beginning time of each bubbling event. If provided, only observations before
#' the first bubbling event are used to estimate diffusive flux.
#'
#' @param minimum_window Integer specifying the minimum number of observations
#' required to estimate diffusive flux. If fewer observations are available
#' before the first bubbling event (or in the entire series when no bubbling
#' occurs), the function returns \code{NA} values.
#'
#' @return A list containing:
#' \describe{
#'   \item{flux}{Estimated diffusive flux.}
#'   \item{SE}{Standard error of the selected flux model.}
#'   \item{n_used}{Number of observations used for the diffusive flux estimate.}
#'   \item{first_bubble_time}{Time of the first detected bubbling event
#'   (NA if no bubbling detected).}
#'   \item{message}{Optional message returned when flux cannot be computed
#'   (e.g., insufficient observations).}
#' }
#'
#' @details
#' Diffusive flux is estimated from the portion of the incubation that is free
#' of ebullition events. When bubbling events are present, the time series is
#' truncated at the first bubbling event to avoid contamination of the
#' diffusive signal by abrupt concentration increases.
#'
#' The function relies on the AquaFlux workflow:
#' \itemize{
#'   \item \code{autoID()} prepares the dataset for flux calculation.
#'   \item \code{goFlux()} fits candidate flux models.
#'   \item \code{best.flux()} selects the optimal model and corresponding flux.
#' }
#'
#' @seealso
#' \code{\link{goAquaFlux.total}},
#' \code{\link{find.bubbles}},
#' \code{\link{goFlux}},
#' \code{\link{best.flux}}
#'
#' @export


goAquaFlux.diffusive <- function(dataframe,
                                 gastype,
                                 auxfile,
                                 criteria = criteria,
                                 bubbles = NULL,
                                 minimum_window = 30) {

  dataframe <- dataframe[!duplicated(dataframe$POSIX.time), ]

  # --- Determine diffusive window
  if (is.null(bubbles) || nrow(bubbles) == 0) {

    # No bubbling detected
    df_diff <- dataframe
    first_bubble_time <- NA

  } else {

    first_bubble_time <- bubbles$start[1]
    df_diff <- dataframe[dataframe$Etime < first_bubble_time, ]

  }

  n_used <- nrow(df_diff)

  # --- Check minimum data requirement
  if (n_used < minimum_window) {
    return(list(
      flux = NA,
      SE = NA,
      n_used = n_used,
      first_bubble_time = first_bubble_time,
      message = "Insufficient diffusive observations"
    ))
  }

  # --- calling goFlux to compute diffusive flux
  if(!is.na(first_bubble_time)){auxfile$obs.length <- first_bubble_time}
  autoIDed <- autoID(inputfile = df_diff, auxfile = auxfile, shoulder = 0)

  aquaFlux.diff <- goFlux(autoIDed, gastype)

  best.flux.diff <- best.flux(aquaFlux.diff)
  best.flux.diff$SE_best_model <- ifelse(best.flux.diff$model =="LM", best.flux.diff$LM.SE, best.flux.diff$HM.SE)



  return(list(
    flux = best.flux.diff$best.flux,
    SE = best.flux.diff$SE_best_model,
    n_used = n_used,
    first_bubble_time = first_bubble_time
  ))
}
