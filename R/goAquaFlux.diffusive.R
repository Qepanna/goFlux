#' Estimate diffusive gas flux from chamber incubation time series
#'
#' Computes the diffusive component of gas flux during an aquatic chamber
#' incubation using concentration time series data. When bubbling events are
#' detected, only the portion of the time series preceding the first bubbling
#' event is used to estimate the diffusive flux. If no bubbling events are
#' detected, the entire time series is used.
#'
#' Diffusive flux is estimated using the \code{goFlux} modelling framework,
#' which fits candidate flux models (e.g. linear or Hutchinson–Mosier models)
#' and selects the best-performing model using \code{best.flux()}.
#'
#' @param df A data.frame containing the incubation time series. Must include
#'   a column \code{Etime} representing elapsed incubation time and the gas
#'   concentration variable specified by \code{gastype}. The data frame should
#'   also contain a water vapour column (\code{H2O_mol}) used by \code{goFlux}.
#'
#' @param gastype Character string specifying the gas concentration variable
#'   to analyse (e.g. \code{"CH4dry_ppb"}, \code{"CO2dry_ppm"}).
#'
#' @param criteria Optional parameter passed to the flux model selection
#'   routines used internally by \code{goFlux} and \code{best.flux}. Typically
#'   used to specify model evaluation criteria.
#'
#' @param bubbles Optional data.frame containing bubbling events detected by
#'   \code{find.bubbles()}. Must contain a column \code{start} representing the
#'   start time of each bubbling event. If provided, only observations occurring
#'   before the first bubbling event are used to estimate the diffusive flux.
#'
#' @param minimum_window Integer specifying the minimum number of observations
#'   required to estimate diffusive flux. If fewer observations are available
#'   in the diffusive portion of the time series, the function returns
#'   \code{NA} values and a diagnostic message.
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{flux}{Estimated diffusive flux from the selected model.}
#'   \item{SE}{Standard error of the selected flux model.}
#'   \item{n_used}{Number of observations used to estimate the diffusive flux.}
#'   \item{first_bubble_time}{Time of the first detected bubbling event
#'   (NA if no bubbling events were detected).}
#'   \item{message}{Optional message returned when flux cannot be computed
#'   (e.g. insufficient observations).}
#' }
#'
#' @details
#' Diffusive flux is estimated from the portion of the incubation time series
#' unaffected by ebullition. When bubbling events are present, the time series
#' is truncated at the start of the first bubbling event to avoid contamination
#' of the diffusive signal by abrupt concentration increases.
#'
#' The function relies on the \code{goFlux} workflow:
#' \itemize{
#'   \item \code{goFlux()} fits candidate flux models to the time series.
#'   \item \code{best.flux()} selects the best-performing model.
#' }
#'
#' The standard error returned corresponds to the model selected by
#' \code{best.flux()}, using either the linear model (LM) or the
#' Hutchinson–Mosier model (HM).
#'
#' @examples
#' diff_flux <- goAquaFlux.diffusive(
#'   df = incubation_data,
#'   gastype = "CH4dry_ppb",
#'   bubbles = bubbles
#' )
#'
#' @seealso
#' \code{\link{find.bubbles}},
#' \code{\link{goFlux}},
#' \code{\link{best.flux}},
#' \code{\link{goAquaFlux.ebullition}},
#' \code{\link{goAquaFlux.total}}
#'
#'
#' @keywords internal
#'
goAquaFlux.diffusive <- function(df,
                                 gastype,
                                 criteria = criteria,
                                 bubbles = NULL,
                                 minimum_window = 30) {

  df <- df[!duplicated(df$Etime), ]

  # --- Determine diffusive window
  if (is.null(bubbles) || nrow(bubbles) == 0) {

    # No bubbling detected
    df_diff <- df
    first_bubble_time <- NA

  } else {

    first_bubble_time <- bubbles$start[1]
    df_diff <- df[df$Etime < first_bubble_time, ]

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
  if(!is.na(first_bubble_time)){df_diff$obs.length <- first_bubble_time}
  # autoIDed <- autoID(inputfile = df_diff, auxfile = auxfile, shoulder = 0)

  aquaFlux.diff <- goFlux(dataframe = df_diff, gastype = gastype, H2O_col = "H2O_mol") # here a doubt if using H2O_col = "H2O_mol" is correct

  best.flux.diff <- best.flux(aquaFlux.diff, criteria = criteria)
  best.flux.diff$SE_best_model <- ifelse(best.flux.diff$model =="LM", best.flux.diff$LM.SE, best.flux.diff$HM.SE)



  return(list(
    flux = best.flux.diff$best.flux,
    SE = best.flux.diff$SE_best_model,
    n_used = n_used,
    first_bubble_time = first_bubble_time,
    best.flux.output = best.flux.diff
  ))
}
