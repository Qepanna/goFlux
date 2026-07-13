#' Estimate diffusive gas flux from a chamber incubation time series
#'
#' Computes the diffusive component of a gas flux for a single aquatic chamber
#' incubation. When bubbling events are present, only the portion of the time
#' series that precedes ebullition is used, so that abrupt bubble-driven steps
#' do not contaminate the diffusive slope. The retained window is then passed to
#' the \code{\link[goFlux]{goFlux}} / \code{\link[goFlux]{best.flux}} machinery,
#' which fits the linear (LM) and Hutchinson-Mosier (HM) models and selects the
#' best one according to \code{criteria}.
#'
#' @param df A data.frame for a single incubation, as prepared by
#'   \code{\link{goAquaFlux}}. Must contain \code{Etime} (starting at 0),
#'   \code{flag}, the gas column named by \code{gastype}, the instrument
#'   precision column (\code{*_prec}), \code{Vtot}, \code{Area}, \code{Pcham},
#'   \code{Tcham}, and (for non-water gases) \code{H2O_mol} and/or \code{H2O_ppm}.
#'
#' @param gastype Character; name of the gas concentration column.
#'
#' @param criteria Character vector of model-selection criteria passed to
#'   \code{\link[goFlux]{best.flux}}. Defaults to the full goFlux criteria set.
#'
#' @param bubble_gas Character; the gas used to detect bubbles (typically
#'   \code{"CH4dry_ppb"}). When \code{gastype == bubble_gas} the diffusive window
#'   is always truncated at the first detected bubble. For other gases the
#'   window is only truncated if a bubble coincides with an abrupt change in
#'   that gas (see Details).
#'
#' @param bubbles Data.frame of bubbling events from \code{\link{find.bubbles}}
#'   (or \code{NULL}). Must contain a \code{start} column.
#'
#' @param minimum_window Integer; minimum number of observations required in the
#'   diffusive window. If fewer are available the function returns \code{NA}.
#'
#' @param abrupt.window Numeric; half-width (seconds) of the window used on each
#'   side of a candidate bubble time when testing for an abrupt slope change
#'   (non-bubble gases only). Default \code{30}.
#'
#' @param abrupt.min.points Integer; minimum points required on each side of the
#'   candidate time to run the abrupt-change test. Default \code{10}.
#'
#' @param abrupt.threshold Numeric; relative slope-change threshold above which a
#'   bubble is deemed to perturb the gas. Default \code{0.5} (i.e. 50\%).
#'
#' @return A list with \code{flux}, \code{SE}, \code{n_used},
#'   \code{first_bubble_time} (the start of the first detected bubble, or
#'   \code{NA}), \code{best.flux.output} (the full \code{best.flux} row) and,
#'   when flux cannot be computed, a \code{message}.
#'
#' @details
#' For the bubble gas itself, ebullition by definition perturbs the signal, so
#' the diffusive window ends at the first detected bubble. For other gases a
#' bubble does not necessarily perturb the concentration; the window is only cut
#' at the first bubble time that is accompanied by an abrupt change in local
#' slope (the ratio of the post- to pre-bubble slope exceeds
#' \code{abrupt.threshold}). If no such change is found, the full series is used.
#'
#' @seealso \code{\link{find.bubbles}}, \code{\link[goFlux]{goFlux}},
#'   \code{\link[goFlux]{best.flux}}, \code{\link{goAquaFlux.ebullition}},
#'   \code{\link{goAquaFlux.total}}
#'
#' @importFrom stats lm coef
#'
#' @keywords internal
#'


# --- Test whether a candidate time separates two clearly different slopes -----
.has_abrupt_change <- function(df, gastype, split_time,
                               window_size = 30,
                               min_points = 10,
                               threshold = 0.5) {

  df_local <- df[df$Etime >= (split_time - window_size) &
                   df$Etime <= (split_time + window_size), ]

  df_before <- df_local[df_local$Etime <  split_time, ]
  df_after  <- df_local[df_local$Etime >= split_time, ]

  # Conservative: if we cannot test, assume the bubble does perturb the gas.
  if (nrow(df_before) < min_points || nrow(df_after) < min_points) return(TRUE)

  fit_before <- try(lm(df_before[[gastype]] ~ df_before$Etime), silent = TRUE)
  fit_after  <- try(lm(df_after[[gastype]]  ~ df_after$Etime),  silent = TRUE)
  if (inherits(fit_before, "try-error") || inherits(fit_after, "try-error")) {
    return(TRUE)
  }

  slope_before <- coef(fit_before)[2]
  slope_after  <- coef(fit_after)[2]
  if (is.na(slope_before) || is.na(slope_after)) return(TRUE)

  rel_diff <- abs(slope_after - slope_before) / max(abs(slope_before), 1e-9)
  rel_diff > threshold
}


# --- Choose the portion of the series used for the diffusive estimate ---------
.select_diffusive_window <- function(df, gastype, bubble_gas, bubbles,
                                     window = 30,
                                     min_points = 10,
                                     threshold = 0.5) {

  # No bubbles: use the whole series.
  if (is.null(bubbles) || nrow(bubbles) == 0 || all(is.na(bubbles$start))) {
    return(list(df_diff = df, stop_time = NA_real_))
  }

  # Bubble gas: ebullition perturbs it by construction -> cut at first bubble.
  if (identical(gastype, bubble_gas) && !is.na(bubbles$start[1])) {
    return(list(df_diff = df[df$Etime < bubbles$start[1], ],
                stop_time = bubbles$start[1]))
  }

  # Other gases: only cut if a bubble coincides with an abrupt slope change.
  bubble_times <- bubbles$start[!is.na(bubbles$start)]
  stop_time <- NA_real_                     # NA => no abrupt change => full series

  for (t_bubble in bubble_times) {
    change <- .has_abrupt_change(df, gastype, t_bubble,
                                 window_size = window,
                                 min_points  = min_points,
                                 threshold   = threshold)
    if (isTRUE(change)) { stop_time <- t_bubble; break }
  }

  ## When no abrupt change is found, keep the FULL series (the strict `<`
  ## used for truncation would otherwise drop the final observation).
  if (is.na(stop_time)) return(list(df_diff = df, stop_time = NA_real_))
  list(df_diff = df[df$Etime < stop_time, ], stop_time = stop_time)
}


# --- Main function ------------------------------------------------------------
goAquaFlux.diffusive <- function(df,
                                 gastype,
                                 criteria = c("MAE", "RMSE", "AICc", "SE",
                                              "g.factor", "kappa", "MDF",
                                              "nb.obs", "intercept", "p-value"),
                                 bubble_gas = "CH4dry_ppb",
                                 bubbles = NULL,
                                 minimum_window = 30,
                                 abrupt.window = 30,
                                 abrupt.min.points = 10,
                                 abrupt.threshold = 0.5) {

  df <- df[!duplicated(df$Etime), ]

  # Actual first bubble time (reported to the user, independent of windowing).
  first_bubble_time <- if (!is.null(bubbles) && nrow(bubbles) > 0 &&
                           !is.na(bubbles$start[1])) bubbles$start[1] else NA_real_

  # Determine the diffusive window.
  res_window <- .select_diffusive_window(
    df, gastype, bubble_gas, bubbles,
    window = abrupt.window, min_points = abrupt.min.points,
    threshold = abrupt.threshold)
  df_diff <- res_window$df_diff

  n_used <- nrow(df_diff)

  if (n_used < minimum_window) {
    return(list(flux = NA_real_, SE = NA_real_, n_used = n_used,
                first_bubble_time = first_bubble_time,
                best.flux.output = NULL,
                message = "Insufficient diffusive observations"))
  }

  ## FIX (water vapour double-correction): the incoming `H2O_mol` column is
  ## already a mole fraction (ppm / 1e6). goFlux() divides its H2O_col by 1e6
  ## AGAIN, so passing H2O_col = "H2O_mol" divides twice and effectively
  ## disables the correction. We pass the original ppm column instead (carried
  ## through by goAquaFlux as `H2O_ppm`) and let goFlux do its single
  ## conversion. If no water column is available, disable the correction
  ## explicitly (quietly, to avoid one warning per incubation).
  h2o_arg <- if ("H2O_ppm" %in% names(df_diff)) "H2O_ppm" else NULL

  aquaFlux.diff <- suppressWarnings(
    goFlux(dataframe = df_diff, gastype = gastype, H2O_col = h2o_arg))

  best.flux.diff <- best.flux(aquaFlux.diff, criteria = criteria)
  best.flux.diff$SE_best_model <- ifelse(best.flux.diff$model == "LM",
                                         best.flux.diff$LM.SE,
                                         best.flux.diff$HM.SE)

  list(flux = best.flux.diff$best.flux,
       SE = best.flux.diff$SE_best_model,
       n_used = n_used,
       first_bubble_time = first_bubble_time,
       best.flux.output = best.flux.diff)
}
