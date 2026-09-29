#' Check of the ebullitive component of a goAquaFlux estimate
#'
#' @description
#' \code{goAquaFlux.diagnostics} assesses the part of a \code{\link{goAquaFlux}}
#' estimate that \code{\link[goFlux]{best.flux}} does not cover: the
#' separation of the flux into its diffusive and ebullitive components. It is
#' called internally by \code{goAquaFlux()} for the gas on which the bubbles
#' are detected (\code{bubble_gas}), the only gas for which an ebullitive flux
#' is estimated.
#'
#' The two checks are complementary and do not overlap:
#' \itemize{
#'   \item the quality of the diffusive fit (goodness of fit, curvature,
#'     detection limit, number of observations, ...) is checked by
#'     \code{best.flux} on the diffusive window, exactly as in
#'     \code{\link[goFlux]{goFlux}}, and reported in \code{quality.check};
#'   \item the ebullitive component is checked here and reported in
#'     \code{ebullition.check}.
#' }
#' For any other gas (e.g. CO2 with bubbles detected on CH4), only the
#' \code{best.flux} quality check applies, so that its assessment rests on the
#' observations of that gas alone.
#'
#' @details
#' \strong{Mass-balance closure.} When bubbles were detected, the separation
#' is checked against the gas that actually accumulated in the chamber, in
#' concentration units and with mean rates only:
#' \deqn{closure = (\bar{s}_D \, T + \sum_i M_i) / \Delta C_{obs}}
#' \eqn{\bar{s}_D} is the mean diffusive accumulation rate over the diffusive
#' window (slope of the linear model fitted by goFlux on that window, whatever
#' model \code{best.flux} selected), \eqn{T} the incubation length, \eqn{M_i}
#' the settled bubble steps and \eqn{\Delta C_{obs}} the difference between the
#' mean concentrations of the last and first \code{window_C0Cf} seconds.
#' \itemize{
#'   \item \eqn{closure < 1}: part of the accumulated gas is not explained
#'     (undetected bubbles, or diffusion faster after the first bubble than
#'     before it);
#'   \item \eqn{closure > 1}: more gas is attributed than accumulated
#'     (over-estimated bubbles, or diffusion slowing down after the first
#'     bubble, e.g. a curved accumulation measured on a short window).
#' }
#' Mean rates are used because the reported diffusive flux is an initial slope
#' when \code{best.flux} selects the Hutchinson-Mosier model, whereas the
#' ebullitive flux and the observed change are mean rates. Comparing the total
#' flux with a two-point estimate would therefore flag curvature, which is
#' already assessed by the g-factor of \code{best.flux}.
#'
#' The closure is evaluated only when the observed change is large compared
#' with the noise, estimated robustly from first differences as
#' \eqn{\sigma = MAD(\Delta C_t)/\sqrt{2}}: \eqn{|\Delta C_{obs}| >}
#' \code{min_snr} \eqn{\times \sigma}.
#'
#' \strong{Detection limit.} An incubation without detected bubbles does not
#' prove the absence of ebullition. \code{bubble_detection_limit} is the
#' smallest bubble detected reliably (\code{dl_sigma} \eqn{\times \sigma};
#' isolated bubbles of 20 \eqn{\sigma} were detected in 96-100\% of synthetic
#' incubations) and \code{ebullition_detection_limit} the ebullitive flux of
#' one such bubble over the incubation.
#'
#' \strong{Calibration.} On synthetic incubations with known fluxes and
#' detected bubbles, with the defaults (\code{tolerance = 0.2},
#' \code{min_snr = 10}), 98\% of the incubations whose closure failed had a
#' total-flux error > 20\%, while those that passed had median absolute errors
#' of 2\% (total flux) and 1.5\% (ebullitive flux).
#'
#' @param df data.frame of one incubation (\code{flag == 1} rows).
#' @param gastype Character; gas column, the gas on which bubbles are
#'   detected (e.g. \code{"CH4dry_ppb"}).
#' @param bubbles data.frame of bubbles from \code{\link{find.bubbles}}, or
#'   \code{NULL}.
#' @param diffusive_flux List returned by \code{goAquaFlux.diffusive}.
#' @param flux.term Numeric; flux term of the incubation.
#' @param window_C0Cf Numeric; seconds averaged at the start and end of the
#'   incubation for \eqn{\Delta C_{obs}}. Default 10.
#' @param tolerance Numeric; accepted deviation of \code{closure} from 1.
#'   Default 0.2.
#' @param min_snr Numeric; minimum \eqn{|\Delta C_{obs}|/\sigma} to evaluate
#'   the closure. Default 10.
#' @param dl_sigma Numeric; bubble detection limit in multiples of
#'   \eqn{\sigma}. Default 20.
#'
#' @return A one-row data.frame with:
#' \describe{
#'   \item{\code{ebullition.check}}{Character, empty when the check is passed,
#'     following the convention of \code{quality.check} in \code{best.flux}.
#'     Otherwise one of \code{"closure < 0.8"} or \code{"closure > 1.2"}
#'     (limits set by \code{tolerance}), \code{"no bubble detected"},
#'     \code{"no diffusive flux"} (closure cannot be evaluated) or
#'     \code{"low signal"} (closure not evaluated).}
#'   \item{\code{closure}}{\eqn{\Delta C_{pred}/\Delta C_{obs}}, or \code{NA}
#'     when not evaluated.}
#'   \item{\code{unexplained_share}}{\eqn{1 - closure}: share of the observed
#'     change not explained by the separation (negative when over-attributed).}
#'   \item{\code{ebullition_share}}{\eqn{\sum M_i / \Delta C_{obs}}.}
#'   \item{\code{dC_obs}, \code{dC_pred}}{Observed and predicted change (gas
#'     units).}
#'   \item{\code{C0_obs}, \code{window_C0Cf}}{Observed mean concentration over
#'     the first \code{window_C0Cf} seconds, and that window.}
#'   \item{\code{diffusive_rate_mean}}{\eqn{\bar{s}_D} (gas units per second).}
#'   \item{\code{snr}}{\eqn{|\Delta C_{obs}|/\sigma}.}
#'   \item{\code{bubble_detection_limit}}{In gas units.}
#'   \item{\code{ebullition_detection_limit}}{In flux units.}
#' }
#'
#' @seealso \code{\link{goAquaFlux}}, \code{\link[goFlux]{best.flux}}
#' @export
goAquaFlux.diagnostics <- function(df, gastype, bubbles = NULL,
                                   diffusive_flux, flux.term,
                                   window_C0Cf = 10,
                                   tolerance = 0.2,
                                   min_snr = 10,
                                   dl_sigma = 20) {

  ok <- !is.na(df$Etime) & !is.na(df[[gastype]])
  t <- df$Etime[ok]; y <- df[[gastype]][ok]
  t_end <- t[length(t)]
  T_inc <- t_end - t[1]

  ## ---- Observed change and noise ------------------------------------------
  C0_obs <- mean(y[t <= t[1] + window_C0Cf])
  dC_obs <- mean(y[t >= t_end - window_C0Cf]) - C0_obs
  sigma  <- stats::mad(diff(y)) / sqrt(2)
  snr    <- if (is.finite(sigma) && sigma > 0) abs(dC_obs) / sigma else Inf

  ## ---- Bubbles and mean diffusive rate --------------------------------------
  n_bub <- if (is.null(bubbles)) 0L else nrow(bubbles)
  sumM  <- if (n_bub > 0) sum(bubbles$magnitude, na.rm = TRUE) else 0

  bf <- diffusive_flux$best.flux.output
  has_diff <- !is.null(bf) && nrow(bf) > 0 && !is.null(diffusive_flux$flux) &&
    !is.na(diffusive_flux$flux)
  s_mean <- if (has_diff) bf$LM.slope[1] else NA_real_

  ## ---- Closure and check ------------------------------------------------------
  dC_pred <- s_mean * T_inc + sumM
  evaluable <- n_bub > 0 && has_diff && is.finite(snr) && snr > min_snr
  closure <- if (evaluable) dC_pred / dC_obs else NA_real_

  ebullition.check <- if (n_bub == 0) "no bubble detected" else
    if (!has_diff) "no diffusive flux" else
      if (!evaluable) "low signal" else
        if (closure < 1 - tolerance) paste0("closure < ", 1 - tolerance) else
          if (closure > 1 + tolerance) paste0("closure > ", 1 + tolerance) else ""

  data.frame(
    ebullition.check = ebullition.check,
    closure = closure,
    unexplained_share = 1 - closure,
    ebullition_share = if (dC_obs > 0) sumM / dC_obs else NA_real_,
    dC_obs = dC_obs, dC_pred = dC_pred,
    C0_obs = C0_obs, window_C0Cf = window_C0Cf,
    diffusive_rate_mean = s_mean,
    snr = snr,
    bubble_detection_limit = dl_sigma * sigma,
    ebullition_detection_limit = if (T_inc > 0) dl_sigma * sigma / T_inc * flux.term else NA_real_,
    stringsAsFactors = FALSE)
}
