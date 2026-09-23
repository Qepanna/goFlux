#' Detect bubbling (ebullition) events in a chamber incubation time series
#'
#' Identifies bubbling events in a gas concentration time series by analysing
#' rolling dispersion within a moving window, then estimates the amount of gas
#' each event added to the chamber headspace. Dispersion is quantified either
#' from the rolling variance of the first differences (\code{"diff"}, the
#' default) or from the rolling variance of the concentration itself
#' (\code{"variance"}). Contiguous periods where dispersion exceeds an adaptive
#' threshold are flagged as candidate bubbling events. The magnitude of each
#' event is then estimated with a local step regression that separates the
#' bubble step from the underlying diffusive trend and, by default, from the
#' transient peak and re-equilibration that typically follow a bubble.
#'
#' @param df A data.frame containing the incubation time series. Must include an
#'   \code{Etime} column (elapsed time, seconds) and the concentration column
#'   named by \code{bubble_source}.
#'
#' @param bubble_source Character; name of the concentration column used for
#'   detection (e.g. \code{"CH4dry_ppb"}).
#'
#' @param window.size Integer; width of the moving window (in interpolated time
#'   steps) used to compute rolling dispersion. Default \code{15}.
#'
#' @param dt Numeric; temporal resolution (seconds) of the regular grid the
#'   signal is interpolated onto before rolling statistics. Default \code{1}.
#'
#' @param method Character; dispersion metric. Either \code{"diff"} (default),
#'   the rolling variance of the first differences, or \code{"variance"}, the
#'   rolling variance of the concentration itself. \code{"diff"} is largely
#'   insensitive to a linear diffusive trend and is the safer general-purpose
#'   choice; \code{"variance"} is more sensitive to small steps on quiet,
#'   low-emission traces (see Details).
#'
#' @param var.quantile Numeric in (0, 1); empirical quantile of the rolling
#'   dispersion distribution used in the adaptive threshold. Default \code{0.7}.
#'
#' @param k Numeric; multiplier on the MAD of the rolling dispersion in the
#'   robust threshold (\eqn{median + k \times MAD}). Larger values are more
#'   conservative. Default \code{4}.
#'
#' @param min_ratio Numeric; minimum ratio of the maximum to the median rolling
#'   dispersion required before any detection is attempted. Guards against
#'   detections in flat, low-variance incubations. Default \code{3}.
#'
#' @param min_sd Numeric or \code{NULL}; if set, incubations whose overall
#'   standard deviation is below this value return \code{NULL} (no detection).
#'
#' @param min_gap Numeric; events separated by less than this gap (seconds) are
#'   merged. Default \code{10}.
#'
#' @param min_length Numeric; minimum event duration (seconds) to retain.
#'   Default \code{5}.
#'
#' @param max_reg_window Numeric; maximum half-window (seconds) used for the
#'   local magnitude regression around each event. Default \code{120}.
#'
#' @param reg.min.obs Integer; minimum observations required for the magnitude
#'   regression. Default \code{10}.
#'
#' @param min_magnitude Numeric or \code{NULL}; minimum step magnitude (same
#'   units as \code{bubble_source}) to retain an event. Default \code{5}.
#'
#' @param min_snr Numeric or \code{NULL}; if set, minimum signal-to-noise ratio
#'   (\code{magnitude / SE}) to retain an event.
#'
#' @param magnitude.model Character; model used to estimate event magnitude.
#'   \code{"step_reequil"} (default) adds an exponential re-equilibration term
#'   after the transient peak, so that \code{magnitude} is the \emph{settled}
#'   step, i.e. the gas that remains in the headspace once the bubble has mixed
#'   (see Details). \code{"step"} is the plain step regression without a
#'   re-equilibration term.
#'
#' @param tau.range Numeric vector of length 2; lower and upper bounds (seconds)
#'   of the re-equilibration time constant \eqn{\tau} explored when
#'   \code{magnitude.model = "step_reequil"}. An \code{NA} upper bound (default)
#'   means the length of the post-peak data available for the regression.
#'   Default \code{c(2, NA)}.
#'
#' @param exclude.ramp Logical; if \code{TRUE} (default), the observations on
#'   the steep rise of the bubble (between the last pre-bubble observation and
#'   the transient peak) are excluded from the magnitude regression, because they
#'   belong neither to the pre-bubble nor to the post-bubble level.
#'
#' @param allow.slow.approach Logical; if \code{FALSE} (default), the
#'   re-equilibration term must describe a positive overshoot that decays
#'   towards the settled level (\eqn{\beta_3 > 0}). If \code{TRUE}, a gradual
#'   approach from below (\eqn{\beta_3 < 0}) is also allowed. This can recover
#'   steps that mix slowly into the headspace, but it can also turn a change in
#'   diffusive slope into a spurious step.
#'
#' @return A data.frame with one row per retained bubbling event and columns:
#' \describe{
#'   \item{\code{start}, \code{end}}{Start and end (seconds, relative to the
#'     first \code{Etime}) of the high-dispersion period that defines the
#'     event.}
#'   \item{\code{magnitude}}{Estimated concentration step (units of
#'     \code{bubble_source}). With \code{magnitude.model = "step_reequil"} this
#'     is the settled step.}
#'   \item{\code{SE}}{Standard error of \code{magnitude}. When a
#'     re-equilibration term is retained, it is conditional on the selected
#'     \eqn{\tau}.}
#'   \item{\code{slope}}{Local trend (\eqn{\beta_1}, units per second).}
#'   \item{\code{n_used}}{Number of observations used in the regression.}
#'   \item{\code{overshoot}}{Amplitude of the transient above the settled level
#'     at the peak (\eqn{\beta_3}); \code{0} when no re-equilibration term was
#'     retained and \code{NA} with \code{magnitude.model = "step"}.}
#'   \item{\code{tau}}{Selected re-equilibration time constant (seconds);
#'     \code{NA} when no re-equilibration term was retained.}
#'   \item{\code{magnitude.step}}{Magnitude from the plain step regression on
#'     the same observations, reported for comparison.}
#'   \item{\code{reequil.complete}}{\code{TRUE} when at least \eqn{3 \tau} of
#'     data follow the peak; \code{FALSE} when the settled level is partly
#'     extrapolated; \code{NA} when no re-equilibration term was retained.}
#' }
#' \code{NULL} is returned if no events are detected or if the series does not
#' meet the minimum variability criteria.
#'
#' @details
#' \strong{Detection.} The signal is first robustly standardised (median / MAD)
#' and interpolated onto a regular \code{dt}-second grid. Rolling dispersion is
#' then computed and compared to an adaptive threshold defined as the maximum
#' of an empirical quantile (\code{var.quantile}) and a robust bound
#' (\eqn{median + k \times MAD}). Contiguous supra-threshold runs are merged
#' (\code{min_gap}) and filtered by duration (\code{min_length}).
#'
#' The two methods differ in what the variance is taken over. \code{"variance"}
#' uses the concentration itself, which responds strongly to a step but is also
#' inflated by a steep diffusive rise, so its detection power falls as the trend
#' grows relative to the step. \code{"diff"} uses the increments: an ebullition
#' step produces one large increment and a sharp spike, whereas a linear
#' diffusive trend produces roughly constant increments and therefore low
#' differenced dispersion. \code{"diff"} is consequently the more robust default
#' across emission regimes, while \code{"variance"} retains an advantage for
#' small steps on quiet traces, where differencing amplifies measurement noise.
#'
#' A rolling median absolute deviation was evaluated as a third metric and
#' removed: being robust by construction, it suppresses the very outlier that
#' marks a bubble. On test incubations it produced only weak contrast, fragmented
#' single events into several, and inflated summed event magnitude accordingly.
#'
#' \strong{Magnitude, plain step model.} With \code{magnitude.model = "step"},
#' event magnitude is estimated with the local model
#' \deqn{C_t = \beta_0 + \beta_1 (t - t_b) + \beta_2 I(t \ge t_s)}
#' where \eqn{t_b} is the time of the largest positive increment within the
#' event, \eqn{I(t \ge t_s)} is a step dummy and \eqn{\beta_2} is the
#' magnitude. The pre- and post-bubble lines share the slope \eqn{\beta_1}, so
#' \eqn{\beta_2} is their vertical offset. The dummy switches at
#' \eqn{t_s = t_b}, or at the transient peak \eqn{t_p} when
#' \code{exclude.ramp = TRUE} (with the rise removed, both choices separate the
#' same two groups of observations). The regression uses the observations
#' between the neighbouring events, limited to \code{max_reg_window} seconds on
#' each side of \eqn{t_b}.
#'
#' \strong{Magnitude, step + re-equilibration model.} Chamber records of
#' bubbling events usually show a jump, a transient peak and a
#' re-equilibration, rather than a clean step: the bubble gas reaches the
#' analyser before it is mixed through the headspace. In the plain step model
#' the whole transient falls on the post-bubble side, which raises the fitted
#' post-bubble level and biases the magnitude upward, in proportion to the size
#' and duration of the overshoot. With \code{magnitude.model = "step_reequil"}
#' (default) the model becomes
#' \deqn{C_t = \beta_0 + \beta_1 (t - t_b) + I(t \ge t_s)
#'   [\beta_2 + \beta_3 e^{-(t - t_p)/\tau}]}
#' where \eqn{\beta_2} is the settled step (the gas added to the headspace),
#' \eqn{\beta_3} the amplitude of the transient at the peak and \eqn{\tau} the
#' re-equilibration time constant. For a given \eqn{\tau} the model is linear;
#' \eqn{\tau} is profiled on a logarithmic grid within \code{tau.range} and
#' refined with a one-dimensional search that minimises the residual sum of
#' squares. The re-equilibration term is retained only if it lowers the AIC of
#' the plain step model, with an additional penalty of 2 for the profiled
#' \eqn{\tau}; otherwise the plain step estimate is returned. The pre-bubble
#' window of the next event starts after \eqn{t_p + 3 \tau} of the previous
#' event (when this leaves at least 3 pre-bubble observations), so that the
#' tail of one event does not bias the baseline of the next.
#'
#' Only events with a positive magnitude are retained, since ebullition adds gas
#' to the headspace. Note that \eqn{\beta_1} next to a large event may still
#' reflect post-bubble dynamics rather than a diffusive rate, because a single
#' slope is shared by the pre- and post-bubble segments.
#'
#' @examples
#' \dontrun{
#' # Default: step + re-equilibration model (settled step)
#' bubbles <- find.bubbles(df = incubation_data,
#'                         bubble_source = "CH4dry_ppb",
#'                         window.size = 15,
#'                         method = "diff")
#'
#' # Plain step regression, as in earlier versions
#' bubbles_step <- find.bubbles(df = incubation_data,
#'                              bubble_source = "CH4dry_ppb",
#'                              window.size = 15,
#'                              method = "diff",
#'                              magnitude.model = "step",
#'                              exclude.ramp = FALSE)
#' }
#'
#' @importFrom zoo rollapply
#' @importFrom stats AIC approx coef lm mad median optimize quantile residuals sd var
#'
#' @keywords internal
#'
find.bubbles <- function(df,
                         bubble_source,
                         window.size = 15,
                         dt = 1,
                         method = c("diff", "variance"),
                         var.quantile = 0.7,
                         k = 4,
                         min_ratio = 3,
                         min_sd = NULL,
                         min_gap = 10,
                         min_length = 5,
                         max_reg_window = 120,
                         reg.min.obs = 10,
                         min_magnitude = 5,
                         min_snr = NULL,
                         magnitude.model = c("step_reequil", "step"),
                         tau.range = c(2, NA),
                         exclude.ramp = TRUE,
                         allow.slow.approach = FALSE) {

  method <- match.arg(method)
  magnitude.model <- match.arg(magnitude.model)

  # Work on time relative to the first observation, so that event times are
  # comparable with Etime when Etime starts at 0 (as enforced by goAquaFlux).
  time0 <- df$Etime[1]
  time  <- as.numeric(df$Etime - time0)
  conc  <- df[[bubble_source]]


  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------

  if (length(time) != length(conc)) stop("time and conc must have equal length")
  if (length(time) < 30)   stop("At least 30 observations required")
  if (window.size < 3)     stop("window.size must be at least 3")
  if (var.quantile <= 0 || var.quantile >= 1) stop("var.quantile must be in (0, 1)")
  if (!is.numeric(tau.range) || length(tau.range) != 2L ||
      is.na(tau.range[1]) || tau.range[1] <= 0 ||
      (!is.na(tau.range[2]) && tau.range[2] <= tau.range[1])) {
    stop("tau.range must be a numeric vector of length 2 with 0 < tau.range[1] ",
         "< tau.range[2] (tau.range[2] may be NA)")
  }
  if (!is.logical(exclude.ramp) || length(exclude.ramp) != 1L || is.na(exclude.ramp)) {
    stop("exclude.ramp must be TRUE or FALSE")
  }
  if (!is.logical(allow.slow.approach) || length(allow.slow.approach) != 1L ||
      is.na(allow.slow.approach)) {
    stop("allow.slow.approach must be TRUE or FALSE")
  }


  # ---------------------------------------------------------------------------
  # Sort and de-duplicate on time
  # ---------------------------------------------------------------------------

  ord  <- order(time)
  time <- time[ord]; conc <- conc[ord]
  dup  <- duplicated(time)
  if (any(dup)) { time <- time[!dup]; conc <- conc[!dup] }
  if (length(time) < window.size) return(NULL)


  # ---------------------------------------------------------------------------
  # Global low-variance guard
  # ---------------------------------------------------------------------------

  global_sd <- sd(conc, na.rm = TRUE)
  if (!is.null(min_sd) && global_sd < min_sd) return(NULL)


  # ---------------------------------------------------------------------------
  # Robust standardisation and interpolation onto a regular grid
  # ---------------------------------------------------------------------------

  # Median / MAD scaling makes the dispersion statistics comparable across
  # incubations and gases. A zero MAD means a flat trace: nothing to detect.
  mad_conc <- mad(conc, na.rm = TRUE)
  if (mad_conc == 0 || is.na(mad_conc)) return(NULL)
  conc_std <- (conc - median(conc, na.rm = TRUE)) / mad_conc

  # Rolling statistics assume equally spaced observations, so the standardised
  # signal is linearly interpolated onto a dt-second grid (detection only; the
  # magnitude regression below uses the raw observations).
  x <- seq(min(time), max(time), by = dt)
  conc_interp <- approx(time, conc_std, xout = x,
                        method = "linear", rule = 2)$y
  if (length(conc_interp) < window.size) return(NULL)


  # ---------------------------------------------------------------------------
  # Rolling dispersion
  # ---------------------------------------------------------------------------

  # "diff": variance of the increments. A linear diffusive trend gives nearly
  # constant increments (low variance), whereas a bubble produces one large
  # increment (strong variance spike). The increment series is padded with a
  # leading 0 to keep the length of the grid.
  # "variance": variance of the (standardised) concentration itself.
  # A rolling MAD is deliberately not offered: it ignores the isolated large
  # increment that marks a bubble (see Details).
  disp_input <- if (method == "diff") {
    c(0, diff(conc_interp))
  } else conc_interp

  disp_fun <- function(v) var(v)

  roll_stat <- zoo::rollapply(disp_input, width = window.size,
                              align = "center", fill = NA, FUN = disp_fun)
  if (all(is.na(roll_stat))) return(NULL)

  valid_stat <- roll_stat[!is.na(roll_stat)]
  if (length(valid_stat) < 5) return(NULL)


  # ---------------------------------------------------------------------------
  # Dispersion ratio guard
  # ---------------------------------------------------------------------------

  # If the largest rolling dispersion is not clearly above the typical level,
  # the trace has no distinct event and no detection is attempted.
  med_stat <- median(valid_stat)
  if (med_stat == 0) return(NULL)
  if (max(valid_stat) / med_stat < min_ratio) return(NULL)


  # ---------------------------------------------------------------------------
  # Adaptive threshold and contiguous supra-threshold runs
  # ---------------------------------------------------------------------------

  # The threshold is the larger of an empirical quantile and a robust
  # median + k * MAD bound, so that at most (1 - var.quantile) of the series
  # can be flagged and isolated noise spikes are ignored.
  q_thresh   <- quantile(valid_stat, var.quantile, na.rm = TRUE)
  rob_thresh <- med_stat + k * mad(valid_stat, na.rm = TRUE)
  thresh     <- max(q_thresh, rob_thresh)

  high_disp <- roll_stat > thresh
  high_disp[is.na(high_disp)] <- FALSE
  if (!any(high_disp)) return(NULL)

  # Run-length encoding turns the logical vector into start / end times of
  # each contiguous high-dispersion period (candidate event).
  r      <- rle(high_disp)
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  chunks <- data.frame(start = x[starts[r$values]],
                       end   = x[ends[r$values]])


  # ---------------------------------------------------------------------------
  # Merge close runs and drop short ones
  # ---------------------------------------------------------------------------

  # A single bubble can produce several short supra-threshold runs (e.g. the
  # jump and the collapse of the transient peak); runs closer than min_gap
  # seconds are merged into one event.
  if (nrow(chunks) > 1) {
    merged <- chunks[1, , drop = FALSE]
    for (i in 2:nrow(chunks)) {
      gap <- chunks$start[i] - merged$end[nrow(merged)]
      if (gap <= min_gap) merged$end[nrow(merged)] <- chunks$end[i]
      else merged <- rbind(merged, chunks[i, ])
    }
    chunks <- merged
  }

  chunks <- chunks[(chunks$end - chunks$start) >= min_length, ]
  if (nrow(chunks) == 0) return(NULL)


  # ---------------------------------------------------------------------------
  # Magnitude of each event
  # ---------------------------------------------------------------------------

  chunks$magnitude        <- NA_real_
  chunks$SE               <- NA_real_
  chunks$slope            <- NA_real_
  chunks$n_used           <- NA_integer_
  chunks$overshoot        <- NA_real_
  chunks$tau              <- NA_real_
  chunks$magnitude.step   <- NA_real_
  chunks$reequil.complete <- NA

  # First differences of the (deduplicated) raw series, aligned so that
  # raw_incr[j] is the increment that leads TO observation j.
  raw_incr <- c(NA_real_, diff(conc))

  # Robust level and spread of the increments. An increment larger than
  # median + 3 * MAD is treated as part of the steep bubble rise (the "ramp").
  incr_med <- median(raw_incr, na.rm = TRUE)
  incr_thr <- 3 * mad(raw_incr, na.rm = TRUE)

  # Time after which the previous event is considered re-equilibrated. It is
  # only updated when a re-equilibration term is retained (t_p + 3 * tau);
  # otherwise the previous event imposes no additional constraint.
  prev_settled <- -Inf

  for (i in seq_len(nrow(chunks))) {

    # --- Step time t_b -------------------------------------------------------
    # Anchor the step at the LARGEST positive increment inside the event rather
    # than at the event's leading edge: the centred rolling window starts to
    # rise before the true step, which would otherwise bias the magnitude low.
    inchunk <- which(time >= chunks$start[i] & time <= chunks$end[i])
    if (length(inchunk) >= 2 && any(is.finite(raw_incr[inchunk]))) {
      ib <- inchunk[which.max(raw_incr[inchunk])]
      tb.start <- time[ib]
    } else {
      ib <- NA_integer_
      tb.start <- chunks$start[i]
    }

    # --- Regression bounds ---------------------------------------------------
    # Between the neighbouring events, and within max_reg_window seconds of t_b.
    tmin <- if (i == 1) min(time) else chunks$end[i - 1] + dt
    tmax <- if (i == nrow(chunks)) max(time) else chunks$start[i + 1] - dt
    tmin <- max(tmin, tb.start - max_reg_window)
    tmax <- min(tmax, tb.start + max_reg_window)

    # --- Steep rise (ramp) and transient peak t_p ----------------------------
    # A bubble rarely produces a single-sample jump: the concentration usually
    # climbs over a few observations, up to a transient peak. Starting from the
    # largest increment, the ramp is extended forward while the increments stay
    # large (the last such observation is the peak) and backward while the
    # observations were themselves reached by large increments. Ramp
    # observations sit between the pre- and post-bubble levels, so they are
    # excluded from the regression when exclude.ramp = TRUE. The peak itself is
    # kept: it is the first observation of the post-bubble segment.
    ramp   <- rep(FALSE, length(time))
    t.peak <- tb.start
    if (!is.na(ib) && is.finite(incr_thr) && incr_thr > 0) {
      big <- (raw_incr - incr_med) > incr_thr
      j <- ib + 1
      while (j <= length(time) && isTRUE(big[j])) j <- j + 1
      ip <- j - 1
      if (ip > ib) ramp[ib:(ip - 1)] <- TRUE
      j <- ib - 1
      while (j > 1 && isTRUE(big[j])) { ramp[j] <- TRUE; j <- j - 1 }
      t.peak <- time[ip]
      if (!exclude.ramp) ramp[] <- FALSE
    }

    # Time at which the step dummy switches from 0 to 1. With the ramp removed,
    # any time within the ramp separates the same two groups of observations;
    # the peak is used. Without ramp exclusion, t_b is used as in the plain
    # step regression.
    t.step <- if (exclude.ramp) t.peak else tb.start

    # --- Observations used in the regression ---------------------------------
    # Preferred: start the pre-bubble segment after the previous event has
    # re-equilibrated and drop the ramp. If this leaves fewer than 3
    # pre-bubble observations, relax the constraints one at a time so the event
    # is not lost.
    tmin_eq <- max(tmin, prev_settled)
    idx <- time >= tmin_eq & time <= tmax & !ramp
    if (sum(time[idx] < t.step) < 3) idx <- time >= tmin & time <= tmax & !ramp
    if (sum(time[idx] < t.step) < 3) idx <- time >= tmin & time <= tmax
    if (sum(idx) < reg.min.obs) next

    df_local <- data.frame(time = time[idx], conc = conc[idx])

    # Require observations on both sides of the step.
    if (sum(df_local$time <  t.step) < 3 ||
        sum(df_local$time >= t.step) < 3) next

    df_local$bubble        <- ifelse(df_local$time >= t.step, 1, 0)
    df_local$time_centered <- df_local$time - tb.start

    # --- Plain step regression -----------------------------------------------
    # C = b0 + b1 * (t - t_b) + b2 * I(t >= t_s): parallel pre- and post-bubble
    # lines, b2 being their vertical offset (the step).
    mod <- try(lm(conc ~ time_centered + bubble, data = df_local), silent = TRUE)
    if (inherits(mod, "try-error")) next

    coefs <- summary(mod)$coefficients
    if (!"bubble" %in% rownames(coefs)) next
    chunks$magnitude.step[i] <- coefs["bubble", "Estimate"]

    # --- Step + re-equilibration regression ----------------------------------
    # C = b0 + b1 * (t - t_b) + I(t >= t_s) * [b2 + b3 * exp(-(t - t_p) / tau)]
    # b2 is then the SETTLED step (gas remaining in the headspace after
    # mixing) and b3 the size of the transient at the peak. For a fixed tau the
    # model is linear, so tau is profiled: coarse log-spaced grid, then a 1-D
    # search between the grid neighbours of the best value.
    best_tau      <- NA_real_
    reeq_complete <- NA
    if (magnitude.model == "step_reequil") {

      # Post-peak data available; tau cannot usefully exceed this span.
      post_span <- max(df_local$time) - t.peak
      tau_hi <- if (is.na(tau.range[2])) post_span else min(tau.range[2], post_span)

      # Fit the linear model for a given tau. Returns NULL when the fit fails,
      # is rank deficient, or (unless allowed) describes an approach from below
      # (b3 <= 0) rather than a decaying overshoot.
      fit_tau <- function(tau) {
        d <- df_local
        d$reeq <- d$bubble * exp(-pmax(d$time - t.peak, 0) / tau)
        m <- try(lm(conc ~ time_centered + bubble + reeq, data = d), silent = TRUE)
        if (inherits(m, "try-error") || anyNA(coef(m))) return(NULL)
        if (!allow.slow.approach && coef(m)[["reeq"]] <= 0) return(NULL)
        m
      }
      rss <- function(tau) {
        m <- fit_tau(tau)
        if (is.null(m)) Inf else sum(residuals(m)^2)
      }

      if (tau_hi > tau.range[1]) {
        g <- exp(seq(log(tau.range[1]), log(tau_hi), length.out = 15))
        r <- vapply(g, rss, numeric(1))
        kb <- which.min(r)
        if (is.finite(r[kb])) {
          opt <- optimize(rss, c(g[max(1, kb - 1)], g[min(length(g), kb + 1)]))
          tau_star <- if (opt$objective <= r[kb]) opt$minimum else g[kb]
          m <- fit_tau(tau_star)

          # Keep the re-equilibration term only if it improves on the plain
          # step model by AIC, with +2 for the profiled tau parameter.
          if (!is.null(m) && AIC(m) + 2 < AIC(mod)) {
            mod <- m
            best_tau <- tau_star
            # Settled level observed (>= 3 tau after the peak) or extrapolated.
            reeq_complete <- post_span >= 3 * tau_star
          }
        }
      }
      coefs <- summary(mod)$coefficients
    }

    # --- Store results -------------------------------------------------------
    chunks$magnitude[i]        <- coefs["bubble", "Estimate"]
    chunks$SE[i]               <- coefs["bubble", "Std. Error"]
    chunks$slope[i]            <- coefs["time_centered", "Estimate"]
    chunks$n_used[i]           <- nrow(df_local)
    chunks$overshoot[i]        <- if (magnitude.model == "step") NA_real_ else
      if (is.na(best_tau)) 0 else coefs["reeq", "Estimate"]
    chunks$tau[i]              <- best_tau
    chunks$reequil.complete[i] <- reeq_complete

    # The next event's baseline should start once this one has re-equilibrated.
    prev_settled <- if (is.na(best_tau)) -Inf else t.peak + 3 * best_tau
  }


  # ---------------------------------------------------------------------------
  # Keep positive, sufficiently large and (optionally) significant steps
  # ---------------------------------------------------------------------------

  valid <- !is.na(chunks$magnitude) & chunks$magnitude > 0
  if (!is.null(min_magnitude)) valid <- valid & chunks$magnitude >= min_magnitude
  if (!is.null(min_snr)) {
    snr   <- chunks$magnitude / pmax(chunks$SE, .Machine$double.eps)
    valid <- valid & snr >= min_snr
  }

  chunks <- chunks[valid, ]
  if (nrow(chunks) == 0) return(NULL)

  chunks
}
