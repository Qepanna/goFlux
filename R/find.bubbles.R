#' Detect bubbling events in incubation time series
#'
#' Identifies bubbling (ebullition) events in gas concentration incubation
#' time series by analysing rolling dispersion within a moving window.
#' Dispersion can be quantified using either rolling variance or rolling
#' median absolute deviation (MAD). Periods where dispersion exceeds an
#' adaptive threshold are classified as bubbling events.
#'
#' The detection algorithm first standardizes the concentration signal using
#' robust statistics, interpolates the signal to a regular time grid, and
#' computes rolling dispersion within a moving window. Contiguous periods
#' where dispersion exceeds an adaptive threshold are identified as potential
#' bubbling events.
#'
#' For each detected event, the magnitude of the concentration step is then
#' estimated using a local linear regression with a step dummy variable.
#' This regression estimates the concentration change attributable to the
#' bubbling event while accounting for the underlying diffusive trend.
#'
#' Minor bubbling events can optionally be filtered based on their estimated
#' magnitude or signal-to-noise ratio.
#'
#' @param df A data.frame containing the incubation time series. Must include
#'   a column \code{Etime} representing elapsed time and the gas concentration
#'   variable specified by \code{bubble_source}.
#'
#' @param bubble_source Character string specifying the column of \code{df}
#'   used to detect bubbling events (e.g. \code{"CH4dry_ppb"}).
#'
#' @param window.size Integer. Width of the moving window used to compute
#'   rolling dispersion (in number of interpolated time steps).
#'
#' @param dt Numeric. Temporal resolution of the interpolated time grid
#'   (default = 1 second).
#'
#' @param method Character string specifying the dispersion metric used for
#'   detection. Options are \code{"mad"} or \code{"variance"}.
#'
#' @param var.quantile Numeric between 0 and 1 defining the empirical quantile
#'   used to derive the adaptive threshold from the rolling dispersion
#'   distribution.
#'
#' @param k Numeric multiplier applied to the MAD of rolling dispersion when
#'   computing the robust threshold (\eqn{median + k * MAD}). Higher values
#'   produce a more conservative bubbling detection.
#'
#' @param min_ratio Numeric. Minimum ratio between the maximum and median
#'   rolling dispersion required to classify a time series as containing
#'   bubbling events. Helps prevent detections in low-variance incubations.
#'
#' @param min_sd Optional numeric threshold defining the minimum standard
#'   deviation of the concentration time series required to perform bubbling
#'   detection. If the global variability is below this threshold, the function
#'   returns \code{NULL}.
#'
#' @param min_gap Numeric. Minimum temporal gap (in seconds) separating two
#'   bubbling events. Detected chunks closer than this value are merged.
#'
#' @param min_length Numeric. Minimum duration (in seconds) required for a
#'   bubbling event to be retained.
#'
#' @param reg.min.obs Integer. Minimum number of observations required to
#'   perform the local regression used to estimate bubble magnitude.
#'
#' @param max_reg_window Numeric. Maximum temporal half-window (in seconds)
#'   used for the local regression surrounding each bubbling event. This
#'   prevents the regression from including excessively long periods that
#'   could bias slope estimation.
#'
#' @param min_magnitude Numeric. Minimum absolute bubble magnitude required
#'   for a bubbling event to be retained (same unit as the concentration
#'   variable, e.g. ppb).
#'
#' @param min_snr Optional numeric threshold defining the minimum
#'   signal-to-noise ratio required for a bubbling event to be retained.
#'   The signal-to-noise ratio is computed as
#'   \code{|magnitude| / SE}.
#'
#' @return
#' A data.frame describing detected bubbling events with the following columns:
#' \describe{
#'   \item{start}{Start time of the bubbling event (seconds since incubation start).}
#'   \item{end}{End time of the bubbling event.}
#'   \item{magnitude}{Estimated concentration step associated with the bubble.}
#'   \item{SE}{Standard error of the magnitude estimate.}
#'   \item{slope}{Estimated diffusive slope during the local regression window.}
#'   \item{n_used}{Number of observations used in the magnitude regression.}
#' }
#'
#' Returns \code{NULL} if no bubbling events are detected or if the time series
#' does not meet the minimum variability criteria.
#'
#' @details
#' Bubbling events are detected using an adaptive dispersion threshold defined
#' as the maximum of:
#' \itemize{
#'   \item an empirical quantile of the rolling dispersion distribution, and
#'   \item a robust threshold defined as \eqn{median + k * MAD}.
#' }
#'
#' Concentration time series are first standardized using the median and MAD
#' to reduce sensitivity to outliers and scale differences between incubations.
#'
#' Bubble magnitudes are estimated using a local regression model of the form:
#'
#' \deqn{C_t = \beta_0 + \beta_1 t + \beta_2 I(t \ge t_b)}
#'
#' where \eqn{I(t \ge t_b)} is a dummy variable representing the bubbling step.
#' The coefficient \eqn{\beta_2} provides the estimated bubble magnitude.
#'
#' @examples
#' bubbles <- find.bubbles(
#'   df = incubation_data,
#'   bubble_source = "CH4dry_ppb",
#'   window.size = 15
#' )
#'
#' @include goFlux-package.R
#'
#' @keywords internal
#'
find.bubbles <- function(df,
                         bubble_source,
                         window.size,
                         dt = 1,
                         method = c("mad","variance"),
                         var.quantile = 0.7,
                         k = 4,
                         min_ratio = 3,
                         min_sd = NULL,
                         min_gap = 10,
                         min_length = 5,
                         max_reg_window = 120,
                         reg.min.obs = 10,
                         min_magnitude = 5, #ppb
                         min_snr = NULL) {

  method <- match.arg(method)

  time0 <- df$Etime[1]
  time <- as.numeric(df$Etime - time0)
  conc <- df[[bubble_source]]


  # --- sort and remove duplicates
  ord <- order(time)
  time <- time[ord]
  conc <- conc[ord]

  dup <- duplicated(time)
  if (any(dup)) {
    # warning("Duplicated time values detected; keeping first occurrence.")
    time <- time[!dup]
    conc <- conc[!dup]
  }

  if (length(time) < window.size)
    return(NULL)

  # --- global low-variance guard
  global_sd <- sd(conc, na.rm = TRUE)

  if (!is.null(min_sd)) {
    if (global_sd < min_sd)
      return(NULL)
  }

  # --- robust scaling (avoid division by zero)
  mad_conc <- mad(conc, na.rm = TRUE)
  if (mad_conc == 0 || is.na(mad_conc))
    return(NULL)

  conc_std <- (conc - median(conc, na.rm = TRUE)) / mad_conc

  # --- interpolation to regular grid
  x <- seq(min(time), max(time), by = dt)
  conc_interp <- approx(time, conc_std,
                        xout = x,
                        method = "linear",
                        rule = 2)$y

  if (length(conc_interp) < window.size)
    return(NULL)

  # --- rolling dispersion
  roll_stat <- zoo::rollapply(
    conc_interp,
    width = window.size,
    align = "center",
    fill = NA,
    FUN = if (method == "variance") {
      function(v) var(v)
    } else {
      function(v) mad(v)
    }
  )

  if (all(is.na(roll_stat)))
    return(NULL)

  # remove NA for threshold estimation
  valid_stat <- roll_stat[!is.na(roll_stat)]

  if (length(valid_stat) < 5)
    return(NULL)

  # --- variance/MAD ratio guard
  med_stat <- median(valid_stat)
  if (med_stat == 0)
    return(NULL)

  if (max(valid_stat) / med_stat < min_ratio)
    return(NULL)

  # --- adaptive threshold
  q_thresh   <- quantile(valid_stat, var.quantile, na.rm = TRUE)
  rob_thresh <- med_stat + k * mad(valid_stat, na.rm = TRUE)

  thresh <- max(q_thresh, rob_thresh)

  high_disp <- roll_stat > thresh
  high_disp[is.na(high_disp)] <- FALSE

  if (!any(high_disp))
    return(NULL)

  # --- find contiguous chunks
  r <- rle(high_disp)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1

  chunks <- data.frame(
    start = x[starts[r$values]],
    end   = x[ends[r$values]]
  )

  # --- merge close chunks
  if (nrow(chunks) > 1) {
    merged <- chunks[1, , drop = FALSE]

    for (i in 2:nrow(chunks)) {
      gap <- chunks$start[i] - merged$end[nrow(merged)]
      if (gap <= min_gap) {
        merged$end[nrow(merged)] <- chunks$end[i]
      } else {
        merged <- rbind(merged, chunks[i, ])
      }
    }
    chunks <- merged
  }

  # --- discard short chunks
  chunks <- chunks[(chunks$end - chunks$start) >= min_length, ]

  if (nrow(chunks) == 0)
    return(NULL)

  # --------------------------------------------------
  # Estimate bubble magnitude using step dummy model
  # --------------------------------------------------

  chunks$magnitude <- NA
  chunks$SE <- NA
  chunks$slope <- NA
  chunks$n_used <- NA

  for (i in seq_len(nrow(chunks))) {

    tb.start <- chunks$start[i]

    # regression limits based on neighboring bubbles
    if (i == 1) {
      tmin <- min(time)
    } else {
      tmin <- chunks$end[i-1] + dt
    }

    if (i == nrow(chunks)) {
      tmax <- max(time)
    } else {
      tmax <- chunks$start[i+1] - dt
    }

    # optional window clamp
    tmin <- max(tmin, tb.start - max_reg_window)
    tmax <- min(tmax, tb.start + max_reg_window)

    idx <- time >= tmin & time <= tmax

    if (sum(idx) < reg.min.obs)
      next

    df_local <- data.frame(
      time = time[idx],
      conc = conc[idx]
    )

    # require observations on both sides
    pre_obs  <- sum(df_local$time < tb.start)
    post_obs <- sum(df_local$time >= tb.start)

    if (pre_obs < 3 || post_obs < 3)
      next

    df_local$bubble <- ifelse(df_local$time >= tb.start, 1, 0)

    mod <- try(lm(conc ~ time + bubble, data = df_local), silent = TRUE)

    if (inherits(mod, "try-error"))
      next

    coefs <- summary(mod)$coefficients

    if (!"bubble" %in% rownames(coefs))
      next

    chunks$magnitude[i] <- coefs["bubble","Estimate"]
    chunks$SE[i] <- coefs["bubble","Std. Error"]
    chunks$slope[i] <- coefs["time","Estimate"]
    chunks$n_used[i] <- nrow(df_local)
  }


  # --------------------------------------------
  # Filter minor bubbles
  # --------------------------------------------

  valid <- !is.na(chunks$magnitude)

  if (!is.null(min_magnitude)) {
    valid <- valid & abs(chunks$magnitude) >= min_magnitude
  }

  if (!is.null(min_snr)) {
    snr <- abs(chunks$magnitude) / pmax(chunks$SE, .Machine$double.eps) # guard against zero SE
    valid <- valid & snr >= min_snr
  }

  chunks <- chunks[valid, ]

  if (nrow(chunks) == 0)
    return(NULL)

  return(chunks)
}
