#' Detect bubbling events in incubation time series
#'
#' Identifies potential bubbling (ebullition) events in gas concentration
#' incubation time series by analysing rolling dispersion within a moving
#' window. Dispersion can be quantified using either rolling variance or
#' rolling median absolute deviation (MAD). Periods where dispersion exceeds
#' an adaptive threshold are classified as bubbling events.
#'
#' The threshold is defined as the maximum between:
#' - a user-defined quantile of the rolling dispersion distribution, and
#' - a robust dispersion criterion (median + k * MAD).
#'
#' Additional safeguards prevent false detections in low-variance time series,
#' including a minimum dispersion ratio and an optional global variability
#' threshold.
#'
#' @param time Numeric vector of time stamps (typically seconds since the
#'   beginning of the incubation).
#'
#' @param conc Numeric vector of gas concentrations corresponding to `time`.
#'
#' @param window.size Integer. Width of the moving window used to compute
#'   rolling dispersion (in number of interpolated time steps).
#'
#' @param dt Numeric. Temporal resolution of the interpolated time grid
#'   (default = 1 second).
#'
#' @param method Character string specifying the dispersion metric used for
#'   detection. Options are `"mad"` (default) for rolling median absolute
#'   deviation or `"variance"` for rolling variance.
#'
#' @param var.quantile Numeric between 0 and 1 defining the empirical quantile
#'   used to derive the adaptive threshold from the rolling dispersion
#'   distribution.
#'
#' @param k Numeric multiplier applied to the MAD of rolling dispersion when
#'   computing the robust threshold (median + k * MAD). Higher values make the
#'   detector more conservative.
#'
#' @param min_ratio Numeric. Minimum ratio between the maximum and median
#'   rolling dispersion required to classify a time series as containing
#'   bubbling events. Helps prevent detections in low-variance incubations.
#'
#' @param min_sd Optional numeric value defining the minimum standard deviation
#'   of the original concentration time series required to perform bubbling
#'   detection. If the global variability is below this threshold, the function
#'   returns `NULL`.
#'
#' @param min_gap Numeric. Minimum gap (in seconds) separating two bubbling
#'   events. Chunks separated by less than this value are merged.
#'
#' @param min_length Numeric. Minimum duration (in seconds) required for a
#'   bubbling event to be retained.
#'
#' @return
#' A data frame with two columns:
#' - `start` : starting time of detected bubbling event
#' - `end`   : ending time of detected bubbling event
#'
#' Returns `NULL` if no bubbling events are detected or if the time series
#' does not meet the minimum variability criteria.
#'
#' @details
#' The algorithm first standardizes the concentration signal using robust
#' statistics (median and MAD), interpolates the signal to a regular time grid,
#' and computes rolling dispersion within a moving window. Bubbling events are
#' identified as contiguous periods where dispersion exceeds an adaptive
#' threshold.
#'
#' This method is designed to detect sustained fluctuations associated with
#' ebullition while minimizing false detections caused by instrumental noise
#' or minor variability in diffusive incubations.
#'
#' @examples
#' bubbles <- find.bubbles(time = df$time,
#'                         conc = df$CH4dry_ppb,
#'                         window.size = 15)
#'
#' @export
find.bubbles <- function(time,
                         conc,
                         window.size,
                         dt = 1,
                         method = c("mad","variance"),
                         var.quantile = 0.7,
                         k = 4,
                         min_ratio = 3,
                         min_sd = NULL,
                         min_gap = 10,
                         min_length = 5) {

  method <- match.arg(method)

  # --- sort and remove duplicates
  ord <- order(time)
  time <- time[ord]
  conc <- conc[ord]

  dup <- duplicated(time)
  if (any(dup)) {
    warning("Duplicated time values detected; keeping first occurrence.")
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
  chunks <- chunks[(chunks$end - chunks$start) > min_length, ]

  if (nrow(chunks) == 0)
    return(NULL)

  return(chunks)
}
