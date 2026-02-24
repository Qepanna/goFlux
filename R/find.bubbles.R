#' Detect Ebullition Events in Floating Chamber CH4 Time Series
#'
#' Identifies putative methane (CH4) ebullition events in floating chamber
#' incubation time series by detecting sustained periods of elevated
#' short-term variance relative to background diffusive dynamics.
#'
#' The algorithm assumes that:
#' \itemize{
#'   \item Diffusive flux produces a smooth, low-variance concentration increase.
#'   \item Ebullition produces abrupt concentration jumps, generating
#'         locally elevated variance.
#' }
#'
#' Detection is performed by:
#' \enumerate{
#'   \item Robust standardization of the concentration time series.
#'   \item Linear interpolation to a regular time grid.
#'   \item Computation of rolling-window variance.
#'   \item Thresholding using an empirical variance quantile.
#'   \item Grouping contiguous high-variance windows into bubbling events.
#'   \item Merging temporally close events.
#'   \item Removing short-duration events.
#' }
#'
#' @param time Numeric vector of elapsed time (seconds).
#'   Must be monotonic (duplicates are removed internally).
#'
#' @param conc Numeric vector of CH4 concentrations corresponding to `time`
#'   (e.g., ppm, ppb, or µmol mol-1). Units do not affect detection
#'   because the algorithm operates on standardized values.
#'
#' @param window.size Integer.
#'   Width of the rolling variance window (in number of interpolated time steps).
#'   Controls the temporal scale of detectable bubbling events.
#'
#' @param dt Numeric.
#'   Interpolation time step (seconds). Default is 1.
#'   Smaller values increase temporal resolution but may amplify noise.
#'
#' @param var.quantile Numeric in (0,1).
#'   Empirical quantile used to derive the variance threshold.
#'   The threshold equals:
#'   \deqn{Q_{var.quantile}(Var_w)}
#'   where \eqn{Var_w} is the rolling variance of the standardized series.
#'   Larger values yield more conservative detection.
#'
#' @param min_gap Numeric.
#'   Maximum time gap (seconds) between adjacent high-variance segments
#'   to be merged into a single bubbling event.
#'
#' @param min_length Numeric.
#'   Minimum duration (seconds) required for a segment to be classified
#'   as an ebullition event.
#'
#' @return
#' A data.frame with columns:
#' \describe{
#'   \item{start}{Start time (seconds) of detected ebullition event.}
#'   \item{end}{End time (seconds) of detected ebullition event.}
#' }
#' Returns NULL if no bubbling events are detected.
#'
#' @details
#' This method implements a variance-based regime classification approach,
#' separating high-frequency concentration instability (ebullition)
#' from smooth diffusive accumulation.
#'
#' The detection threshold is data-adaptive, making the method robust
#' across systems with differing background flux magnitudes.
#'
#' Users are encouraged to evaluate sensitivity to `window.size` and
#' `var.quantile`, as these parameters control temporal resolution and
#' detection conservativeness.
#'
#' @section Assumptions:
#' \itemize{
#'   \item Ebullition events manifest as short-term variance increases.
#'   \item Background diffusion is locally smooth relative to bubbling.
#'   \item Interpolation does not distort event structure.
#' }
#'
#' @section Recommended practice:
#' Perform sensitivity analysis across multiple `var.quantile` values
#' and visually inspect detected events against raw time series.
#'
#' @examples
#' # Example:
#' # chunks <- find.bubbles(time, conc, window.size = 30)
#'
#' @export
find.bubbles <- function(time,
                         conc,
                         window.size,
                         dt = 1,
                         var.quantile = 0.7,
                         min_gap = 10,
                         min_length = 5) {

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

  # --- scaling
  conc_std <- (conc - median(conc)) / mad(conc)

  # --- interpolation
  x <- seq(min(time), max(time), by = dt)
  conc_interp <- approx(time, conc_std,
                        xout = x,
                        method = "linear",
                        rule = 2)$y

  # --- rolling variance
  roll_var <- zoo::rollapply(conc_interp,
                             width = window.size,
                             FUN = var,
                             align = "center",
                             fill = NA)

  # --- threshold
  thresh <- max(1e-4,
                quantile(roll_var, var.quantile, na.rm = TRUE))

  high_var <- roll_var > thresh
  high_var[is.na(high_var)] <- FALSE

  if (!any(high_var)) return(NULL)

  # --- find contiguous chunks
  r <- rle(high_var)
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

  if (nrow(chunks) == 0) return(NULL)

  return(chunks)
}

