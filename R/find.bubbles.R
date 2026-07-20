#' Detect bubbling (ebullition) events in a chamber incubation time series
#'
#' Identifies bubbling events in a gas concentration time series by analysing
#' rolling dispersion within a moving window. Dispersion can be quantified from
#' the rolling median absolute deviation (\code{"mad"}), the rolling variance
#' (\code{"variance"}), or the rolling variance of the first differences
#' (\code{"diff"}). Contiguous periods where dispersion exceeds an adaptive
#' threshold are flagged as candidate bubbling events.
#' of the concentration step is then estimated with a local step-dummy
#' regression that separates the abrupt bubble step from the underlying
#' diffusive trend.
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
#' @param method Character; dispersion metric. One of \code{"mad"} (default),
#'   \code{"variance"} or \code{"diff"}. \code{"diff"} computes rolling variance
#'   of the first differences and is largely insensitive to a linear diffusive
#'   trend, so it is recommended when diffusion is strong (see Details).
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
#' @param min_magnitude Numeric or \code{NULL}; minimum absolute step magnitude
#'   (same units as \code{bubble_source}) to retain an event. Default \code{5}.
#'
#' @param min_snr Numeric or \code{NULL}; if set, minimum signal-to-noise ratio
#'   (\code{|magnitude| / SE}) to retain an event.
#'
#' @return A data.frame with one row per retained bubbling event and columns
#'   \code{start}, \code{end}, \code{magnitude}, \code{SE}, \code{slope} and
#'   \code{n_used}; or \code{NULL} if no events are detected or the series does
#'   not meet the minimum variability criteria.
#'
#' @details
#' The signal is first robustly standardised (median / MAD) and interpolated
#' onto a regular \code{dt}-second grid. Rolling dispersion is then computed and
#' compared to an adaptive threshold defined as the maximum of an empirical
#' quantile (\code{var.quantile}) and a robust bound (\eqn{median + k \times
#' MAD}). Contiguous supra-threshold runs are merged (\code{min_gap}) and
#' filtered by duration (\code{min_length}).
#'
#' A steep but purely diffusive rise inflates the rolling variance/MAD even in
#' the absence of bubbles, which can produce false positives. The \code{"diff"}
#' method mitigates this by operating on the increments of the signal: a genuine
#' ebullition step produces a large positive spike in the first differences,
#' whereas a linear diffusive trend produces roughly constant increments and
#' therefore low differenced dispersion.
#'
#' Event magnitude is estimated with the local model
#' \deqn{C_t = \beta_0 + \beta_1 t + \beta_2 I(t \ge t_b)}
#' where \eqn{I(t \ge t_b)} is a step dummy at the event start; \eqn{\beta_2} is
#' the estimated magnitude. Only events with a positive magnitude are retained
#' (ebullition adds gas to the headspace).
#'
#' @examples
#' \dontrun{
#' bubbles <- find.bubbles(df = incubation_data,
#'                         bubble_source = "CH4dry_ppb",
#'                         window.size = 15,
#'                         method = "diff")
#' }
#'
#' @importFrom zoo rollapply
#' @importFrom stats approx mad median quantile sd var
#'
#' @keywords internal
#'
find.bubbles <- function(df,
                         bubble_source,
                         window.size = 15,     # secs
                         dt = 1,
                         method = c("mad", "variance", "diff"),
                         var.quantile = 0.7,
                         k = 4,
                         min_ratio = 3,
                         min_sd = NULL,
                         min_gap = 10,
                         min_length = 5,
                         max_reg_window = 120,
                         reg.min.obs = 10,
                         min_magnitude = 5,       # ppb
                         min_snr = NULL) {

  method <- match.arg(method)

  time0 <- df$Etime[1]
  time  <- as.numeric(df$Etime - time0)
  conc  <- df[[bubble_source]]

  # ---- Input validation ----
  if (length(time) != length(conc)) stop("time and conc must have equal length")
  if (length(time) < 30)   stop("At least 30 observations required")
  if (window.size < 3)     stop("window.size must be at least 3")
  if (var.quantile <= 0 || var.quantile >= 1) stop("var.quantile must be in (0, 1)")

  # ---- Sort and de-duplicate on time ----
  ord  <- order(time)
  time <- time[ord]; conc <- conc[ord]
  dup  <- duplicated(time)
  if (any(dup)) { time <- time[!dup]; conc <- conc[!dup] }
  if (length(time) < window.size) return(NULL)

  # ---- Global low-variance guard ----
  global_sd <- sd(conc, na.rm = TRUE)
  if (!is.null(min_sd) && global_sd < min_sd) return(NULL)

  # ---- Robust standardisation (avoid division by zero) ----
  mad_conc <- mad(conc, na.rm = TRUE)
  if (mad_conc == 0 || is.na(mad_conc)) return(NULL)
  conc_std <- (conc - median(conc, na.rm = TRUE)) / mad_conc

  # ---- Interpolate onto a regular grid ----
  x <- seq(min(time), max(time), by = dt)
  conc_interp <- approx(time, conc_std, xout = x,
                        method = "linear", rule = 2)$y
  if (length(conc_interp) < window.size) return(NULL)

  # ---- Rolling dispersion ----
  ## The "diff" method rolls the VARIANCE over the first differences. A
  ## constant (linear) diffusive slope gives near-constant increments -> low
  ## differenced variance, while an ebullition step produces one large
  ## increment -> a strong variance spike. (Rolling MAD of increments would
  ## *ignore* that lone spike, which is why variance is used here.)
  disp_input <- if (method == "diff") {
    c(0, diff(conc_interp))   # pad to keep length; leading increment = 0
  } else conc_interp

  disp_fun <- if (method == "mad") function(v) mad(v) else function(v) var(v)

  roll_stat <- zoo::rollapply(disp_input, width = window.size,
                              align = "center", fill = NA, FUN = disp_fun)
  if (all(is.na(roll_stat))) return(NULL)

  valid_stat <- roll_stat[!is.na(roll_stat)]
  if (length(valid_stat) < 5) return(NULL)

  # ---- Dispersion ratio guard ----
  med_stat <- median(valid_stat)
  if (med_stat == 0) return(NULL)
  if (max(valid_stat) / med_stat < min_ratio) return(NULL)

  # ---- Adaptive threshold ----
  q_thresh   <- quantile(valid_stat, var.quantile, na.rm = TRUE)
  rob_thresh <- med_stat + k * mad(valid_stat, na.rm = TRUE)
  thresh     <- max(q_thresh, rob_thresh)

  high_disp <- roll_stat > thresh
  high_disp[is.na(high_disp)] <- FALSE
  if (!any(high_disp)) return(NULL)

  # ---- Contiguous supra-threshold runs ----
  r      <- rle(high_disp)
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  chunks <- data.frame(start = x[starts[r$values]],
                       end   = x[ends[r$values]])

  # ---- Merge close runs ----
  if (nrow(chunks) > 1) {
    merged <- chunks[1, , drop = FALSE]
    for (i in 2:nrow(chunks)) {
      gap <- chunks$start[i] - merged$end[nrow(merged)]
      if (gap <= min_gap) merged$end[nrow(merged)] <- chunks$end[i]
      else merged <- rbind(merged, chunks[i, ])
    }
    chunks <- merged
  }

  # ---- Drop short runs ----
  chunks <- chunks[(chunks$end - chunks$start) >= min_length, ]
  if (nrow(chunks) == 0) return(NULL)

  # ---- Estimate magnitude with a local step-dummy regression ----
  chunks$magnitude <- NA_real_
  chunks$SE        <- NA_real_
  chunks$slope     <- NA_real_
  chunks$n_used    <- NA_integer_

  # First differences on the (deduplicated) raw series, aligned to each point.
  raw_incr <- c(NA_real_, diff(conc))

  for (i in seq_len(nrow(chunks))) {

    ## Anchor the step dummy at the LARGEST positive jump inside the chunk
    ## rather than at the chunk's leading edge. Detection windows (especially
    ## the symmetric "diff"/"variance" ones) start before the true step, which
    ## would otherwise bias the magnitude low.
    inchunk <- which(time >= chunks$start[i] & time <= chunks$end[i])
    if (length(inchunk) >= 2 && any(is.finite(raw_incr[inchunk]))) {
      tb.start <- time[inchunk[which.max(raw_incr[inchunk])]]
    } else {
      tb.start <- chunks$start[i]
    }

    # Regression bounds: between neighbouring events, clamped to a half-window.
    tmin <- if (i == 1) min(time) else chunks$end[i - 1] + dt
    tmax <- if (i == nrow(chunks)) max(time) else chunks$start[i + 1] - dt
    tmin <- max(tmin, tb.start - max_reg_window)
    tmax <- min(tmax, tb.start + max_reg_window)

    idx <- time >= tmin & time <= tmax
    if (sum(idx) < reg.min.obs) next

    df_local <- data.frame(time = time[idx], conc = conc[idx])

    # Require observations on both sides of the step.
    if (sum(df_local$time <  tb.start) < 3 ||
        sum(df_local$time >= tb.start) < 3) next

    df_local$bubble        <- ifelse(df_local$time >= tb.start, 1, 0)
    df_local$time_centered <- df_local$time - tb.start

    mod <- try(lm(conc ~ time_centered + bubble, data = df_local), silent = TRUE)
    if (inherits(mod, "try-error")) next

    coefs <- summary(mod)$coefficients
    if (!"bubble" %in% rownames(coefs)) next

    chunks$magnitude[i] <- coefs["bubble", "Estimate"]
    chunks$SE[i]        <- coefs["bubble", "Std. Error"]
    chunks$slope[i]     <- coefs["time_centered", "Estimate"]
    chunks$n_used[i]    <- nrow(df_local)
  }

  # ---- Filter: keep positive, sufficiently large / significant steps ----
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
