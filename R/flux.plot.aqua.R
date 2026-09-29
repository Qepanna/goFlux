## ---------------------------------------------------------------------------
## flux.plot.aqua() and its internal helpers.
##
## Diagnostic plotting for aquatic floating-chamber incubations, in which the
## measured concentration signal is the sum of a diffusive component (a smooth
## trend fitted over an early, bubble-free window) and an ebullitive component
## (discrete, short-lived concentration jumps).
##
## The figure is laid out so that each visual channel carries exactly one kind
## of information:
##   * the x-axis is elapsed time, and both shaded bands mark time windows;
##   * fill encodes which window (diffusive vs. ebullitive);
##   * colour encodes which model fit (the diffusive model selected by
##     best.flux, or the bubble fits);
##   * shape and opacity encode whether an observation was retained or
##     discarded by the quality flag, and, for a gas other than the bubble gas,
##     light grey marks retained observations after a diffusive window cut
##     short by ebullition;
##   * the numeric flux estimates live in the plot header, outside the panel;
##   * the quality checks are reported in the caption, as in flux.plot(): the
##     best.flux check of the diffusive fit for every gas, and the ebullition
##     check for the gas the bubbles were detected on.
##
## Across both fill and colour, blue tones always denote the diffusive
## component and vermillion the ebullitive one.
##
## Keeping the estimates out of the panel is deliberate: chamber concentration
## series are usually monotonic, so any in-panel corner is eventually occupied
## by data. A header block cannot be overplotted for any incubation.
## ---------------------------------------------------------------------------


#' Hutchinson and Mosier non-linear concentration model
#'
#' Evaluates the HM model \eqn{C(t) = C_i + (C_0 - C_i) e^{-kt}}, used to
#' describe curvature in closed-chamber concentration series.
#'
#' @param Ci Numeric; asymptotic concentration.
#' @param C0 Numeric; initial concentration.
#' @param k Numeric; curvature parameter.
#' @param x Numeric vector; elapsed time (s).
#'
#' @return Numeric vector of modelled concentrations, the same length as
#'   \code{x}.
#'
#' @noRd
.hm_model <- function(Ci, C0, k, x) Ci + (C0 - Ci) * exp(-k * x)


#' Convert a plotmath unit string to plain Unicode text
#'
#' Flux units are conventionally supplied in plotmath syntax (for example
#' \code{"nmol~m^-2*s^-1"}) so that they can be passed to
#' \code{\link[ggplot2]{ylab}}. Header and caption text is not parsed as
#' plotmath, so such a string would otherwise be rendered verbatim. This helper
#' rewrites the common exponent patterns as Unicode superscripts.
#'
#' @param u Character string; unit label, in plotmath or plain syntax.
#'
#' @return Character string suitable for display as ordinary text.
#'
#' @noRd
.plain_unit <- function(u) {
  u <- gsub("~", " ", u, fixed = TRUE)
  u <- gsub("*", " ", u, fixed = TRUE)
  u <- gsub("^-1", "\u207B\u00B9", u, fixed = TRUE)
  u <- gsub("^-2", "\u207B\u00B2", u, fixed = TRUE)
  u <- gsub("^-3", "\u207B\u00B3", u, fixed = TRUE)
  gsub("\\s+", " ", trimws(u))
}


#' Format a flux estimate and its standard error for display
#'
#' Uses significant digits rather than a fixed number of decimals, so that the
#' same formatter reads correctly both for CO2 fluxes of order unity and for CH4
#' fluxes several orders of magnitude smaller.
#'
#' @param v Numeric; flux estimate.
#' @param se Numeric; standard error of the estimate.
#'
#' @return Character string of the form \code{"1.23 \u00B1 0.04"}, or
#'   \code{"NA"} when the estimate is missing.
#'
#' @noRd
.fmt_flux <- function(v, se) {
  if (length(v) == 0 || is.na(v)) return("NA")
  paste0(signif(v, 3), " \u00B1 ",
         if (length(se) == 0 || is.na(se)) "NA" else signif(se, 2))
}


#' Read one diagnostic value, or NA when it is not available
#'
#' @param d One-row data.frame of diagnostics, or \code{NULL}.
#' @param col Character; column name.
#'
#' @return The value of \code{col} in \code{d}, or \code{NA}.
#'
#' @noRd
.diag_value <- function(d, col) {
  if (is.null(d) || !col %in% names(d) || length(d[[col]]) == 0) return(NA)
  d[[col]][1]
}


#' Build the quality-check lines of a diagnostic figure
#'
#' Two complementary checks are reported, each from its own source:
#' \itemize{
#'   \item the quality check of the diffusive fit, i.e. the columns
#'     \code{model} and \code{quality.check} returned by
#'     \code{\link[goFlux]{best.flux}}, reported for every gas as in
#'     \code{\link[goFlux]{flux.plot}};
#'   \item the ebullition check of \code{\link{goAquaFlux.diagnostics}}, only
#'     available for the gas on which the bubbles were detected.
#' }
#'
#' @param model,qc Character; \code{model} and \code{quality.check} of the
#'   diffusive fit, or \code{NA}.
#' @param d One-row data.frame with the ebullition check of this incubation
#'   and gas, or \code{NULL}.
#' @param unit Character; flux unit, as plain text.
#' @param conv Numeric; conversion factor applied to displayed fluxes.
#'
#' @return Character vector of caption lines (possibly empty).
#'
#' @noRd
.quality_lines <- function(model, qc, d, unit, conv) {
  lines <- character(0)

  ## Diffusive fit (best.flux).
  if (!is.na(qc)) {
    lines <- c(lines, paste0("quality check (diffusive",
                             if (!is.na(model)) paste0(", ", model) else "", "): ",
                             if (nzchar(qc)) qc else "passed"))
  }

  ## Ebullitive component (goAquaFlux.diagnostics), bubble gas only.
  chk <- as.character(.diag_value(d, "ebullition.check"))
  if (!is.na(chk)) {
    cl <- .diag_value(d, "closure")
    detail <- if (chk == "no bubble detected") {
      dl <- .diag_value(d, "ebullition_detection_limit")
      if (is.finite(dl)) paste0(" (detection limit ", signif(dl * conv, 2), " ", unit, ")") else ""
    } else if (!is.finite(cl)) {
      ""
    } else if (startsWith(chk, "closure")) {
      sprintf(" (%.2f)", cl)
    } else sprintf(" (closure %.2f)", cl)
    lines <- c(lines, paste0("quality check (ebullitive): ",
                             if (nzchar(chk)) chk else "passed", detail))
  }
  lines
}


#' Evaluate the fitted step + re-equilibration model of one bubbling event
#'
#' Rebuilds the local model fitted by \code{find.bubbles},
#' \deqn{C_t = \beta_0 + \beta_1 (t - t_b) + I(t \ge t_s)
#'   [\beta_2 + \beta_3 e^{-(t - t_p)/\tau}],}
#' from the columns of one row of \code{bubbles}. The curve starts at the
#' beginning of the event band (\code{start}, or \code{fit.start} if later)
#' rather than at \code{fit.start}: the full pre-bubble branch would cover the
#' diffusive fits and the tail of the previous event, while a short lead-in is
#' enough to anchor the jump. It ends at \code{fit.end}. The pre- and post-step
#' branches share the abscissa \eqn{t_s}, so that the jump is drawn as a
#' vertical segment.
#'
#' @param b One-row data.frame from \code{find.bubbles} (via
#'   \code{goAquaFlux}), with an added integer column \code{event} that
#'   identifies the event within the incubation.
#' @param n Integer; number of points used for the post-step branch. The
#'   pre-step lead-in uses a quarter as many.
#'
#' @return A list with \code{fit}, the modelled curve, and \code{settled},
#'   the settled post-bubble level (\eqn{\beta_2} without the transient), or
#'   \code{NULL} when no re-equilibration term was retained. Both are
#'   data.frames with columns \code{x}, \code{y} and \code{event}.
#'
#' @noRd
.bubble_curve <- function(b, n = 200) {
  # overshoot is 0 when the plain step was retained and NA with
  # magnitude.model = "step": in both cases there is no transient term.
  has_reeq <- is.finite(b$overshoot) && b$overshoot != 0 && is.finite(b$tau)

  level <- function(t, on) {
    y <- b$intercept + b$slope * (t - b$t.bubble)
    if (on) y <- y + b$magnitude
    y
  }

  t_lead <- min(max(b$start, b$fit.start), b$t.step)
  t_pre  <- seq(t_lead, b$t.step, length.out = max(2L, n %/% 4))
  t_post <- seq(b$t.step,    b$fit.end, length.out = n)

  y_post <- level(t_post, TRUE)
  if (has_reeq) {
    y_post <- y_post + b$overshoot * exp(-pmax(t_post - b$t.peak, 0) / b$tau)
  }

  fit <- data.frame(x = c(t_pre, t_post),
                    y = c(level(t_pre, FALSE), y_post),
                    event = b$event)

  settled <- if (has_reeq) {
    data.frame(x = c(b$t.step, b$fit.end),
               y = level(c(b$t.step, b$fit.end), TRUE),
               event = b$event)
  } else NULL

  list(fit = fit, settled = settled)
}


#' Plot aquatic chamber incubations with diffusive and ebullitive components
#'
#' Produces one diagnostic figure per incubation from the output of
#' \code{\link{goAquaFlux}}, showing the measured concentration series, the
#' window over which the diffusive flux was fitted, the detected ebullition
#' events, the diffusive model selected by \code{\link[goFlux]{best.flux}}
#' (linear, LM, or Hutchinson-Mosier, HM), and the resulting total, diffusive
#' and ebullitive flux estimates.
#'
#' @details
#' Each figure is composed of the following elements.
#'
#' \describe{
#'   \item{Observations}{All records for the incubation are drawn. Points
#'     retained by the quality flag (\code{flag == 1}) are filled and opaque;
#'     discarded points are hollow and faded, so that they remain visible for
#'     diagnosis without competing with the retained series. For a gas other
#'     than the one the bubbles were detected on (e.g. CO2 with bubbles
#'     detected on CH4), when ebullition cut the diffusive window short, only
#'     the retained points of the diffusive window are black: those after it,
#'     which do not enter any flux estimate for that gas, are light grey
#'     (\emph{outside diffusive window}).}
#'   \item{Diffusive window}{A full-height band from \eqn{t = 0} to the end of
#'     the window used to fit the diffusive flux. The extent of the window is
#'     taken from \code{n_obs.diffusion} in \code{flux_summary} and applied to
#'     the time-ordered retained series.}
#'   \item{Ebullition events}{Full-height bands spanning the start and end times
#'     of each detected bubbling event. Events are detected on a single gas
#'     (\code{bubble.gas}) and shared by all gases of an incubation. They are
#'     drawn for every gas, including one without ebullition, because they
#'     show whether the diffusive window was cut short by bubbling or by
#'     another selection step. When they come from another gas, the legend
#'     names it, e.g. \emph{ebullition event (CH4)}.}
#'   \item{Diffusive fit}{Only the model selected by
#'     \code{\link[goFlux]{best.flux}} (column \code{model} of
#'     \code{diffusive}) is drawn, in dark blue, and the legend names it, e.g.
#'     \emph{diffusive fit (HM)}. It is drawn only across the diffusive window,
#'     since it is not fitted to the ebullitive part of the series. A fit that
#'     stops short of, or runs past, the bubble-free portion of the record is
#'     then a direct indication that the window was misidentified. For output
#'     without a \code{model} column (earlier versions), both the LM (dark
#'     blue) and HM (sky blue) fits are drawn.}
#'   \item{Bubble fits}{For each event whose magnitude is significant
#'     (\code{magnitude / SE >= bubble.snr}), the step + re-equilibration model
#'     fitted by \code{find.bubbles}, in vermillion. It is drawn from the start
#'     of the event band to the end of that event's fit window: pre-bubble
#'     lead-in, jump to the transient peak and exponential re-equilibration.
#'     When a re-equilibration term was retained, the settled post-bubble level
#'     is added as a dashed line; the estimated magnitude is the vertical
#'     offset between it and the pre-bubble trend. A curve that has not
#'     converged on the dashed line by the end of the fit window indicates an
#'     extrapolated settled level (\code{reequil.complete = FALSE}).
#'
#'     The models are in the units of the gas the events were detected on, so
#'     they are drawn only on that gas's panel (see \code{bubble.gas}), and
#'     only when it has an ebullitive component: a finite, non-zero
#'     \code{flux_ebullition} and a total flux that differs from the diffusive
#'     flux. Redrawing a model requires the columns \code{intercept},
#'     \code{t.bubble}, \code{t.step}, \code{t.peak}, \code{fit.start} and
#'     \code{fit.end} returned by \code{find.bubbles}; if \code{bubbles}
#'     lacks them (output of an earlier version), the fits are skipped with a
#'     warning and the rest of the figure is unaffected.}
#'   \item{Flux estimates}{Reported in the plot subtitle, with their unit in the
#'     caption. They are placed outside the panel so that they cannot overlap
#'     the data for any incubation.}
#'   \item{Quality checks}{When \code{quality.check = TRUE}, the caption
#'     reports the two complementary checks of \code{\link{goAquaFlux}}. The
#'     quality check of the diffusive fit (\code{quality.check} and
#'     \code{model} from \code{\link[goFlux]{best.flux}}) is shown for every
#'     gas, as in \code{\link[goFlux]{flux.plot}}. The ebullition check
#'     (\code{ebullition.check}, see \code{\link{goAquaFlux.diagnostics}}) is
#'     shown only on the panel of the gas the bubbles were detected on, the only
#'     gas for which it is computed: the figure of any other gas (e.g. CO2 with
#'     bubbles detected on CH4) is assessed from that gas's own fit alone. The
#'     closure is given in brackets and, when no bubble was detected, the
#'     detection limit of ebullition.}
#' }
#'
#' If the \pkg{ggtext} package is available, the subtitle is rendered with the
#' diffusive and ebullitive estimates coloured to match their respective bands
#' and fits; otherwise a plain-text subtitle is used. The two variants are
#' identical in content.
#'
#' Layers are drawn back to front: bands, observations, bubble fits, then the
#' diffusive fit. The y-axis range covers every drawn element within the
#' x-window, including discarded points and extrapolated bubble-fit levels, so
#' that nothing is silently clipped.
#'
#' Colours follow the Okabe-Ito palette, which remains distinguishable under the
#' common forms of colour vision deficiency and in greyscale print.
#'
#' @param flux.results.ls The list returned by \code{\link{goAquaFlux}} (with
#'   \code{return_df = TRUE}), containing \code{flux_summary}, \code{bubbles},
#'   \code{diffusive} and \code{diagnostics}. \code{bubbles} may be \code{NULL} when no
#'   ebullition detection was run. For backwards compatibility a plain
#'   \code{best.flux}-style data.frame may also be supplied, in which case the
#'   call is delegated to \code{\link[goFlux]{flux.plot}} and only the diffusive
#'   component is plotted.
#' @param dataframe Data.frame of measurements used by \code{\link{goAquaFlux}},
#'   containing at least \code{UniqueID}, \code{Etime}, \code{flag} and the
#'   column named by \code{gastype}.
#' @param gastype Character string; the gas column to plot. One of
#'   \code{"CO2dry_ppm"}, \code{"COdry_ppb"}, \code{"CH4dry_ppb"},
#'   \code{"N2Odry_ppb"}, \code{"NO2dry_ppb"}, \code{"NOdry_ppb"},
#'   \code{"NH3dry_ppb"} or \code{"H2O_ppm"}.
#' @param shoulder Numeric; padding in seconds added before and after the
#'   measurement on the x-axis. Default \code{30}.
#' @param plot.display Character vector of overlays to draw, any of
#'   \code{"diffusive.window"}, \code{"ebullition.events"} and
#'   \code{"bubble.fits"}; other values are an error. Pass \code{NULL} to draw
#'   the observations and the diffusive fit only. All three overlays are shown
#'   by default.
#' @param bubble.snr Numeric or \code{NULL}; minimum signal-to-noise ratio
#'   (\code{magnitude / SE}) for an event's fitted model to be drawn. The
#'   default \code{2} corresponds roughly to a magnitude significantly greater
#'   than zero at the 5\% level. \code{NULL} draws the fit of every event,
#'   including those whose \code{SE} is missing.
#' @param bubble.gas Character string or \code{NULL}; the gas on which
#'   ebullition events were detected by \code{\link{goAquaFlux}}, e.g.
#'   \code{"CH4dry_ppb"}. Bubble fits are drawn only when \code{gastype}
#'   matches it, and when it differs the legend names it (e.g.
#'   \emph{ebullition event (CH4)}). A \code{bubble.gas} (or
#'   \code{bubble_source}) column in \code{bubbles}, when present, takes
#'   precedence. When neither is available, the events are assumed to belong
#'   to \code{gastype}.
#' @param flux.unit Character string or \code{NULL}; the flux unit shown in the
#'   caption. Plotmath syntax (for example \code{"nmol~m^-2*s^-1"}) is accepted
#'   and converted to plain text. When \code{NULL}, a unit consistent with
#'   \code{gastype} is chosen.
#' @param quality.check Logical; if \code{TRUE} (default), the quality check
#'   of the diffusive fit and, for the bubble gas, the ebullition check are
#'   reported in the figures (see \strong{Details}). Results of an earlier
#'   version without ebullition check show the quality check of the diffusive
#'   fit only, with a message.
#' @param conversion.factor Numeric greater than zero; multiplier applied to the
#'   displayed flux estimates and their standard errors, for reporting in a unit
#'   other than the one returned by \code{\link{goAquaFlux}}. Default \code{1}.
#'
#' @return A named list of \code{\link[ggplot2]{ggplot}} objects, one per
#'   \code{UniqueID} and named by it. The objects are returned unprinted and can
#'   be modified further with standard \pkg{ggplot2} syntax.
#'
#' @seealso \code{\link{goAquaFlux}} for the flux calculation itself, and
#'   \code{\link[goFlux]{flux.plot}} for the diffusion-only equivalent.
#'
#' @examples
#' \dontrun{
#' res <- goAquaFlux(mydata, gastype = "CH4dry_ppb", return_df = TRUE)
#'
#' plots <- flux.plot.aqua(res, mydata, gastype = "CH4dry_ppb")
#'
#' # Inspect a single incubation by name
#' plots[["LAKE01-2026-05-12-01"]]
#'
#' # CO2 of the same incubations, with bubbles detected on CH4: the CH4
#' # ebullition events are shown as bands, without the CH4 bubble fits
#' res_co2 <- goAquaFlux(mydata, gastype = "CO2dry_ppm",
#'                       bubble.gas = "CH4dry_ppb", return_df = TRUE)
#' plots_co2 <- flux.plot.aqua(res_co2, mydata, gastype = "CO2dry_ppm",
#'                             bubble.gas = "CH4dry_ppb")
#'
#' # Only the incubations whose ebullition check failed
#' chk <- res$flux_summary$ebullition.check
#' plots[res$flux_summary$UniqueID[!is.na(chk) & nzchar(chk) &
#'                                   chk != "no bubble detected"]]
#'
#' # Write all diagnostics to a multi-page PDF
#' pdf("ch4_diagnostics.pdf", width = 8, height = 5)
#' invisible(lapply(plots, print))
#' dev.off()
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_rect geom_segment geom_line
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 scale_colour_manual scale_fill_manual scale_shape_manual
#' @importFrom ggplot2 scale_alpha_manual scale_x_continuous xlab ylab labs
#' @importFrom ggplot2 coord_cartesian theme_bw theme element_text element_blank
#' @importFrom ggplot2 element_line guides guide_legend margin unit
#' @importFrom dplyr %>% right_join group_by group_split filter
#' @importFrom pbapply pblapply pboptions
#' @importFrom stats na.omit complete.cases
#' @importFrom rlang .data
#'
#' @export
#'
flux.plot.aqua <- function(flux.results.ls, dataframe, gastype, shoulder = 30,
                           plot.display = c("diffusive.window", "ebullition.events",
                                            "bubble.fits"),
                           bubble.snr = 2,
                           bubble.gas = NULL,
                           flux.unit = NULL,
                           quality.check = TRUE,
                           conversion.factor = 1) {

  # ---- Argument validation --------------------------------------------------

  if (is.null(shoulder)) stop("'shoulder' is required")
  if (!is.numeric(shoulder) || shoulder < 0) {
    stop("'shoulder' must be numeric and non-negative")
  }

  if (missing(dataframe)) stop("'dataframe' is required")
  if (!is.data.frame(dataframe)) stop("'dataframe' must be a data.frame")

  if (missing(gastype)) stop("'gastype' is required")
  if (!is.character(gastype)) stop("'gastype' must be a character string")

  allowed_gastypes <- c("CO2dry_ppm", "COdry_ppb", "CH4dry_ppb", "N2Odry_ppb",
                        "NO2dry_ppb", "NOdry_ppb", "NH3dry_ppb", "H2O_ppm")
  if (!(gastype %in% allowed_gastypes)) {
    stop("'gastype' must be one of: ", paste(allowed_gastypes, collapse = ", "))
  }
  if (!any(grepl(paste0("\\<", gastype, "\\>"), names(dataframe)))) {
    stop("'dataframe' must contain a column matching 'gastype'")
  }

  if (missing(flux.results.ls)) stop("'flux.results.ls' is required")

  # A bare data.frame is assumed to be legacy best.flux output, which carries no
  # ebullition information; the diffusion-only routine handles it.
  if (is.data.frame(flux.results.ls)) {
    message("flux.results.ls is a data.frame; delegating to flux.plot() (diffusion only).")
    return(flux.plot(flux.results = flux.results.ls, dataframe = dataframe,
                     gastype = gastype, quality.check = TRUE,
                     plot.legend = c("MAE", "AICc", "k.ratio", "g.factor"),
                     plot.display = c("Ci", "C0", "MDF", "prec", "nb.obs", "flux.term")))
  }
  if (!is.list(flux.results.ls)) stop("'flux.results.ls' must be a list or a data.frame")

  flux.results <- flux.results.ls$flux_summary
  if (!is.data.frame(flux.results)) {
    stop("'flux.results.ls$flux_summary' must be a data.frame")
  }

  bubbles <- flux.results.ls$bubbles  # NULL when no ebullition detection was run

  required_cols <- c("UniqueID", "flux_total", "flux_diffusive", "flux_ebullition",
                     "SE_total", "SE_diffusive", "SE_ebullition", "first_bubble_time")
  missing_cols <- setdiff(required_cols, names(flux.results))
  if (length(missing_cols) > 0) {
    stop("'flux_summary' missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # n_obs.diffusion delimits the diffusive window. It is treated as optional so
  # that output from earlier versions still plots, but the fallback (the whole
  # retained series) is a weaker diagnostic and is therefore announced.
  has_n_obs_diff <- "n_obs.diffusion" %in% names(flux.results)
  if (!has_n_obs_diff) {
    warning("'flux_summary' has no 'n_obs.diffusion' column; the diffusive window ",
            "will span the whole retained series.")
  }

  if (!is.null(flux.unit) && !is.character(flux.unit)) {
    stop("'flux.unit' must be a character string or NULL")
  }
  if (!is.logical(quality.check)) stop("'quality.check' must be TRUE or FALSE")
  if (!is.numeric(conversion.factor) || conversion.factor <= 0) {
    stop("'conversion.factor' must be positive")
  }
  allowed_display <- c("diffusive.window", "ebullition.events", "bubble.fits")
  if (!is.null(plot.display)) {
    if (!is.character(plot.display)) {
      stop("'plot.display' must be a character vector or NULL")
    }
    bad_display <- setdiff(plot.display, allowed_display)
    if (length(bad_display) > 0) {
      stop("unknown 'plot.display' value(s): ", paste(bad_display, collapse = ", "),
           ". Supported: ", paste(allowed_display, collapse = ", "))
    }
  }

  # ---- Quality checks ----------------------------------------------------------

  # The quality check of the diffusive fit comes from the best.flux row of each
  # incubation ($diffusive). The ebullition check exists only for the gas the
  # bubbles were detected on: the full table ($diagnostics) is preferred, with
  # the columns copied into flux_summary as a fallback.
  diagnostics <- flux.results.ls$diagnostics
  if (!is.data.frame(diagnostics) && "ebullition.check" %in% names(flux.results)) {
    diagnostics <- flux.results
  }
  has_ebullition_check <- is.data.frame(diagnostics) &&
    "ebullition.check" %in% names(diagnostics)
  if (isTRUE(quality.check) && !has_ebullition_check &&
      !"ebullition.check" %in% names(flux.results)) {
    message("No ebullition check in 'flux.results.ls' (output of an earlier goAquaFlux ",
            "version); only the quality check of the diffusive fit is shown.")
  }

  # ---- Bubble-fit options ---------------------------------------------------

  if (!is.null(bubble.gas) &&
      (!is.character(bubble.gas) || length(bubble.gas) != 1L || is.na(bubble.gas))) {
    stop("'bubble.gas' must be a single character string or NULL")
  }
  # Column of $bubbles recording the gas on which the events were detected,
  # if goAquaFlux provides one; it takes precedence over 'bubble.gas'.
  bubble_gas_col <- if (is.data.frame(bubbles)) {
    intersect(c("bubble.gas", "bubble_source"), names(bubbles))[1]
  } else NA_character_
  if (!is.null(bubble.snr) &&
      (!is.numeric(bubble.snr) || length(bubble.snr) != 1L || is.na(bubble.snr) ||
       bubble.snr < 0)) {
    stop("'bubble.snr' must be a single non-negative number or NULL")
  }

  # The fitted event model can only be redrawn if find.bubbles() returned all
  # of its terms. Output from earlier versions lacks some of them: the figure
  # is then drawn without the bubble fits, and the user is told why.
  bubble_model_cols <- c("start", "magnitude", "SE", "slope", "overshoot", "tau",
                         "intercept", "t.bubble", "t.step", "t.peak",
                         "fit.start", "fit.end")
  draw_bubble_fits <- !is.null(plot.display) && "bubble.fits" %in% plot.display &&
    is.data.frame(bubbles) && nrow(bubbles) > 0
  if (draw_bubble_fits) {
    missing_bcols <- setdiff(bubble_model_cols, names(bubbles))
    if (length(missing_bcols) > 0) {
      warning("'bubbles' lacks the model terms needed to draw bubble fits (",
              paste(missing_bcols, collapse = ", "), "); re-run goAquaFlux() ",
              "with the current find.bubbles(). Bubble fits are skipped.")
      draw_bubble_fits <- FALSE
    }
  }

  # ---- Appearance constants -------------------------------------------------

  # Okabe-Ito palette. Blue tones are used throughout for the diffusive
  # component (dark blue for the diffusive band and the diffusive fit; sky blue
  # for the HM fit when both models are drawn for output without a 'model'
  # column) and vermillion for the ebullitive one (event bands and bubble
  # fits), so that the two components stay identifiable without the legend.
  col_diffusive  <- "#0072B2"
  col_hm         <- "#56B4E9"
  col_ebullitive <- "#D55E00"
  col_points     <- "grey15"
  col_outside    <- "grey75"

  use_markdown <- requireNamespace("ggtext", quietly = TRUE)

  # Concentration axis label, with subscripts, per gas.
  ylab_plot <- switch(gastype,
                      "CO2dry_ppm" = ylab(expression(CO["2"] * " dry (ppm)")),
                      "CH4dry_ppb" = ylab(expression(CH["4"] * " dry (ppb)")),
                      "N2Odry_ppb" = ylab(expression(N["2"] * "O dry (ppb)")),
                      "NO2dry_ppb" = ylab(expression(NO["2"] * " dry (ppb)")),
                      "NOdry_ppb"  = ylab("NO dry (ppb)"),
                      "COdry_ppb"  = ylab(expression(CO * " dry (ppb)")),
                      "NH3dry_ppb" = ylab(expression(NH["3"] * " dry (ppb)")),
                      "H2O_ppm"    = ylab(expression(H["2"] * "O (ppm)")))

  # Fluxes of the ppm gases are conventionally reported in umol m-2 s-1, and
  # those of the trace (ppb) gases in nmol m-2 s-1.
  if (is.null(flux.unit)) {
    flux.unit <- switch(gastype,
                        "CO2dry_ppm" = "\u00B5mol~m^-2*s^-1",
                        "H2O_ppm"    = "\u00B5mol~m^-2*s^-1",
                        "nmol~m^-2*s^-1")
  }
  flux.unit.plain <- .plain_unit(flux.unit)

  # ---- Data preparation -----------------------------------------------------

  # right_join keeps only the incubations that have a flux result, and attaches
  # the per-incubation estimates to every record of that incubation.
  data_split <- dataframe %>%
    right_join(flux.results, by = "UniqueID") %>%
    group_by(UniqueID) %>%
    group_split()

  data_corr      <- lapply(data_split, function(d) d %>% filter(flag == 1))
  data_diffusion <- flux.results.ls$diffusive

  # Silence R CMD check notes on columns referenced by non-standard evaluation.
  UniqueID <- Etime <- flag <- flag_lab <- HM_mod <- start <- end <- NULL
  x <- y <- event <- NULL

  # ---- One figure per incubation --------------------------------------------

  pboptions(char = "=")
  plot_list <- pblapply(seq_along(data_split), function(f) {

    # Sort by elapsed time, so that no later step has to assume that the storage
    # order of the records matches their chronological order.
    df_all  <- data_split[[f]][order(data_split[[f]]$Etime), ]
    df_good <- data_corr[[f]][order(data_corr[[f]]$Etime), ]

    incubation_id <- unique(df_all$UniqueID)

    flux_total <- unique(df_all$flux_total)      * conversion.factor
    SE_total   <- unique(df_all$SE_total)        * conversion.factor
    flux_diff  <- unique(df_all$flux_diffusive)  * conversion.factor
    SE_diff    <- unique(df_all$SE_diffusive)    * conversion.factor
    flux_ebull <- unique(df_all$flux_ebullition) * conversion.factor
    SE_ebull   <- unique(df_all$SE_ebullition)   * conversion.factor

    ## Diffusion model coefficients, when this incubation was fitted, and the
    ## model selected by best.flux. Only that model is drawn; output without a
    ## 'model' column (earlier versions) gets both.
    plot_diffusion <- FALSE
    best_model <- NA_character_
    if (!is.null(data_diffusion)) {
      ind_diff <- which(data_diffusion$UniqueID == incubation_id)
      if (length(ind_diff) >= 1) {
        plot_diffusion <- TRUE
        if ("model" %in% names(data_diffusion)) {
          best_model <- toupper(as.character(data_diffusion$model[ind_diff][1]))
          if (!best_model %in% c("LM", "HM")) best_model <- NA_character_
        }
        LM.slope <- unique(data_diffusion$LM.slope[ind_diff])
        LM.C0    <- unique(data_diffusion$LM.C0[ind_diff])
        HM.Ci    <- unique(data_diffusion$HM.Ci[ind_diff])
        HM.C0    <- unique(data_diffusion$HM.C0[ind_diff])
        HM.k     <- unique(data_diffusion$HM.k[ind_diff])
        df_all$HM_mod <- .hm_model(HM.Ci, HM.C0, HM.k, df_all$Etime)
      }
    }
    draw_lm <- plot_diffusion && (is.na(best_model) || best_model == "LM")
    draw_hm <- plot_diffusion && (is.na(best_model) || best_model == "HM")

    ## Diffusive window.
    ## n_obs.diffusion counts observations of the retained series, so it is
    ## applied to df_good and then translated into a cut-off time. Selecting on
    ## time rather than on row position keeps the window correct when retained
    ## records are interleaved with discarded ones.
    n_obs_diff <- if (has_n_obs_diff) {
      flux.results$n_obs.diffusion[flux.results$UniqueID == incubation_id]
    } else numeric(0)

    if (length(n_obs_diff) == 1 && !is.na(n_obs_diff) && n_obs_diff >= 1 &&
        nrow(df_good) > 0) {
      t_end   <- df_good$Etime[min(n_obs_diff, nrow(df_good))]
      df_diff <- df_good[df_good$Etime <= t_end, ]
    } else {
      df_diff <- df_good
    }

    ## Ebullition events recorded for this incubation.
    bubbles_f        <- NULL
    can.plot.bubbles <- FALSE
    if (!is.null(bubbles)) {
      bubbles_f        <- bubbles[bubbles$UniqueID == incubation_id, ]
      can.plot.bubbles <- nrow(bubbles_f) > 0
    }

    ## Bubble events are detected on a single gas (bubble.gas) and shared by
    ## every gas of the incubation. Their bands are always drawn, because they
    ## show whether the diffusive window was cut short by bubbling or by another
    ## selection step, even for a gas without ebullition. Only the fitted event
    ## models require this gas to have an ebullitive component: flux_ebullition
    ## finite and non-zero, and total flux different from diffusive flux.
    ebull_active <- length(flux_ebull) == 1L &&
      isTRUE(is.finite(flux_ebull) && flux_ebull != 0) &&
      !isTRUE(all.equal(flux_total, flux_diff))

    ## The fitted event models are in the units of the gas they were detected
    ## on, so they are drawn only on that gas's panel, even when this gas has a
    ## non-zero ebullitive flux estimated over the same windows.
    bgas <- if (can.plot.bubbles && !is.na(bubble_gas_col)) {
      as.character(unique(bubbles_f[[bubble_gas_col]])[1])
    } else if (!is.null(bubble.gas)) bubble.gas else gastype
    fits_same_gas <- identical(bgas, gastype)

    ## When the events come from another gas, the legend says so, so that
    ## e.g. a CO2 panel does not suggest the bands were detected on CO2.
    ebull_label <- if (fits_same_gas) "ebullition event" else
      paste0("ebullition event (", sub("(dry)?_pp[mb]$", "", bgas), ")")

    ## Fitted model of each significant bubbling event. The SE guard avoids a
    ## division by zero; an event with a missing SE has no finite SNR and is
    ## only drawn when bubble.snr = NULL.
    bfit_df <- bset_df <- NULL
    if (draw_bubble_fits && can.plot.bubbles && fits_same_gas && ebull_active) {
      bb <- bubbles_f
      bb$event <- seq_len(nrow(bb))
      snr  <- bb$magnitude / pmax(bb$SE, .Machine$double.eps)
      keep <- complete.cases(bb[, c("intercept", "slope", "magnitude",
                                    "t.bubble", "t.step", "fit.start", "fit.end")])
      if (!is.null(bubble.snr)) keep <- keep & is.finite(snr) & snr >= bubble.snr
      if (any(keep)) {
        curves  <- lapply(which(keep), function(j) .bubble_curve(bb[j, ]))
        bfit_df <- do.call(rbind, lapply(curves, `[[`, "fit"))
        bset_df <- do.call(rbind, lapply(curves, `[[`, "settled"))  # NULL if none
      }
    }

    ## Ebullition check of this incubation, for the gas it was computed on.
    diag_f <- NULL
    if (isTRUE(quality.check) && has_ebullition_check) {
      ind_q <- which(diagnostics$UniqueID == incubation_id)
      if ("gastype" %in% names(diagnostics)) {
        ind_q <- ind_q[diagnostics$gastype[ind_q] == gastype]
      }
      if (length(ind_q) >= 1) diag_f <- diagnostics[ind_q[1], , drop = FALSE]
    }
    ebull_check_f <- as.character(.diag_value(diag_f, "ebullition.check"))

    ## Axis ranges.
    ## The x-range is the retained series padded by 'shoulder'. The y-range spans
    ## everything actually drawn within that x-window, including discarded points
    ## and the fitted curves, so that no plotted element is silently clipped.
    xmax <- max(na.omit(df_good$Etime)) + shoulder
    xmin <- -shoulder

    y_vals <- df_all[[gastype]][df_all$Etime >= xmin & df_all$Etime <= xmax]
    if (plot_diffusion && nrow(df_diff) > 0) {
      in_window <- df_all$Etime >= 0 & df_all$Etime <= max(df_diff$Etime, na.rm = TRUE)
      if (draw_hm) y_vals <- c(y_vals, df_all$HM_mod[in_window])
      if (draw_lm) y_vals <- c(y_vals, LM.C0 + LM.slope * range(df_diff$Etime, na.rm = TRUE))
    }
    ## The bubble fits are included, so that an extrapolated settled level or
    ## an overshoot above the observations is not clipped.
    for (d in list(bfit_df, bset_df)) {
      if (!is.null(d)) y_vals <- c(y_vals, d$y[d$x >= xmin & d$x <= xmax])
    }
    y_vals <- na.omit(y_vals)
    ymax <- max(y_vals)
    ymin <- min(y_vals)
    # Guard against a degenerate range (a perfectly flat series).
    ydiff <- if (ymax > ymin) ymax - ymin else max(abs(ymax), 1) * 0.1

    df_all$UniqueID <- incubation_id

    # A labelled factor rather than the raw 0/1 flag, so that the legend is
    # readable without reference to the documentation. Unused levels are dropped
    # so that incubations with no discarded records do not advertise an empty
    # category.
    df_all$flag_lab <- ifelse(df_all$flag == 1, "retained", "discarded")

    ## For a gas other than the bubble gas, no ebullitive flux is estimated: when
    ## ebullition cut the diffusive window short, the retained points after it
    ## enter no flux estimate and are shown in light grey.
    if (!fits_same_gas && nrow(df_diff) > 0 && nrow(df_good) > 0) {
      t_diff_end <- max(df_diff$Etime, na.rm = TRUE)
      if (t_diff_end < max(df_good$Etime, na.rm = TRUE)) {
        df_all$flag_lab[df_all$flag == 1 & df_all$Etime > t_diff_end] <-
          "outside diffusive window"
      }
    }
    df_all$flag_lab <- droplevels(
      factor(df_all$flag_lab,
             levels = c("retained", "outside diffusive window", "discarded")))

    ## Point colour per category. The two colours are drawn as separate layers,
    ## since the colour scale is used by the model fits; the legend keys are
    ## coloured to match.
    key_colours <- c("retained" = col_points,
                     "outside diffusive window" = col_outside,
                     "discarded" = col_points)[levels(df_all$flag_lab)]

    # ---- Layers -------------------------------------------------------------

    # Layers are added back to front: time windows first, then observations,
    # then fits, so that the semi-transparent bands never wash out the data.
    plot <- ggplot(df_all, aes(x = Etime))

    ## Diffusive window, as a full-height band.
    if (!is.null(plot.display) && "diffusive.window" %in% plot.display &&
        nrow(df_diff) > 0) {
      rect_df <- data.frame(xmin = 0,
                            xmax = max(df_diff$Etime, na.rm = TRUE),
                            ymin = -Inf, ymax = Inf)
      plot <- plot +
        geom_rect(data = rect_df,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                      fill = "diffusive window"),
                  alpha = 0.16, inherit.aes = FALSE)
    }

    ## Ebullition events, drawn in the same visual idiom as the diffusive window
    ## so that both read as intervals of time rather than as regions of the
    ## concentration space.
    if (!is.null(plot.display) && "ebullition.events" %in% plot.display &&
        can.plot.bubbles) {
      plot <- plot +
        geom_rect(data = bubbles_f,
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf,
                      fill = "ebullition event"),
                  alpha = 0.16, inherit.aes = FALSE)
    }

    ## Observations. The quality flag is carried by shape and opacity, which
    ## leaves the colour scale free for the model fits.
    is_outside <- df_all$flag_lab == "outside diffusive window"
    plot <- plot +
      geom_point(data = df_all[!is_outside, ],
                 aes(y = .data[[gastype]], shape = flag_lab, alpha = flag_lab),
                 colour = col_points, size = 0.5)
    if (any(is_outside)) {
      plot <- plot +
        geom_point(data = df_all[is_outside, ],
                   aes(y = .data[[gastype]], shape = flag_lab, alpha = flag_lab),
                   colour = col_outside, size = 0.5)
    }

    ## Bubble fits, drawn below the diffusive fit so that it stays visible
    ## where they meet. geom_path (not geom_line) keeps the row order, so the
    ## two points sharing t_s draw the jump as a vertical segment. The settled
    ## level is dashed and kept out of the legend: it belongs to the fit.
    if (!is.null(bset_df)) {
      plot <- plot +
        geom_path(data = bset_df, aes(x = x, y = y, group = event),
                  colour = col_ebullitive, linewidth = 0.5, linetype = "22",
                  inherit.aes = FALSE)
    }
    if (!is.null(bfit_df)) {
      plot <- plot +
        geom_path(data = bfit_df,
                  aes(x = x, y = y, group = event, colour = "bubble fit"),
                  linewidth = 0.8, inherit.aes = FALSE)
    }

    ## Diffusive fit, restricted to the interval over which it was estimated.
    ## The legend names the model selected by best.flux.
    if (plot_diffusion && nrow(df_diff) > 0) {
      x_start <- min(df_diff$Etime, na.rm = TRUE)
      x_stop  <- max(df_diff$Etime, na.rm = TRUE)

      lab_lm <- if (is.na(best_model)) "LM fit" else "diffusive fit (LM)"
      lab_hm <- if (is.na(best_model)) "HM fit" else "diffusive fit (HM)"

      if (draw_lm) {
        lm_df <- data.frame(x    = x_start,
                            xend = x_stop,
                            y    = LM.C0 + LM.slope * x_start,
                            yend = LM.C0 + LM.slope * x_stop)
        plot <- plot +
          geom_segment(data = lm_df,
                       aes(x = x, xend = xend, y = y, yend = yend, colour = lab_lm),
                       linewidth = 0.9, inherit.aes = FALSE)
      }
      if (draw_hm) {
        hm_df <- df_all[df_all$Etime >= 0 & df_all$Etime <= x_stop, ]
        plot <- plot +
          geom_line(data = hm_df, aes(y = HM_mod, colour = lab_hm),
                    linewidth = 0.9)
      }
    }

    # ---- Header, scales and theme -------------------------------------------

    ## Flux estimates. Where ggtext is available, the diffusive and ebullitive
    ## values are coloured to match their bands and fits; the content is
    ## otherwise identical.
    if (use_markdown) {
      subtitle_lab <- paste0(
        "total ", .fmt_flux(flux_total, SE_total),
        "   \u2502   <span style='color:", col_diffusive, "'>diffusive ",
        .fmt_flux(flux_diff, SE_diff), "</span>",
        "   \u2502   <span style='color:", col_ebullitive, "'>ebullitive ",
        .fmt_flux(flux_ebull, SE_ebull), "</span>")
      subtitle_element <- ggtext::element_markdown(size = 8.5, colour = "grey25",
                                                   margin = margin(b = 6))
    } else {
      subtitle_lab <- paste0(
        "total ",                  .fmt_flux(flux_total, SE_total),
        "   \u2502   diffusive ",  .fmt_flux(flux_diff,  SE_diff),
        "   \u2502   ebullitive ", .fmt_flux(flux_ebull, SE_ebull))
      subtitle_element <- element_text(size = 8.5, colour = "grey25",
                                       margin = margin(b = 6))
    }

    ## Quality checks, reported in the caption only; the subtitle carries the
    ## flux estimates alone.
    qc_model <- best_model
    qc_text  <- NA_character_
    if (isTRUE(quality.check) && plot_diffusion) {
      if ("quality.check" %in% names(data_diffusion)) {
        qc_text <- as.character(data_diffusion$quality.check[ind_diff][1])
      }
    }
    q_lines <- if (isTRUE(quality.check)) {
      .quality_lines(qc_model, qc_text,
                     d = if (fits_same_gas) diag_f else NULL,
                     unit = flux.unit.plain, conv = conversion.factor)
    } else character(0)
    caption_lab <- paste(c(q_lines, paste0("flux units: ", flux.unit.plain)),
                         collapse = "\n")

    plot +
      scale_shape_manual(NULL, values = c("retained" = 16,
                                          "outside diffusive window" = 16,
                                          "discarded" = 1)) +
      scale_alpha_manual(NULL, values = c("retained" = 0.9,
                                          "outside diffusive window" = 0.9,
                                          "discarded" = 0.45)) +
      scale_colour_manual(NULL, values = c("diffusive fit (LM)" = col_diffusive,
                                           "diffusive fit (HM)" = col_diffusive,
                                           "LM fit"             = col_diffusive,
                                           "HM fit"             = col_hm,
                                           "bubble fit"         = col_ebullitive)) +
      # The fill labels are set by function so that the ebullition key can name
      # the gas the events were detected on, without changing the fill values.
      scale_fill_manual(NULL, values = c("diffusive window"  = col_diffusive,
                                         "ebullition event"  = col_ebullitive),
                        labels = function(b) ifelse(b == "ebullition event",
                                                    ebull_label, b)) +
      # The band keys are drawn at a higher opacity than the bands themselves,
      # which would otherwise be barely visible at legend-key size.
      guides(fill  = guide_legend(override.aes = list(alpha = 0.35)),
             shape = guide_legend(override.aes = list(colour = unname(key_colours))),
             alpha = guide_legend(override.aes = list(size = 2,
                                                      colour = unname(key_colours)))) +
      xlab("Time (s)") + ylab_plot +
      # Breaks are derived from the data range rather than set at a fixed
      # interval, which keeps the axis legible for incubations of any duration.
      scale_x_continuous(breaks = function(lim) pretty(lim, n = 6),
                         minor_breaks = NULL) +
      coord_cartesian(xlim = c(xmin, xmax),
                      ylim = c(ymin - ydiff * 0.05, ymax + ydiff * 0.05)) +
      labs(title    = incubation_id,
           subtitle = subtitle_lab,
           caption  = caption_lab) +
      theme_bw(base_size = 11) +
      theme(plot.title       = element_text(size = 11, face = "bold"),
            plot.subtitle    = subtitle_element,
            plot.caption     = element_text(size = 7.5, colour = "grey45"),
            axis.title.x     = element_text(size = 10, face = "bold"),
            axis.title.y     = element_text(size = 10, face = "bold"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
            legend.position  = "bottom",
            legend.box       = "horizontal",
            legend.key.size  = unit(0.8, "lines"),
            legend.margin    = margin(t = -4))
  })

  # Naming the list lets callers retrieve a single diagnostic by incubation
  # rather than by its position in the batch.
  names(plot_list) <- vapply(data_split,
                             function(d) as.character(unique(d$UniqueID)[1]),
                             character(1))

  return(plot_list)
}
