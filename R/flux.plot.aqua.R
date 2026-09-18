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
##   * colour encodes which model fit (LM vs. HM);
##   * shape and opacity encode whether an observation was retained or
##     discarded by the quality flag;
##   * the numeric flux estimates live in the plot header, outside the panel.
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


#' Plot aquatic chamber incubations with diffusive and ebullitive components
#'
#' Produces one diagnostic figure per incubation from the output of
#' \code{\link{goAquaFlux}}, showing the measured concentration series, the
#' window over which the diffusive flux was fitted, the detected ebullition
#' events, the linear (LM) and Hutchinson-Mosier (HM) fits, and the resulting
#' total, diffusive and ebullitive flux estimates.
#'
#' @details
#' Each figure is composed of the following elements.
#'
#' \describe{
#'   \item{Observations}{All records for the incubation are drawn. Points
#'     retained by the quality flag (\code{flag == 1}) are filled and opaque;
#'     discarded points are hollow and faded, so that they remain visible for
#'     diagnosis without competing with the retained series.}
#'   \item{Diffusive window}{A full-height band from \eqn{t = 0} to the end of
#'     the window used to fit the diffusive flux. The extent of the window is
#'     taken from \code{n_obs.diffusion} in \code{flux_summary} and applied to
#'     the time-ordered retained series.}
#'   \item{Ebullition events}{Full-height bands spanning the start and end times
#'     of each detected bubbling event.}
#'   \item{Model fits}{The LM and HM fits are drawn only across the diffusive
#'     window, since neither is fitted to the ebullitive part of the series. A
#'     fit that stops short of, or runs past, the bubble-free portion of the
#'     record is then a direct indication that the window was misidentified.}
#'   \item{Flux estimates}{Reported in the plot subtitle, with their unit in the
#'     caption. They are placed outside the panel so that they cannot overlap
#'     the data for any incubation.}
#' }
#'
#' If the \pkg{ggtext} package is available, the subtitle is rendered with the
#' diffusive and ebullitive estimates coloured to match their respective bands
#' and fits; otherwise a plain-text subtitle is used. The two variants are
#' identical in content.
#'
#' Colours follow the Okabe-Ito palette, which remains distinguishable under the
#' common forms of colour vision deficiency and in greyscale print.
#'
#' @param flux.results.ls The list returned by \code{\link{goAquaFlux}} (with
#'   \code{return_df = TRUE}), containing \code{flux_summary}, \code{bubbles}
#'   and \code{diffusive}. For backwards compatibility a plain
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
#' @param plot.display Character vector of overlays to draw. Supported values
#'   are \code{"diffusive.window"} and \code{"ebullition.events"}. Pass
#'   \code{NULL} to draw the observations and fits only. Both are shown by
#'   default.
#' @param flux.unit Character string or \code{NULL}; the flux unit shown in the
#'   caption. Plotmath syntax (for example \code{"nmol~m^-2*s^-1"}) is accepted
#'   and converted to plain text. When \code{NULL}, a unit consistent with
#'   \code{gastype} is chosen.
#' @param quality.check Logical; reserved for forthcoming quality-check
#'   annotations. Currently ignored, with a message. Default \code{FALSE}.
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
#' # Write all diagnostics to a multi-page PDF
#' pdf("ch4_diagnostics.pdf", width = 8, height = 5)
#' invisible(lapply(plots, print))
#' dev.off()
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_rect geom_segment geom_line
#' @importFrom ggplot2 scale_colour_manual scale_fill_manual scale_shape_manual
#' @importFrom ggplot2 scale_alpha_manual scale_x_continuous xlab ylab labs
#' @importFrom ggplot2 coord_cartesian theme_bw theme element_text element_blank
#' @importFrom ggplot2 element_line guides guide_legend margin unit
#' @importFrom dplyr %>% right_join group_by group_split filter
#' @importFrom pbapply pblapply pboptions
#' @importFrom stats na.omit
#' @importFrom rlang .data
#'
#' @export
#'
flux.plot.aqua <- function(flux.results.ls, dataframe, gastype, shoulder = 30,
                           plot.display = c("diffusive.window", "ebullition.events"),
                           flux.unit = NULL,
                           quality.check = FALSE,
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
  if (isTRUE(quality.check)) {
    message("'quality.check = TRUE' is reserved for future use and is ignored.")
  }

  # ---- Appearance constants -------------------------------------------------

  # Okabe-Ito blue and vermillion. Blue is used throughout for the diffusive
  # component and vermillion for the ebullitive one, in both the bands and the
  # model fits, so that the two components stay identifiable without the legend.
  col_diffusive  <- "#0072B2"
  col_ebullitive <- "#D55E00"
  col_points     <- "grey15"

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

    ## Diffusion model coefficients, when this incubation was fitted.
    plot_diffusion <- FALSE
    if (!is.null(data_diffusion)) {
      ind_diff <- which(data_diffusion$UniqueID == incubation_id)
      if (length(ind_diff) >= 1) {
        plot_diffusion <- TRUE
        LM.slope <- unique(data_diffusion$LM.slope[ind_diff])
        LM.C0    <- unique(data_diffusion$LM.C0[ind_diff])
        HM.Ci    <- unique(data_diffusion$HM.Ci[ind_diff])
        HM.C0    <- unique(data_diffusion$HM.C0[ind_diff])
        HM.k     <- unique(data_diffusion$HM.k[ind_diff])
        df_all$HM_mod <- .hm_model(HM.Ci, HM.C0, HM.k, df_all$Etime)
      }
    }

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

    ## Axis ranges.
    ## The x-range is the retained series padded by 'shoulder'. The y-range spans
    ## everything actually drawn within that x-window, including discarded points
    ## and the fitted curves, so that no plotted element is silently clipped.
    xmax <- max(na.omit(df_good$Etime)) + shoulder
    xmin <- -shoulder

    y_vals <- df_all[[gastype]][df_all$Etime >= xmin & df_all$Etime <= xmax]
    if (plot_diffusion && nrow(df_diff) > 0) {
      in_window <- df_all$Etime >= 0 & df_all$Etime <= max(df_diff$Etime, na.rm = TRUE)
      y_vals <- c(y_vals,
                  df_all$HM_mod[in_window],
                  LM.C0 + LM.slope * range(df_diff$Etime, na.rm = TRUE))
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
    df_all$flag_lab <- droplevels(
      factor(ifelse(df_all$flag == 1, "retained", "discarded"),
             levels = c("retained", "discarded")))

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
    plot <- plot +
      geom_point(aes(y = .data[[gastype]], shape = flag_lab, alpha = flag_lab),
                 colour = col_points, size = 1.5)

    ## Model fits, restricted to the interval over which they were estimated.
    if (plot_diffusion && nrow(df_diff) > 0) {
      x_start <- min(df_diff$Etime, na.rm = TRUE)
      x_stop  <- max(df_diff$Etime, na.rm = TRUE)

      lm_df <- data.frame(x    = x_start,
                          xend = x_stop,
                          y    = LM.C0 + LM.slope * x_start,
                          yend = LM.C0 + LM.slope * x_stop)
      hm_df <- df_all[df_all$Etime >= 0 & df_all$Etime <= x_stop, ]

      plot <- plot +
        geom_segment(data = lm_df,
                     aes(x = x, xend = xend, y = y, yend = yend, colour = "LM fit"),
                     linewidth = 0.9, inherit.aes = FALSE) +
        geom_line(data = hm_df, aes(y = HM_mod, colour = "HM fit"),
                  linewidth = 0.9)
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

    plot +
      scale_shape_manual(NULL, values = c("retained" = 16, "discarded" = 1)) +
      scale_alpha_manual(NULL, values = c("retained" = 0.9, "discarded" = 0.45)) +
      scale_colour_manual(NULL, values = c("LM fit" = col_diffusive,
                                           "HM fit" = col_ebullitive)) +
      scale_fill_manual(NULL, values = c("diffusive window"  = col_diffusive,
                                         "ebullition event"  = col_ebullitive)) +
      # The band keys are drawn at a higher opacity than the bands themselves,
      # which would otherwise be barely visible at legend-key size.
      guides(fill  = guide_legend(override.aes = list(alpha = 0.35)),
             alpha = guide_legend(override.aes = list(size = 2))) +
      xlab("Time (s)") + ylab_plot +
      # Breaks are derived from the data range rather than set at a fixed
      # interval, which keeps the axis legible for incubations of any duration.
      scale_x_continuous(breaks = function(lim) pretty(lim, n = 6),
                         minor_breaks = NULL) +
      coord_cartesian(xlim = c(xmin, xmax),
                      ylim = c(ymin - ydiff * 0.05, ymax + ydiff * 0.05)) +
      labs(title    = incubation_id,
           subtitle = subtitle_lab,
           caption  = paste0("flux units: ", flux.unit.plain)) +
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
