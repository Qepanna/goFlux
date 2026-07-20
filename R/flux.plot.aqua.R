#' Plot aquatic chamber incubations with diffusive and ebullitive components
#'
#' Produces one diagnostic plot per incubation from the output of
#' \code{\link{goAquaFlux}}: the measured concentration time series, the
#' diffusive window (shaded), the detected bubbling events, and a compact legend
#' of the total / diffusive / ebullitive flux estimates.
#'
#' @param flux.results.ls The list returned by \code{\link{goAquaFlux}} (with
#'   \code{return_df = TRUE}), i.e. a list with \code{flux_summary},
#'   \code{bubbles} and \code{diffusive}. For backwards compatibility, a plain
#'   \code{best.flux}-style data.frame may also be supplied, in which case the
#'   function delegates to \code{\link[goFlux]{flux.plot}} (diffusion only).
#'
#' @param dataframe The data.frame of measurements used by
#'   \code{\link{goAquaFlux}} (with \code{Etime}, \code{flag} and the gas column).
#'
#' @param gastype Character; gas column to plot (e.g. \code{"CH4dry_ppb"}).
#'
#' @param shoulder Numeric; padding (seconds) added before/after the measurement
#'   on the x-axis. Default \code{30}.
#'
#' @param plot.display Character vector of elements to overlay. Currently
#'   supported: \code{"diffusive.window"} and \code{"ebullition.events"}.
#'   Default \code{c("diffusive.window", "ebullition.events")}.
#'
#' @param flux.unit Character or \code{NULL}; flux-unit label. If \code{NULL}, a
#'   sensible default is chosen from \code{gastype}.
#'
#' @param quality.check Logical; reserved for quality-check annotations.
#'   Default \code{FALSE}.
#'
#' @param conversion.factor Numeric > 0; multiplies displayed flux values.
#'   Default \code{1}.
#'
#' @return A list of \code{ggplot} objects, one per \code{UniqueID}.
#'
#' @seealso \code{\link{goAquaFlux}}, \code{\link[goFlux]{flux.plot}}
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_rect geom_abline geom_line
#' @importFrom ggplot2 annotate scale_color_manual scale_x_continuous xlab ylab
#' @importFrom ggplot2 coord_cartesian theme_bw theme element_text ggtitle
#' @importFrom dplyr right_join group_by group_split filter
#' @importFrom pbapply pblapply pboptions
#' @importFrom rlang .data
#'
#' @export
#'
flux.plot.aqua <- function(flux.results.ls, dataframe, gastype, shoulder = 30,
                           plot.display = c("diffusive.window", "ebullition.events"),
                           flux.unit = NULL,
                           quality.check = FALSE,
                           conversion.factor = 1) {

  # ------------------------- Argument checks -------------------------
  if (is.null(shoulder)) stop("'shoulder' is required")
  if (!is.numeric(shoulder) || shoulder < 0) stop("'shoulder' must be numeric and non-negative")

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


  if (is.data.frame(flux.results.ls)) {
    message("flux.results.ls is a data.frame; delegating to flux.plot() (diffusion only).")
    return(flux.plot(flux.results = flux.results.ls, dataframe = dataframe,
                     gastype = gastype, quality.check = TRUE,
                     plot.legend = c("MAE", "AICc", "k.ratio", "g.factor"),
                     plot.display = c("Ci", "C0", "MDF", "prec", "nb.obs", "flux.term")))
  }
  if (!is.list(flux.results.ls)) stop("'flux.results.ls' must be a list or a data.frame")

  flux.results <- flux.results.ls$flux_summary
  if (!is.data.frame(flux.results)) stop("'flux.results.ls$flux_summary' must be a data.frame")

  bubbles <- flux.results.ls$bubbles  # NULL -> ebullition events not drawn

  required_cols <- c("UniqueID", "flux_total", "flux_diffusive", "flux_ebullition",
                     "SE_total", "SE_diffusive", "SE_ebullition", "first_bubble_time")
  missing_cols <- setdiff(required_cols, names(flux.results))
  if (length(missing_cols) > 0) {
    stop("'flux_summary' missing columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.null(flux.unit) && !is.character(flux.unit)) stop("'flux.unit' must be a character string or NULL")
  if (!is.logical(quality.check)) stop("'quality.check' must be TRUE or FALSE")
  if (!is.numeric(conversion.factor) || conversion.factor <= 0) stop("'conversion.factor' must be positive")

  # Hutchinson and Mosier model (for overlaying the HM fit)
  HMmod <- function(Ci, C0, k, x) Ci + (C0 - Ci) * exp(-k * x)

  # Bind NULLs to silence R CMD check notes on NSE variables
  UniqueID <- Etime <- flag <- NULL

  # y-axis label per gas
  ylab_plot <- switch(gastype,
                      "CO2dry_ppm" = ylab(expression(CO["2"] * " dry (ppm)")),
                      "CH4dry_ppb" = ylab(expression(CH["4"] * " dry (ppb)")),
                      "N2Odry_ppb" = ylab(expression(N["2"] * "O dry (ppb)")),
                      "NO2dry_ppb" = ylab(expression(NO["2"] * " dry (ppb)")),
                      "NOdry_ppb"  = ylab("NO dry (ppb)"),
                      "COdry_ppb"  = ylab(expression(CO * " dry (ppb)")),
                      "NH3dry_ppb" = ylab(expression(NH["3"] * " dry (ppb)")),
                      "H2O_ppm"    = ylab(expression(H["2"] * "O (ppm)")))

  # Default flux units
  if (is.null(flux.unit)) {
    flux.unit <- switch(gastype,
                        "CO2dry_ppm" = "\u00B5mol~m^-2*s^-1",
                        "H2O_ppm"    = "\u00B5mol~m^-2*s^-1",
                        "nmol~m^-2*s^-1")  # all ppb gases
  }

  # Join measurements with per-incubation flux results, then split by UniqueID
  data_split <- dataframe %>%
    right_join(flux.results, by = "UniqueID") %>%
    group_by(UniqueID) %>%
    group_split()

  data_corr <- lapply(data_split, function(d) d %>% filter(flag == 1))
  data_diffusion <- flux.results.ls$diffusive

  # ------------------------- Build one plot per incubation -------------------------
  pboptions(char = "=")
  plot_list <- pblapply(seq_along(data_split), function(f) {

    df_all  <- data_split[[f]]
    df_good <- data_corr[[f]]

    UniqueID   <- unique(df_all$UniqueID)
    flux_total <- unique(df_all$flux_total)      * conversion.factor
    SE_total   <- unique(df_all$SE_total)        * conversion.factor
    flux_diff  <- unique(df_all$flux_diffusive)  * conversion.factor
    SE_diff    <- unique(df_all$SE_diffusive)    * conversion.factor
    flux_ebull <- unique(df_all$flux_ebullition) * conversion.factor
    SE_ebull   <- unique(df_all$SE_ebullition)   * conversion.factor

    # Diffusion model fit for this incubation (if available)
    plot_diffusion <- FALSE
    if (!is.null(data_diffusion)) {
      ind_diff <- which(data_diffusion$UniqueID == UniqueID)
      if (length(ind_diff) >= 1) {
        plot_diffusion <- TRUE
        LM.slope <- unique(data_diffusion$LM.slope[ind_diff])
        LM.C0    <- unique(data_diffusion$LM.C0[ind_diff])
        HM.Ci    <- unique(data_diffusion$HM.Ci[ind_diff])
        HM.C0    <- unique(data_diffusion$HM.C0[ind_diff])
        HM.k     <- unique(data_diffusion$HM.k[ind_diff])
        df_all$HM_mod <- HMmod(HM.Ci, HM.C0, HM.k, df_all$Etime)
      }
    }

    ## df_diff (the diffusive window) is used below by geom_rect. Define it
    ## robustly and fall back to the full good series if n_obs.diffusion is
    ## missing, so the plot never errors on an undefined object.
    n_obs_diff <- flux.results$n_obs.diffusion[flux.results$UniqueID == UniqueID]
    if (length(n_obs_diff) == 1 && !is.na(n_obs_diff) && n_obs_diff >= 1) {
      df_diff <- df_all[seq_len(min(n_obs_diff, nrow(df_all))), ]
    } else {
      df_diff <- df_good
    }

    ## Define first_bubble
    can.plot.bubbles <- FALSE
    first_bubble <- NA_real_
    bubbles_f <- NULL
    if (!is.null(bubbles)) {
      bubbles_f <- bubbles[bubbles$UniqueID == UniqueID, ]
      can.plot.bubbles <- nrow(bubbles_f) > 0
      if (can.plot.bubbles) first_bubble <- bubbles_f$start[1]
    }

    # Plot limits
    xmax <- max(na.omit(df_good$Etime)) + shoulder
    xmin <- -shoulder
    xdiff <- xmax - xmin

    y_noNAs <- na.omit(df_good[[gastype]])
    ymax <- max(y_noNAs); ymin <- min(y_noNAs)
    ydiff <- ymax - ymin

    flux.dec <- 2
    df_all$UniqueID <- UniqueID

    # ---- Base plot ----
    plot <- ggplot(df_all, aes(x = Etime)) +
      geom_point(aes(y = .data[[gastype]], color = as.factor(flag)))

    # ---- Diffusive window (shaded rectangle) ----
    if (!is.null(plot.display) && "diffusive.window" %in% plot.display &&
        nrow(df_diff) > 0) {
      rect_df <- data.frame(
        xmin = 0,
        xmax = max(df_diff$Etime, na.rm = TRUE),
        ymin = min(df_diff[[gastype]], na.rm = TRUE),
        ymax = max(df_diff[[gastype]], na.rm = TRUE))
      plot <- plot +
        geom_rect(data = rect_df,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                  fill = "blue", alpha = 0.2, inherit.aes = FALSE)
    }

    # ---- Ebullition events (shaded vertical bands) ----
    if (!is.null(plot.display) && "ebullition.events" %in% plot.display &&
        can.plot.bubbles) {
      plot <- plot +
        geom_rect(data = bubbles_f,
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                  fill = "red", alpha = 0.2, inherit.aes = FALSE)
    }

    # ---- Diffusion model overlays ----
    if (plot_diffusion) {
      plot <- plot +
        geom_abline(slope = LM.slope, intercept = LM.C0, linewidth = 1, col = "blue") +
        geom_line(aes(y = HM_mod), linewidth = 1, col = "red")
    }

    # ---- Compact flux legend (top-right) ----
    legend_df <- data.frame(
      x = xmax - xdiff * 0.02,
      y = seq(ymax + ydiff * 0.12, ymax - ydiff * 0.02, length.out = 4),
      lab = c(
        paste0("Total: ",     round(flux_total, flux.dec), " \u00B1 ", round(SE_total, flux.dec)),
        paste0("Diffusive: ", round(flux_diff,  flux.dec), " \u00B1 ", round(SE_diff,  flux.dec)),
        paste0("Ebullitive: ",round(flux_ebull, flux.dec), " \u00B1 ", round(SE_ebull, flux.dec)),
        paste0("units: ", flux.unit)),
      col = c("black", "blue", "red", "grey30"))
    plot <- plot +
      annotate("text", x = legend_df$x, y = legend_df$y, label = legend_df$lab,
               colour = legend_df$col, hjust = 1, size = 3)

    # ---- Scales & styling ----
    plot +
      scale_color_manual(
        values = c("0" = "darkgrey", "1" = "black",
                   "black" = "black", "blue" = "blue", "red" = "red"),
        guide = "none") +
      xlab("Time (sec)") + ylab_plot +
      scale_x_continuous(
        breaks = seq(-60, max(df_good$Etime, na.rm = TRUE), 30),
        minor_breaks = seq(-60, max(df_good$Etime, na.rm = TRUE) + 60, 10)) +
      coord_cartesian(
        xlim = c(xmin + xdiff * 0.05, xmax - xdiff * 0.05),
        ylim = c(ymin - ydiff * 0.15, ymax + ydiff * 0.20)) +
      theme_bw() +
      theme(axis.title.x = element_text(size = 10, face = "bold"),
            axis.title.y = element_text(size = 10, face = "bold")) +
      ggtitle(UniqueID)
  })

  ## Return the list explicitly
  return(plot_list)
}
