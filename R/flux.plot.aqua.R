#' Plot aquatic chamber incubation time series with detected flux components
#'
#' @param flux.results Data frame output from \code{\link[goFlux]{goAquaFlux}}
#' @param dataframe Data frame with gas measurements, Etime, and flag columns
#' @param gastype Character; gas type (e.g., "CH4dry_ppb", "CO2dry_ppm")
#' @param shoulder Numeric; time before/after measurement in window (seconds). Default 30
#' @param plot.display Character vector; parameters to display on plot.
#'                     Choose from: "C0", "Cf", "diffusive.window", "ebullition.events",
#'                     "prec", "flux.term", "MDF", "n.obs". Default: c("diffusive.window", "ebullition.events")
#' @param flux.unit Character; flux units for display
#' @param quality.check Logical; display quality check notes
#' @param conversion.factor Numeric; factor to multiply flux values by. Default 1
#'
#' @return A list of plots, one per UniqueID
#'
#' @export
#'
flux.plot.aqua <- function(flux.results, dataframe, gastype, shoulder = 30,
                           plot.display = c("diffusive.window", "ebullition.events"),
                           flux.unit = NULL,
                           quality.check = FALSE,
                           conversion.factor = 1) {

  # --------- Check arguments ---------
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

  if (missing(flux.results)) stop("'flux.results' is required")
  if (!is.data.frame(flux.results)) stop("'flux.results' must be a data.frame")

  # Check required columns in flux.results
  required_cols <- c("UniqueID", "flux_total", "flux_diffusive", "flux_ebullition",
                     "SE_total", "SE_diffusive", "SE_ebullition", "first_bubble_time")
  missing_cols <- setdiff(required_cols, names(flux.results))
  if (length(missing_cols) > 0) {
    stop("'flux.results' missing columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.null(flux.unit) && !is.character(flux.unit)) {
    stop("'flux.unit' must be a character string or NULL")
  }

  if (!is.logical(quality.check)) stop("'quality.check' must be TRUE or FALSE")
  if (!is.numeric(conversion.factor) || conversion.factor <= 0) {
    stop("'conversion.factor' must be positive")
  }

  # Assign NULL to variables without binding
  UniqueID <- Etime <- flag <- NULL

  # Define gas units
  gas.unit <- switch(gastype,
                     "CO2dry_ppm" = "ppm",
                     "CH4dry_ppb" = "ppb",
                     "N2Odry_ppb" = "ppb",
                     "NO2dry_ppb" = "ppb",
                     "NOdry_ppb" = "ppb",
                     "COdry_ppb" = "ppb",
                     "NH3dry_ppb" = "ppb",
                     "H2O_ppm" = "ppm")

  # Define y-axis label
  ylab_plot <- switch(gastype,
                      "CO2dry_ppm" = ylab(expression(CO["2"] * " dry (ppm)")),
                      "CH4dry_ppb" = ylab(expression(CH["4"] * " dry (ppb)")),
                      "N2Odry_ppb" = ylab(expression(N["2"] * "O dry (ppb)")),
                      "NO2dry_ppb" = ylab(expression(NO["2"] * " dry (ppb)")),
                      "NOdry_ppb" = ylab("NO dry (ppb)"),
                      "COdry_ppb" = ylab(expression(CO * " dry (ppb)")),
                      "NH3dry_ppb" = ylab(expression(NH["3"] * " dry (ppb)")),
                      "H2O_ppm" = ylab(expression(H["2"] * "O (ppm)")))

  # Define flux units
  if (is.null(flux.unit)) {
    flux.unit <- switch(gastype,
                        "CO2dry_ppm" = "\u00B5mol~m^-2*s^-1",
                        "CH4dry_ppb" = "nmol~m^-2*s^-1",
                        "N2Odry_ppb" = "nmol~m^-2*s^-1",
                        "NO2dry_ppb" = "nmol~m^-2*s^-1",
                        "NOdry_ppb" = "nmol~m^-2*s^-1",
                        "COdry_ppb" = "nmol~m^-2*s^-1",
                        "NH3dry_ppb" = "nmol~m^-2*s^-1",
                        "H2O_ppm" = "\u00B5mol~m^-2*s^-1")
  }

  # Split data by UniqueID and merge with flux results
  data_split <- dataframe %>%
    right_join(flux.results, by = "UniqueID") %>%
    group_by(UniqueID) %>%
    group_split()

  # Filter to flag == 1 (good measurements)
  data_corr <- lapply(seq_along(data_split), function(f) {
    data_split[[f]] %>% filter(flag == 1)
  })

  # Create plots
  pboptions(char = "=")
  plot_list <- pblapply(seq_along(data_split), function(f) {

    df_all <- data_split[[f]]
    df_good <- data_corr[[f]]

    # Extract flux results for this UniqueID
    flux_total <- unique(df_all$flux_total) * conversion.factor
    SE_total <- unique(df_all$SE_total) * conversion.factor
    flux_diff <- unique(df_all$flux_diffusive) * conversion.factor
    SE_diff <- unique(df_all$SE_diffusive) * conversion.factor
    flux_ebull <- unique(df_all$flux_ebullition) * conversion.factor
    SE_ebull <- unique(df_all$SE_ebullition) * conversion.factor
    first_bubble <- unique(df_all$first_bubble_time)

    # Plot limits
    xmax <- max(na.omit(df_good$Etime)) + shoulder
    xmin <- -shoulder
    xdiff <- xmax - xmin

    y_noNAs <- na.omit(df_good[[gastype]])

    ymax <- max(y_noNAs)
    ymin <- min(y_noNAs)
    yend <- mean(y_noNAs[seq(length(y_noNAs)-30, length(y_noNAs))])

    ydiff <- ymax - ymin

    # Prepare legend data
    flux.dec <- 2  # decimal places for flux display

    legend_data <- data.frame(
      content = c(
        "Component",
        "Total",
        "Diffusive",
        "Ebullitive",
        paste0("'Flux units: '", "~", flux.unit)
      ),
      color = c("black", "black", "blue", "red", "black"),
      x = xmax - xdiff * 0.15,
      y = seq(ymax + ydiff * 0.25, ymax + ydiff * 0.05, length.out = 5)
    )

    legend_data$label <- c(
      "Component",
      paste0(round(flux_total, flux.dec), " (±", round(SE_total, flux.dec), ")"),
      paste0(round(flux_diff, flux.dec), " (±", round(SE_diff, flux.dec), ")"),
      paste0(round(flux_ebull, flux.dec), " (±", round(SE_ebull, flux.dec), ")"),
      ""
    )

    # Initialize display elements
    display_elements <- list()

    # Plot diffusive window
    if (!is.null(plot.display) && any(grepl("\\<diffusive.window\\>", plot.display))) {
      if (!is.na(first_bubble)) {
        display_elements$diffusive_window <- annotate(
          "rect",
          xmin = 0, xmax = first_bubble, ymin = -Inf, ymax = Inf,
          alpha = 0.1, fill = "blue"
        )
      }
    }

    # Plot ebullition events
    if (!is.null(plot.display) && any(grepl("\\<ebullition.events\\>", plot.display))) {
      if (!is.na(first_bubble)) {
        display_elements$ebullition_marker <- annotate(
          "vline", xintercept = first_bubble,
          linetype = "dashed", color = "red", size = 0.8
        )
        display_elements$ebullition_label <- annotate(
          "text", x = first_bubble, y = ymax+0.1*ydiff, label = "Diffusive window",
          vjust = -0.5, color = "blue", size = 3
        )
      }
    }


    # Prepare component labels and flux values separately
    delta <- 0.03
    component_labels <- data.frame(
      label = c("Total", "Diffusive", "Ebullitive","Component"),
      color = c("black", "blue", "red","black"),
      x = xmin + xdiff * 0.7,
      y = seq(from=ymin + ydiff * delta, by = ydiff * delta, length.out = 4)
    )

    flux_values <- data.frame(
      label = c(
        paste0(round(flux_total, flux.dec), " ± ", round(SE_total, flux.dec)),
        paste0(round(flux_diff, flux.dec), " ± ", round(SE_diff, flux.dec)),
        paste0(round(flux_ebull, flux.dec), " ± ", round(SE_ebull, flux.dec))
      ),
      color = c("black", "blue", "red"),
      x = xmin + xdiff * 0.72,
      y = seq(from=ymin + ydiff * delta, by = ydiff * delta, length.out = 3)
    )

    flux_unit_label <- data.frame(
      label = paste0("Flux units: ", flux.unit),
      color = "black",
      x = xmin + xdiff * 0.8,
      y = ymin-ydiff*0.1
    )

    # Build plot
    plot <- ggplot(df_all, aes(x = Etime)) +
      geom_point(aes(y = .data[[gastype]], color = as.factor(flag))) +

      # Display elements
      display_elements +

      # Component labels (left column)
      geom_text(
        data = component_labels, parse = FALSE, size = 3.5,
        aes(x = x, y = y, label = label, color = color),
        hjust = 1
      ) +

      # Flux values (right column)
      geom_text(
        data = flux_values, parse = FALSE, size = 3.5,
        aes(x = x, y = y, label = label, color = color),
        hjust = 0
      ) +

      # Flux unit label
      geom_text(
        data = flux_unit_label, parse = FALSE, size = 2.8,
        aes(x = x, y = y, label = label),
        color = "black", hjust = 1, style = "italic"
      ) +

      # Single color scale that covers all elements
      scale_color_manual(
        values = c(
          "darkgrey" = "darkgrey",
          "black" = "black",
          "pink" = "pink",
          "blue" = "blue",
          "red" = "red"
        ),
        guide = "none"
      ) +

      # Styling
      xlab("Time (sec)") +
      ylab_plot +
      scale_x_continuous(
        breaks = seq(-60, max(df_good$Etime), 30),
        minor_breaks = seq(-60, max(df_good$Etime) + 60, 10)
      ) +
      coord_cartesian(
        xlim = c(xmin + xdiff * 0.05, xmax - xdiff * 0.05),
        ylim = c(ymin - ydiff * 0.15, yend + ydiff * 0.15)
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold")
      )



    return(plot)
  })

  return(plot_list)
}
