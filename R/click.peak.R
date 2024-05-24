#' Manual identification of start and end of gas measurements
#'
#' DEPRECATED: This function will be removed in a future version of the package.
#' Use the function \code{\link[goFlux]{click.peak2}} instead. \cr \cr
#' Identify the start and the end of a measurement by clicking on them in a
#' scatter plot. To use in a loop with multiple measurements, first apply the
#' function \code{\link[goFlux]{obs.win}} to identify the observation
#' window of each measurement and then use the wrapper function
#' \code{\link[goFlux]{click.peak.loop}} with
#' \code{\link[base]{lapply}} (see example below).
#'
#' @param flux.unique data.frame; output from the function
#'                    \code{\link[goFlux]{obs.win}}.
#'                    Must contain the columns \code{gastype} (see below),
#'                    \code{POSIX.time} and \code{UniqueID}.
#' @param gastype character string; specifies which gas should be displayed on
#'                the plot to manually select start time and end time of
#'                measurements. Must be one of the following: "CO2dry_ppm",
#'                "COdry_ppb", "CH4dry_ppb", "N2Odry_ppb", "NH3dry_ppb" or
#'                "H2O_ppm". Default is "CO2dry_ppm".
#' @param sleep numerical value; delay before closing the resulting plot. When
#'              used with the function \code{\link[goFlux]{click.peak.loop}},
#'              grants a delay between measurements to visually inspect the
#'              output before processing the next measurement. Sleep must be
#'              shorter than 10 seconds. If \code{sleep = NULL}, the  plots will
#'              not close.
#' @param plot.lim numerical vector of length 2; sets the Y axis limits in the
#'                 plots. Default values are set for a typical gas measurement
#'                 of "CO2dry_ppm" from soils: \code{plot.lim = c(380,1000)}.
#' @param warn.length numerical value; limit under which a measurement is flagged
#'                    for being too short (\code{nb.obs < warn.length}). Default
#'                    value is \code{warn.length = 60}.
#' @param save.plots character string; a file name without the extension .pdf to
#'                   save the plots produced with click.peak. By default,
#'                   \code{save.plot = NULL} and plots are not saved.
#'
#' @returns A data.frame, identical to the input \code{flux.unique}, with the
#'          additional columns \code{flag}, \code{Etime}, \code{start.time_corr},
#'          \code{end.time_corr} and \code{obs.length_corr}.
#'
#' @include goFlux-package.R
#'
#' @seealso To use the function \code{\link[goFlux]{click.peak}} in a
#'          loop with \code{\link[base]{lapply}}, use
#'          \code{\link[goFlux]{click.peak.loop}}. See also
#'          \code{\link[goFlux]{obs.win}} to prepare a list of data.frame.
#'
#' @details
#' The argument \code{plot.lim} is used to remove any data points below and
#' above the plot limits for a better view of the scatter plot. If the gas
#' measurements are larger than the minimum or smaller than the maximum plot
#' limit values, then the plot will automatically zoom in and adjust to those
#' values. The default plot limits are set for a typical gas measurement of
#' "CO2dry_ppm" from a soil respiration measurement: \code{plot.lim = c(380,1000)},
#' where 380 ppm is the minimum plotted concentration, which should be close to
#' atmospheric concentration, and 1000 ppm is the maximum plotted concentration,
#' which correspond to a maximal accumulated concentration in the chamber before
#' considering it an outlier (e.g. caused by breath or gas bubble). For other
#' gasses, the user must specify the plot limits themselves. Here are some
#' suggestions of plot limits for the other gases:
#' \itemize{
#'   \item "CH4dry_ppb": \code{plot.lim = c(2200, 1800)}
#'   \item "N2Odry_ppb": \code{plot.lim = c(250, 500)}
#'   \item "NH3dry_ppb": \code{plot.lim = c(0, 200)}
#'   \item "COdry_ppb": \code{plot.lim = c(0, 200)}
#'   \item "H2O_ppm": \code{plot.lim = c(10000, 20000)}
#' }
#' These values will vary depending on ecosystem type and chamber application scheme.
#'
#' \code{warn.length} is the limit below which the chamber closure time is
#' flagged for being too short (\code{nb.obs < warn.length}). Portable
#' greenhouse gas analyzers typically measure at a frequency of 1 Hz. Therefore,
#' for the default setting of \code{warn.length = 60}, the chamber closure time
#' should be approximately one minute (60 seconds). If the number of
#' observations is smaller than the threshold, a warning is printed: "Number of
#' observations for UniqueID: 'UniqueID' is X observations".
#'
#' In \code{gastype}, the gas species listed are the ones for which this package
#' has been adapted. Please write to the maintainer of this package for
#' adaptation of additional gases.
#'
#' @examples
#' \dontrun{
#' # How to use in multiple situations:
#' # Note that gastype = "CO2dry_ppm" is the default setting
#' library(dplyr)
#'
#' ## with a LGR instrument and an auxiliary file (.txt)
#' aux.path <- system.file("extdata", "aux_UGGA/aux_UGGA.txt", package = "goFlux")
#' auxfile <- read.delim(aux.path) %>%
#'   mutate(start.time = as.POSIXct(start.time, tz = "UTC"))
#' data(imp.UGGA)
#' ow.UGGA <- obs.win(inputfile = imp.UGGA, auxfile = auxfile,
#'                    obs.length = 180, shoulder = 60)
#' manID.UGGA <- click.peak(ow.UGGA[[1]], save.plots = "manID.UGGA")
#'
#' ## with a LI-COR instrument and the Smart Chamber as auxiliary file
#' data(imp.LI8200)
#' data(imp.LI7820)
#' ow.LI7820 <- obs.win(inputfile = imp.LI7820, auxfile = imp.LI8200, shoulder = 60)
#' manID.LI7820 <- click.peak(ow.LI7820[[1]], gastype = "N2Odry_ppb", plot.lim = c(250,500))
#'
#' ## with the LI-6400 and no auxiliary file
#' data(imp.LI6400)
#' ow.LI6400 <- obs.win(inputfile = imp.LI6400, shoulder = 0)
#' manID.LI6400 <- click.peak(ow.LI6400[[1]])
#' }

click.peak <- function(flux.unique, gastype = "CO2dry_ppm", sleep = 3,
                       plot.lim = c(380,1000), warn.length = 60,
                       save.plots = NULL){

  # Deprecated function
  .Deprecated("click.peak2")

  # Check arguments ####
  if(!is.numeric(plot.lim) | length(plot.lim) != 2){
    stop("'plot.lim' must be numeric and of length 2")}
  if(!is.numeric(warn.length)) {stop("'warn.length' must be of class numeric")
  } else {if(warn.length <= 0) stop("'warn.length' must be greater than 0")}
  if(!is.null(save.plots)){
    if(!is.character(save.plots)) stop("'save.plot' must be a character string")}

  ## Check flux.unique ####
  if(missing(flux.unique)) stop("'flux.unique' is required")
  if(!is.null(flux.unique) & !is.data.frame(flux.unique)){
    stop("'flux.unique' must be of class data.frame")}

  ### UniqueID ####
  if(!any(grepl("\\<UniqueID\\>", names(flux.unique)))){
    stop("'UniqueID' is required and was not found in 'flux.unique'")}

  ### POSIX.time ####
  if(!any(grepl("\\<POSIX.time\\>", names(flux.unique)))){
    stop("'POSIX.time' is required and was not found in 'flux.unique'")
  } else if(!is.POSIXct(flux.unique$POSIX.time)){
    stop("'POSIX.time' in 'flux.unique' must be of class POSIXct")}

  ### gastype and match in flux.unique ####
  if(is.null(gastype)) stop("'gastype' is required") else {
    if(!is.character(gastype)) stop("'gastype' must be a character string")}
  if(!any(grepl(paste("\\<", gastype, "\\>", sep = ""),
                c("CO2dry_ppm", "COdry_ppb", "CH4dry_ppb", "N2Odry_ppb", "NH3dry_ppb", "H2O_ppm")))){
    stop("'gastype' must be of class character and one of the following: 'CO2dry_ppm', 'COdry_ppm', 'CH4dry_ppb', 'N2Odry_ppb', 'NH3dry_ppb' or 'H2O_ppm'")}
  if(!any(grepl(paste("\\<", gastype, "\\>", sep = ""), names(flux.unique)))){
    stop("'flux.unique' must contain a column that matches 'gastype'")
  } else if(!is.numeric(Reduce("c", flux.unique[, gastype]))){
    stop("The column that matches 'gastype' in 'flux.unique' must be of class numeric")}

  ## sleep ####
  if(!is.numeric(sleep)) stop("'sleep' must be of class numeric")
  if(sleep > 10) stop("'sleep' must be shorter than 10 seconds")
  if(sleep < 0) stop("'sleep' cannot be negative")

  # Assign NULL to variables without binding ####
  flag <- . <- POSIX.time <- identify.error <- rownum <- NULL

  # Function that takes a break for a few seconds between each loop
  sleeploop <- function(x)
  {
    p <- proc.time()
    Sys.sleep(x)
    proc.time() - p # The CPU usage should be negligible
  }

  # FUNCTION STARTS ####

  # Extract data from data.frame
  flux.meas <- Reduce("c", flux.unique[, gastype])
  time.meas <- Reduce("c", flux.unique[, "POSIX.time"])

  # Graph limits
  ymax <- max(flux.meas, na.rm = TRUE)
  ymin <- min(flux.meas, na.rm = TRUE)
  ydiff <- ymax - ymin

  ylim <- c(ymin - ydiff*0.05, ymax + ydiff*0.05)
  ylim.min <- ifelse(ylim[1] < plot.lim[1], plot.lim[1], ylim[1])
  ylim.max <- ifelse(ylim[2] > plot.lim[2], plot.lim[2], ylim[2])

  # Open plot in a new window to avoid problems with the identify function
  tryCatch(
    {
      # Modify default settings for graphic device
      device <- getOption("device")
      options(device = "X11")

      # Open a new window
      dev.new(noRStudioGD = TRUE, width = 14, height = 8)

      # Plot individual measurements
      plot(flux.meas ~ time.meas,
           main = paste(unique(flux.unique$UniqueID)),
           xlab = "Time", ylab = gastype, xaxt = 'n',
           ylim = c(ylim.min, ylim.max))

      # Force axis.POSIXct to use the right time zone
      time.zone <- attr(time.meas, "tzone")
      Sys.setenv(TZ = time.zone)
      axis.POSIXct(1, at = seq(min(time.meas), max(time.meas), by = "10 secs"),
                   format = "%H:%M:%S")
      Sys.unsetenv("TZ") # change back to default

      # Use the identify function to select start and end points
      rownum <- identify(time.meas, flux.meas, pos = FALSE, n = 2, plot = TRUE,
                         atpen = FALSE, offset = 0.5, tolerance = 0.25)
    },
    # Catch error "graphics device closed during call to locator or identify"
    error = function(e) {identify.error <<- e}
  )

  if(isTRUE(identify.error[[1]] == "graphics device closed during call to locator or identify")){

    # Revert back to default settings for graphic device
    options(device = device)

    # Close identify plot
    dev.flush()
    dev.off()

    # Stop and return error
    warning(paste(identify.error[[1]], "for UniqueID:",
                  unique(flux.unique$UniqueID)), call. = FALSE)

  } else {

    # Close identify plot
    dev.flush()
    dev.off()

    # Assign fictional values to rownum for the function check to work
    if(length(rownum) < 2){rownum <- c(1,1)}

    # Extract data from the identification
    start.time_corr <- time.meas[rownum[1]]
    end.time_corr <- time.meas[rownum[2]]
    flux.flag <- which(between(time.meas, start.time_corr, end.time_corr))

    # Based on these identifications, the flagging and Etime columns are added
    flux.corr <- flux.unique %>%
      # 0 == no measurement, 1 == measurement point to be used for flux calculation
      mutate(flag = if_else(row_number() %in% flux.flag, 1, 0)) %>%
      # Set to 0 at start of measurement and count seconds to end of measurement
      mutate(Etime = as.numeric(POSIX.time - start.time_corr, units = "secs")) %>%
      # Add start.time_corr and end.time_corr
      mutate(start.time_corr = start.time_corr,
             end.time_corr = end.time_corr) %>%
      # Add obs.length
      mutate(obs.length_corr = as.numeric(end.time_corr - start.time_corr, units = "secs"))

    # xaxis in validation plot: get lowest and highest Etime, rounded around 30s
    xmin <- min(round_any(flux.corr$Etime, 30, f = floor)) %>% if_else(. == 0, -15, .)
    xmax <- max(round_any(flux.corr$Etime, 30, f = ceiling)) %>% if_else(. == 0, 15, .)
    xmult <- (xmax + abs(xmin))/30

    # yaxis in validation plot: zoom on the selected values
    ymax2 <- flux.corr %>% filter(flag == 1) %>% select(all_of(gastype)) %>% max()
    ymin2 <- flux.corr %>% filter(flag == 1) %>% select(all_of(gastype)) %>% min()
    ydiff2 <- ymax2 - ymin2

    ylim2 <- c(ymin2 - ydiff2, ymax2 + ydiff2)
    ylim.min2 <- ifelse(ylim2[1] < ylim.min, ylim.min, ylim2[1])
    ylim.max2 <- ifelse(ylim2[2] > ylim.max, ylim.max, ylim2[2])

    # Inspect the full data set to see if it looks OK
    dev.new(noRStudioGD = TRUE, width = 14, height = 8)
    plot(flux.meas ~ flux.corr$Etime, col = flux.corr$flag+1,
         main = paste(unique(flux.corr$UniqueID)),
         xlab = "Etime", ylab = gastype, xaxp = c(xmin, xmax, xmult),
         ylim = c(ylim.min2, ylim.max2))

    # Wait a few seconds before closing the window to inspect the plot
    if(!is.null(sleep) | sleep > 0) sleeploop(sleep)
    dev.off()

    # Revert back to default settings for graphic device
    options(device = device)

    # Print warning if nb.obs < warn.length (default 60 observations)
    if(nrow(filter(flux.corr, flag == 1)) < warn.length){
      warning("Number of observations for UniqueID: ", unique(flux.corr$UniqueID),
              " is ", nrow(filter(flux.corr, flag == 1)), " observations",
              call. = FALSE)
    } else {
      message("Good window of observation for UniqueID: ", unique(flux.corr$UniqueID))
    }

    # Save plots
    if(!is.null(save.plots)){

      if(save.plots == "click.peak.loop.secret.string"){

        # Save plot with flux.corr
        dev.new(width = 11.6, height = 8.2, unit = "in")
        par(mfrow = c(1,2))

        # Plot 1
        plot(flux.meas ~ time.meas,
             xlab = "Time", ylab = gastype, xaxt = 'n',
             ylim = c(ylim.min, ylim.max))
        time.zone <- attr(time.meas, "tzone")
        Sys.setenv(TZ = time.zone)
        axis.POSIXct(1, at = seq(min(time.meas), max(time.meas), by = "10 secs"),
                     format = "%H:%M:%S")
        Sys.unsetenv("TZ")

        # Title
        mtext(paste("UniqueID :", unique(flux.unique$UniqueID)),
              line = -3, outer = T, cex = 1.5)

        # Plot 2
        plot(flux.meas ~ flux.corr$Etime, col = flux.corr$flag+1,
             xlab = "Etime", ylab = gastype, xaxp = c(xmin, xmax, xmult),
             ylim = c(ylim.min2, ylim.max2))

        plots <- recordPlot()

        # Close plot
        dev.off()

        # Add plots to flux.corr
        flux.corr <- list(flux.corr, plots)
      } else {

        # outfile
        outfile <- paste(getwd(), "/", save.plots, ".pdf", sep = "")

        # Print pdf
        pdf(file = outfile, width = 11.6, height = 8.2)
        par(mfrow = c(1,2))

        # Plot 1
        plot(flux.meas ~ time.meas,
             xlab = "Time", ylab = gastype, xaxt = 'n',
             ylim = c(ylim.min, ylim.max))
        time.zone <- attr(time.meas, "tzone")
        Sys.setenv(TZ = time.zone)
        axis.POSIXct(1, at = seq(min(time.meas), max(time.meas), by = "10 secs"),
                     format = "%H:%M:%S")
        Sys.unsetenv("TZ")

        # Title
        mtext(paste("UniqueID :", unique(flux.unique$UniqueID)),
              line = -3, outer = T, cex = 1.5)

        # Plot 2
        plot(flux.meas ~ flux.corr$Etime, col = flux.corr$flag+1,
             xlab = "Etime", ylab = gastype, xaxp = c(xmin, xmax, xmult),
             ylim = c(ylim.min2, ylim.max2))

        # Close pdf
        dev.off()
      }
    }

    # Return results
    return(flux.corr)

  }
}
