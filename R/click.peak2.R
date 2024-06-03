#' Manual identification of start and end of gas measurements
#'
#' Identify the start and the end of a measurement by clicking on them in a
#' scatter plot. To use in a loop with multiple measurements, first apply the
#' function \code{\link[goFlux]{obs.win}} to identify the observation
#' window of each measurement and then use the function
#' \code{\link[goFlux]{click.peak2}} (see examples below).
#'
#' @param ow.list list of data.frame; output from the function
#'                \code{\link[goFlux]{obs.win}}. Must contain the columns
#'                \code{gastype} (see below), \code{POSIX.time} and \code{UniqueID}.
#' @param gastype character string; specifies which gas should be displayed on
#'                the plot to manually select start time and end time of
#'                measurements. Must be one of the following: "CO2dry_ppm",
#'                "COdry_ppb", "CH4dry_ppb", "N2Odry_ppb", "NH3dry_ppb" or
#'                "H2O_ppm". Default is "CO2dry_ppm".
#' @param sleep numerical value; delay before closing the resulting plot. Grants
#'              a delay between measurements to visually inspect the output
#'              before processing the next measurement. Sleep must be shorter
#'              than 10 seconds. If \code{sleep = NULL}, the  plots will not close.
#' @param plot.lim numerical vector of length 2; sets the Y axis limits in the
#'                 plots. Default values are set for a typical gas measurement
#'                 of "CO2dry_ppm" from soils: \code{plot.lim = c(380,1000)}.
#' @param seq a numerical sequence that indicates objects in a list. By default,
#'            \code{seq = NULL} and the function loops through all data frames
#'            in \code{ow.list}.
#' @param warn.length numerical value; limit under which a measurement is flagged
#'                    for being too short (\code{nb.obs < warn.length}). Default
#'                    value is \code{warn.length = 60}.
#' @param save.plots character string; a file path with the extension .pdf to
#'                   save the plots produced with click.peak2. By default,
#'                   \code{save.plot = NULL} and plots are not saved.
#'
#' @returns A list of data.frame, identical to an unlisted version of the input
#'          \code{ow.list}, with the additional columns \code{flag}, \code{Etime},
#'          \code{start.time_corr}, \code{end.time_corr} and \code{obs.length_corr}.
#'
#' @include goFlux-package.R
#'
#' @seealso See also \code{\link[goFlux]{obs.win}} to prepare a list of data.frame.
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
#' IMPORTANT! This function makes use of the function \code{\link[graphics]{identify}}
#' which is only supported on screen devices such as X11, windows and quartz.
#' It is therefore essential to verify that your system options are compatible
#' with this function before running it, to avoid errors. Use the function
#' \code{getOption("device")} to find which device your system uses.
#'
#' The argument \code{seq} is used to select a subset of data frame from the list
#' of data frames in \code{ow.list}. For example, to apply the function on the
#' first measurement in the list, set \code{seq = 1}, or \code{seq = seq(1,10)}
#' for the first 10 measurements.
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
#' # IMPORTANT! This function makes use of the function graphics::identify()
#' # which is only supported on screen devices such as X11, windows and quartz.
#' # It is therefore essential to verify that your system options are compatible
#' # with this function before running it, to avoid errors. Here is an example
#' # of how to modify your system options for graphics device:
#' \dontrun{
#' default.device <- getOption("device") # save default option
#' options(device = "X11") # change system option to device = "X11"
#' options(device = default.device) # revert back to default option }
#'
#' # There are multiple ways to use this function. Here are some examples:
#'
#' library(dplyr)
#' \donttest{
#' ## with a LGR instrument and an auxiliary file (.txt),
#' ## and plots saved as pdf
#' aux.path <- system.file("extdata", "aux_UGGA/aux_UGGA.txt", package = "goFlux")
#' auxfile <- read.delim(aux.path) %>%
#'   mutate(start.time = as.POSIXct(start.time, tz = "UTC"))
#' data(imp.UGGA)
#' ow.UGGA <- obs.win(inputfile = imp.UGGA, auxfile = auxfile,
#'                    obs.length = 180, shoulder = 60)
#' save.to <- paste(tempdir(), "manID.UGGA.pdf", sep = "/")
#' manID.UGGA <- click.peak2(ow.UGGA, save.plots = save.to)
#' # In this example, plots are saved in a temporary directory using
#' # the function tempdir(). To save files in your working directory
#' # instead, use the function getwd().
#'
#'
#' ## with a LI-COR instrument, the Smart Chamber as auxiliary file
#' ## and only the first data frame selected
#' data(imp.LI8200)
#' data(imp.LI7820)
#' ow.LI7820 <- obs.win(inputfile = imp.LI7820, auxfile = imp.LI8200, shoulder = 60)
#' manID.LI7820 <- click.peak2(ow.LI7820, gastype = "N2Odry_ppb",
#'                             plot.lim = c(250,500), seq = 1)
#'
#'
#' ## with the LI-6400, no auxiliary file
#' ## and seq through the first 3 data frames
#' data(imp.LI6400)
#' ow.LI6400 <- obs.win(inputfile = imp.LI6400, shoulder = 0)
#' manID.LI6400 <- click.peak2(ow.LI6400, seq = seq(1,3)) }
#' @export

click.peak2 <- function(ow.list, gastype = "CO2dry_ppm", sleep = 3,
                        plot.lim = c(380,1000), seq = NULL,
                        warn.length = 60, save.plots = NULL){

  # Check arguments ####
  if(!is.numeric(plot.lim) | length(plot.lim) != 2){
    stop("'plot.lim' must be numeric and of length 2")}
  if(!is.numeric(warn.length)) {stop("'warn.length' must be of class numeric")
  } else {if(warn.length <= 0) stop("'warn.length' must be greater than 0")}
  if(!is.null(save.plots)){
    if(!is.character(save.plots)) stop("'save.plot' must be a character string")}

  ## seq ####
  if(!is.null(seq)) if(!is.numeric(seq)) stop("'seq' must be of class numeric")
  if(is.null(seq)) seq <- 1:length(ow.list)

  ## Check ow.list ####
  if(missing(ow.list)) stop("'ow.list' is required")
  if(!is.list(ow.list)) stop("'ow.list' must be of class list")
  for (ow in seq){
    if(!is.data.frame(ow.list[[ow]])){
      stop(paste("The ", toOrdinal(ow), " element in 'ow.list' is of class ",
                 class(ow.list[[ow]])[1],
                 ". All elements in 'ow.list' must be of class data.frame",
                 sep = ""))}}

  ### UniqueID ####
  for (ow in seq){
    if(!any(grepl("\\<UniqueID\\>", names(ow.list[[ow]])))){
      stop(paste("'UniqueID' is required and was not found in the",
                 toOrdinal(ow), "element of 'ow.list'"))}}

  ### POSIX.time ####
  for (ow in seq){
    if(!any(grepl("\\<POSIX.time\\>", names(ow.list[[ow]])))){
      stop(paste("'POSIX.time' is required and was not found in the",
                 toOrdinal(ow), "element of 'ow.list'"))
    } else if(!is.POSIXct(ow.list[[ow]]$POSIX.time)){
      stop("'POSIX.time' in 'ow.list' must be of class POSIXct")}}

  ### gastype and match in ow.list ####
  if(is.null(gastype)) stop("'gastype' is required") else {
    if(!is.character(gastype)) stop("'gastype' must be a character string")}
  if(!any(grepl(paste("\\<", gastype, "\\>", sep = ""),
                c("CO2dry_ppm", "COdry_ppb", "CH4dry_ppb", "N2Odry_ppb", "NH3dry_ppb", "H2O_ppm")))){
    stop(paste("'gastype' must be of class character and one of the following:",
               "'CO2dry_ppm', 'COdry_ppm', 'CH4dry_ppb', 'N2Odry_ppb', 'NH3dry_ppb' or 'H2O_ppm'"))}
  for (ow in seq){
    if(!any(grepl(paste("\\<", gastype, "\\>", sep = ""), names(ow.list[[ow]])))){
      stop("data frames in 'ow.list' must contain a column that matches 'gastype'")
    } else if(!is.numeric(Reduce("c", ow.list[[ow]][, gastype]))){
      stop(paste("The column that matches 'gastype' in each data frame of",
                 "'ow.list' must be of class numeric"))}}

  ## sleep ####
  if(!is.null(sleep)){
    if(!is.numeric(sleep)) stop("'sleep' must be of class numeric")
    if(sleep > 10) stop("'sleep' must be shorter than 10 seconds")
    if(sleep < 0) stop("'sleep' cannot be negative")}

  # Assign NULL to variables without binding ####
  flag <- . <- POSIX.time <- identify.error <- rownum <- NULL

  # Function that takes a break for a few seconds between each loop
  sleeploop <- function(x) {
    p <- proc.time()
    Sys.sleep(x)
    proc.time() - p # The CPU usage should be negligible
  }

  # Restore default options on exit
  default.par <- par(no.readonly = TRUE)
  on.exit(par(default.par))

  # default.device <- getOption("device")
  # on.exit(options(device = default.device))

  on.exit(Sys.unsetenv("TZ"))

  # FUNCTION STARTS ####

  # Extract name of data frame ow.list
  ow.list.name <- deparse(substitute(ow.list))

  # Create empty lists to store results
  ow.corr.ls <- list()
  plots.ls <- list()

  # click.peak.loop
  for (ow in seq){

    # Extract data from data.frame
    flux.meas <- Reduce("c", ow.list[[ow]][, gastype])
    time.meas <- Reduce("c", ow.list[[ow]][, "POSIX.time"])

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
        identify.error <- NULL

        # Modify default settings for graphic device
        # options(device = "X11")

        # Open a new window
        dev.new(noRStudioGD = TRUE, width = 14, height = 8)

        # Plot individual measurements
        plot(flux.meas ~ time.meas,
             main = paste(unique(ow.list[[ow]]$UniqueID)),
             xlab = "Time", ylab = gastype, xaxt = 'n',
             ylim = c(ylim.min, ylim.max))

        # Force axis.POSIXct to use the right time zone
        time.zone <- attr(time.meas, "tzone")
        Sys.setenv(TZ = time.zone)
        axis.POSIXct(1, at = seq(min(time.meas), max(time.meas), by = "10 secs"),
                     format = "%H:%M:%S")
        # Sys.unsetenv("TZ") # change back to default

        # Use the identify function to select start and end points
        rownum <- identify(time.meas, flux.meas, pos = FALSE, n = 2, plot = TRUE,
                           atpen = FALSE, offset = 0.5, tolerance = 0.25)
      },
      # Catch error "graphics device closed during call to locator or identify"
      error = function(e) {identify.error <<- e}
    )

    if(isTRUE(identify.error[[1]] == "graphics device closed during call to locator or identify")){

      # Close identify plot
      dev.flush()
      dev.off()

      # Revert back to default settings for graphic device
      # options(device = device)

      # Stop and return error
      warning(paste(ow.list.name, "[[", ow, "]] UniqueID ",
                    unique(ow.list[[ow]]$UniqueID), ": ",
                    identify.error[[1]], sep = ""), call. = FALSE)

      # Save plots
      if(!is.null(save.plots)){

        # Open new plot window
        dev.new()
        par(mfrow = c(1,2))

        # Plot 1
        plot(flux.meas ~ time.meas,
             xlab = "Time", ylab = gastype, xaxt = 'n',
             ylim = c(ylim.min, ylim.max))
        time.zone <- attr(time.meas, "tzone")
        Sys.setenv(TZ = time.zone)
        axis.POSIXct(1, at = seq(min(time.meas), max(time.meas), by = "10 secs"),
                     format = "%H:%M:%S")
        # Sys.unsetenv("TZ")

        # Title
        mtext(line = -3, outer = T, cex = 1.5,
              paste(ow.list.name, "[[", ow, "]] UniqueID ",
                    unique(ow.list[[ow]]$UniqueID), sep = ""))

        # Plot 2
        par(mar = c(0, 0, 0, 0))
        plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n", xaxt = "n", yaxt = "n")

        # Print identify error message
        identify.message <- paste(ow.list.name, "[[", ow, "]] UniqueID ",
                                  unique(ow.list[[ow]]$UniqueID),
                                  ": ", identify.error[[1]], sep = "")

        mes <- strsplit( strwrap(identify.message, width=60), "\n")
        for(i in seq(along=mes)) text(mes[[i]], y = 6-i*0.5, x = 5)

        # Save plots
        plots.ls[[ow]] <- recordPlot()

        # Close plot
        dev.off()
      }

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
      ow.corr.ls[[ow]] <- ow.list[[ow]] %>%
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
      xmin <- min(round_any(ow.corr.ls[[ow]]$Etime, 30, f = floor)) %>% if_else(. == 0, -15, .)
      xmax <- max(round_any(ow.corr.ls[[ow]]$Etime, 30, f = ceiling)) %>% if_else(. == 0, 15, .)
      xmult <- (xmax + abs(xmin))/30

      # yaxis in validation plot: zoom on the selected values
      ymax2 <- ow.corr.ls[[ow]] %>% filter(flag == 1) %>% select(all_of(gastype)) %>% max()
      ymin2 <- ow.corr.ls[[ow]] %>% filter(flag == 1) %>% select(all_of(gastype)) %>% min()
      ydiff2 <- ymax2 - ymin2

      ylim2 <- c(ymin2 - ydiff2, ymax2 + ydiff2)
      ylim.min2 <- ifelse(ylim2[1] < ylim.min, ylim.min, ylim2[1])
      ylim.max2 <- ifelse(ylim2[2] > ylim.max, ylim.max, ylim2[2])

      # Inspect the full data set to see if it looks OK
      dev.new(noRStudioGD = TRUE, width = 14, height = 8)
      plot(flux.meas ~ ow.corr.ls[[ow]]$Etime, col = ow.corr.ls[[ow]]$flag+1,
           main = paste(unique(ow.corr.ls[[ow]]$UniqueID)),
           xlab = "Etime", ylab = gastype, xaxp = c(xmin, xmax, xmult),
           ylim = c(ylim.min2, ylim.max2))

      # Wait a few seconds before closing the window to inspect the plot
      if(!is.null(sleep)){
        if(sleep > 0) sleeploop(sleep)
        dev.off()}

      # Revert back to default settings for graphic device
      # options(device = device)

      # Print warning if nb.obs < warn.length (default 60 observations)
      if(nrow(filter(ow.corr.ls[[ow]], flag == 1)) < warn.length){
        warning(paste(ow.list.name, "[[", ow, "]] UniqueID ",
                      unique(ow.corr.ls[[ow]]$UniqueID),
                      ": Number of observations (",
                      nrow(filter(ow.corr.ls[[ow]], flag == 1)),
                      ") smaller than warn.length=",
                      warn.length, sep = ""), call. = FALSE)
      } else {
        message(paste(ow.list.name, "[[", ow, "]] UniqueID ",
                      unique(ow.corr.ls[[ow]]$UniqueID),
                      ": Good window of observation", sep = ""))
      }

      # Save plots
      if(!is.null(save.plots)){

        # Open new plot window
        dev.new()
        par(mfrow = c(1,2))

        # Plot 1
        plot(flux.meas ~ time.meas,
             xlab = "Time", ylab = gastype, xaxt = 'n',
             ylim = c(ylim.min, ylim.max))
        time.zone <- attr(time.meas, "tzone")
        Sys.setenv(TZ = time.zone)
        axis.POSIXct(1, at = seq(min(time.meas), max(time.meas), by = "10 secs"),
                     format = "%H:%M:%S")
        # Sys.unsetenv("TZ")

        # Title
        mtext(line = -3, outer = T, cex = 1.5,
              paste(ow.list.name, "[[", ow, "]] UniqueID ",
                    unique(ow.corr.ls[[ow]]$UniqueID), sep = ""))

        # Plot 2
        plot(flux.meas ~ ow.corr.ls[[ow]]$Etime, col = ow.corr.ls[[ow]]$flag+1,
             xlab = "Etime", ylab = gastype, xaxp = c(xmin, xmax, xmult),
             ylim = c(ylim.min2, ylim.max2))

        # Save plots
        plots.ls[[ow]] <- recordPlot()

        # Close plot
        dev.off()
      }
    }
  }

  if (length(plots.ls) > 0){

    # pdf outfile
    # outfile <- paste(getwd(), "/", save.plots, ".pdf", sep = "")

    # Save plots as pdf
    pdf(file = save.plots, width = 11.6, height = 8.2)
    for (p in 1:length(plots.ls)){
      if(!is.null(plots.ls[[p]])) print(plots.ls[[p]])
    }
    dev.off()
  }

  # Unlist ow.corr.ls
  ow.corr <- map_df(ow.corr.ls,  ~as.data.frame(.x))

  # Return results
  return(ow.corr)
}
