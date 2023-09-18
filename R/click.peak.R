#' Manual identification of peaks on gas measurements
#'
#' Identify the start and the end of a measurement by clicking on them in a
#' scatter plot. Requires start time and UniqueID. To use in a loop with
#' multiple measurements,first use the function obs.win to identify the
#' observation window of each measurement and use the wrapper function
#' click.peak.loop with lapply.
#'
#' @param flux.unique A data.frame. Output from the function obs.win.
#'                    Must contain a gastype (see gastype below) and the
#'                    columns POSIX.time and UniqueID.
#' @param gastype Character string. Specify which gas should be displayed on the
#'                plot to manually select start time and end time of measurements.
#'                Must be one of the following: "CO2dry_ppm", "CH4dry_ppb",
#'                "N2Odry_ppb" or "H2O_ppm". Default is "CO2dry_ppm".
#' @param sleep Delay before closing the resulting plot. When used with the
#'              function click.peak.loop, grants a delay between measurements to
#'              let the user verify the output before processing the next measurement.
#' @param plot.lim Numerical vector of length 2. Y axis limits. Removes any data
#'                 points below and above the plot limits for a better view of
#'                 the scatter plot. Default values are set for a normal gas
#'                 measurement of CO2dry_ppm from the forest floor:
#'                 plot.lim = c(380,1000), where 380ppm is the minimum plotted
#'                 concentration, which corresponds to atmospheric concentration,
#'                 and 1000ppm is the maximum plotter concentraion, which correspond
#'                 to a maximal accumulated concentration in the chamber before
#'                 considering it an outlier (e.g. caused by breath or gas bubble).
#' @param warn.length Minimum amount of observations accepted (number of data points).
#'                    With nowadays portable greenhouse gas analyzers, the frequency
#'                    of measurement is 1 measurement per second. Therefore, the
#'                    amount of observation is equal to the chamber closure time
#'                    length (seconds). Default is one minute (60 seconds).
#'
#' @returns a list of data frame, split by UniqueID.
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' # Examples on how to use it in multiple situations:
#' # Note that gastype = "CO2dry_ppm" is the default setting
#' library(dplyr)
#'
#' ## with a LGR instrument and an auxiliary file (.txt)
#' aux.path <- system.file("extdata", "LGR/example_LGR_aux.txt",
#'                         package = "GoFluxYourself")
#' auxfile <- read.delim(aux.path) %>%
#'   mutate(start.time = as.POSIXct(start.time, tz = "UTC"))
#' data(example_LGR_imp)
#' example_LGR_ow <- obs.win(inputfile = example_LGR_imp, auxfile = auxfile,
#'                           obs.length = 180, shoulder = 60)
#' example_LGR_manID <- click.peak(example_LGR_ow[[1]])
#'
#' ## with a LI-COR instrument and the Smart Chamber as auxiliary file
#' data(example_LI8200_imp)
#' data(example_LI7810_imp)
#' example_LI7810_ow <- obs.win(inputfile = example_LI7810_imp,
#'                              auxfile = example_LI8200_imp,
#'                              shoulder = 30)
#' example_LI7810_manID <- click.peak(example_LI7810_ow[[1]])
#'
#' ## with the LI-6400 and no auxiliary file
#' data(example_LI6400_imp)
#' example_LI6400_ow <- obs.win(inputfile = example_LI6400_imp, shoulder = 0)
#' example_LI6400_manID <- click.peak(example_LI6400_ow[[1]])
#'
#' @export
#'
click.peak <- function(flux.unique, gastype = "CO2dry_ppm", sleep = 3,
                       plot.lim = c(380,1000), warn.length = 60) {

  # Assign NULL to variables without binding
  flag <- . <- NULL

  # Function that takes a break for a few seconds between each loop
  sleeploop <- function(x)
  {
    p <- proc.time()
    Sys.sleep(x)
    proc.time() - p # The CPU usage should be negligible
  }

  # Extract data from data.frame
  flux.meas <- Reduce("c", flux.unique[, gastype])
  time.meas <- Reduce("c", flux.unique[, "POSIX.time"])

  # Graph limits
  yaxis.limit.max <- (max(flux.meas, na.rm = TRUE) + 0.01*max(flux.meas, na.rm = TRUE)) %>%
    ifelse(. > plot.lim[2], plot.lim[2], .)
  yaxis.limit.min <- (min(flux.meas, na.rm = TRUE) - 0.01*max(flux.meas, na.rm = TRUE)) %>%
    ifelse(. < plot.lim[1], plot.lim[1], .)

  # Open plot in a new window to avoid problems with the identify function
  dev.new(noRStudioGD = TRUE, width = 14, height = 8)

  # Plot individual measurements
  plot(flux.meas ~ time.meas,
       main = paste(unique(flux.unique$UniqueID)),
       xlab = "Time", ylab = gastype, xaxt = 'n',
       ylim = c(yaxis.limit.min, yaxis.limit.max))
  axis.POSIXct(1, at = seq(min(time.meas), max(time.meas), by = "10 secs"), format = "%H:%M:%S")

  # Use the identify function to select start and end points
  rownum <- identify(time.meas, flux.meas, pos = FALSE, n = 2, plot = TRUE,
                     atpen = FALSE, offset = 0.5, tolerance = 0.25)

  # Close identify plot
  dev.flush()
  dev.off()

  # Assign fictional values to rownum for the function check to work
  if (length(rownum) < 2) {rownum <- c(1,2)}

  flux.start <- min(time.meas)
  flux.start_corr <- time.meas[rownum[1]]
  flux.end <- time.meas[rownum[2]]
  flux.flag <- which(between(time.meas, flux.start_corr, flux.end))
  flux.diff <- as.numeric(flux.start - flux.start_corr, units = "secs") -1

  # Based on these identifications, the flagging and Etime columns are added
  flux.corr <- flux.unique %>%
    # 0 == no measurement, 1 == measurement point to be used for flux calculation
    mutate(flag = if_else(row_number() %in% flux.flag, 1, 0)) %>%
    # Set to 0 at start of measurement and count seconds to end of measurement
    mutate(Etime = seq(flux.diff, length(time.meas) + flux.diff - 1)) %>%
    # Add start.time_corr and end.time
    mutate(start.time_corr = flux.start_corr,
           end.time = flux.end)

  # Inspect the full data set to see if it looks OK
  dev.new(noRStudioGD = TRUE, width = 14, height = 8)
  plot(flux.meas ~ flux.corr$Etime, col = flux.corr$flag+1,
       main = paste(unique(flux.corr$UniqueID)),
       xlab = "Etime", ylab = gastype, xaxp = c(-60, 300, 12),
       ylim = c(yaxis.limit.min, yaxis.limit.max))

  # Wait a few seconds before closing the window to inspect the plot
  sleeploop(sleep)
  dev.off()

  # Print warning if observation length < warn.length (default 60 observations)
  if (nrow(filter(flux.corr, flag == 1)) < warn.length) {
    warning("Observation length for UniqueID: ", unique(flux.corr$UniqueID),
            " is ", nrow(filter(flux.corr, flag == 1)), " observations",
            call. = FALSE)
  } else {
    message("Good window of observation for UniqueID: ", unique(flux.corr$UniqueID))
  }

  # Return results
  return(flux.corr)

}
