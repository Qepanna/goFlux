#' Manual identification of peaks on gas measurements
#'
#' Identify the start and the end of a measurement by clicking on them in a
#' scatter plot. To use in a loop with multiple measurements, first use the
#' function \code{\link[GoFluxYourself]{obs.win}} to identify the observation
#' window of each measurement and then use the wrapper function
#' \code{\link[GoFluxYourself]{click.peak.loop}} with
#' \code{\link[base]{lapply}} (see example below).
#'
#' @param flux.unique data.frame; output from the function
#'                    \code{\link[GoFluxYourself]{obs.win}}.
#'                    Must contain the columns \code{gastype} (see below),
#'                    \code{POSIX.time} and \code{UniqueID}.
#' @param gastype character string; specifies which gas should be displayed on
#'                the plot to manually select start time and end time of
#'                measurements. Must be one of the following: "CO2dry_ppm",
#'                "COdry_ppb", "CH4dry_ppb", "N2Odry_ppb", "NH3dry_ppb" or
#'                "H2O_ppm". Default is "CO2dry_ppm".
#' @param sleep numerical value; delay before closing the resulting plot. When
#'              used with the function \code{\link[GoFluxYourself]{click.peak.loop}},
#'              grants a delay between measurements to visually inspect the
#'              output before processing the next measurement. Sleep must be
#'              shorter than 10 seconds.
#' @param plot.lim numerical vector of length 2; Y axis limits. Default values
#'                 are set for a normal gas measurement of "CO2dry_ppm" from the
#'                 forest floor: \code{plot.lim = c(380,1000)}.
#' @param warn.length numerical value; limit under which a measurement is flagged
#'                    for being too short (\code{nb.obs < warn.length}).
#'
#' @returns A data.frame, identical to the input \code{flux.unique}, with the
#'          additional columns \code{flag}, \code{Etime}, \code{start.time_corr},
#'          \code{end.time_corr} and \code{obs.length_corr}.
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso To use the function \code{\link[GoFluxYourself]{click.peak}} in a
#'          loop with \code{\link[base]{lapply}}, use
#'          \code{\link[GoFluxYourself]{click.peak.loop}}. See also
#'          \code{\link[GoFluxYourself]{obs.win}} to prepare a list of data.frame.
#'
#' @details
#' The argument \code{plot.lim} is used to remove any data points below and
#' above the plot limits for a better view of the scatter plot. Default values
#' are set for a normal gas measurement of "CO2dry_ppm" from the forest floor:
#' \code{plot.lim = c(380,1000)}, where 380 ppm is the minimum plotted
#' concentration, which corresponds to atmospheric concentration, and 1000 ppm
#' is the maximum plotted concentration, which correspond to a maximal
#' accumulated concentration in the chamber before considering it an outlier
#' (e.g. caused by breath or gas bubble). For other gasses, one may use the
#' following suggested values:
#' \itemize{
#'   \item "CH4dry_ppb": \code{plot.lim = c(2200, 1800)}
#'   \item "N2Odry_ppb": \code{plot.lim = c(250, 500)}
#'   \item "NH3dry_ppb": \code{plot.lim = c(0, 200)}
#'   \item "COdry_ppb": \code{plot.lim = c(0, 200)}
#'   \item "H2O_ppm": \code{plot.lim = c(10000, 20000)}
#' }
#'
#' \code{warn.length} is the limit under which a measurement is flagged for
#' being too short (\code{nb.obs < warn.length}). With nowadays' portable
#' greenhouse gas analyzers, the frequency of measurement is usually one
#' observation per second. Therefore, for the default setting of
#' \code{warn.length = 60}, the chamber closure time should be approximately
#' one minute (60 seconds). If the number of observations is smaller than the
#' threshold, a warning is printed after the loop: "Number of observations for
#' UniqueID: 'UniqueID' is X observations".
#'
#' @examples
#' # How to use in multiple situations:
#' # Note that gastype = "CO2dry_ppm" is the default setting
#' library(dplyr)
#'
#' ## with a LGR instrument and an auxiliary file (.txt)
#' aux.path <- system.file("extdata", "LGR/LGR_aux.txt", package = "GoFluxYourself")
#' auxfile <- read.delim(aux.path) %>%
#'   mutate(start.time = as.POSIXct(start.time, tz = "UTC"))
#' data(LGR_imp)
#' LGR_ow <- obs.win(inputfile = LGR_imp, auxfile = auxfile,
#'                   obs.length = 180, shoulder = 60)
#' LGR_manID <- click.peak(LGR_ow[[1]])
#'
#' ## with a LI-COR instrument and the Smart Chamber as auxiliary file
#' data(LI8200_imp)
#' data(LI7820_imp)
#' LI7820_ow <- obs.win(inputfile = LI7820_imp, auxfile = LI8200_imp, shoulder = 60)
#' LI7820_manID <- click.peak(LI7820_ow[[1]], gastype = "N2Odry_ppb", plot.lim = c(250,500))
#'
#' ## with the LI-6400 and no auxiliary file
#' data(LI6400_imp)
#' LI6400_ow <- obs.win(inputfile = LI6400_imp, shoulder = 0)
#' LI6400_manID <- click.peak(LI6400_ow[[1]])
#'
#' @export
#'
click.peak <- function(flux.unique, gastype = "CO2dry_ppm", sleep = 3,
                       plot.lim = c(380,1000), warn.length = 60){

  # Check arguments ####
  if(!is.numeric(plot.lim) | length(plot.lim) != 2){
    stop("'plot.lim' must be numeric and of length 2")}
  if(!is.numeric(warn.length)) {stop("'warn.length' must be of class numeric")
  } else {if(warn.length <= 0) stop("'warn.length' must be greater than 0")}

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
  flag <- . <- POSIX.time <- NULL

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

  # Close identify plot
  dev.flush()
  dev.off()

  # Assign fictional values to rownum for the function check to work
  if(length(rownum) < 2){rownum <- c(1,2)}

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

  # Print warning if nb.obs < warn.length (default 60 observations)
  if(nrow(filter(flux.corr, flag == 1)) < warn.length){
    warning("Number of observations for UniqueID: ", unique(flux.corr$UniqueID),
            " is ", nrow(filter(flux.corr, flag == 1)), " observations",
            call. = FALSE)
  } else {
    message("Good window of observation for UniqueID: ", unique(flux.corr$UniqueID))
  }

  # Return results
  return(flux.corr)

}
