#' Looped manual identification of peaks on gas measurements
#'
#' Identify the start and the end of a measurement by clicking on them in a
#' scatter plot. To use in a loop with multiple measurements, first use the
#' function \code{\link[GoFluxYourself]{obs.win}} to identify the observation
#' window of each measurement and then use the wrapper function
#' \code{\link[GoFluxYourself]{click.peak.loop}} with
#' \code{\link[base]{lapply}} (see example below).
#'
#' @param x a numerical sequence that indicates objects in a list. Must be used
#'          with \code{\link[base]{lapply}} after the function
#'          \code{\link[GoFluxYourself]{obs.win}}. See examples below to see
#'          how to use with \code{\link[base]{lapply}}.
#' @param flux.unique a list of data.frame; output from the function
#'                    \code{\link[GoFluxYourself]{obs.win}}. Must contain the
#'                    columns \code{gastype} (see below), \code{POSIX.time} and
#'                    \code{UniqueID}.
#' @param gastype character string; specifies which gas should be displayed on the
#'                plot to manually select start time and end time of measurements.
#'                Must be one of the following: "CO2dry_ppm", "COdry_ppb",
#'                "CH4dry_ppb", "N2Odry_ppb", "NH3dry_ppb" or "H2O_ppm".
#'                Default is "CO2dry_ppm".
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
#' @returns A list of data.frame, split by \code{UniqueID}, identical to the
#'          input \code{flux.unique}, with the additional columns \code{flag},
#'          \code{Etime}, \code{start.time_corr}, \code{end.time_corr} and
#'          \code{obs.length_corr}.
#'
#' @include GoFluxYourself-package.R
#' @include click.peak.R
#'
#' @seealso Use the function \code{\link[GoFluxYourself]{click.peak}} for
#'          individual measurements, instead of using in a loop with
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
#' library(purrr)
#'
#' ## with a LGR instrument and an auxiliary file (.txt)
#' aux.path <- system.file("extdata", "LGR_aux/LGR_aux.txt", package = "GoFluxYourself")
#' auxfile <- read.delim(aux.path) %>%
#'   mutate(start.time = as.POSIXct(start.time, tz = "UTC"))
#' data(LGR_imp)
#' LGR_ow <- obs.win(inputfile = LGR_imp, auxfile = auxfile,
#'                   obs.length = 180, shoulder = 60)
#' LGR_manID <- lapply(seq(1,3), click.peak.loop, flux.unique = LGR_ow) %>%
#'   map_df(., ~as.data.frame(.x))
#'
#' ## with a LI-COR instrument and the Smart Chamber as auxiliary file
#' data(LI8200_imp)
#' data(LI7820_imp)
#' LI7820_ow <- obs.win(inputfile = LI7820_imp, auxfile = LI8200_imp, shoulder = 60)
#' LI7810_manID <- lapply(seq(1,3), click.peak.loop, flux.unique = LI7820_ow,
#'                        gastype = "N2Odry_ppb", plot.lim = c(250,500)) %>%
#'   map_df(., ~as.data.frame(.x))
#'
#' ## with the LI-6400 and no auxiliary file
#' data(LI6400_imp)
#' LI6400_ow <- obs.win(inputfile = LI6400_imp, shoulder = 0)
#' LI6400_manID <- lapply(seq(1,3), click.peak.loop, flux.unique = LI6400_ow) %>%
#'   map_df(., ~as.data.frame(.x))
#'
#' @export
#'
click.peak.loop <- function(x, flux.unique, gastype = "CO2dry_ppm", sleep = 3,
                            plot.lim = c(380,1000), warn.length = 60) {

  # Check arguments
  ## x
  if(missing(x)) stop("'x' is required") else {
    if(!is.numeric(x)) stop("'x' must be of class numeric")
  }
  ## flux.unique
  if(missing(flux.unique)) stop("'flux.unique' is required") else {
    if(!is.list(flux.unique)) {
      stop("'flux.unique' must be of class list")} else {
        if(all(grepl("data.frame", sapply(flux.unique, class)))){
          stop("all elements of 'flux.unique' must be of class data.frame")}
      }
  }

  ## gastype
  if(is.null(gastype)) stop("'gastype' is required")
  if(!is.null(gastype) & !is.character(gastype)) stop("'gastype' must be a character string")
  if(!any(grepl(paste("\\<", gastype, "\\>", sep = ""),
                c("CO2dry_ppm", "COdry_ppb", "CH4dry_ppb", "N2Odry_ppb", "NH3dry_ppb", "H2O_ppm")))){
    stop("'gastype' must be of class character and one of the following: 'CO2dry_ppm', 'COdry_ppm', 'CH4dry_ppb', 'N2Odry_ppb', 'NH3dry_ppb' or 'H2O_ppm'")}

  ## sleep
  if(!is.numeric(sleep)) stop("'sleep' must be of class numeric")
  if(sleep > 10) stop("'sleep' must be shorter than 10 seconds")
  if(sleep < 0) stop("'sleep' cannot be negative")

  ## plot.lim and warn.length
  if(!is.numeric(plot.lim) | length(plot.lim) != 2){
    stop("'plot.lim' must be numeric and of length 2")}
  if(!is.numeric(warn.length)) {stop("'warn.length' must be of class numeric")
  } else {if(warn.length <= 0) stop("'warn.length' must be greater than 0")}

  # Catch warnings from click.peak function to print after loop
  warn <- character(0)
  withCallingHandlers(

    # Function to apply in the loop. Adapt parameters to your needs.
    flux.corr <- click.peak(flux.unique[[x]], gastype = gastype, sleep = sleep,
                            plot.lim = plot.lim, warn.length = warn.length),

    warning = function(w) {warn <<- c(warn, conditionCall(w))})

  # Print warnings after loop
  warn <- trimws(warn); for (w in warn) warning(w, call. = F)

  return(flux.corr)
}
