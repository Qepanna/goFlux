#' Manual identification of peaks on gas measurements
#'
#' Identify the start and the end of a measurement by clicking on them in a
#' scatter plot. Requires start time and UniqueID. To use in a loop with
#' multiple measurements,first use the function obs.win to identify the
#' observation window of each measurement and use the wrapper function
#' click.peak.loop with lapply.
#'
#' @param x A numerical sequence that indicates objects in a list. Must be used
#'          with lapply after the function obs.win. See examples on how to use
#'          with lapply.
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
#' @returns a list of data frame, split by UniqueID.
#'
#' @include GoFluxYourself-package.R
#' @include click.peak.R
#'
#' @seealso [click.peak()]
#' @seealso [obs.win()]
#'
#' @examples
#' # Examples on how to use it in multiple situations:
#' # Note that gastype = "CO2dry_ppm" is the default setting
#' library(dplyr)
#' library(purrr)
#'
#' ## with a LGR instrument and an auxiliary file (.txt)
#' aux.path <- system.file("extdata", "LGR/example_LGR_aux.txt",
#'                         package = "GoFluxYourself")
#' auxfile <- read.delim(aux.path) %>%
#'   mutate(start.time = as.POSIXct(start.time, tz = "UTC"))
#' data(example_LGR_imp)
#' example_LGR_ow <- obs.win(inputfile = example_LGR_imp, auxfile = auxfile,
#'                           obs.length = 180, shoulder = 60)
#' example_LGR_manID <- lapply(seq(1,3), click.peak.loop,
#'                             flux.unique = example_LGR_ow) %>%
#'   map_df(., ~as.data.frame(.x))
#'
#' ## with a LI-COR instrument and the Smart Chamber as auxiliary file
#' data(example_LI8200_imp)
#' data(example_LI7810_imp)
#' example_LI7810_ow <- obs.win(inputfile = example_LI7810_imp,
#'                              auxfile = example_LI8200_imp,
#'                              shoulder = 30)
#' example_LI7810_manID <- lapply(seq(1,3), click.peak.loop,
#'                                flux.unique = example_LI7810_ow) %>%
#'   map_df(., ~as.data.frame(.x))
#'
#' ## with the LI-6400 and no auxiliary file
#' data(example_LI6400_imp)
#' example_LI6400_ow <- obs.win(inputfile = example_LI6400_imp, shoulder = 0)
#' example_LI6400_manID <- lapply(seq(1,3), click.peak.loop,
#'                                flux.unique = example_LI6400_ow) %>%
#'   map_df(., ~as.data.frame(.x))
#'
#' @export
#'
click.peak.loop <- function(x, flux.unique, gastype = "CO2dry_ppm", sleep = 3,
                            plot.lim = c(380,1000), warn.length = 60) {

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
