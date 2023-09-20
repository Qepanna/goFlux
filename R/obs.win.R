#' Define measurements' window for manual identification
#'
#' Split gas data into one observation window per gas measurement
#' and create a list of data frame (one data frame per UniqueID).
#'
#' @param inputfile Character string. The name of a file with the extension .RData
#'                  (output from import or align functions).
#' @param gastype Character string. Specify which gas should be displayed on the
#'                plot to manually select start time and end time of measurements.
#'                Must be one of the following: "CO2dry_ppm", "CH4dry_ppb",
#'                "N2Odry_ppb" or "H2O_ppm". Default is "CO2dry_ppm".
#' @param auxfile Auxiliary data frame containing the columns "start.time" and
#'                "UniqueID". start.time must contain a date and be in POSIXct
#'                format. The time zone must be the same as the POSIX.time in
#'                the inputfile. The default time zone for the import functions
#'                is "UTC". A file from the Smart Chamber (LI-8200) can be
#'                used as an auxiliary file in this case.
#' @param obs.length Numerical. Chamber closure time (seconds). Default is NULL.
#'                   If obs.length is not provided, a column "obs.length" should
#'                   be contained in the auxiliary file.
#' @param shoulder Numerical. Time before and after measurement in observation
#'                 window (seconds). Default is 120 seconds.
#'
#' @returns a list of data frame, split by UniqueID.
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso [click.peak.loop()]
#' @seealso [click.peak()]
#' @seealso [obs.win()]
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
#'
#' ## with a LI-COR instrument and the Smart Chamber as auxiliary file
#' data(example_LI8200_imp)
#' data(example_LI7810_imp)
#' example_LI7810_ow <- obs.win(inputfile = example_LI7810_imp,
#'                              auxfile = example_LI8200_imp,
#'                              shoulder = 30)
#'
#' ## with the LI-6400 and no auxiliary file
#' data(example_LI6400_imp)
#' example_LI6400_ow <- obs.win(inputfile = example_LI6400_imp, shoulder = 0)
#'
#' @export
#'
obs.win <- function(inputfile, auxfile = NULL, gastype = "CO2dry_ppm",
                    obs.length = NULL, shoulder = 120){

  # Assign NULL to variables without binding
  POSIX.time <- chamID <- start.time <- UniqueID <- NULL

  # Convert milliseconds to seconds, for compatibility
  inputfile <- inputfile %>%
    mutate(POSIX.time = as.POSIXct(round(POSIX.time, "secs")))

  # Get start.time and UniqueID
  if (is.null(auxfile)){
    aux.data <- inputfile %>% select(UniqueID = chamID, start.time) %>% distinct()
  } else {
    # Rename chamID to UniqueID if auxfile is a smart chamber
    if (any(grepl("chamID", names(auxfile))) == TRUE){
      auxfile <- auxfile %>% mutate(UniqueID = chamID)
    }
    aux.data <- auxfile %>% select(UniqueID, start.time) %>% distinct()
  }

  # Get obs.length
  if (is.null(obs.length)){
    # If there is no auxfile
    if (is.null(auxfile)){
      aux.data <- aux.data %>% left_join(
        inputfile %>% select(c(UniqueID = chamID, obs.length)) %>% distinct(), by = "UniqueID")
    } else {
      aux.data <- aux.data %>% left_join(
        auxfile %>% select(c(UniqueID, obs.length)) %>% distinct(), by = "UniqueID")
    }
  } else {
    aux.data <- aux.data %>% mutate(obs.length = obs.length)
  }

  # Define windows of time ranges to keep
  time_range <- aux.data %>% group_by(UniqueID, start.time) %>%
    reframe(time_min = start.time - shoulder,
            time_max = start.time + obs.length + shoulder)

  time_filter.ls <- list()
  for (i in 1:nrow(time_range)) {
    time_filter.ls[[i]] <- cbind.data.frame(UniqueID = time_range$UniqueID[[i]],
                                            start.time = time_range$start.time[[i]],
                                            POSIX.time = seq(from = time_range$time_min[i],
                                                             to = time_range$time_max[i],
                                                             by = 'sec'))
  }
  time_filter <- map_df(time_filter.ls, ~as.data.frame(.x)) %>% distinct()

  # If there is no auxfile, remove start.time from inputfile at this point
  if (is.null(auxfile)){
    inputfile <- inputfile %>% select(!c(start.time))
  }

  # Add start.time to inputfile and filter POSIXct
  data.filter <- inputfile %>%
    right_join(time_filter, relationship = "many-to-many", by = "POSIX.time") %>%
    drop_na(matches(gastype))

  # Split data.filter into a list of data frame unique per measurement
  flux.unique <- data.filter %>% group_split(start.time) %>% as.list()

  if (length(flux.unique) > 20){
  message("WARNING! Do not loop through more than 20 measurements at a time to avoid mistakes.",
          "\nYou have ", length(flux.unique), " measurements in your dataset.",
          "\nYou should split the next step into at least ", round(length(flux.unique)/20), " loops.")
  }

  return(flux.unique)
}
