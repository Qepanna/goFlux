#' Manual editing of observation window of flux measurements
#'
#' Crop measurements from the start, from the end, or adjust the deadband.
#' Individual parameters can be provided for each measurement, in an auxiliary
#' file, or the same parameters can be applied to all measurements.
#'
#' @param dataframe a dataframe containing gas measurements, output from
#'                  import or ID functions (click.peak2 or manID). Must contain
#'                  the columns \code{UniqueID}, \code{start.time},
#'                  \code{end.time} and \code{Etime}.
#' @param auxfile data.frame; auxiliary data frame containing the columns
#'                \code{UniqueID}, \code{crop_start}, \code{crop_end},
#'                \code{deadband} and/or \code{max.obs.length}.
#' @param crop_start numerical; number of observations to remove at the start
#'                   of the measurement. To provide a unique value per
#'                   UniqueID, the column \code{crop_start} must be present
#'                   in the auxfile and the argument must be set to
#'                   \code{crop_start = "aux"}.
#' @param crop_end numerical; number of observations to remove at the
#'                 end of the measurement. To provide a unique value per
#'                 UniqueID, the column \code{crop_end} must be present
#'                 in the auxfile and the argument must be set to
#'                 \code{crop_end = "aux"}.
#' @param deadband numerical; define a deadband at the start of measurements
#'                 (seconds). To provide a unique value per UniqueID, the column
#'                 \code{deadband} must be present in the auxfile and the
#'                 argument must be set to \code{deadband = "aux"}.
#' @param max.obs.length numerical; maximum length of measurement (seconds) to
#'                       keep after \code{cham.close}. To provide a unique value
#'                       per UniqueID, the column \code{max.obs.length} must be
#'                       present in the auxfile and the argument must be set to
#'                       \code{max.obs.length = "aux"}.
#' @param cham.close character; if \code{cham.close = "start.time"} (default), the
#'                   original \code{start.time} before cropping will be saved into
#'                   a new column \code{cham.close}. Set \code{cham.close = NULL}
#'                   to drop the original \code{start.time}.
#' @param cham.open character; if \code{cham.open = "end.time"} (default), the
#'                  original \code{end.time} before cropping will be saved into
#'                  a new column \code{cham.open}. Set \code{cham.open = NULL}
#'                  to drop the original \code{end.time}.
#'
#' @include goFlux-package.R
#'
#' @returns a data frame, with corrected Etime and flag, based on newly defined
#'          start.time, end.time and deadband.
#'
#' @details
#' Note that \code{crop_start} and \code{deadband} "crop" the start of the
#' measurements in the same way.
#'
#' Note that if \code{max.obs.length} is defined, \code{end.time} is calculated
#' based on \code{cham.close} (original \code{start.time} in \code{dataframe}, if
#' \code{cham.close = "start.time"}) and \code{max.obs.length}, regardless of other
#' defined values in the function. If \code{cham.close = NULL}, \code{end.time}
#' is calculated based on \code{start.time} and \code{max.obs.length}.
#'
#' If \code{cham.close} and \code{cham.open} are originally present in
#' \code{dataframe}, set arguments \code{cham.close} and \code{cham.open}
#' to \code{NULL}.
#'
#' Note that the arguments \code{crop_start} and \code{crop_end} accept negative
#' values and can be used to include more data points before and after the
#' previously defined \code{start.time} and \code{end.time}.
#'
#' @examples
#' # Load example data
#' data(manID.G2201i)
#'
#' # Create an auxfile to define different crop_start and crop_end per UniqueID
#' auxfile <- cbind.data.frame(UniqueID = c("meas1", "meas2"),
#'                             crop_start = c(2,10),
#'                             crop_end = c(5,0))
#'
#' # The arguments "crop_start" and "crop_end" must be set to "aux" to use
#' # unique values from the auxfile
#' crop.G2201i <- crop.meas(dataframe = manID.G2201i, auxfile = auxfile,
#'                          crop_start = "aux", crop_end = "aux")
#'
#' # Alternatively, cropping can be applied equally across all measurements
#' crop.G2201i <- crop.meas(dataframe = manID.G2201i, auxfile = auxfile,
#'                          crop_start = 5, crop_end = 5)
#' @export

crop.meas <- function(dataframe, auxfile = NULL,
                      crop_start = 0, crop_end = 0,
                      deadband = 0, max.obs.length = NULL,
                      cham.close = "start.time", cham.open = "end.time") {

  # Check arguments ####
  if(!is.numeric(crop_start) & crop_start != "aux"){
    stop("'crop_start' must be of class numeric or defined to 'aux'")}
  if(!is.numeric(crop_end) & crop_end != "aux"){
    stop("'crop_end' must be of class numeric or defined to 'aux'")}

  if(!is.numeric(deadband) & deadband != "aux"){
    stop("'deadband' must be of class numeric or defined to 'aux'")}
  if(is.numeric(deadband) & deadband < 0){
    stop("'deadband' must be larger than 0")}

  if(!is.null(max.obs.length)){
    if(!is.numeric(max.obs.length) & max.obs.length != "aux"){
      stop("'max.obs.length' must be of class numeric or defined to 'aux'")}
    if(is.numeric(max.obs.length) & max.obs.length < 0){
      stop("'max.obs.length' must be larger than 0")}}

  if(!is.null(cham.close) & cham.close != "start.time"){
    stop("'cham.close' must be set to 'start.time' or NULL")}
  if(!is.null(cham.open) & cham.open != "end.time"){
    stop("'cham.open' must be set to 'end.time' or NULL")}

  ## Check dataframe ####
  if(missing(dataframe)) stop("'dataframe' is required")
  if(!is.data.frame(dataframe)) stop("'dataframe' must be of class data.frame")

  if(!any(grepl(paste("\\<UniqueID\\>", sep = ""), names(dataframe)))){
    stop("'dataframe' must contain the column 'UniqueID'")}
  if(!any(grepl(paste("\\<start.time\\>", sep = ""), names(dataframe)))){
    stop("'dataframe' must contain the column 'start.time'")}
  if(!any(grepl(paste("\\<end.time\\>", sep = ""), names(dataframe)))){
    stop("'dataframe' must contain the column 'end.time'")}

  ## Check auxfile ####
  if(!is.null(auxfile)){
    if(!is.data.frame(auxfile)) stop("'auxfile' must be of class data.frame")

    if(!any(grepl(paste("\\<UniqueID\\>", sep = ""), names(auxfile)))){
      stop("'auxfile' must contain the column 'UniqueID'")}

    if(crop_start == "aux"){
      if(!any(grepl(paste("\\<crop_start\\>", sep = ""), names(auxfile)))){
        stop("'auxfile' must contain the column 'crop_start' when crop_start = 'aux'")}
      if(!is.numeric(auxfile$crop_start)){
        stop("'crop_start' in 'auxfile' must be of class numeric")}
    }
    if(crop_end == "aux"){
      if(!any(grepl(paste("\\<crop_end\\>", sep = ""), names(auxfile)))){
        stop("'auxfile' must contain the column 'crop_end' when crop_end = 'aux'")}
      if(!is.numeric(auxfile$crop_end)){
        stop("'crop_end' in 'auxfile' must be of class numeric")}
    }
    if(deadband == "aux"){
      if(!any(grepl(paste("\\<deadband\\>", sep = ""), names(auxfile)))){
        stop("'auxfile' must contain the column 'deadband' when deadband = 'aux'")}
      if(!is.numeric(auxfile$deadband)){
        stop("'deadband' in 'auxfile' must be of class numeric")}
      if(is.numeric(auxfile$deadband) & min(na.omit(auxfile$deadband)) < 0){
        stop("'deadband' in 'auxfile' must be larger than 0")}
    }
    if(!is.null(max.obs.length)){
      if(max.obs.length == "aux"){
        if(!any(grepl(paste("\\<max.obs.length\\>", sep = ""), names(auxfile)))){
          stop("'auxfile' must contain the column 'max.obs.length' when max.obs.length = 'aux'")}
        if(!is.numeric(auxfile$max.obs.length)){
          stop("'max.obs.length' in 'auxfile' must be of class numeric")}
        if(is.numeric(auxfile$max.obs.length) & min(na.omit(auxfile$max.obs.length)) < 0){
          stop("'max.obs.length' in 'auxfile' must be larger than 0")}
      }
    }
  }

  # Assign NULL to variables without binding ####
  UniqueID <- start.time <- end.time <- unique_obs.length <- POSIX.time <- NULL

  # FUNCTION STARTS ####

  # Define a new metadata per UniqueID
  meta <- dataframe %>%
    select(UniqueID, start.time, end.time,
           any_of(c("cham.close", "cham.open"))) %>%
    distinct()

  # Save original start.time and end.time as cham.close and cham.open
  # This replaces original cham.close and cham.open unless set to NULL
  if(any(cham.close == "start.time")) meta <- mutate(meta, cham.close = start.time)
  if(any(cham.open == "end.time")) meta <- mutate(meta, cham.open = end.time)

  # Add a column deadband
  if(deadband == "aux"){
    unique_deadband <- select(auxfile, UniqueID, deadband)
    meta <- left_join(meta, unique_deadband, by = "UniqueID")
  } else {meta <- mutate(meta, deadband = deadband)}

  # Add a column crop_start
  if(crop_start == "aux"){
    unique_crop_start <- select(auxfile, UniqueID, crop_start)
    meta <- left_join(meta, unique_crop_start, by = "UniqueID")
  } else {meta <- mutate(meta, crop_start = crop_start)}

  # Add a column crop_end
  if(crop_end == "aux"){
    unique_crop_end <- select(auxfile, UniqueID, crop_end)
    meta <- left_join(meta, unique_crop_end, by = "UniqueID")
  } else {meta <- mutate(meta, crop_end = crop_end)}

  # Add a column for max.obs.length
  if(!is.null(max.obs.length)){
    if(max.obs.length == "aux"){
      unique_max.obs.length <- select(auxfile, UniqueID, max.obs.length = max.obs.length)
      meta <- left_join(meta, unique_obs.length, by = "UniqueID")
    } else {meta <- mutate(meta, max.obs.length = max.obs.length)}
  }

  # Correct start.time based on crop_start and deadband
  meta <- mutate(meta, start.time = start.time + crop_start + deadband)

  # Correct end.time based on crop_end
  meta <- mutate(meta, end.time = end.time - crop_end)

  # Correct end.time based on max.obs.length
  if(!is.null(max.obs.length)){
    if(any(grepl("cham.close", names(meta)))){
      meta <- mutate(meta, end.time = cham.close + max.obs.length)
    } else {
      meta <- mutate(meta, end.time = start.time + max.obs.length)
    }
  }

  # Correct Etime, flag and obs.length based on newly defined metadata
  crop_meas <- select(dataframe, -any_of(
    c("flag", "Etime", "obs.length", names(meta)[2:ncol(meta)]))) %>%
    left_join(meta, by = "UniqueID") %>%
    mutate(Etime = as.numeric(POSIX.time - start.time, units = "secs")) %>%
    mutate(flag = if_else(between(POSIX.time, start.time, end.time), 1, 0)) %>%
    mutate(obs.length = as.numeric(end.time - start.time, units = "secs"))

  return(crop_meas)

}
