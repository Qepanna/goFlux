#' Define measurements' window for manual identification
#'
#' Split gas data into one observation window per gas measurement
#' and create a list of data frames (one data frame per UniqueID).
#'
#' @param inputfile data.frame; output from import or align functions.
#' @param auxfile data.frame; auxiliary data frame containing the columns
#'                \code{start.time} and \code{UniqueID}. \code{start.time} must
#'                contain a date and be in POSIXct format. The time zone must be
#'                the same as the POSIX.time in \code{inputfile}. The default
#'                time zone for the import functions is "UTC". A data frame from
#'                the Smart Chamber (LI-8200) can be used as an auxiliary file.
#'                In that case, \code{chamID} will be used instead of \code{UniqueID},
#'                if \code{UniqueID} cannot be found.
#' @param obs.length numerical; chamber closure time (seconds). Default is NULL.
#'                   If \code{obs.length} is not provided, a column \code{obs.length}
#'                   should be contained in \code{auxfile} or \code{inputfile}.
#'                   Alternatively, \code{obs.length} will be calculated from
#'                   \code{start.time} and \code{cham.open} or \code{end.time}
#'                   if found in \code{auxfile} or \code{inputfile}.
#' @param shoulder numerical; time before and after measurement in observation
#'                 window (seconds). Default is 120 seconds.
#' @param gastype deprecated
#'
#' @returns a list of data frames, split by UniqueID, merging \code{inputfile},
#'          and \code{auxfile}. Additionally, adds some time (shoulder) before
#'          and after the chamber closure time to help identify the best window
#'          of measurement with the function \code{\link[goFlux]{click.peak2}}.
#'
#' @include goFlux-package.R
#'
#' @seealso After defining the observation window with the function \code{obs.win},
#'          Use the function \code{\link[goFlux]{click.peak2}}.
#'
#' @seealso For an automatic identification of gas measurements, see the function
#' \code{\link[goFlux]{autoID}}.
#'
#' @examples
#' # How to use in multiple situations:
#' library(dplyr)
#'
#' ## with a LGR instrument and an auxiliary file (.txt)
#' aux.path <- system.file("extdata", "aux_UGGA/aux_UGGA.txt", package = "goFlux")
#' auxfile <- read.delim(aux.path) %>%
#'   mutate(start.time = as.POSIXct(start.time, tz = "UTC"))
#' data(imp.UGGA)
#' ow.UGGA <- obs.win(inputfile = imp.UGGA, auxfile = auxfile,
#'                    obs.length = 180, shoulder = 60)
#'
#' ## with a LI-COR instrument and the Smart Chamber as auxiliary file
#' data(imp.LI8200)
#' data(imp.LI7820)
#' ow.LI7820 <- obs.win(inputfile = imp.LI7820, auxfile = imp.LI8200, shoulder = 60)
#'
#' ## with the LI-6400 and no auxiliary file
#' data(imp.LI6400)
#' ow.LI6400 <- obs.win(inputfile = imp.LI6400, shoulder = 0)
#'
#' @export
#'
obs.win <- function(inputfile, auxfile = NULL, gastype = deprecated(),
                    obs.length = NULL, shoulder = 120){

  # Check arguments ####
  if(missing(inputfile)) stop("'inputfile' is required")
  if(!is.null(inputfile) & !is.data.frame(inputfile)) stop("'inputfile' must be of class data.frame")
  if(!is.null(auxfile) & !is.data.frame(auxfile)) stop("'auxfile' must be of class data.frame")
  if(is.null(shoulder)) stop("'shoulder' is required") else{
    if(!is.numeric(shoulder)) stop("'shoulder' must be of class numeric") else{
      if(shoulder < 0) stop("'shoulder' cannot be a negative value")}}

  ## Check if user used the argument gastype
  if (lifecycle::is_present(gastype)) {
    # Signal the deprecation to the user
    deprecate_warn("0.2.0", "goFlux::obs.win(gastype = )")
  }

  ## UniqueID and chamID ####
  if (is.null(auxfile)) {
    if (!any(grepl("\\<UniqueID\\>", names(inputfile))) &
        !any(grepl("\\<chamID\\>", names(inputfile)))) {
      stop("'UniqueID' is required and was not found in 'inputfile'. Alternatively, provide chamID in 'inputfile'.")
    }
  } else {
    if (!any(grepl("\\<UniqueID\\>", names(auxfile))) &
        !any(grepl("\\<chamID\\>", names(auxfile)))) {
      stop("'UniqueID' is required and was not found in 'auxfile'. Alternatively, provide chamID in 'auxfile'.")
    }
  }

  ## DATE ####
  if (is.null(auxfile)) {
    if (!any(grepl("\\<UniqueID\\>", names(inputfile))) &
        any(grepl("\\<chamID\\>", names(inputfile)))) {
      if(!any(grepl("\\<DATE\\>", names(inputfile)))){
        stop("The column DATE in required in 'inputfile' to create a UniqueID from chamID.")}
    }
  } else {
    if (!any(grepl("\\<UniqueID\\>", names(auxfile))) &
        any(grepl("\\<chamID\\>", names(auxfile)))) {
      if(!any(grepl("\\<DATE\\>", names(inputfile)))){
        stop("The column DATE in required in 'inputfile' to create a UniqueID from chamID.")}
    }
  }

  ## start.time ####
  if (is.null(auxfile)) {
    if (!any(grepl("\\<start.time\\>", names(inputfile)))) {
      stop("'start.time' is required and was not found in 'inputfile'")
    }
    if (any(grepl("\\<start.time\\>", names(inputfile))) &
        !is.POSIXct(inputfile$start.time)) {
      stop("'start.time' in 'inputfile' must be of class POSIXct")
    }
    if (any(grepl("\\<start.time\\>", names(inputfile))) &
        is.POSIXct(inputfile$start.time) &
        attr(inputfile$start.time, "tzone") != attr(inputfile$POSIX.time, "tzone")) {
      stop("'start.time' in 'inputfile' must be in the same time zone as 'POSIX.time'")
    }
  } else {
    if (!any(grepl("\\<start.time\\>", names(auxfile)))) {
      stop("'start.time' is required and was not found in 'auxfile'")
    }
    if (any(grepl("\\<start.time\\>", names(auxfile))) &
        !is.POSIXct(auxfile$start.time)) {
      stop("'start.time' in 'auxfile' must be of class POSIXct")
    }
    if (any(grepl("\\<start.time\\>", names(auxfile))) &
        is.POSIXct(auxfile$start.time) &
        attr(auxfile$start.time, "tzone") != attr(inputfile$POSIX.time, "tzone")) {
      stop("'start.time' in 'auxfile' must be in the same time zone as 'POSIX.time' in 'inputfile'")
    }
  }

  ## obs.length ####
  if(!is.null(obs.length) & !is.numeric(obs.length)) {
    stop("'obs.length' must be of class numeric")}
  # if obs.length = NULL
  if(is.null(obs.length)){
    # look for it in auxfile and inputfile
    if(!any(grepl("\\<obs.length\\>", c(names(inputfile), names(auxfile))))){
      # otherwise, look for alternative arguments in auxfile and inputfile
      if(!is.null(auxfile)){
        if(length(grep(paste(c("\\<end.time\\>", "\\<cham.open\\>"), collapse = "|"),
                       c(names(inputfile), names(auxfile)))) <1){
          stop("'obs.length' missing. 'inputfile' or 'auxfile' must contain alternative arguments to calculate 'obs.length': 'start.time' and 'cham.open' or 'end.time'.")}

      } else {
        if(length(grep(paste(c("\\<end.time\\>", "\\<cham.open\\>"), collapse = "|"),
                       names(inputfile))) <1){
          stop("'obs.length' missing. 'inputfile' must contain alternative arguments to calculate 'obs.length': start.time' and 'cham.open' or 'end.time'.")}
      }
    }
  }

  ## end.time ####
  if(any(grepl("\\<end.time\\>", names(inputfile)))){
    if(!is.POSIXct(inputfile$end.time)){
      stop("'end.time' in 'inputfile' must be of class POSIXct")
    } else if(attr(inputfile$end.time, "tzone") != attr(inputfile$POSIX.time, "tzone")){
      stop("'end.time' in 'inputfile' must be in the same time zone as 'POSIX.time'")
    }
  }
  if(any(grepl("\\<end.time\\>", names(auxfile)))){
    if(!is.POSIXct(auxfile$end.time)){
      stop("'end.time' in 'auxfile' must be of class POSIXct")
    } else if(attr(auxfile$end.time, "tzone") != attr(inputfile$POSIX.time, "tzone")){
      stop("'end.time' in 'auxfile' must be in the same time zone as 'POSIX.time' in 'inputfile'")
    }
  }

  ## cham.open ####
  if(any(grepl("\\<cham.open\\>", names(inputfile)))){
    if(!is.POSIXct(inputfile$cham.open)){
      stop("'cham.open' in 'inputfile' must be of class POSIXct")
    } else if(attr(inputfile$cham.open, "tzone") != attr(inputfile$POSIX.time, "tzone")){
      stop("'cham.open' in 'inputfile' must be in the same time zone as 'POSIX.time'")
    }
  }
  if(any(grepl("\\<cham.open\\>", names(auxfile)))){
    if(!is.POSIXct(auxfile$cham.open)){
      stop("'cham.open' in 'auxfile' must be of class POSIXct")
    } else if(attr(auxfile$cham.open, "tzone") != attr(inputfile$POSIX.time, "tzone")){
      stop("'cham.open' in 'auxfile' must be in the same time zone as 'POSIX.time' in 'inputfile'")
    }
  }

  # Assign NULL to variables without binding ####
  POSIX.time <- chamID <- start.time <- UniqueID <- Etime <- flag <- DATE <-
    CO2dry_ppm <- COdry_ppb <- CH4dry_ppb <- N2Odry_ppb <- NH3dry_ppb <-
    H2O_ppm <- cham.open <- end.time <- Tcham <- Pcham <- . <- NULL

  # FUNCTION STARTS ####

  # Convert milliseconds to seconds, for compatibility between auxfile and flux data
  inputfile <- inputfile %>%
    mutate(POSIX.time = as.POSIXct(round(POSIX.time, "secs")))

  # Get start.time and UniqueID
  if (is.null(auxfile)){
    # Rename chamID to UniqueID
    if (any(grepl("\\<chamID\\>", names(inputfile)))){
      inputfile <- inputfile %>% mutate(UniqueID = paste(chamID, DATE, sep = "_")) %>%
        # Convert milliseconds to seconds, for compatibility
        mutate(start.time = as.POSIXct(round(start.time, "secs")))
    }
    aux.data <- inputfile %>% select(UniqueID, start.time) %>% distinct()
  } else {
    # Rename chamID to UniqueID
    if (any(grepl("\\<chamID\\>", names(auxfile)))){
      auxfile <- auxfile %>% mutate(UniqueID = paste(chamID, DATE, sep = "_"))
    }
    aux.data <- auxfile %>% select(UniqueID, start.time) %>% distinct()
  }

  # Rename cham.open to end.time if found in auxfile or inputflle
  if (any(grepl("\\<cham.open\\>", names(inputfile)))){
    inputfile <- inputfile %>% mutate(end.time = cham.open)}
  if (any(grepl("\\<cham.open\\>", names(auxfile)))){
    auxfile <- auxfile %>% mutate(end.time = cham.open)}

  # Get obs.length
  # If obs.length is provided
  if (!is.null(obs.length)){
    aux.data <- aux.data %>% mutate(obs.length = obs.length)
  } else {
    # Or if obs.length is NULL
    ## And it can be found in auxfile
    if (!is.null(auxfile) & any(grepl("\\<obs.length\\>", names(auxfile)))){
      aux.data <- aux.data %>% left_join(
        auxfile %>% select(UniqueID, obs.length) %>% distinct(), by = "UniqueID")
      ## Or there is an auxfile with alternative arguments to calculate obs.length
    } else if (!is.null(auxfile) & any(grepl("\\<end.time\\>", names(auxfile)))){
      aux.data <- aux.data %>% left_join(
        auxfile %>% select(UniqueID, end.time) %>%
          distinct(), by = "UniqueID") %>% group_by(UniqueID) %>%
        mutate(obs.length = as.numeric(end.time - start.time, units = "secs")) %>%
        ungroup() %>% select(-end.time)
      ## Or it can be found in inputfile
    } else if (any(grepl("\\<obs.length\\>", names(inputfile)))){
      aux.data <- aux.data %>% left_join(
        inputfile %>% select(UniqueID, obs.length) %>% distinct(), by = "UniqueID")
      ## Or there are alternative arguments to calculate obs.length in inputfile
    } else if (any(grepl("\\<end.time\\>", names(inputfile)))){
      aux.data <- aux.data %>% left_join(
        inputfile %>% select(UniqueID, end.time) %>%
          distinct(), by = "UniqueID") %>% group_by(UniqueID) %>%
        mutate(obs.length = as.numeric(end.time - start.time, units = "secs")) %>%
        ungroup() %>% select(-end.time)
    }
  }

  # Define windows of time ranges to keep
  time_range <- aux.data %>% group_by(UniqueID) %>%
    reframe(time_min = start.time - shoulder,
            time_max = start.time + obs.length + shoulder,
            obs.length = obs.length,
            start.time = start.time)

  time_filter.ls <- list()
  for (i in 1:nrow(time_range)) {
    time_filter.ls[[i]] <- cbind.data.frame(
      UniqueID = time_range$UniqueID[[i]],
      start.time = time_range$start.time[[i]],
      obs.length = time_range$obs.length[[i]],
      POSIX.time = seq(from = time_range$time_min[i],
                       to = time_range$time_max[i],
                       by = 'sec'))
  }
  time_filter <- map_df(time_filter.ls, ~as.data.frame(.x)) %>% distinct() %>%
    # Convert milliseconds to seconds, for compatibility between auxfile and flux data
    mutate(start.time = as.POSIXct(round(start.time, "secs")),
           POSIX.time = as.POSIXct(round(POSIX.time, "secs")))

  # Remove from inputfile columns that are also present in time_filter,
  # except POSIX.time, before combining them
  names.i_f <- intersect(names(inputfile), names(time_filter)) %>% .[!. == "POSIX.time"]
  if (length(names.i_f) > 0){
    for (i in 1:length(names.i_f)){
      if (any(grepl(paste("\\<", names.i_f[[i]], "\\>", sep = ""), names(inputfile)))){
        inputfile <- inputfile %>% select(-names.i_f[[i]])}}}

  # Add time_filter to inputfile and filter POSIXct
  data.filter <- inputfile %>%
    right_join(time_filter, relationship = "many-to-many", by = "POSIX.time") %>%
    drop_na(DATE)

  # Add the rest of the auxiliary data from the auxfile to the output file
  if (!is.null(auxfile)){
    # 1. Remove from auxfile columns that are also present in data.filter,
    #    except POSIX.time and UniqueID, before combining them
    names.a_d <- intersect(names(auxfile), names(data.filter)) %>%
      .[!. == "POSIX.time" & !. == "UniqueID"]
    if (length(names.a_d) > 0){
      for (i in 1:length(names.a_d)){
        if (any(grepl(paste("\\<", names.a_d[[i]], "\\>", sep = ""), names(auxfile)))){
          auxfile <- auxfile %>% select(-names.a_d[[i]])}}
    }
    # 2. Remove Etime and flag, if present
    if (any(grepl("\\<Etime\\>", names(auxfile)))){
      auxfile <- auxfile %>% select(-Etime)}
    if (any(grepl("\\<flag\\>", names(auxfile)))){
      auxfile <- auxfile %>% select(-flag)}

    # 3. Merge by UniqueID and POSIX.time:
    if (any(grepl("\\<POSIX.time\\>", names(auxfile)))){
      data.filter <- data.filter %>%
        left_join(auxfile, by = c("UniqueID", "POSIX.time"))
    } else {
      data.filter <- data.filter %>%
        full_join(auxfile, by = "UniqueID")
    }
    # 4. Replace NAs
    replace <- setdiff(names(data.filter),
                       c("POSIX.time", "TIME", "Etime", "UniqueID",
                         names(data.filter)[grepl("ppm|ppb", names(data.filter))]))
    data.filter <- data.filter %>% group_by(UniqueID) %>%
      mutate_at(vars(replace), ~ if_else(is.na(.x), first(na.omit(.x)), .x))
  }

  # Split data.filter into a list of data frames unique per measurement
  flux.unique <- data.filter %>% group_by(UniqueID, .add = TRUE) %>%
    group_split() %>% as.list()

  if (length(flux.unique) > 20){
    message("WARNING! Do not loop through more than 20 measurements at a time to avoid mistakes.",
            "\nYou have ", length(flux.unique), " measurements in your dataset.",
            "\nYou should split the next step into at least ", round(length(flux.unique)/20), " loops.")
  }

  return(flux.unique)
}
