#' Looping function for quick import of multiple raw gas measurement files
#'
#' Imports all raw gas measurement files contained in a folder.
#' Adapted to multiple greenhouse gas analyzers and other instruments:
#' * LI-COR: LI-6400, LI-7810, LI-7820, LI-8100, LI-8200 (smart chamber)
#' * Los Gatos Research instruments: (e.g. UGGA and m-GGA)
#' * GAIA2TECH (DMR) automated chamber ECOFlux
#' * Picarro: G2508
#' * Gasmet: DX4015
#' @md
#'
#' @param path character string; a folder path containing all files to be imported.
#'             Ideally, to avoid any errors, the folder should only contain the
#'             files to be imported.
#' @param instrument character string; specifies which instrument was used to
#'                   generate the files contained in the folder path. Chose one
#'                   of the following: "DX4015", "LGR", "G2508", "GAIA", "LI-6400",
#'                   "LI-7810", "LI-7820", "LI-8100", or "LI-8200". For more
#'                   information about an instrument, see the section "See also"
#'                   below.
#' @param date.format date format; the date format used in the raw data file.
#'                    Chose one of the following: "dmy", "ymd", or "mdy".
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @returns a data frame saved as RData in a newly created folder, RData, into
#'          the working directory.
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For most instruments, the date is
#' one of the columns in the raw data file:
#' \itemize{
#' \item DX4015: column Date
#' \item G2508: column DATE
#' \item GAIA: column Titles:
#' \item LGR: column Time
#' \item LI-7810: column DATE
#' \item LI-7820: column DATE
#' \item LI-8100: column Date
#' }
#' For the instrument LI-6400, the date is found in one of the first lines in
#' a format containing abbreviations, for example "Thr Aug 6 2020", which would
#' be the date format "mdy". For the instrument LI-8200, the date is found under
#' one of the measurement, next to "Date":.
#'
#' @include GoFluxYourself-package.R
#' @include DX4015_import.R
#' @include G2508_import.R
#' @include GAIA_import.R
#' @include LGR_import.R
#' @include LI6400_import.R
#' @include LI7810_import.R
#' @include LI7820_import.R
#' @include LI8100_import.R
#' @include LI8200_import.R
#'
#' @seealso Use the wraper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso Import functions for individual instruments:
#'          \code{\link[GoFluxYourself]{DX4015_import}},
#'          \code{\link[GoFluxYourself]{G2508_import}},
#'          \code{\link[GoFluxYourself]{GAIA_import}},
#'          \code{\link[GoFluxYourself]{LGR_import}},
#'          \code{\link[GoFluxYourself]{LI6400_import}},
#'          \code{\link[GoFluxYourself]{LI7810_import}},
#'          \code{\link[GoFluxYourself]{LI7820_import}},
#'          \code{\link[GoFluxYourself]{LI8100_import}},
#'          \code{\link[GoFluxYourself]{LI8200_import}}
#'
#' @examples
#' # Examples on how to use it with all instruments.
#' # The default time zone "UTC" is used in all cases.
#'
#' # with the Gasmet instrument DX4015
#' import2RData(path = "inst/extdata/DX4015", instrument = "DX4015", date.format = "ymd")
#'
#' # with the Picarro instrument G2508
#' import2RData(path = "inst/extdata/G2508", instrument = "G2508", date.format = "ymd")
#'
#' # with the automated chamber ECOFlux (GAIA2TECH)
#' import2RData(path = "inst/extdata/GAIA", instrument = "GAIA", date.format = "ymd")
#'
#' # with Los Gatos Research instruments (e.g. UGGA or m-GGA)
#' import2RData(path = "inst/extdata/LGR", instrument = "LGR", date.format = "dmy")
#'
#' # with LI-COR instruments
#' import2RData(path = "inst/extdata/LI6400", instrument = "LI-6400", date.format = "mdy")
#' import2RData(path = "inst/extdata/LI7810", instrument = "LI-7810", date.format = "ymd")
#' import2RData(path = "inst/extdata/LI7820", instrument = "LI-7820", date.format = "ymd")
#' import2RData(path = "inst/extdata/LI8100", instrument = "LI-8100", date.format = "ymd")
#'
#' # with the LI-COR smart chamber (LI-8200)
#' # with this instrument, "keep_all" is not a valid argument.
#' import2RData(path = "inst/extdata/LI8200", instrument = "LI-8200", date.format = "ymd")
#'
#' @export
#'
import2RData <- function(path, instrument, date.format, timezone = "UTC"){

  # Check arguments ####
  if(missing(path)) stop("'path' is required")
  if(!is.null(path) & !is.character(path)) stop("'path' must be a character string")
  if(missing(instrument)) stop("'instrument' is required")
  if(length(instrument) != 1) stop("'instrument' must be of length 1")
  if(!any(grepl(paste("\\<", instrument, "\\>", sep = ""),
                c("DX4015", "LGR", "G2508", "GAIA", "LI-6400", "LI-7810", "LI-7820", "LI-8100", "LI-8200")))){
    stop("'instrument' must be of class character and one of the following: 'DX4015', 'LGR', 'G2508', 'GAIA', 'LI-6400', 'LI-7810', 'LI-7820', 'LI-8100', 'LI-8200'")}
  if(!missing(date.format)){
    if(length(date.format) != 1) stop("'date.format' must be of length 1")
    if(!any(grepl(paste("\\<", date.format, "\\>", sep = ""), c("ymd", "dmy", "mdy")))){
      stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}}
  if (!is.character(timezone)) stop("'timezone' must be of class character")

  # FUNCTION STARTS ####

  # Progress bar options
  pboptions(char = "=")

  # Catch errors and messages from import function to print after progress bar
  errs <- character(0)
  msgs <- character(0)

  # DX4015 ####
  if(instrument == "DX4015"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.TXT", recursive = T,
                            full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        DX4015_import(inputfile = file_list[i],
                      date.format = date.format,
                      timezone = timezone,
                      save = TRUE),

        error = function(e){
          errs <<- c(errs, message(
            paste("Error occurred in file", file_list[[i]],":\n"), e))
        },
        message = function(m){
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
    })
  }

  # G2508 ####
  if(instrument == "G2508"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.dat", recursive = T,
                            full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        G2508_import(inputfile = file_list[i],
                     date.format = date.format,
                     timezone = timezone,
                     save = TRUE),

        error = function(e){
          errs <<- c(errs, message(
            paste("Error occurred in file", file_list[[i]],":\n"), e))
        },
        message = function(m){
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
    })
  }

  # GAIA ####
  if(instrument == "GAIA"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.csv", full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        GAIA_import(inputfile = file_list[i],
                    date.format = date.format,
                    timezone = timezone,
                    save = TRUE),

        error = function(e){
          errs <<- c(errs, message(
            paste("Error occurred in file", file_list[[i]],":\n"), e))
        },
        message = function(m){
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
    })
  }

  # Los Gatos Research ####
  if(instrument == "LGR"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.txt", full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        LGR_import(inputfile = file_list[i],
                   date.format = date.format,
                   timezone = timezone,
                   save = TRUE),

        error = function(e){
          errs <<- c(errs, message(
            paste("Error occurred in file", file_list[[i]],":\n"), e))
        },
        message = function(m){
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
    })
  }

  # LI-6400 ####
  if(instrument == "LI-6400"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.txt", full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        LI6400_import(inputfile = file_list[i],
                      date.format = date.format,
                      timezone = timezone,
                      save = TRUE),

        error = function(e){
          errs <<- c(errs, message(
            paste("Error occurred in file", file_list[[i]],":\n"), e))
        },
        message = function(m){
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
    })
  }

  # LI-7810 ####
  if(instrument == "LI-7810"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.data", full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        LI7810_import(inputfile = file_list[i],
                      date.format = date.format,
                      timezone = timezone,
                      save = TRUE),

        error = function(e){
          errs <<- c(errs, message(
            paste("Error occurred in file", file_list[[i]],":\n"), e))
        },
        message = function(m){
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
    })
  }

  # LI-7820 ####
  if(instrument == "LI-7820"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.data", full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        LI7820_import(inputfile = file_list[i],
                      date.format = date.format,
                      timezone = timezone,
                      save = TRUE),

        error = function(e){
          errs <<- c(errs, message(
            paste("Error occurred in file", file_list[[i]],":\n"), e))
        },
        message = function(m){
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
    })
  }

  # LI-8100 ####
  if(instrument == "LI-8100"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.81x", full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        LI8100_import(inputfile = file_list[i],
                      date.format = date.format,
                      timezone = timezone,
                      save = TRUE),

        error = function(e){
          errs <<- c(errs, message(
            paste("Error occurred in file", file_list[[i]],":\n"), e))
        },
        message = function(m){
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
    })
  }

  # LI-8200 Smart Chamber ####
  if(instrument == "LI-8200"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.json", full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        LI8200_import(inputfile = file_list[i], save = TRUE),

        error = function(e){
          errs <<- c(errs, message(
            paste("Error occurred in file", file_list[[i]],":\n"), e))
        },
        message = function(m){
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
    })
  }
  # Print errors and messages after progress bar
  errs <- trimws(errs); for (e in errs) warning(e, call. = F)
  msgs <- trimws(msgs); for (m in msgs) message(m)
}
