#' Looping function for quick import of multiple raw gas measurement files
#'
#' Imports all raw gas measurement files contained in a folder.
#' Adapted to multiple greenhouse gas analyzers:
#' * LI-COR: LI-6400, LI-7810, LI-7820, LI-8100, LI-8200 (smart chamber)
#' * Los Gatos Research instruments: (e.g. UGGA and m-GGA)
#' * GAIA2TECH (DMR) automated chamber ECOFlux
#' * Picarro: G2508
#' @md
#'
#' @param path Character string. A folder path containing all files to be imported.
#'             Ideally, the folder should only contain the files to be imported.
#' @param instrument Character string. Specify which instrument was used to generate
#'                   the files contained in the folder path. Chose one of the
#'                   following: "LGR", "G2508", "GAIA", "LI-6400", "LI-7810",
#'                   "LI-7820", "LI-8100", or "LI-8200".
#' @param date.format Date format. Chose one of the following: "dmy", "ymd", or "mdy".
#' @param timezone a time zone in which to import the data to POSIXct format.
#'                 Default is "UTC". Note about time zone: I recommend using
#'                 the time zone "UTC" to avoid any issue related to summer
#'                 time and winter time changes.
#' @returns a data frame saved as Rdata in a newly created folder, Rdata, into
#'          your working directory.
#'
#' @include GoFluxYourself-package.R
#' @include G2508_import.R
#' @include GAIA_import.R
#' @include LGR_import.R
#' @include LI6400_import.R
#' @include LI7810_import.R
#' @include LI7820_import.R
#' @include LI8100_import.R
#' @include LI8200_import.R
#'
#' @seealso [G2508_import()]
#' @seealso [GAIA_import()]
#' @seealso [LGR_import()]
#' @seealso [LI6400_import()]
#' @seealso [LI7810_import()]
#' @seealso [LI7820_import()]
#' @seealso [LI8100_import()]
#' @seealso [LI8200_import()]
#'
#' @examples
#' # Examples on how to use it with all instruments.
#' # The default time zone "UTC" is used in all cases.
#'
#' # with Los Gatos Research instruments (e.g. UGGA or m-GGA)
#' import2Rdata(path = "inst/extdata/LGR", instrument = "LGR", date.format = "dmy")
#'
#' # with the Picarro instrument G2508
#' import2Rdata(path = "inst/extdata/G2508", instrument = "G2508", date.format = "ymd")
#'
#' # with the automated chamber ECOFlux (GAIA2TECH)
#' import2Rdata(path = "inst/extdata/GAIA", instrument = "GAIA", date.format = "ymd")
#'
#' # with LI-COR instruments
#' import2Rdata(path = "inst/extdata/LI6400", instrument = "LI-6400", date.format = "ymd")
#' import2Rdata(path = "inst/extdata/LI7810", instrument = "LI-7810", date.format = "ymd")
#' import2Rdata(path = "inst/extdata/LI7820", instrument = "LI-7820", date.format = "ymd")
#' import2Rdata(path = "inst/extdata/LI8100", instrument = "LI-8100", date.format = "ymd")
#'
#' # with the LI-COR smart chamber (LI-8200)
#' # with this instrument, date.format is not a useful parameter.
#' import2Rdata(path = "inst/extdata/LI8200", instrument = "LI-8200")
#'
#' @export
#'
import2Rdata <- function(path, instrument, date.format, timezone = "UTC"){

  # Progress bar options
  pboptions(char = "=")

  # Catch errors and messages from import function to print after progress bar
  errs <- character(0)
  msgs <- character(0)

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
