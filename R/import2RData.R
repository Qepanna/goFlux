#' Looping function for quick import of multiple raw gas measurement files
#'
#' Imports all raw gas measurement files contained in a folder.
#' Adapted to multiple greenhouse gas analyzers and other instruments:
#' * \strong{LI-COR}: LI-6400, LI-7810, LI-7820, LI-8100, LI-8200 (smart chamber)
#' * \strong{Los Gatos Research (LGR)}: ultra-portable GGA (GLA132 series) and
#'                                      micro ultra-portable GGA (GLA131 series)
#' * \strong{GAIA2TECH (DMR)} automated chamber ECOFlux
#' * \strong{Picarro}: G2508 and G4301
#' * \strong{Gasmet}: DX4015
#' * \strong{PP-Systems}: EGM-5
#' @md
#'
#' @param path character string; a folder path containing all files to be imported.
#'             Ideally, to avoid any errors, the folder should only contain the
#'             files to be imported.
#' @param instrument character string; specifies which instrument was used to
#'                   generate the files contained in the folder path. Choose one
#'                   of the following: "DX4015", "EGM5", "G2508", "G4301" "GAIA",
#'                   "LI-6400", "LI-7810", "LI-7820", "LI-8100", "LI-8200" or
#'                   "LGR". For more information about an instrument, see the
#'                   section "See Also" below.
#' @param date.format character string; specifies the date format found in the
#'                    raw data file. Choose one of the following: "dmy", "ymd",
#'                    or "mdy".
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param keep_all logical; if \code{keep_all = TRUE}, keep all columns from raw
#'                 file. The default is \code{keep_all = FALSE}, and columns that
#'                 are not necessary for gas flux calculation are removed.
#' @param prec numerical vector; the precision of the instrument for each gas.
#'             Look at the examples below, or the help for each import function
#'             of each instrument, to know what values to use.
#'
#' @returns A data frame saved as RData in a newly created folder, RData, into
#'          the working directory.
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For most instruments, the date is
#' one of the columns in the raw data file:
#' \itemize{
#' \item DX4015: column Date
#' \item EGM5: column Date
#' \item G2508: column DATE
#' \item G4301: column DATE
#' \item GAIA: column Titles:
#' \item LGR: column Time
#' \item LI-7810: column DATE
#' \item LI-7820: column DATE
#' \item LI-8100: column Date
#' }
#' For the instrument LI-6400, the date is found in one of the first lines in
#' a format containing abbreviations, for example "Thr Aug 6 2020", which would
#' be the date format "mdy". For the instrument LI-8200, the date is found under
#' one of the measurements, next to "Date":.
#'
#' @include GoFluxYourself-package.R
#' @include DX4015_import.R
#' @include EGM5_import.R
#' @include G2508_import.R
#' @include G4301_import.R
#' @include GAIA_import.R
#' @include LGR_import.R
#' @include LI6400_import.R
#' @include LI7810_import.R
#' @include LI7820_import.R
#' @include LI8100_import.R
#' @include LI8200_import.R
#'
#' @seealso Use the wrapper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso Import functions for individual instruments:
#'          \code{\link[GoFluxYourself]{DX4015_import}},
#'          \code{\link[GoFluxYourself]{EGM5_import}},
#'          \code{\link[GoFluxYourself]{G2508_import}},
#'          \code{\link[GoFluxYourself]{G4301_import}},
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
#' file.path <- system.file("extdata/DX4015", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "DX4015",
#'              date.format = "ymd", keep_all = FALSE,
#'              prec = c(1.6, 23, 13, 2, 23, 33))
#'
#' # with the PP-Systems EGM-5
#' file.path <- system.file("extdata/EGM5", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "EGM5",
#'              date.format = "dmy", keep_all = FALSE,
#'              prec = c(3, 1, 500))
#'
#' # with the Picarro instrument G2508
#' file.path <- system.file("extdata/G2508", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "G2508",
#'              date.format = "ymd", keep_all = FALSE,
#'              prec = c(0.24, 0.3, 5, 0.16, 500))
#'
#' # with the Picarro instrument G4301
#' file.path <- system.file("extdata/G4301", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "G4301",
#'              date.format = "ymd", keep_all = FALSE,
#'              prec = c(0.025, 0.1, 10))
#'
#' # with the automated chamber ECOFlux (GAIA2TECH)
#' # with this instrument, "keep_all" is not a valid argument.
#' file.path <- system.file("extdata/GAIA", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "GAIA",
#'              date.format = "ymd", prec = c(3.5, 0.6, 0.4, 45, 45))
#'
#' # with Los Gatos Research UGGA (GLA132 series)
#' file.path <- system.file("extdata/LGR", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "LGR",
#'              date.format = "dmy", keep_all = FALSE,
#'              prec = c(0.2, 1.4, 50))
#'
#' # with Los Gatos Research m-GGA (GLA131 series)
#' file.path <- system.file("extdata/LGR", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "LGR",
#'              date.format = "dmy", keep_all = FALSE,
#'              prec = c(0.35, 0.9, 200))
#'
#' # with the LI-COR LI-6400 gas analyzer
#' file.path <- system.file("extdata/LI6400", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "LI-6400",
#'              date.format = "mdy", keep_all = FALSE,
#'              prec = c(0.15, 20))
#'
#' # with the LI-COR LI-7810 gas analyzer
#' file.path <- system.file("extdata/LI7810", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "LI-7810",
#'              date.format = "ymd", keep_all = FALSE,
#'              prec = c(3.5, 0.6, 45))
#'
#' # with the LI-COR LI-7820 gas analyzer
#' file.path <- system.file("extdata/LI7820", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "LI-7820",
#'              date.format = "ymd", keep_all = FALSE,
#'              prec = c(0.4, 45))
#'
#' # with the LI-COR LI-8100 gas analyzer
#' file.path <- system.file("extdata/LI8100", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "LI-8100",
#'              date.format = "ymd", keep_all = FALSE,
#'              prec = c(1, 10))
#'
#' # with the LI-COR smart chamber (LI-8200)
#' # with this instrument, "keep_all" and "prec" are not valid arguments.
#' file.path <- system.file("extdata/LI8200", package = "GoFluxYourself")
#' import2RData(path = file.path, instrument = "LI-8200",
#'              date.format = "ymd")
#'
#' @export
#'
import2RData <- function(path, instrument, date.format, timezone = "UTC",
                         keep_all, prec){

  # Check arguments ####
  if(missing(path)) stop("'path' is required")
  if(!is.null(path) & !is.character(path)) stop("'path' must be a character string")
  if(missing(instrument)) stop("'instrument' is required")
  if(length(instrument) != 1) stop("'instrument' must be of length 1")
  if(!any(grepl(paste("\\<", instrument, "\\>", sep = ""),
                c("DX4015", "LGR", "G4301", "G2508", "GAIA", "LI-6400", "EGM5",
                  "LI-7810", "LI-7820", "LI-8100", "LI-8200")))){
    stop("'instrument' must be of class character and one of the following: 'DX4015', 'EGM5', 'G2508', 'G4301', 'GAIA', 'LI-6400', 'LI-7810', 'LI-7820', 'LI-8100', 'LI-8200', 'LGR'")}
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
                      save = TRUE,
                      keep_all = keep_all,
                      prec = prec),

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

  # EGM5 ####
  if(instrument == "EGM5"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.TXT", recursive = T,
                            full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        EGM5_import(inputfile = file_list[i],
                      date.format = date.format,
                      timezone = timezone,
                      save = TRUE,
                      keep_all = keep_all,
                      prec = prec),

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
                     save = TRUE,
                     keep_all = keep_all,
                     prec = prec),

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

  # G4301 ####
  if(instrument == "G4301"){

    # List all the files contained in the specified path
    file_list <- list.files(path = path, pattern = "\\.dat", recursive = T,
                            full.names = TRUE)

    # Loop through files in "file_list" and apply import functions
    pblapply(seq_along(file_list), function(i) {

      withCallingHandlers(

        G4301_import(inputfile = file_list[i],
                     date.format = date.format,
                     timezone = timezone,
                     save = TRUE,
                     keep_all = keep_all,
                     prec = prec),

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
                    save = TRUE,
                    prec = prec),

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
                   save = TRUE,
                   keep_all = keep_all,
                   prec = prec),

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
                      save = TRUE,
                      keep_all = keep_all,
                      prec = prec),

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
                      save = TRUE,
                      keep_all = keep_all,
                      prec = prec),

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
                      save = TRUE,
                      keep_all = keep_all,
                      prec = prec),

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
                      save = TRUE,
                      keep_all = keep_all,
                      prec = prec),

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
