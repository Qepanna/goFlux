#' Import function for the eosMX 12-Channel Multiplexer
#'
#' Imports single raw data files from the Eosense 12-Channel Autochamber Multiplexer
#'
#' @param inputfile character string; the name of a file with the extension .log
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param save logical; if \code{save = TRUE}, saves the file as an .RData file
#'             in a RData folder in the current working directory. If
#'             \code{save = FALSE}, returns the file in the Console, or load in
#'             the Environment if assigned to an object.
#'
#' @returns A data frame containing raw data from the Eosense 12-Channel
#' Autochamber Multiplexer FRMonitor Files. Acts as auxiliary data sheet
#' recording chamber port, status, closures, openings, temperature, pressure,
#' and readings from 5 AUX sensors.
#'
#' @include goFlux-package.R
#'
#' @details
#' Note that this function was designed for the following units in the raw files:
#' \itemize{
#'   \item Epoch (unix) time for \code{date.format}
#'   \item kPa for atmospheric pressure
#'   \item Voltage for temperature
#'   \item Voltage for other AUX sensors}
#' If your eosMX instrument uses different units, either convert the units after
#' import, change the settings on your instrument, or contact the maintainer of
#' this package for support.
#'
#' @seealso Use the wrapper function \code{\link[goFlux]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso See also, import functions for other instruments:
#'          \code{\link[goFlux]{import.DX4015}},
#'          \code{\link[goFlux]{import.EGM5}},
#'          \code{\link[goFlux]{import.G2201i}},
#'          \code{\link[goFlux]{import.G2508}},
#'          \code{\link[goFlux]{import.G4301}},
#'          \code{\link[goFlux]{import.GAIA}},
#'          \code{\link[goFlux]{import.GasmetPD}},
#'          \code{\link[goFlux]{import.GT5000}},
#'          \code{\link[goFlux]{import.HT8850}},
#'          \code{\link[goFlux]{import.LI6400}},
#'          \code{\link[goFlux]{import.LI7810}},
#'          \code{\link[goFlux]{import.LI7820}},
#'          \code{\link[goFlux]{import.LI8100}},
#'          \code{\link[goFlux]{import.LI8150}},
#'          \code{\link[goFlux]{import.LI8200}},
#'          \code{\link[goFlux]{import.LI8250}},
#'          \code{\link[goFlux]{import.N2OM1}},
#'          \code{\link[goFlux]{import.N2Oi2}},
#'          \code{\link[goFlux]{import.skyline}},
#'          \code{\link[goFlux]{import.uCH4}},
#'          \code{\link[goFlux]{import.uN2O}},
#'          \code{\link[goFlux]{import.UGGA}}
#'
#' @seealso See \code{\link[base]{timezones}} for a description of the underlying
#'          timezone attribute.
#'
#' @examples
#' # Load file from downloaded package
#' file.path <- system.file("extdata", "eosMX12/eosMX12.log", package = "goFlux")
#'
#' # Import FRMonitor file as auxfile
#' imp.eosMX12 <- import.eosMX12(inputfile = file.path)
#'
#' # Match the multiplexer file with gas data using the function autoID
#' data(imp.G2508)
#' autoID.eosMX12 <- autoID(imp.G2508, imp.eosMX12)
#' @export

import.eosMX12 <- function(inputfile, timezone = "UTC", save = FALSE){

  # Check arguments
  if (missing(inputfile)) stop("'inputfile' is required")
  if (!is.character(inputfile)) stop("'inputfile' must be of class character")
  if (!is.character(timezone)) stop("'timezone' must be of class character")
  if (save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")

  # Assign NULL to variables without binding
  POSIX.time <- import.error <- Epoch_Time <- cham_num <- chamID <- Pcham <-
    cham_status <- DATE <- cham.close <- cham.open <- port_num <- Obs <- NULL

  # Conversion factor for Tcham_volt to Celsius
  Tvolt_convert <- function(temp_volt){
    1 / (
      0.0000000876656 * log((5.0 / (temp_volt / 2150)) - 2150)^3 +
        0.000234126 * log((5.0 / (temp_volt / 2150)) - 2150) +
        0.001129138
    ) - 273.15
  }

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data file
  try.import <- tryCatch(
    {read.table(inputfile)},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    # Column names
    Cham_colnames <- c("port_num", "valve_status", "cham_status",
                       "Sensor1", "Sensor2", "Sensor3", "Sensor4", "Sensor5",
                       "Tcham_volt", "Pcham")
    reps <- (ncol(try.import)-1)/length(Cham_colnames)

    # Load data file
    data.raw <- try.import %>%

      # Replace column names
      `colnames<-`(c(
        "Epoch_Time", outer(Cham_colnames, sprintf("_cham%02d", 1:reps), paste0))) %>%
      # Convert Epoch Time to POSIXct
      mutate(POSIX.time = as.POSIXct(Epoch_Time, tz = timezone), .after = Epoch_Time) %>%
      # Add a column for DATE alone
      mutate(DATE = substr(POSIX.time, 0, 10), .after = POSIX.time) %>%

      # Pivot longer
      tidyr::pivot_longer(
        cols = matches(paste0("^(", paste(Cham_colnames, collapse="|"), ")_cham\\d{2}$")),
        names_to = c(".value", "cham_num"),
        names_pattern = "^(.*)_cham(\\d{2})$"
      ) %>% mutate(cham_num = paste0("cham", cham_num)) %>%
      # Filter out Chamber status == -1 (keep only information about active chambers)
      filter(cham_status != -1)

    # Create metadata
    meta_all <- data.raw %>%
      # Create chamID
      arrange(Epoch_Time) %>%
      mutate(Obs = rleid(cham_num)) %>%
      mutate(chamID = paste(cham_num, Obs, sep = "_")) %>%
      # Create cham.close and cham.open
      summarise(.by = c(chamID, Obs),
                time_min = first(POSIX.time),
                time_max = last(POSIX.time))

    meta_active <- data.raw %>%
      # Create chamID
      arrange(Epoch_Time) %>%
      mutate(Obs = data.table::rleid(cham_num)) %>%
      mutate(chamID = paste(cham_num, Obs, sep = "_")) %>%
      filter(cham_status == 1) %>%
      # Create cham.close and cham.open
      summarise(.by = c(chamID, Obs),
                cham.close = first(POSIX.time),
                cham.open = last(POSIX.time))

    meta <- full_join(meta_all, meta_active, by = c("chamID", "Obs"))

    # Create a 1Hz frequency time table from metadata
    time_filter.ls <- list()
    for (i in 1:nrow(meta)) {
      time_filter.ls[[i]] <- cbind.data.frame(
        chamID = meta$chamID[[i]],
        POSIX.time = seq(from = meta$time_min[[i]],
                         to = meta$time_max[[i]],
                         by = 'sec'))
    }
    time_filter <- purrr::map_df(time_filter.ls, ~as.data.frame(.x)) %>%
      # Add meta
      full_join(meta, by = "chamID")

    # Combine data.raw and time_filter
    data.filter <- right_join(time_filter, data.raw, by = "POSIX.time",
                              relationship = "many-to-many") %>%
      # Filter out duplicated rows
      filter(stringr::str_detect(chamID, stringr::fixed(cham_num))) %>%
      # Select and reorder columns
      select(POSIX.time, DATE, chamID, cham.close, cham.open, port_num:Pcham) %>%
      # Remove chamID if cham.close = NA (all inactive chambers)
      mutate(chamID = if_else(is.na(cham.close), NA, chamID)) %>%

      # Convert Tcham_volt to Celcius
      mutate(Tcham = Tvolt_convert(Tcham_volt)) %>% select(-Tcham_volt)

    # Save cleaned data file
    if(save == TRUE){
      # Create RData folder in working directory
      RData_folder <- paste(getwd(), "RData", sep = "/")
      if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

      # Create output file: change extension to .RData, and
      # add instrument name and "imp" for import to file name
      file.name <- gsub(".*/", "", sub("\\.log", "", inputfile))
      outputfile <- paste("eosMX12_", file.name, "_imp.RData", sep = "")

      save(data.filter, file = paste(RData_folder, outputfile, sep = "/"))

      message(inputfile.name, " saved as ", outputfile,
              " in RData folder, in working directory\n", sep = "")
    }

    if(save == FALSE){
      return(data.filter)
    }
  }
}
