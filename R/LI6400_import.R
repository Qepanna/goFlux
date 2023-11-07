#' Import function for LI-COR GHG analyzer LI-6400
#'
#' Imports single raw gas measurement files from the LI-COR 6400
#' (\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}} GHG analyzer)
#'
#' @param inputfile character string; the name of a file with the extension .txt
#' @param date.format date format; the date format used in the raw data file.
#'                    Chose one of the following: "dmy", "ymd", or "mdy". Default
#'                    is "mdy", as it is the date format from the example data
#'                    file provided.
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param save logical; if save = TRUE, saves the file as RData in a RData folder
#'             in the current working directory. If save = FALSE, returns the file
#'             in the Console, or load in the Environment if assigned to an object.
#' @param keep_all logical; if \code{keep_all = TRUE}, keep all columns from raw
#'                 file. The default is \code{keep_all = FALSE}, and columns that
#'                 are not necessary for gas flux calculation are removed.
#'
#' @returns a data frame containing raw data from LI-COR GHG analyzer LI-6400.
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument LI-6400, the
#' date is found in one of the first lines in a format containing abbreviations,
#' for example "Thr Aug 6 2020", which would be the date format "mdy".
#'
#' Note that this function was designed for the following default units:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#'   \item mmol/mol for \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item kPa for pressure
#'   \item \ifelse{html}{\out{cm<sup>3</sup>}}{\eqn{cm^3}{ASCII}} for volumes
#'   \item Celsius for temperature}
#' If your instrument uses different units, either convert the units after import,
#' change the settings on your instrument, or contact the maintainer of this
#' package for support.
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso Use the wrapper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso Import functions for individual instruments:
#'          \code{\link[GoFluxYourself]{DX4015_import}},
#'          \code{\link[GoFluxYourself]{G2508_import}},
#'          \code{\link[GoFluxYourself]{GAIA_import}},
#'          \code{\link[GoFluxYourself]{LGR_import}},
#'          \code{\link[GoFluxYourself]{LI7810_import}},
#'          \code{\link[GoFluxYourself]{LI7820_import}},
#'          \code{\link[GoFluxYourself]{LI8100_import}},
#'          \code{\link[GoFluxYourself]{LI8200_import}}
#' @seealso See \code{\link[base]{timezones}} for a description of the underlying
#'          timezone attribute.
#'
#' @examples
#' # Load file from downloaded package
#' file.path <- system.file("extdata", "LI6400/example_LI6400.txt", package = "GoFluxYourself")
#'
#' # Run function
#' LI6400.data <- LI6400_import(inputfile = file.path)
#'
#' @export
#'
LI6400_import <- function(inputfile, date.format = "mdy", timezone = "UTC",
                          save = FALSE, keep_all = FALSE){

  # Check arguments
  if (missing(inputfile)) stop("'inputfile' is required")
  if (!is.character(inputfile)) stop("'inputfile' must be of class character")
  if (length(date.format) != 1) stop("'date.format' must be of length 1")
  if (!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}
  if (!is.character(timezone)) stop("'timezone' must be of class character")
  if (save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")
  if (keep_all != TRUE & keep_all != FALSE) stop("'keep_all' must be TRUE or FALSE")

  # Assign NULL to variables without binding
  cham.close <- cham.open <- POSIX.time <- chamID <- DATE <- TIME <- H2O_mmol <-
    Etime <- CO2dry_ppm <- Meas.type <- plotID <- H2OS <- Cdry <- Press <-
    Tair <- Mode <- ETime <- HHMMSS <- Meas.type..NEE.ER. <- Plot. <- Obs <-
    V4 <- V1 <- start.time <- met.date.warning <- flag <- offset <- Vcham <-
    Area <- Pcham <- Tcham <- H2O_ppm <- TCham <- NULL

  # Find how many rows need to be skipped
  skip.rows <- tryapply(seq(1:30), function(i) {
    if (read.delim(inputfile, header = F, skip = i)[1,1] == "$STARTOFDATA$") {
      return(i) }
  }) %>% unlist %>% as.numeric() +1

  # Import metadata from LI6400 (.txt)
  metadata <- read.delim(inputfile, header = F, comment.char = "<") %>%
    filter(V1 == "Const=" | V4 == "")

  # Extract date from metadata
  tryCatch(
    {if (date.format == "dmy") {
      try.met.date <- dmy(substr(metadata[2,1], 5, nchar(metadata[2,1])-9), tz = timezone)
    } else if (date.format == "mdy") {
      try.met.date <- mdy(substr(metadata[2,1], 5, nchar(metadata[2,1])-9), tz = timezone)
    } else if (date.format == "ymd") {
      try.met.date <- ymd(substr(metadata[2,1], 5, nchar(metadata[2,1])-9), tz = timezone)
    }}, warning = function(w) {met.date.warning <<- "date.format.error"}
  )

  if(isTRUE(met.date.warning == "date.format.error")){
    stop(paste("An error occured while converting DATE and TIME into POSIX.time.",
               "Verify that 'date.format' corresponds in",
               "the raw data file. Here is a sample:",
               substr(metadata[2,1], 5, nchar(metadata[2,1])-9)[1]))
  } else met.date <- try.met.date

  if(keep_all == TRUE){
    # Import raw data file from LI6400 (.txt)
    data.raw <- read.delim(inputfile, skip = skip.rows) %>%
      # Standardize column names
      rename(plotID = Plot., Meas.type = Meas.type..NEE.ER., TIME = HHMMSS,
             Etime = ETime, Tcham = Tair, Pcham = Press, CO2dry_ppm = Cdry,
             H2O_mmol = H2OS) %>% select(!TCham) %>%
      # Create a unique chamber ID
      mutate(chamID = paste(plotID, Meas.type, sep = "_")) %>%
      # Remove comments
      filter(!CO2dry_ppm == "") %>% filter(!Obs == "Obs") %>%
      # Convert column class automatically
      type.convert(as.is = TRUE) %>%
      # plotID must be as.character
      mutate(plotID = as.character(plotID)) %>%
      # Remove Mode == 2 (indicates when a measurement ends)
      filter(!Mode == 2) %>% select(!Mode) %>%
      # As the LICOR only saves rows when you have passed all prompts after
      # pressing start, Etime below 4 seconds is not possible.
      filter(Etime > 4) %>%
      # Remove NAs and negative gas measurements, if any
      filter(CO2dry_ppm > 0) %>%
      filter(H2O_mmol > 0) %>%
      # Convert mmol into ppm for H2O
      mutate(H2O_ppm = H2O_mmol*1000) %>%
      # Create new columns containing date and time (POSIX format)
      mutate(DATE = as.character(met.date),
             POSIX.time = as.POSIXct(paste(DATE, TIME), tz = timezone)) %>%
      # Extract other useful information from metadata
      mutate(Area = as.numeric(metadata[which(metadata[,3] == "Area")[1],4]),
             Vcham = as.numeric(metadata[which(metadata[,3] == "Vtot")[1],4]),
             offset = as.numeric(metadata[which(metadata[,3] == "Offset")[1],4])) %>%
      # Convert Vcham from cm3 to L
      mutate(Vcham = Vcham/1000) %>%
      # Add time related variables (POSIX.time)
      group_by(chamID) %>%
      mutate(cham.close = min(na.omit(POSIX.time)),
             start.time = cham.close,
             chamID = paste(chamID, start.time),
             cham.open = max(na.omit(POSIX.time)),
             Etime = as.numeric(POSIX.time - start.time, units = "secs")) %>%
      ungroup() %>%
      # Add flag
      mutate(flag = 1)
  }
  # Keep only useful columns for gas flux calculation
  if(keep_all == FALSE){
    # Import raw data file from LI6400 (.txt)
    data.raw <- read.delim(inputfile, skip = skip.rows) %>%
      # Select useful columns and standardize column names
      select(Obs, plotID = Plot., Meas.type = Meas.type..NEE.ER.,
             TIME = HHMMSS, Etime = ETime, Mode,
             Tcham = Tair, Pcham = Press,
             CO2dry_ppm = Cdry, H2O_mmol = H2OS) %>%
      # Create a unique chamber ID
      mutate(chamID = paste(plotID, Meas.type, sep = "_")) %>%
      # Remove comments
      filter(!CO2dry_ppm == "") %>% filter(!Obs == "Obs") %>%
      # Convert column class automatically
      type.convert(as.is = TRUE) %>%
      # plotID must be as.character
      mutate(plotID = as.character(plotID)) %>%
      # Remove Mode == 2 (indicates when a measurement ends)
      filter(!Mode == 2) %>% select(!Mode) %>%
      # As the LICOR only saves rows when you have passed all prompts after
      # pressing start, Etime below 4 seconds is not possible.
      filter(Etime > 4) %>%
      # Remove NAs and negative gas measurements, if any
      filter(CO2dry_ppm > 0) %>%
      filter(H2O_mmol > 0) %>%
      # Convert mmol into ppm for H2O
      mutate(H2O_ppm = H2O_mmol*1000) %>%
      # Create new columns containing date and time (POSIX format)
      mutate(DATE = as.character(met.date),
             POSIX.time = as.POSIXct(paste(DATE, TIME), tz = timezone)) %>%
      # Extract other useful information from metadata
      mutate(Area = as.numeric(metadata[which(metadata[,3] == "Area")[1],4]),
             Vcham = as.numeric(metadata[which(metadata[,3] == "Vtot")[1],4]),
             offset = as.numeric(metadata[which(metadata[,3] == "Offset")[1],4])) %>%
      # Convert Vcham from cm3 to L
      mutate(Vcham = Vcham/1000) %>%
      # Add time related variables (POSIX.time)
      group_by(chamID) %>%
      mutate(cham.close = first(na.omit(POSIX.time)),
             start.time = cham.close,
             chamID = paste(chamID, start.time),
             cham.open = last(na.omit(POSIX.time)),
             Etime = as.numeric(POSIX.time - start.time, units = "secs")) %>%
      ungroup() %>%
      # Add flag
      mutate(flag = 1)
  }

  # Save cleaned data file
  if(save == TRUE){
    # Create RData folder in working directory
    RData_folder <- paste(getwd(), "RData", sep = "/")
    if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

    # Create output file: change extension to .RData, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.txt", "", inputfile))
    outputfile <- paste("LI6400_", file.name, "_imp.RData", sep = "")

    save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
