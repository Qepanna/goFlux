#' Import function for LI-COR GHG analyzer LI-8100
#'
#' Imports single raw gas measurement files from the LI-COR 8100
#' (CO2 and H2O GHG analyzer)
#'
#' @param inputfile character string; the name of a file with the extension .81x
#' @param date.format date format; the date format used in the raw data file.
#'                    Chose one of the following: "dmy", "ymd", or "mdy". Default
#'                    is "ymd", as it is the date format from the example data
#'                    file provided.
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param save logical; if save = TRUE, saves the file as RData in a RData folder
#'             in the current working directory. If save = FALSE, returns the file
#'             in the Console, or load in the Environment if assigned to an object.
#'
#' @returns a data frame containing raw data from LI-COR GHG analyzer LI-8100.
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument LI-8100, the
#' date is found in the column "Date".
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso Use the wrapper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso See also, import functions for other instruments:
#'          \code{\link[GoFluxYourself]{G2508_import}},
#'          \code{\link[GoFluxYourself]{GAIA_import}},
#'          \code{\link[GoFluxYourself]{LGR_import}},
#'          \code{\link[GoFluxYourself]{LI6400_import}},
#'          \code{\link[GoFluxYourself]{LI7810_import}},
#'          \code{\link[GoFluxYourself]{LI7820_import}},
#'          \code{\link[GoFluxYourself]{LI8200_import}}
#' @seealso See \code{\link[base]{timezones}} for a description of the underlying
#'          timezone attribute.
#'
#' @examples
#' # Load file from downloaded package
#' file.path <- system.file("extdata", "LI8100/example_LI8100.81x", package = "GoFluxYourself")
#'
#' # Run function
#' LI8100.data <- LI8100_import(inputfile = file.path)
#'
#' @export
#'
LI8100_import <- function(inputfile, date.format = "ymd",
                          timezone = "UTC", save = FALSE) {

  # Check arguments
  if (missing(inputfile)) stop("'inputfile' is required")
  if (!is.character(inputfile)) stop("'inputfile' must be of class character")
  if (length(date.format) != 1) stop("'date.format' must be of length 1")
  if (!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}
  if (!is.character(timezone)) stop("'timezone' must be of class character")
  if (save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")

  # Assign NULL to variables without binding
  Type <- Etime <- Tcham <- Pressure <- H2O <- Cdry <- V1 <- V2 <- V3 <-
    V4 <- H2O_mmol <- DATE_TIME <- Obs <- . <- cham.close <- cham.open <-
    deadband <- start.time <- obs.start <- POSIX.time <- plotID <-
    Date <- CO2dry_ppm <- POSIX.warning <- NULL

  # Find how many rows need to be skipped
  skip.rows <- as.numeric(which(read.delim(inputfile) == "Type"))[1]

  # Import raw data file from LI8100 (.81x)
  data.raw <- read.delim(inputfile, skip = skip.rows) %>%
    # Keep only Type == 1, as everything else is metadata
    filter(Type == "1") %>%
    # Select useful columns and standardize column names
    select(Etime, DATE_TIME = Date, Tcham, Pcham = Pressure,
           H2O_mmol = H2O, CO2dry_ppm = Cdry, V1, V2, V3, V4) %>%
    # Convert column class automatically
    type.convert(as.is = TRUE) %>%
    # Remove NAs and negative gas measurements, if any
    filter(CO2dry_ppm > 0) %>%
    filter(H2O_mmol > 0) %>%
    # Convert mmol into ppm for H2O
    mutate(H2O_ppm = H2O_mmol*1000) %>% select(!c(H2O_mmol)) %>%
    # Detect new observations
    arrange(DATE_TIME) %>%
    mutate(Obs = ifelse(is.na(Etime - lag(Etime)), 0, Etime - lag(Etime))) %>%
    mutate(Obs = rleid(cumsum(Obs < 0)))

  # Create a new column containing date and time (POSIX format)
  tryCatch(
    {if(date.format == "dmy"){
      try.POSIX <- as.POSIXct(dmy_hms(data.raw$DATE_TIME, tz = timezone))
    } else if(date.format == "mdy"){
      try.POSIX <- as.POSIXct(mdy_hms(data.raw$DATE_TIME, tz = timezone))
    } else if(date.format == "ymd"){
      try.POSIX <- as.POSIXct(ymd_hms(data.raw$DATE_TIME, tz = timezone))
    }}, warning = function(w) {POSIX.warning <<- "date.format.error"}
  )

  if(isTRUE(POSIX.warning == "date.format.error")){
    stop(paste("An error occured while converting DATE and TIME into POSIX.time.",
               "Verify that 'date.format' corresponds to the column 'DATE' in",
               "the raw data file. Here is a sample:", data.raw$DATE[1]))
  } else data.raw$POSIX.time <- try.POSIX

  # Import metadata from LI8100 (.81x)
  meta <- read.delim(inputfile, header = F) %>% select(c(1:2)) %>%
    filter(V1 == "Obs#:" | V1 == "Label:" | V1 == "Area:" | V1 == "Vcham:" |
             V1 == "Offset:" | V1 == "Dead Band:")

  if (nrow(meta)/6 == ceiling(nrow(meta)/6)) {
    metadata <- meta %>% reframe(
      Obs = as.numeric(.[which(.[,1] == "Obs#:"),2]),
      plotID = .[which(.[,1] == "Label:"),2],
      Area = as.numeric(.[which(.[,1] == "Area:"),2]),
      Vcham = as.numeric(.[which(.[,1] == "Vcham:"),2]),
      offset = as.numeric(.[which(.[,1] == "Offset:"),2]),
      deadband = as.numeric(ms(meta[which(meta[,1] == "Dead Band:"),2]), units = "secs"))
  } else {
    metadata <- meta %>% reframe(
      Obs = as.numeric(.[which(.[,1] == "Obs#:"),2]),
      plotID = .[which(.[,1] == "Label:"),2],
      Area = as.numeric(.[which(.[,1] == "Area:"),2]),
      Vcham = as.numeric(.[which(.[,1] == "Vcham:"),2]),
      offset = as.numeric(.[which(.[,1] == "Offset:"),2]),
      deadband = c(as.numeric(ms(meta[which(meta[,1] == "Dead Band:"),2]), units = "secs"),
                   last(as.numeric(ms(meta[which(meta[,1] == "Dead Band:"),2]), units = "secs"))))
  }

  # Add metadata to data.raw
  data.raw <- data.raw %>%
    left_join(metadata, by = "Obs") %>% group_by(Obs) %>%
    # Calculate cham.close, cham.open, flag and correct negative values of Etime
    mutate(cham.close = POSIX.time[which(Etime == 0)],
           cham.open = last(POSIX.time),
           obs.start = min(POSIX.time)) %>%
    ungroup() %>%
    mutate(DATE = substr(POSIX.time, 0, 10),
           chamID = paste(plotID, Obs, sep = "_"),
           start.time = cham.close + deadband,
           Etime = as.numeric(POSIX.time - start.time, units = "secs"),
           flag = if_else(between(POSIX.time, start.time, cham.open), 1, 0))

  # Save cleaned data file
  if(save == TRUE){
    # Create RData folder in working directory
    RData_folder <- paste(getwd(), "RData", sep = "/")
    if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

    # Create output file: change extension to .RData, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.81x", "", inputfile))
    outputfile <- paste("LI8100_", file.name, "_imp.RData", sep = "")

    save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
