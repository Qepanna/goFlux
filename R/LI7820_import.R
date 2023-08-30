#' Import function for LI-COR GHG analyzer LI-7820
#'
#' Imports single raw gas measurement files from the LI-COR 7820
#' (N2O and H2O GHG analyzer)
#'
#' @param inputfile the name of a file with the extension .data
#' @param date.format Date format. Chose one of the following: "dmy", "ymd", or "mdy".
#'                    Default is "ymd" as it is the date format from the example
#'                    data file provided.
#' @param timezone a time zone in which to import the data to POSIXct format.
#'                 Default is "UTC". Note about time zone: I recommend using
#'                 the time zone "UTC" to avoid any issue related to summer
#'                 time and winter time changes.
#' @param save logical. If save = TRUE, save the file as Rdata in the current
#'             working directory. If save = FALSE, return the file in the Console,
#'             or load in the Environment if assigned to an object.
#' @returns a data frame
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' # Examples on how to use:
#' file.path <- system.file("extdata", "LI7820/example_LI7820.data", package = "GoFluxYourself")
#'
#' # 1. If you want to import the data and work with it directly
#' LI7820.data <- LI7820_import(inputfile = file.path)
#'
#' # 2. Or if you want to import the data and save it as Rdata
#' LI7820_import(inputfile = file.path, save = TRUE)
#' @export
#'
LI7820_import <- function(inputfile, date.format = "ymd",
                          timezone = "UTC", save = FALSE){

  # Assign NULL to variables without binding
  H2O_ppm <- H2O <- N2O <- TIME <- DATE <- DATAH <- NULL

  # Find how many rows need to be skipped
  skip.rows <- as.numeric(which(read.delim(inputfile, nrows = 20) == "DATAH",
                                arr.ind = TRUE)[1])

  # Import raw data file from LI7820 (.data)
  data.raw <- read.delim(inputfile, skip = skip.rows) %>%
    # Remove the row "DATAU"
    filter(!DATAH == 'DATAU') %>%
    # Keep only usefyl columns and standardize column names
    select(DATE, TIME, N2Odry_ppb = N2O, H2O_ppm = H2O) %>%
    # Convert column class automatically
    type.convert(as.is = TRUE) %>%
    # Remove NAs
    filter(H2O_ppm != "NaN")

  # Create a new column containing date and time (POSIX format)
  if(date.format == "dmy"){
    data.raw$POSIX.time <- as.POSIXct(
      dmy_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone))
  }
  if(date.format == "mdy"){
    data.raw$POSIX.time <- as.POSIXct(
      mdy_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone))
  }
  if(date.format == "ymd"){
    data.raw$POSIX.time <- as.POSIXct(
      ymd_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone))
  }

  # Save cleaned data file
  if(save == TRUE){
    save(data.raw, file = paste(sub("\\.data", "\\.Rdata", inputfile), sep = "/"))
    message("'data.raw' was saved as ", getwd(), "/",
            paste(sub("\\.data", "\\.Rdata", inputfile), sep = "/"), sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
