#' Import function for LI-COR GHG analyzer LI-7820
#'
#' Imports single raw gas measurement files from the LI-COR 7820
#' (N2O and H2O GHG analyzer)
#'
#' @param inputfile character string; the name of a file with the extension .data
#' @param date.format character string; chose one of the following: "dmy", "ymd",
#'                    or "mdy". Default is "ymd", as it is the date format from
#'                    the example data file provided.
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param save logical; if save = TRUE, saves the file as RData in a RData folder
#'             in the current working directory. If save = FALSE, returns the file
#'             in the Console, or load in the Environment if assigned to an object.
#' @returns a data frame containing raw data from LI-COR GHG analyzer LI-7820.
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso Use the wrapper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso Import functions for individual instruments:
#'          \code{\link[GoFluxYourself]{G2508_import}},
#'          \code{\link[GoFluxYourself]{GAIA_import}},
#'          \code{\link[GoFluxYourself]{LGR_import}},
#'          \code{\link[GoFluxYourself]{LI6400_import}},
#'          \code{\link[GoFluxYourself]{LI7810_import}},
#'          \code{\link[GoFluxYourself]{LI8100_import}},
#'          \code{\link[GoFluxYourself]{LI8200_import}}
#' @seealso See \code{\link[base]{timezones}} for a description of the underlying
#'          timezone attribute.
#'
#' @examples
#' # Load file from downloaded package
#' file.path <- system.file("extdata", "LI7820/example_LI7820.data", package = "GoFluxYourself")
#'
#' # Run function
#' LI7820.data <- LI7820_import(inputfile = file.path)
#'
#' @export
#'
LI7820_import <- function(inputfile, date.format = "ymd",
                          timezone = "UTC", save = FALSE){

  # Check arguments
  if (missing(inputfile)) stop("'inputfile' is required")
  if (!is.character(inputfile)) stop("'inputfile' must be of class character")
  if (length(date.format) != 1) stop("'date.format' must be of length 1")
  if (!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}
  if (!is.character(timezone)) stop("'timezone' must be of class character")
  if (save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")

  # Assign NULL to variables without binding
  H2O_ppm <- H2O <- N2O <- TIME <- DATE <- DATAH <- N2Odry_ppb <- NULL

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
    # Remove NAs and negative gas measurements, if any
    filter(N2Odry_ppb > 0) %>%
    filter(H2O_ppm > 0)

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
    # Create RData folder in working directory
    RData_folder <- paste(getwd(), "RData", sep = "/")
    if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

    # Create output file: change extension to .RData, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.data", "", inputfile))
    outputfile <- paste("LI7820_", file.name, "_imp.RData", sep = "")

    save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
