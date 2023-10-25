#' Import function for Picarro G2508 GHG analyzer
#'
#' Imports single raw gas measurement files from the Picarro G2508 with the
#' extension .dat (CO2, CH4, N2O, and H2O GHG analyzer)
#'
#' @param inputfile character string; the name of a file with the extension .dat
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
#' @returns a data frame containing raw data from Picarro G2508 GHG analyzer.
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso Use the wrapper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso Import functions for individual instruments:
#'          \code{\link[GoFluxYourself]{GAIA_import}},
#'          \code{\link[GoFluxYourself]{LGR_import}},
#'          \code{\link[GoFluxYourself]{LI6400_import}},
#'          \code{\link[GoFluxYourself]{LI7810_import}},
#'          \code{\link[GoFluxYourself]{LI7820_import}},
#'          \code{\link[GoFluxYourself]{LI8100_import}},
#'          \code{\link[GoFluxYourself]{LI8200_import}}
#' @seealso See \code{\link[base]{timezones}} for a description of the underlying
#'          timezone attribute.
#'
#' @examples
#' # Load file from downloaded package
#' file.path <- system.file("extdata", "G2508/2022/08/01/example_G2508.dat",
#'                          package = "GoFluxYourself")
#'
#' # Run function
#' G2508.data <- G2508_import(inputfile = file.path)
#'
#' @export
#'
G2508_import <- function(inputfile, date.format = "ymd",
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
  ALARM_STATUS <- H2O <- N2O_dry30s <- N2O_dry <- CH4_dry <- CavityPressure <-
    CO2_dry <- TIME <- DATE <- WarmBoxTemp <- EtalonTemp <- DasTemp <-
    CavityTemp <- Amb_P <- N2Odry_ppb <- CH4dry_ppb <- H2O_ppm <- NULL

  # Import raw data file from G2508 (.dat)
  data.raw <- read.delim(inputfile, sep = "") %>%
    # Select useful columns and standardize column names
    select(ALARM_STATUS, Amb_P, CavityPressure, CavityTemp, DasTemp, EtalonTemp,
           WarmBoxTemp, DATE, TIME, CO2dry_ppm = CO2_dry, CH4_dry,
           N2O_dry, N2O_dry30s, H2O) %>%
    # Convert column class automatically
    type.convert(as.is = TRUE) %>%
    # Convert mmol into ppm for H2O and ppm into ppb for N2O and CH4
    mutate(H2O_ppm = H2O*1000,
           N2Odry_ppb = N2O_dry*1000,
           N2Odry_30s_ppb = N2O_dry30s*1000,
           CH4dry_ppb = CH4_dry*1000) %>%
    # Remove unnecessary columns
    select(!c(CH4_dry, N2O_dry, N2O_dry30s, H2O)) %>%
    # Remove NAs and negative gas measurements, if any
    filter(N2Odry_ppb > 0) %>%
    filter(CH4dry_ppb > 0) %>%
    filter(H2O_ppm > 0)

  # Create a new column containing date and time (POSIX format)
  op <- options()
  # Include miliseconds with digits.secs = 6
  options(digits.secs=6)
  if(date.format == "dmy"){
    data.raw$POSIX.time <- as.POSIXct(
      dmy_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone),
      format = "%Y-%m-%d %H:%M:%OS")
  }
  if(date.format == "mdy"){
    data.raw$POSIX.time <- as.POSIXct(
      mdy_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone),
      format = "%Y-%m-%d %H:%M:%OS")
  }
  if(date.format == "ymd"){
    data.raw$POSIX.time <- as.POSIXct(
      ymd_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone),
      format = "%Y-%m-%d %H:%M:%OS")
  }
  options(op)

  # Save cleaned data file
  if(save == TRUE){
    # Create RData folder in working directory
    RData_folder <- paste(getwd(), "RData", sep = "/")
    if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

    # Create output file: change extension to .RData, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.dat", "", inputfile))
    outputfile <- paste("G2508_", file.name, "_imp.RData", sep = "")

    save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
