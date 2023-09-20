#' Import function for Picarro G2508 GHG analyzer
#'
#' Imports single raw gas measurement files from the Picarro G2508 with the
#' extension .dat (CO2, CH4, N2O, and H2O GHG analyzer)
#'
#' @param inputfile the name of a file with the extension .dat
#' @param date.format Date format. Chose one of the following: "dmy", "ymd", or "mdy".
#'                    Default is "ymd" as it is the date format from the example
#'                    data file provided.
#' @param timezone a time zone in which to import the data to POSIXct format.
#'                 Default is "UTC". Note about time zone: I recommend using
#'                 the time zone "UTC" to avoid any issue related to summer
#'                 time and winter time changes.
#' @param save logical. If save = TRUE, save the file as Rdata in a Rdata folder
#'             in the current working directory. If save = FALSE, return the file
#'             in the Console, or load in the Environment if assigned to an object.
#' @returns a data frame
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso [import2Rdata()]
#' @seealso [GAIA_import()]
#' @seealso [LGR_import()]
#' @seealso [LI6400_import()]
#' @seealso [LI7810_import()]
#' @seealso [LI7820_import()]
#' @seealso [LI8100_import()]
#' @seealso [LI8200_import()]
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

  # Assign NULL to variables without binding
  ALARM_STATUS <- H2O <- N2O_dry30s <- N2O_dry <- CH4_dry <- CavityPressure <-
    CO2_dry <- TIME <- DATE <- WarmBoxTemp <- EtalonTemp <- DasTemp <-
    CavityTemp <- Amb_P <- NULL

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
    select(!c(CH4_dry, N2O_dry, N2O_dry30s, H2O))

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
    # Create Rdata folder in working directory
    Rdata_folder <- paste(getwd(), "Rdata", sep = "/")
    if(dir.exists(Rdata_folder) == FALSE){dir.create(Rdata_folder)}

    # Create output file: change extension to .RData, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.dat", "", inputfile))
    outputfile <- paste("G2508_", file.name, "_imp.RData", sep = "")

    save(data.raw, file = paste(Rdata_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in Rdata folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
