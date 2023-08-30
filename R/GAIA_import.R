#' Import function for GAIA synced with multiple chambers and instruments
#'
#' Imports single raw gas measurement files from the automated chamber
#' ECOFlux (GAIA2TECH) with the extension .csv
#' (LI-7810: CO2, CH4 and H2O / LI7820: N2O and H2O)
#'
#' @param inputfile the name of a file with the extension .csv
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
#' file.path <- system.file("extdata", "GAIA/example_GAIA.csv", package = "GoFluxYourself")
#'
#' # 1. If you want to import the data and work with it directly
#' GAIA.data <- GAIA_import(inputfile = file.path)
#'
#' # 2. Or if you want to import the data and save it as Rdata
#' GAIA_import(inputfile = file.path, save = TRUE)
#' @export
#'
GAIA_import <- function(inputfile, date.format = "ymd",
                        timezone = "UTC", save = FALSE){

  # Assign NULL to variables without binding
  POSIX.time <- Chamber <- DATE_TIME <- XT3C05_H2O <- XT3C04_N2O <- . <-
    XT2C06_H2O <- XT2C04_CH4 <- XT2C05_CO2 <- SEQUENCE <- Titles. <- NULL

  # Import raw data file from GAIA (.csv)
  data.raw <- read.delim(inputfile, skip = 1, colClasses = "character") %>%
    # Remove first row containing units
    filter(!Titles. == 'Units:') %>%
    # Modify useful column names
    setNames(gsub("COM5A0", "CH", names(.))) %>%
    setNames(gsub("1C07_Soil.Temperature", "_Tsoil", names(.))) %>%
    setNames(gsub("2C07_Chamber.Temperature", "_Tcham", names(.))) %>%
    setNames(gsub("1C08_Soil.Moisture", "_SWC", names(.))) %>%
    setNames(gsub("3C07_Sunlight", "_PAR", names(.))) %>%
    setNames(gsub("0C06_OperatingStatus", "_Op.stat", names(.))) %>%
    mutate(SEQUENCE = substr(SEQUENCE, 9, 9)) %>%
    dplyr::rename(DATE_TIME = Titles., Chamber = SEQUENCE) %>%
    # Gas measurements from GHG analyzers need to be renamed manually
    dplyr::rename(
      # LI-7810: CO2dry_ppm, CH4dry_ppb, H2O_ppm_LI7810
      CO2dry_ppm = XT2C05_CO2,
      CH4dry_ppb = XT2C04_CH4,
      H2O_ppm_LI7810 = XT2C06_H2O,
      # LI-7820: N2Odry_ppb, H2O_ppm_LI7820
      N2Odry_ppb = XT3C04_N2O,
      H2O_ppm_LI7820 = XT3C05_H2O) %>%
    # Select only useful columns
    select(contains(c("DATE_TIME", "Chamber", "Tsoil", "Tcham", "SWC", "PAR",
                      "Op.stat", "ppm", "ppb")))

  # Create a new column containing date and time (POSIX format)
  op <- options()
  options(digits.secs=6)
  if(date.format == "dmy"){
    data.raw$POSIX.time <- as.POSIXct(dmy_hms(data.raw$DATE_TIME, tz = timezone),
                                      format = "%Y-%m-%d %H:%M:%OS")
  }
  if(date.format == "mdy"){
    data.raw$POSIX.time <- as.POSIXct(mdy_hms(data.raw$DATE_TIME, tz = timezone),
                                      format = "%Y-%m-%d %H:%M:%OS")
  }
  if(date.format == "ymd"){
    data.raw$POSIX.time <- as.POSIXct(ymd_hms(data.raw$DATE_TIME, tz = timezone),
                                      format = "%Y-%m-%d %H:%M:%OS")
  }
  options(op)

  # Group together all columns containing information on
  # Operating Status from each chamber
  Op.stat <- data.raw %>% select(DATE_TIME, contains("Op.stat")) %>%
    pivot_longer(contains("Op.stat"), values_to = "Op.stat", names_to = "Chamber") %>%
    mutate(Chamber = substr(Chamber, 3, 3))

  # Merge data and op.stat information
  data.raw <- data.raw %>% left_join(Op.stat, by = c("DATE_TIME", "Chamber")) %>%
    select(!contains("_Op.stat")) %>%
    # Add a column for DATE
    mutate(DATE = substr(POSIX.time, 0, 10))

  # Save cleaned data file
  if(save == TRUE){
    save(data.raw, file = paste(sub("\\.csv", "\\.Rdata", inputfile), sep = "/"))
    message("'data.raw' was saved as ", getwd(), "/",
            paste(sub("\\.csv", "\\.Rdata", inputfile), sep = "/"), sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
