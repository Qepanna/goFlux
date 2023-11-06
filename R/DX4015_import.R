#' Import function for the Gasmet DX4015 portable FTIR gas analyzer for humid
#' conditions
#'
#' Imports single raw gas measurement files from the Gasmet DX4015 gas analyzer
#' (CO, \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}},
#' \ifelse{html}{\out{NH<sub>3</sub>}}{\eqn{NH[3]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}) with the extension .TXT
#'
#' @param inputfile character string; the name of a file with the extension .TXT
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
#' @returns a data frame containing raw data from the Gasmet DX4015 gas analyzer.
#'
#' @include GoFluxYourself-package.R
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument DX4015, the date
#' is found in the column "Date".
#'
#' @seealso Use the wrapper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso Import functions for individual instruments:
#'          \code{\link[GoFluxYourself]{G2508_import}},
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
#' # Examples on how to use:
#' file.path <- system.file("extdata/DX4015", "DX4015.TXT", package = "GoFluxYourself")
#'
#' DX4015_imp <- DX4015_import(inputfile = file.path)
#'
#' @export
#'
DX4015_import <- function(inputfile, date.format = "ymd",
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
  POSIX.warning <- Line <- SpectrumFile <- H2O_frac <- Time <- Date <-
    Ambient.pressure <- Status <- LibraryFile <- NH3dry_ppb <- N2Odry_ppb <-
    H2O_ppm <- CH4dry_ppb <- COdry_ppb <- CO2dry_ppm <- `H2O_vol-%` <-
    COdry_ppm <- NH3dry_ppm <- N2Odry_ppm <- CH4dry_ppm <- . <- NULL

  # Load data file
  data.raw <- read.delim(inputfile, colClasses = "character") %>%
    # Remove useless data
    select(!c(Line, SpectrumFile, LibraryFile, Status, Ambient.pressure,
              contains(c("Residual", "temperature", "IFG")))) %>%
    # Modify column names for DATE and TIME
    rename(DATE = Date, TIME = Time) %>%
    # Modify column names for gas units
    rename(H2O_unit = which(names(.) == "Water.vapor.H2O") +1) %>%
    rename(CO2_unit = which(names(.) == "Carbon.dioxide.CO2") +1) %>%
    rename(CH4_unit = which(names(.) == "Methane.CH4") +1) %>%
    rename(N2O_unit = which(names(.) == "Nitrous.oxide.N2O") +1) %>%
    rename(CO_unit = which(names(.) == "Carbon.monoxide.CO") +1) %>%
    rename(NH3_unit = which(names(.) == "Ammonia.NH3") +1) %>%
    # Modify column names for gas compensation (wet or dry fraction)
    rename(H2O_frac = which(names(.) == "Water.vapor.H2O") +2) %>%
    select(!H2O_frac) %>%
    rename(CO2_frac = which(names(.) == "Carbon.dioxide.CO2") +2) %>%
    rename(CH4_frac = which(names(.) == "Methane.CH4") +2) %>%
    rename(N2O_frac = which(names(.) == "Nitrous.oxide.N2O") +2) %>%
    rename(CO_frac = which(names(.) == "Carbon.monoxide.CO") +2) %>%
    rename(NH3_frac = which(names(.) == "Ammonia.NH3") +2) %>%
    # In "Compensation", wet means that the gases are compensated for water
    # vapor, meaning that "wet" stands for the dry fraction.
    mutate_all(str_replace_all, "wet", "dry")

  # Column names for gases
  H2O_col_name <- paste("H2O_", unique(na.omit(data.raw$H2O_unit)), sep = "")
  CO2_col_name <- paste("CO2", unique(na.omit(data.raw$CO2_frac)), "_",
                        unique(na.omit(data.raw$CO2_unit)), sep = "")
  CO_col_name <- paste("CO", unique(na.omit(data.raw$CO_frac)), "_",
                        unique(na.omit(data.raw$CO_unit)), sep = "")
  CH4_col_name <- paste("CH4", unique(na.omit(data.raw$CH4_frac)), "_",
                        unique(na.omit(data.raw$CH4_unit)), sep = "")
  N2O_col_name <- paste("N2O", unique(na.omit(data.raw$N2O_frac)), "_",
                        unique(na.omit(data.raw$N2O_unit)), sep = "")
  NH3_col_name <- paste("NH3", unique(na.omit(data.raw$NH3_frac)), "_",
                        unique(na.omit(data.raw$NH3_unit)), sep = "")

  # Rename gas columns
  data.raw <- data.raw %>%
    setNames(gsub("Water.vapor.H2O", H2O_col_name, names(.))) %>%
    setNames(gsub("Carbon.dioxide.CO2", CO2_col_name, names(.))) %>%
    setNames(gsub("Carbon.monoxide.CO", CO_col_name, names(.))) %>%
    setNames(gsub("Methane.CH4", CH4_col_name, names(.))) %>%
    setNames(gsub("Nitrous.oxide.N2O", N2O_col_name, names(.))) %>%
    setNames(gsub("Ammonia.NH3", NH3_col_name, names(.))) %>%
    # Remove unit and compensation columns
    select(!contains(c("Unit", "Compensation", "frac"))) %>%
    # Convert column class automatically
    type.convert(as.is = TRUE) %>%
    # Convert units
    mutate(CH4dry_ppb = CH4dry_ppm*1000) %>% select(!CH4dry_ppm) %>%
    mutate(N2Odry_ppb = N2Odry_ppm*1000) %>% select(!N2Odry_ppm) %>%
    mutate(NH3dry_ppb = NH3dry_ppm*1000) %>% select(!NH3dry_ppm) %>%
    mutate(COdry_ppb = COdry_ppm*1000) %>% select(!COdry_ppm) %>%
    mutate(H2O_ppm = `H2O_vol-%`*10000) %>% select(!`H2O_vol-%`) %>%
    # Remove NAs and negative gas measurements, if any
    filter(CO2dry_ppm > 0) %>%
    filter(COdry_ppb > 0) %>%
    filter(CH4dry_ppb > 0) %>%
    filter(H2O_ppm > 0) %>%
    filter(N2Odry_ppb > 0) %>%
    filter(NH3dry_ppb > 0)

  # Create a new column containing date and time (POSIX format)
  tryCatch(
    {op <- options()
    options(digits.secs=6)
    if(date.format == "dmy"){
      try.POSIX <- as.POSIXct(dmy_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone),
                              format = "%Y-%m-%d %H:%M:%OS")
    } else if(date.format == "mdy"){
      try.POSIX <- as.POSIXct(mdy_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone),
                              format = "%Y-%m-%d %H:%M:%OS")
    } else if(date.format == "ymd"){
      try.POSIX <- as.POSIXct(ymd_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone),
                              format = "%Y-%m-%d %H:%M:%OS")}
    options(op)}, warning = function(w) {POSIX.warning <<- "date.format.error"}
  )

  if(isTRUE(POSIX.warning == "date.format.error")){
    stop(paste("An error occured while converting DATE and TIME into POSIX.time.",
               "Verify that 'date.format' corresponds to the column 'DATE' in",
               "the raw data file. Here is a sample:", data.raw$DATE[1]))
  } else data.raw$POSIX.time <- try.POSIX

  # Save cleaned data file
  if(save == TRUE){
    # Create RData folder in working directory
    RData_folder <- paste(getwd(), "RData", sep = "/")
    if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

    # Create output file: change extension to .RData, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.txt", "", inputfile))
    outputfile <- paste("DX4015_", file.name, "_imp.RData", sep = "")

    save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
