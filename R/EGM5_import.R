#' Import function for the PP-Systems EGM-5 Portable
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} Gas Analyzer
#'
#' Imports single raw gas measurement files from the PP-Systems EGM-5 Portable
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} Gas Analyzer, with
#' the extension .TXT
#'
#' @param inputfile character string; the name of a file with the extension .TXT
#' @param date.format character string; specifies the date format found in the
#'                    raw data file. Choose one of the following: "dmy", "ymd",
#'                    or "mdy". Default is "dmy", as it is the date format from
#'                    the example data file provided.
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param save logical; if \code{save = TRUE}, saves the file as RData in a
#'             RData folder in the current working directory. If
#'             \code{save = FALSE}, returns the file in the Console, or load in
#'             the Environment if assigned to an object.
#' @param keep_all logical; if \code{keep_all = TRUE}, keep all columns from raw
#'                 file. The default is \code{keep_all = FALSE}, and columns that
#'                 are not necessary for gas flux calculation are removed.
#' @param prec numerical vector; the precision of the instrument for each gas,
#'             in the following order: "CO2dry_ppm", "O2dry_ppm" and H2O_ppm".
#'             The default is \code{prec = c(1, 1, 1)}. Note that the precision
#'             of this instrument is currently unknown, so a value of 1 is used
#'             for each gas.
#'
#' @returns A data frame containing raw data from the PP-Systems EGM-5 Portable
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} Gas Analyzer.
#'
#' @include GoFluxYourself-package.R
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument EGM5, the date
#' is found in the column "Date".
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#'   \item \% for \ifelse{html}{\out{O<sub>2</sub>}}{\eqn{O[2]}{ASCII}} (pct)
#'   \item mb for \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item mb for pressure
#'   \item Celsius for temperature}
#' If your instrument uses different units, either convert the units after import,
#' change the settings on your instrument, or contact the maintainer of this
#' package for support.
#'
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[GoFluxYourself]{k.max}}) in the non-linear flux calculation
#' (\code{\link[GoFluxYourself]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm for
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}, or 1\% for
#' \ifelse{html}{\out{O<sub>2</sub>}}{\eqn{O[2]}{ASCII}}) to allow for more
#' curvature, especially for water vapor fluxes, or very long measurements, that
#' are normally curved. The default values given for instrument precision are
#' the ones found for the latest model of this instrument, available at the
#' time of the creation of this package (11-2023).
#'
#' @seealso Use the wrapper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso See also, import functions for other instruments:
#'          \code{\link[GoFluxYourself]{DX4015_import}},
#'          \code{\link[GoFluxYourself]{G2508_import}},
#'          \code{\link[GoFluxYourself]{G4301_import}},
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
#' file.path <- system.file("extdata", "EGM5/EGM5.TXT", package = "GoFluxYourself")
#'
#' EGM5_imp <- EGM5_import(inputfile = file.path)
#'
#' @export
#'
EGM5_import <- function(inputfile, date.format = "dmy", timezone = "UTC",
                       save = FALSE, keep_all = FALSE, prec = c(1, 1, 1)){

  # Check arguments
  if (missing(inputfile)) stop("'inputfile' is required")
  if (!is.character(inputfile)) stop("'inputfile' must be of class character")
  if (length(date.format) != 1) stop("'date.format' must be of length 1")
  if (!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}
  if (!is.character(timezone)) stop("'timezone' must be of class character")
  if (save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")
  if (keep_all != TRUE & keep_all != FALSE) stop("'keep_all' must be TRUE or FALSE")
  if(is.null(prec)) stop("'prec' is required") else{
    if(!is.numeric(prec)) stop("'prec' must be of class numeric") else{
      if(length(prec) != 3) stop("'prec' must be of length 3")}}

  # Assign NULL to variables without binding
  POSIX.time <- DATE_TIME <- H2O_ppm <- Time <- . <- Plot_No <- start_flag <-
    Obs <- nb.obs <- Date <- CO2 <- O2 <- Tair <- Msoil <- H2O <- Tag.M3. <-
    H2O_mb <- Pressure <- O2wet_pct <- O2wet_ppm <- O2dry_ppm <- DATE <-
    TIME <- O2dry_pct <- PAR <- Tsoil <- Tcham <- SWC <- chamID <-
    start.time <- end.time <- cham.open <- NH3 <- NH3wet_ppm <- NH3dry_ppb <-
    CO2dry_ppm <- POSIX.warning <- CO2_ppm <- CO2wet_ppm <- NULL

  # Load data file
  data.raw <- read.delim(inputfile, sep = ",") %>%
    # Detect start.time
    mutate(start_flag = if_else(
      row_number() %in% (which(.$Tag.M3. == "Start")+1), 1, 0)) %>%
    # Convert column classes
    mutate(across(Plot_No:ncol(.), as.numeric)) %>%
    # Remove NAs and negative gas measurements, if any
    drop_na() %>%
    # Detect new observations
    mutate(Obs = rleid(cumsum(start_flag == 1))) %>%
    # Remove measurements with less than 5 observations
    group_by(Obs) %>% mutate(nb.obs = n()) %>% ungroup() %>%
    filter(nb.obs > 5) %>% select(!nb.obs) %>%
    # Correct Obs
    mutate(Obs = rleid(cumsum(start_flag == 1))) %>% select(!start_flag) %>%
    # Plot_No must be as.character
    mutate(Plot_No = as.character(Plot_No)) %>%
    # Correct Date characters
    mutate(Date = gsub("/", "-", Date)) %>%
    # Rename columns
    rename(CO2wet_ppm = CO2, O2wet_pct = O2, DATE = Date, TIME = Time,
           Tcham = Tair, SWC = Msoil, H2O_mb = H2O, Tag = Tag.M3.) %>%
    # Convert H2O_mb to ppm
    mutate(H2O_ppm = (H2O_mb / Pressure)*1000)

  # Compensate for water vapor
  if(sum(data.raw$H2O_ppm) > 0){
    data.raw <- data.raw %>%
      mutate(CO2dry_ppm = CO2wet_ppm/(1-H2O_ppm/1000000)) %>%
      mutate(O2wet_ppm = O2wet_pct * 10000,
             O2dry_ppm = O2wet_ppm/(1-H2O_ppm/1000000),
             O2dry_pct = O2dry_ppm / 10000) %>%
      select(!c(O2dry_ppm, O2wet_ppm))
  }

  # Keep only useful columns for gas flux calculation
  if(keep_all == FALSE){
    if(sum(data.raw$H2O_ppm) > 0){
      data.raw <- data.raw %>%
        select(DATE, TIME, Plot_No, Obs, CO2dry_ppm, O2dry_pct,
               H2O_ppm, PAR, Tsoil, Tcham, SWC)
    } else {
      data.raw <- data.raw %>%
        select(DATE, TIME, Plot_No, Obs, CO2wet_ppm, O2wet_pct,
               PAR, Tsoil, Tcham, SWC)
    }
  }

  # Create a new column containing date and time (POSIX format)
  tryCatch(
    {if(date.format == "dmy"){
      try.POSIX <- as.POSIXct(dmy_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone))
    } else if(date.format == "mdy"){
      try.POSIX <- as.POSIXct(mdy_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone))
    } else if(date.format == "ymd"){
      try.POSIX <- as.POSIXct(ymd_hms(paste(data.raw$DATE, data.raw$TIME), tz = timezone))
    }}, warning = function(w) {POSIX.warning <<- "date.format.error"}
  )

  if(isTRUE(POSIX.warning == "date.format.error")){
    stop(paste("An error occured while converting DATE and TIME into POSIX.time.",
               "Verify that 'date.format' corresponds to the column 'DATE' in",
               "the raw data file. Here is a sample:", data.raw$DATE[1]))
  } else data.raw$POSIX.time <- try.POSIX

  data.raw <- data.raw %>%
    # Correct DATE
    mutate(DATE = substr(POSIX.time, 0, 10)) %>%
    # Create chamID
    mutate(chamID = paste(Plot_No, Obs, DATE, sep = "_")) %>%
    # Add start.time and end.time
    group_by(chamID) %>%
    mutate(start.time = first(POSIX.time),
           end.time = last(POSIX.time)) %>% ungroup() %>%
    # Add cham.close and cham.open
    mutate(cham.close = start.time, cham.open = end.time) %>%
    # Calculate Etime
    mutate(Etime = as.numeric(POSIX.time - start.time, units = "secs"),
           flag = if_else(between(POSIX.time, start.time, cham.open), 1, 0))

  # Add instrument precision for each gas
  data.raw <- data.raw %>%
    mutate(CO2_prec = prec[1], O2_prec = prec[2],  H2O_prec = prec[3])

  # Save cleaned data file
  if(save == TRUE){
    # Create RData folder in working directory
    RData_folder <- paste(getwd(), "RData", sep = "/")
    if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

    # Create output file: change extension to .RData, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.TXT", "", inputfile))
    outputfile <- paste("EGM5_", file.name, "_imp.RData", sep = "")

    save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
