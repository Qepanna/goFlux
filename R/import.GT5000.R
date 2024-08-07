#' Import function for the Gasmet GT5000 Terra - Splashproof multigas FTIR
#' analyzer
#'
#' Imports single raw gas measurement files from the Gasmet GT5000 gas analyzer
#' (CO, \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}},
#' \ifelse{html}{\out{NH<sub>3</sub>}}{\eqn{NH[3]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}})
#' with the extension .TXT
#'
#' @param inputfile character string; the name of a file with the extension .TXT
#' @param date.format character string; specifies the date format found in the
#'                    raw data file. Choose one of the following: "dmy", "ymd",
#'                    or "mdy". Default is "ymd", as it is the date format from
#'                    the example data file provided.
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param save logical; if \code{save = TRUE}, saves the file as an .RData file
#'             in a RData folder in the current working directory. If
#'             \code{save = FALSE}, returns the file in the Console, or load in
#'             the Environment if assigned to an object.
#' @param keep_all logical; if \code{keep_all = TRUE}, keep all columns from the raw
#'                 file. The default is \code{keep_all = FALSE}, and columns that
#'                 are not necessary for gas flux calculation are removed.
#' @param prec numerical vector; the precision of the instrument for each gas,
#'             in the following order: "CO2dry_ppm", "COdry_ppb", "CH4dry_ppb",
#'             "N2Odry_ppb", "NH3dry_ppb" and "H2O_ppm". The default is
#'             \code{prec = c(1.6, 23, 13, 2, 23, 33)}.
#'
#' @returns A data frame containing raw data from the Gasmet GT5000 gas analyzer.
#'
#' @include goFlux-package.R
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument GT5000, the
#' date is found in the column "Date".
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#'   CO, \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#'   \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} and
#'   \ifelse{html}{\out{NH<sub>3</sub>}}{\eqn{NH[3]}{ASCII}}
#'   \item vol-\% for \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item mbar for pressure
#'   \item Celsius for temperature}
#' If your Gasmet GT5000 uses different units, either convert the units after
#' import, change the settings on your instrument, or contact the maintainer of
#' this package for support.
#'
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[goFlux]{k.max}}) in the non-linear flux calculation
#' (\code{\link[goFlux]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm for
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}, or 1 ppb for
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}, CO
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} and
#' \ifelse{html}{\out{NH<sub>3</sub>}}{\eqn{NH[3]}{ASCII}}) to allow for more
#' curvature, especially for water vapor fluxes, or very long measurements, that
#' are normally curved. The default values given for instrument precision are
#' the ones provided by the manufacturer upon request, for the latest model of
#' this instrument available at the time of the creation of this function (08-2024).
#'
#' @seealso Use the wrapper function \code{\link[goFlux]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso See also, import functions for other instruments:
#'          \code{\link[goFlux]{import.DX4015}},
#'          \code{\link[goFlux]{import.EGM5}},
#'          \code{\link[goFlux]{import.G2508}},
#'          \code{\link[goFlux]{import.G4301}},
#'          \code{\link[goFlux]{import.GAIA}},
#'          \code{\link[goFlux]{import.GasmetPD}},
#'          \code{\link[goFlux]{import.LI6400}},
#'          \code{\link[goFlux]{import.LI7810}},
#'          \code{\link[goFlux]{import.LI7820}},
#'          \code{\link[goFlux]{import.LI8100}},
#'          \code{\link[goFlux]{import.LI8200}},
#'          \code{\link[goFlux]{import.LI8250}},
#'          \code{\link[goFlux]{import.N2OM1}},
#'          \code{\link[goFlux]{import.uCH4}},
#'          \code{\link[goFlux]{import.uN2O}},
#'          \code{\link[goFlux]{import.UGGA}}
#'
#' @seealso See \code{\link[base]{timezones}} for a description of the underlying
#'          timezone attribute.
#'
#' @examples
#' # Load file from downloaded package
#' file.path <- system.file("extdata", "GT5000/GT5000.TXT", package = "goFlux")
#'
#' # Run function
#' imp.GT5000 <- import.GT5000(inputfile = file.path)
#' @export

import.GT5000 <- function(inputfile, date.format = "ymd", timezone = "UTC",
                          save = FALSE, keep_all = FALSE,
                          prec = c(1.6, 23, 13, 2, 23, 33)){

  # Check arguments
  if(missing(inputfile)) stop("'inputfile' is required")
  if(!is.character(inputfile)) stop("'inputfile' must be of class character")
  if(length(date.format) != 1) stop("'date.format' must be of length 1")
  if(!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}
  if(!is.character(timezone)) stop("'timezone' must be of class character")
  if(save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")
  if(keep_all != TRUE & keep_all != FALSE) stop("'keep_all' must be TRUE or FALSE")
  if(is.null(prec)) stop("'prec' is required") else{
    if(!is.numeric(prec)) stop("'prec' must be of class numeric") else{
      if(length(prec) != 6) stop("'prec' must be of length 6")}}

  # Assign NULL to variables without binding
  POSIX.warning <- Line <- SpectrumFile <- H2O_frac <- Time <- Date <-
    Pressure <- Status <- LibraryFile <- NH3dry_ppb <- N2Odry_ppb <-
    H2O_ppm <- CH4dry_ppb <- COdry_ppb <- CO2dry_ppm <- `H2O_vol-%` <-
    COdry_ppm <- NH3dry_ppm <- N2Odry_ppm <- CH4dry_ppm <- P_unit <- . <-
    H2O_unit <- CO2_unit <- CH4_unit <- N2O_unit <- CO_unit <- NH3_unit <-
    COwet_ppm <- NH3wet_ppm <- N2Owet_ppm <- CH4wet_ppm <- CO2wet_ppm <-
    DATE <- TIME <- import.error <- NULL

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data file
  try.import <- tryCatch(
    {read.delim(inputfile, colClasses = "character")},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    # Load data file
    data.raw <- try.import %>%
      # Modify column names for DATE and TIME
      rename(DATE = Date, TIME = Time) %>%
      # Modify column names for units
      rename(H2O_unit = which(names(.) == "Water.vapor.H2O") +1) %>%
      rename(CO2_unit = which(names(.) == "Carbon.dioxide.CO2") +1) %>%
      rename(CH4_unit = which(names(.) == "Methane.CH4") +1) %>%
      rename(N2O_unit = which(names(.) == "Nitrous.oxide.N2O") +1) %>%
      rename(CO_unit = which(names(.) == "Carbon.monoxide.CO") +1) %>%
      rename(NH3_unit = which(names(.) == "Ammonia.NH3") +1) %>%
      rename(P_unit = which(names(.) == "Pressure") +1) %>%
      rename(Cell_temp_unit = which(names(.) == "Cell.temperature") +1) %>%
      rename(Eclect_temp_unit = which(names(.) == "Electronics.temperature") +1) %>%
      # Modify column names for gas compensation (wet or dry fraction)
      rename(H2O_frac = which(names(.) == "Water.vapor.H2O") +2) %>%
      rename(CO2_frac = which(names(.) == "Carbon.dioxide.CO2") +2) %>%
      rename(CH4_frac = which(names(.) == "Methane.CH4") +2) %>%
      rename(N2O_frac = which(names(.) == "Nitrous.oxide.N2O") +2) %>%
      rename(CO_frac = which(names(.) == "Carbon.monoxide.CO") +2) %>%
      rename(NH3_frac = which(names(.) == "Ammonia.NH3") +2) %>%
      # Modify column names for residual
      rename(H2O_resid = which(names(.) == "Water.vapor.H2O") +3) %>%
      rename(CO2_resid = which(names(.) == "Carbon.dioxide.CO2") +3) %>%
      rename(CH4_resid = which(names(.) == "Methane.CH4") +3) %>%
      rename(N2O_resid = which(names(.) == "Nitrous.oxide.N2O") +3) %>%
      rename(CO_resid = which(names(.) == "Carbon.monoxide.CO") +3) %>%
      rename(NH3_resid = which(names(.) == "Ammonia.NH3") +3) %>%
      # Remove irrelevant columns
      select(!contains(c("Compensation", "Residual")))

    # Remove columns that are not used for gas flux calculations
    if(keep_all == FALSE){
      data.raw <- data.raw %>%
        select(!c(Line, SpectrumFile, LibraryFile, Status, Pressure, P_unit,
                  contains(c("temperature", "_temp_")))) %>%
        select(!H2O_frac)}

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
      # Convert column class automatically
      type.convert(as.is = TRUE) %>%
      # In "Compensation", wet means that the gases are NOT compensated for
      # water vapor, meaning that "wet" stands for the wet fraction.
      mutate(H2O_ppm = `H2O_vol-%`*10000) %>%
      mutate(CO2dry_ppm = CO2wet_ppm/(1-H2O_ppm/1000000)) %>%
      mutate(CH4dry_ppm = CH4wet_ppm/(1-H2O_ppm/1000000)) %>%
      mutate(N2Odry_ppm = N2Owet_ppm/(1-H2O_ppm/1000000)) %>%
      mutate(NH3dry_ppm = NH3wet_ppm/(1-H2O_ppm/1000000)) %>%
      mutate(COdry_ppm = COwet_ppm/(1-H2O_ppm/1000000)) %>%
      # Remove unit and compensation columns
      select(!c(H2O_unit, CO2_unit, CH4_unit, N2O_unit,
                CO_unit, NH3_unit, contains(c("frac")))) %>%
      # Convert units
      mutate(CH4dry_ppb = CH4dry_ppm*1000) %>%
      mutate(N2Odry_ppb = N2Odry_ppm*1000) %>%
      mutate(NH3dry_ppb = NH3dry_ppm*1000) %>%
      mutate(COdry_ppb = COdry_ppm*1000) %>%
      # Order columns alphabetically
      select(order(colnames(.))) %>% relocate(DATE, TIME)

    # Remove columns that are not used for gas flux calculations
    if(keep_all == FALSE){
      data.raw <- data.raw %>%
        # Remove residuals
        select(!contains("_resid")) %>%
        # Remove columns with original wet fraction
        select(!c(CH4wet_ppm, N2Owet_ppm, NH3wet_ppm, CO2wet_ppm, COwet_ppm)) %>%
        # Remove columns with original gas units
        select(!c(CH4dry_ppm, N2Odry_ppm, NH3dry_ppm, COdry_ppm, `H2O_vol-%`))}

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
      warning("Error occurred in file ", inputfile.name, ":\n",
              "   An error occured while converting DATE and TIME into POSIX.time.\n",
              "   Verify that the 'date.format' you specified (", date.format,
              ") corresponds to the\n",
              "   column 'Date' in the raw data file. Here is a sample: ",
              data.raw$DATE[1], "\n", call. = F)
    } else {

      data.raw$POSIX.time <- try.POSIX

      # Add instrument precision for each gas
      data.raw <- data.raw %>%
        mutate(CO2_prec = prec[1], CO_prec = prec[2], CH4_prec = prec[3],
               N2O_prec = prec[4], NH3_prec = prec[5], H2O_prec = prec[6])

      # Save cleaned data file
      if(save == TRUE){
        # Create RData folder in working directory
        RData_folder <- paste(getwd(), "RData", sep = "/")
        if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

        # Create output file: change extension to .RData, and
        # add instrument name and "imp" for import to file name
        file.name <- gsub(".*/", "", sub("\\.TXT", "", inputfile))
        outputfile <- paste("GT5000_", file.name, "_imp.RData", sep = "")

        save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

        message(inputfile.name, " saved as ", outputfile,
                " in RData folder, in working directory\n", sep = "")
      }

      if(save == FALSE){
        return(data.raw)
      }
    }
  }
}
