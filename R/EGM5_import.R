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
#' @param save logical; if \code{save = TRUE}, saves the file as an .RData file
#'             in a RData folder in the current working directory. If
#'             \code{save = FALSE}, returns the file in the Console, or load in
#'             the Environment if assigned to an object.
#' @param keep_all logical; if \code{keep_all = TRUE}, keep all columns from the raw
#'                 file. The default is \code{keep_all = FALSE}, and columns that
#'                 are not necessary for gas flux calculation are removed.
#' @param prec numerical vector; the precision of the instrument for each gas,
#'             in the following order: "CO2dry_ppm", "O2dry_pct" and "H2O_ppm".
#'             The default is \code{prec = c(3, 1, 500)}.
#' @param proc.data.field numeric value; select the process data field used with
#'                        the EGM-5. There are 5 different modes available: (1)
#'                        Measure mode; (2) SRC or Custom mode; (3) CPY mode;
#'                        (4) Injection mode; or (5) Static mode. If no value
#'                        is specified, the parameters will be named "param1",
#'                        "param2", "param3", "param4" and "param5".
#'
#' @returns A data frame containing raw data from the PP-Systems EGM-5 Portable
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} Gas Analyzer.
#'
#' @include goFlux-package.R
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
#' If your PP-Systems EGM-5 uses different units, either convert the units after
#' import, change the settings on your instrument, or contact the maintainer of
#' this package for support.
#'
#' Note that this instrument measures the "wet" fraction of
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{O<sub>2</sub>}}{\eqn{O[2]}{ASCII}}. If water vapor is
#' measured, the dry fraction of the gases is calculated automatically. If not,
#' it is important to measure the ambient water vapor concentration to convert
#' gases into their dry fraction and use those in the following steps of this
#' package. For example, \code{CO2dry_ppm = CO2wet_ppm/(1-H2O_ppm/1000000)}.
#'
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[goFlux]{k.max}}) in the non-linear flux calculation
#' (\code{\link[goFlux]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm for
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}, or 1\% for
#' \ifelse{html}{\out{O<sub>2</sub>}}{\eqn{O[2]}{ASCII}}) to allow for more
#' curvature, especially for water vapor fluxes, or very long measurements, that
#' are normally curved. The default values given for instrument precision are
#' the ones provided by the manufacturer upon request, for the latest model of
#' this instrument available at the time of the creation of this function (11-2023).
#'
#' Look up the
#' \href{https://ppsystems.com/download/technical_manuals/80109-1-EGM-5_Operation_V103.pdf}{EGM-5 Operation Manual}
#' at page 90 for more details about the different Process Data Fields
#' (\code{proc.data.field}).
#'
#' Note that the default file extension for this instrument is .TXT, however,
#' it is also possible to use with a .csv file format.
#'
#' @seealso Use the wrapper function \code{\link[goFlux]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso See also, import functions for other instruments:
#'          \code{\link[goFlux]{import.DX4015}},
#'          \code{\link[goFlux]{import.G2508}},
#'          \code{\link[goFlux]{import.G4301}},
#'          \code{\link[goFlux]{import.GAIA}},
#'          \code{\link[goFlux]{import.LI6400}},
#'          \code{\link[goFlux]{import.LI7810}},
#'          \code{\link[goFlux]{import.LI7820}},
#'          \code{\link[goFlux]{import.LI8100}},
#'          \code{\link[goFlux]{import.LI8200}},
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
#' file.path <- system.file("extdata", "EGM5/EGM5.TXT", package = "goFlux")
#'
#' # Run function
#' imp.EGM5 <- import.EGM5(inputfile = file.path)
#'
#' @export
#'
EGM5_import <- function(inputfile, date.format = "dmy", timezone = "UTC",
                        save = FALSE, keep_all = FALSE, prec = c(3, 1, 500),
                        proc.data.field = NULL){

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
  if(!is.null(proc.data.field)){
    if(!is.numeric(proc.data.field)) stop("'proc.data.field' must be of class numeric") else{
      if(!between(proc.data.field, 1,5)) stop("'proc.data.field' must be a value from 1 to 5")}}

  # Assign NULL to variables without binding
  POSIX.time <- DATE_TIME <- H2O_ppm <- Time <- . <- Plot_No <- start_flag <-
    Obs <- nb.obs <- Date <- CO2 <- O2 <- Tair <- Msoil <- H2O <- Tag.M3. <-
    H2O_mb <- Pressure <- O2wet_pct <- O2wet_ppm <- O2dry_ppm <- DATE <-
    TIME <- O2dry_pct <- PAR <- Tsoil <- Tcham <- SWC <- chamID <-
    start.time <- end.time <- cham.open <- NH3 <- NH3wet_ppm <- NH3dry_ppb <-
    CO2dry_ppm <- POSIX.warning <- CO2_ppm <- CO2wet_ppm <- param1 <-
    param2 <- param3 <- param4 <- param5 <- import.error <- NULL

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data file
  try.import <- tryCatch(
    {read.delim(inputfile, sep = ",", nrows = 0)},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    # Column names
    col.names <- try.import %>% names(.)

    # Load data file
    data.raw <- read.delim(inputfile, sep = ",", header = F) %>%
      # Rename headers
      rename_all(~c(col.names[1:17], paste("param", seq(1,5), sep = ""))) %>%
      # Detect start.time
      mutate(start_flag = if_else(
        row_number() %in% (which(.$Tag.M3. == "Start")+1), 1, 0))

    # Convert column classes
    suppressWarnings(
      data.raw <- data.raw %>%
        mutate(across(Plot_No:ncol(.), as.numeric))
    )

    # Clean data frame
    data.raw <- data.raw %>%
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
      # Find and rename Tag
      rename(Tag = grep("Tag", names(.), value = T)) %>%
      # Find and rename SWC
      rename(SWC = grep("Msoil", names(.), value = T)) %>%
      rename(SWC = grep("RH", names(.), value = T)) %>%
      # Rename columns
      rename(CO2wet_ppm = CO2, O2wet_pct = O2, DATE = Date, TIME = Time,
             Tcham = Tair, H2O_mb = H2O) %>%
      # Convert H2O_mb to ppm
      mutate(H2O_ppm = (H2O_mb / Pressure)*1000) %>%
      # Remove NAs and negative gas measurements, if any
      filter(CO2wet_ppm >= 0) %>%
      filter(O2wet_pct >= 0) %>%
      filter(H2O_ppm >= 0)

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
          select(DATE, TIME, Plot_No, Obs, CO2dry_ppm, O2dry_pct, H2O_ppm, PAR,
                 Tsoil, Tcham, SWC, param1, param2, param3, param4, param5)
      } else {
        data.raw <- data.raw %>%
          select(DATE, TIME, Plot_No, Obs, CO2wet_ppm, O2wet_pct, PAR, Tsoil,
                 Tcham, SWC, param1, param2, param3, param4, param5)
      }
    }

    # Rename Process Data Fields
    if(!is.null(proc.data.field)){
      if(proc.data.field == 1){ # Measure mode
        data.raw <- data.raw %>% rename(
          Probe = param1, Bat.pct = param2, Zero.pct = param3,
          Bat.volt = param4, Bat.time = param5)}
      if(proc.data.field == 2){ # SRC or Custom mode
        data.raw <- data.raw %>% rename(
          Process = param1, DC = param2, DT = param3,
          SRL.ass = param4, SRQ.ass = param5)}
      if(proc.data.field == 3){ # CPY mode
        data.raw <- data.raw %>% rename(
          Process = param1, DC.inv = param2, DT = param3,
          SRL.res = param4, SRQ.res = param5)}
      if(proc.data.field == 4){ # Injection mode
        data.raw <- data.raw %>% rename(
          Process = param1, C_F = param2, Volume = param3,
          Base = param4, CO2.int = param5)}
      if(proc.data.field == 5){ # Static mode
        data.raw <- data.raw %>% rename(
          Process = param1, na_1 = param2, DT = param3, CO2 = param4, na_2 = param5)}
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
      warning("Error occurred in file ", inputfile.name, ":\n",
              "   An error occured while converting DATE and TIME into POSIX.time.\n",
              "   Verify that the 'date.format' you specified (", date.format,
              ") corresponds to the\n",
              "   column 'DATE' in the raw data file. Here is a sample: ",
              data.raw$DATE[1], "\n", call. = F)
    } else {

      data.raw$POSIX.time <- try.POSIX

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

      # New function name
      if (as.character(match.call()[[1]]) == "EGM5_import") {
        warning(paste("All import functions have changed names in this new version of goFlux.",
                      "\nIn the future, use import.EGM5() instead of EGM5_import()"), call. = FALSE)
      }

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

        message(inputfile.name, " saved as ", outputfile,
                " in RData folder, in working directory\n", sep = "")
      }

      if(save == FALSE){
        return(data.raw)
      }
    }
  }
}

#' @export
#' @rdname EGM5_import
import.EGM5 <- EGM5_import
