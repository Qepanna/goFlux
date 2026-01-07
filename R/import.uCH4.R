#' Import function for the MIRA Ultra
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}/
#' \ifelse{html}{\out{C<sub>2</sub>H<sub>6</sub>}}{\eqn{C[2]H[6]}{ASCII}} analyzer
#' from Aeris Technologies
#'
#' Imports single raw gas measurement files from the MIRA Ultra
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}/
#' \ifelse{html}{\out{C<sub>2</sub>H<sub>6</sub>}}{\eqn{C[2]H[6]}{ASCII}}
#' Natural Gas Leak Detection System (with GPS) from Aeris Technologies
#' (\ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#' \ifelse{html}{\out{C<sub>2</sub>H<sub>6</sub>}}{\eqn{C[2]H[6]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}) with the extension .txt
#'
#' @param inputfile character string; the name of a file with the extension .txt
#' @param date.format character string; specifies the date format found in the
#'                    raw data file. Choose one of the following: "dmy", "ymd",
#'                    or "mdy". Default is "mdy", as it is the date format from
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
#'             in the following order: "CH4_ppb", "C2H6dry_ppb" and "H2O_ppm".
#'             The default is \code{prec = c(1, 0.5, 15)}. The precision for
#'             water vapor is currently unknown, and a default of 1 ppm is used.
#'
#' @returns A data frame containing raw data from the MIRA Ultra
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}/
#' \ifelse{html}{\out{C<sub>2</sub>H<sub>6</sub>}}{\eqn{C[2]H[6]}{ASCII}} analyzer
#' from Aeris Technologies
#'
#' @include goFlux-package.R
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For these instruments, the date
#' is found in the column "Time Stamp".
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} and
#'   \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item ppb for \ifelse{html}{\out{C<sub>2</sub>H<sub>6</sub>}}{\eqn{C[2]H[6]}{ASCII}}
#'   \item mbars for pressure
#'   \item Celsius for temperature}
#' If your Aeris instrument (MIRA Ultra
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}/
#' \ifelse{html}{\out{C<sub>2</sub>H<sub>6</sub>}}{\eqn{C[2]H[6]}{ASCII}} analyzer) uses
#' different units, either convert the units after import, change the settings
#' on your instrument, or contact the maintainer of this package for support.
#'
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[goFlux]{k.max}}) in the non-linear flux calculation
#' (\code{\link[goFlux]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm for
#' \ifelse{html}{\out{C<sub>2</sub>H<sub>6</sub>}}{\eqn{C[2]H[6]}{ASCII}} and
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}, or 1 ppb for
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}) to allow for more
#' curvature, especially for water vapor fluxes, or very long measurements, that
#' are normally curved. The default values given for instrument precision are
#' the ones found \href{https://aerissensors.com/mira-pico-mobile-lds/}{online}
#' for the latest model of this instrument available at the
#' time of the creation of this function (01-2024).
#'
#' @seealso Use the wrapper function \code{\link[goFlux]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso See also, import functions for other instruments:
#'          \code{\link[goFlux]{import.DX4015}},
#'          \code{\link[goFlux]{import.EGM5}},
#'          \code{\link[goFlux]{import.G2201i}},
#'          \code{\link[goFlux]{import.G2508}},
#'          \code{\link[goFlux]{import.G4301}},
#'          \code{\link[goFlux]{import.GAIA}},
#'          \code{\link[goFlux]{import.GasmetPD}},
#'          \code{\link[goFlux]{import.GT5000}},
#'          \code{\link[goFlux]{import.HT8850}},
#'          \code{\link[goFlux]{import.LI6400}},
#'          \code{\link[goFlux]{import.LI7810}},
#'          \code{\link[goFlux]{import.LI7820}},
#'          \code{\link[goFlux]{import.LI8100}},
#'          \code{\link[goFlux]{import.LI8150}},
#'          \code{\link[goFlux]{import.LI8200}},
#'          \code{\link[goFlux]{import.LI8250}},
#'          \code{\link[goFlux]{import.N2OM1}},
#'          \code{\link[goFlux]{import.N2Oi2}},
#'          \code{\link[goFlux]{import.skyline}},
#'          \code{\link[goFlux]{import.uN2O}},
#'          \code{\link[goFlux]{import.UGGA}}
#'
#' @seealso See \code{\link[base]{timezones}} for a description of the underlying
#'          timezone attribute.
#'
#' @examples
#' # Load file from downloaded package
#' file.path <- system.file("extdata", "uCH4/uCH4.txt", package = "goFlux")
#'
#' # Run function
#' imp.uCH4 <- import.uCH4(inputfile = file.path)
#' @export

import.uCH4 <- function(inputfile, date.format = "mdy", timezone = "UTC",
                        save = FALSE, keep_all = FALSE, prec = c(1, 0.5, 15)){

  # Check arguments
  if (missing(inputfile)) stop("'inputfile' is required")
  if (!is.character(inputfile)) stop("'inputfile' must be of class character")
  if (length(date.format) != 1) stop("'date.format' must be of length 1")
  if (!is.character(date.format)) stop("'date.format' must be of class character")
  if (!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be one of the following: 'ymd', 'dmy' or 'mdy'")}
  if (!is.character(timezone)) stop("'timezone' must be of class character")
  if (save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")
  if (keep_all != TRUE & keep_all != FALSE) stop("'keep_all' must be TRUE or FALSE")
  if(is.null(prec)) stop("'prec' is required") else{
    if(!is.numeric(prec)) stop("'prec' must be of class numeric") else{
      if(length(prec) != 3) stop("'prec' must be of length 3")}}

  # Assign NULL to variables without binding
  POSIX.time <- DATE_TIME <- H2O_ppm <- CH4dry_ppb <- Time <- InletNumber <-
    CH4dry_ppm <- C2H6dry_ppb <- POSIX.warning <- C2H6_ppb <- TimeStamp <- . <-
    CH4_ppm <- import.error <- TgasdegC <- SOC_ <- NULL

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data file
  try.import <- tryCatch(
    {read.delim(inputfile, sep = ",")},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    # Load data file
    data.raw <- try.import %>%
      # Clean column names
      `colnames<-`(gsub("__", "_",
                        gsub("\\.", "",
                             gsub("\\.\\.", "_", names(.))))) %>%
      rename(Time = TimeStamp, Inlet = InletNumber, Tgas_degC = TgasdegC,
             SOC = SOC_, C2H6dry_ppb = C2H6_ppb, CH4dry_ppm = CH4_ppm) %>%
      # Remove rows at the end of the file
      drop_na(C2H6dry_ppb) %>%
      # Convert ppm into ppb for CH4dry
      mutate(CH4dry_ppb = CH4dry_ppm*1000) %>%
      # Replace characters in Time ("/" -> "-") and remove first space
      mutate(DATE_TIME = gsub("/", "-", Time))

    # Keep only useful columns for gas flux calculation
    if(keep_all == FALSE){
      data.raw <- data.raw %>%
        select(DATE_TIME, CH4dry_ppb, C2H6dry_ppb, H2O_ppm)}

    # Create a new column containing date and time (POSIX format)
    tryCatch(
      {op <- options()
      options(digits.secs=6)
      if(date.format == "dmy"){
        try.POSIX <- as.POSIXct(dmy_hms(data.raw$DATE_TIME, tz = timezone),
                                format = "%Y-%m-%d %H:%M:%OS")
      } else if(date.format == "mdy"){
        try.POSIX <- as.POSIXct(mdy_hms(data.raw$DATE_TIME, tz = timezone),
                                format = "%Y-%m-%d %H:%M:%OS")
      } else if(date.format == "ymd"){
        try.POSIX <- as.POSIXct(ymd_hms(data.raw$DATE_TIME, tz = timezone),
                                format = "%Y-%m-%d %H:%M:%OS")}
      options(op)}, warning = function(w) {POSIX.warning <<- "date.format.error"}
    )

    if(isTRUE(POSIX.warning == "date.format.error")){
      warning("Error occurred in file ", inputfile.name, ":\n",
              "   An error occured while converting DATE and TIME into POSIX.time.\n",
              "   Verify that the 'date.format' you specified (", date.format,
              ") corresponds to the\n",
              "   column 'Time Stamp' in the raw data file. Here is a sample: ",
              data.raw$DATE_TIME[1], "\n", call. = F)
    } else {

      data.raw$POSIX.time <- try.POSIX

      # Add a column for DATE alone
      data.raw <- data.raw %>% mutate(DATE = substr(POSIX.time, 0, 10))

      # Add instrument precision for each gas
      data.raw <- data.raw %>%
        mutate(CH4_prec = prec[1],  C2H6_prec = prec[2], H2O_prec = prec[3])

      # Save cleaned data file
      if(save == TRUE){
        # Create RData folder in working directory
        RData_folder <- paste(getwd(), "RData", sep = "/")
        if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

        # Create output file: change extension to .RData, and
        # add instrument name and "imp" for import to file name
        file.name <- gsub(".*/", "", sub("\\.txt", "", inputfile))
        outputfile <- paste("uCH4_", file.name, "_imp.RData", sep = "")

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
