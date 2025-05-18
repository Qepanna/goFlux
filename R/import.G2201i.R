#' Import function for the Picarro G2201-i Isotopic Analyzer
#'
#' Imports single raw gas measurement files from the Picarro G2201-i with the
#' extension .dat (\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} isotopic analyzer)
#'
#' @param inputfile character string; the name of a file with the extension .dat
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
#'             in the following order: "CO2dry_ppm", "CH4dry_ppb" and "H2O_ppm".
#'             The default is \code{prec = c(0.2, 50, 100)}.
#' @param CH4 character string; which column should be used for
#'            \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} concentration.
#'            By default, \code{CH4 = "HP_12CH4_dry"}.
#' @param CO2 character string; which column should be used for
#'            \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} concentration.
#'            By default, \code{CO2 = "12CO2_dry"}.
#' @param sum logical; if \code{sum = TRUE} (default), the arguments \code{CH4} and
#'            \code{CO2} are ignored and, instead, both fractions (carbon isotope
#'            12 and 13) of each gas, are summed after correction for water vapor,
#'            to obtain the total gas concentration. See the argument \code{range}
#'            for the selection between the HP or the HR measurement of
#'            \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} concentration.
#' @param range numerical value; at which threshold should the function select
#'              between high precision (HP) and high resolution (HR) of
#'              \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}
#'              concentration? The default is \code{range = 10}. If the methane
#'              concentration recorded in the column \code{HP_12CH4_dry} is above
#'              10 ppm, the concentration from the columns \code{HR_12CH4_dry}
#'              and \code{HR_13CH4} are used for the sum instead of HP.
#'
#' @returns A data frame containing raw data from Picarro G2201-i isotopic analyzer.
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument G2201-i, the date
#' is found in the column "DATE".
#'
#' The arguments \code{CH4} and \code{CO2} require the user to specify which
#' columns contain the two gases of interest among the following for \code{CH4}:
#' \itemize{
#'   \item HP_12CH4
#'   \item HP_12CH4_dry
#'   \item HP_13CH4
#'   \item HR_12CH4
#'   \item HR_12CH4_dry
#'   \item HR_13CH4}
#' And among the following for \code{CO2}:
#' \itemize{
#'   \item 12CO2
#'   \item 12CO2_dry
#'   \item 13CO2}
#' Note that, HP stands for \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}
#' concentration measured with strong line for high precision but limited range
#' and HR stands for \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}
#' concentration measured with high range (between 10 and 1000 ppm). In addition,
#' _dry stands for dry mole fraction of gases, corrected for water vapor. Finally,
#' 12 and 13 indicate each carbon isotope contained in each gas. If one selects
#' one of the non-dry mole fraction, it will be corrected for water vapor during
#' the import process. Note however that, even though there is no indication of
#' _dry with the 13CX fractions, they are, in fact, corrected for water vapor,
#' according to the manufacturer.
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#'         \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}
#'   \item percent (\%) for \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item Torr for pressure
#'   \item Celsius for temperature}
#' If your Picarro G2201-i uses different units, either convert the units after
#' import, change the settings on your instrument, or contact the maintainer of
#' this package for support.
#'
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[goFlux]{k.max}}) in the non-linear flux calculation
#' (\code{\link[goFlux]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm) to allow for more curvature,
#' especially for water vapor fluxes, or very long measurements, that are
#' normally curved. The default values given for instrument precision are the
#' ones found \href{https://www.picarro.com/environmental/products/g2201i_isotopic_analyzer}{online}
#' for the latest model of this instrument, available at the time of the
#' creation of this function (05-2025).
#'
#' @include goFlux-package.R
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
#'          \code{\link[goFlux]{import.GT5000}},
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
#' file.path <- system.file("extdata", "G2201i/2025/05/13/G2201i.dat", package = "goFlux")
#'
#' # Run function
#' imp.G2201i <- import.G2201i(inputfile = file.path)
#' @export

import.G2201i <- function(inputfile, date.format = "ymd", timezone = "UTC",
                          save = FALSE, keep_all = FALSE, prec = c(0.2, 50, 100),
                          CH4 = "HP_12CH4_dry", CO2 = "12CO2_dry",
                          sum = TRUE, range = 10){

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
  if (is.null(prec)) stop("'prec' is required") else{
    if(!is.numeric(prec)) stop("'prec' must be of class numeric") else{
      if(length(prec) != 3) stop("'prec' must be of length 3")}}
  if (sum != TRUE & sum != FALSE) stop("'sum' must be TRUE or FALSE")
  if (!is.numeric(range)) stop("'range' must be of class numeric")

  if (sum == FALSE) {
    # CH4
    if (length(CH4) != 1) stop("'CH4' must be of length 1")
    if (!is.character(CH4)) stop("'CH4' must be of class character")
    if (!any(grepl(CH4, c("HP_12CH4", "HP_12CH4_dry", "HP_13CH4",
                          "HR_12CH4", "HR_12CH4_dry", "HR_13CH4")))) {
      stop(paste("'CH4' must be one of the following:",
                 "'HP_12CH4', 'HP_12CH4_dry', 'HP_13CH4', 'HR_12CH4', 'HR_12CH4_dry', 'HR_13CH4'"))}
    # CO2
    if (length(CO2) != 1) stop("'CO2' must be of length 1")
    if (!is.character(CO2)) stop("'CO2' must be of class character")
    if (!any(grepl(CO2, c("12CO2", "12CO2_dry", "13CO2")))) {
      stop("'CO2' must be one of the following: '12CO2', '12CO2_dry', '13CO2'")}
  }

  # Assign NULL to variables without binding
  POSIX.warning <- import.error <- CH4dry_12C <- CH4dry_13C <- CH4dry_ppb <-
    CH4dry_ppm <- CO2dry_12C <- CO2dry_13C <- CO2dry_ppm <- DATE <- H2O <-
    H2O_pct <- H2O_ppm <- HP_12CH4_dry <- HP_13CH4 <- HR_12CH4_dry <-
    HR_13CH4 <- TIME <- X12CO2 <- X12CO2_dry <- X13CO2 <- . <- NULL

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data file
  try.import <- tryCatch(
    {read.delim(inputfile, sep = "")},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    if(sum == FALSE){
      # Import raw data file from G2201i (.dat)
      data.raw <- try.import %>%
        # Standardize column names
        setNames(gsub(paste("X", CO2, sep = ""), "CO2dry_ppm", names(.))) %>%
        setNames(gsub(CH4, "CH4dry_ppm", names(.))) %>%
        rename(H2O_pct = H2O) %>%
        # Convert column class automatically
        type.convert(as.is = TRUE) %>%
        # Convert percent into ppm for H2O and ppm into ppb for CH4
        mutate(H2O_ppm = H2O_pct*10000,
               CH4dry_ppb = CH4dry_ppm*1000) %>%
        # Add info about columns' origin
        mutate(CO2_origin = CO2, CH4_origin = CH4)

      # Correct for water vapor
      if(grepl("12CO2", CO2)) {data.raw <- data.raw %>%
        mutate(CO2dry_ppm = (CO2dry_ppm/(1-H2O_ppm/1000000)))}
      if(any(grepl(CH4, c("HP_12CH4", "HR_12CH4")))) {data.raw <- data.raw %>%
        mutate(CH4dry_ppm = (CH4dry_ppm/(1-H2O_ppm/1000000)))}

      # Keep only useful columns for gas flux calculation
      if(keep_all == FALSE){
        data.raw <- data.raw %>%
          select(DATE, TIME, CO2dry_ppm, CH4dry_ppb, H2O_ppm)}
    }

    if(sum == TRUE){
      # Import raw data file from G2201i (.dat)
      data.raw <- try.import %>%
        # Standardize column names
        rename(H2O_pct = H2O, CO2_12C = X12CO2,
               CO2dry_12C = X12CO2_dry, CO2dry_13C = X13CO2) %>%
        # Convert column class automatically
        type.convert(as.is = TRUE) %>%
        # Selection between HP_CH4 and HR_CH4
        mutate(res = if_else(HP_12CH4_dry > 10, "HR", "HP"),
               CH4dry_12C = if_else(HP_12CH4_dry > 10, HP_12CH4_dry, HR_12CH4_dry),
               CH4dry_13C = if_else(HP_12CH4_dry > 10, HP_13CH4, HR_13CH4)) %>%
        # Sum 13CO2 and 12CO2
        mutate(CO2dry_ppm = CO2dry_12C + CO2dry_13C) %>%
        # Sum 13CH4 and 12CH4
        mutate(CH4dry_ppm = CH4dry_12C + CH4dry_13C) %>%
        # Convert percent into ppm for H2O and ppm into ppb for CH4
        mutate(H2O_ppm = H2O_pct*10000,
               CH4dry_ppb = CH4dry_ppm*1000)

      # Keep only useful columns for gas flux calculation
      if(keep_all == FALSE){
        data.raw <- data.raw %>%
          select(DATE, TIME, CO2dry_ppm, CH4dry_ppb, H2O_ppm)}
    }

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
              "   column 'DATE' in the raw data file. Here is a sample: ",
              data.raw$DATE[1], "\n", call. = F)
    } else {

      data.raw$POSIX.time <- try.POSIX

      # Add instrument precision for each gas
      data.raw <- data.raw %>%
        mutate(CO2_prec = prec[1], CH4_prec = prec[2], H2O_prec = prec[3])

      # Save cleaned data file
      if(save == TRUE){
        # Create RData folder in working directory
        RData_folder <- paste(getwd(), "RData", sep = "/")
        if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

        # Create output file: change extension to .RData, and
        # add instrument name and "imp" for import to file name
        file.name <- gsub(".*/", "", sub("\\.dat", "", inputfile))
        outputfile <- paste("G2201i_", file.name, "_imp.RData", sep = "")

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
