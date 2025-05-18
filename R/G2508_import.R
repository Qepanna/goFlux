#' Import function for Picarro G2508 GHG analyzer
#'
#' Imports single raw gas measurement files from the Picarro G2508 with the
#' extension .dat (\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}},
#' \ifelse{html}{\out{NH<sub>3</sub>}}{\eqn{NH[3]}{ASCII}}, and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}} GHG analyzer)
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
#'             in the following order: "CO2dry_ppm", "CH4dry_ppb", "N2Odry_ppb",
#'             "NH3dry_ppb" and "H2O_ppm". The default is
#'             \code{prec = c(0.24, 0.3, 5, 0.16, 500)}.
#'
#' @returns A data frame containing raw data from Picarro G2508 GHG analyzer.
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument G2508, the date
#' is found in the column "DATE".
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#'   \item ppb for \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#'         \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} and
#'         \ifelse{html}{\out{NH<sub>3</sub>}}{\eqn{NH[3]}{ASCII}}
#'   \item mmol/mol for \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item Torr for pressure
#'   \item Celsius for temperature}
#' If your Picarro G2508 uses different units, either convert the units after
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
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} and
#' \ifelse{html}{\out{NH<sub>3</sub>}}{\eqn{NH[3]}{ASCII}}) to allow for more
#' curvature, especially for water vapor fluxes, or very long measurements, that
#' are normally curved. The default values given for instrument precision are
#' the ones found \href{https://www.picarro.com/environmental/products/g2508_gas_concentration_analyzer}{online}
#' for the latest model of this instrument, available at the time of the
#' creation of this function (11-2023).
#'
#' @include goFlux-package.R
#'
#' @seealso Use the wrapper function \code{\link[goFlux]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso See also, import functions for other instruments:
#'          \code{\link[goFlux]{import.DX4015}},
#'          \code{\link[goFlux]{import.EGM5}},
#'          \code{\link[goFlux]{import.G2201i}},
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
#' file.path <- system.file("extdata", "G2508/2023/08/01/G2508.dat", package = "goFlux")
#'
#' # Run function
#' imp.G2508 <- import.G2508(inputfile = file.path)

G2508_import <- function(inputfile, date.format = "ymd", timezone = "UTC",
                         save = FALSE, keep_all = FALSE,
                         prec = c(0.24, 0.3, 5, 0.16, 500)){

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
      if(length(prec) != 5) stop("'prec' must be of length 5")}}

  # Assign NULL to variables without binding
  H2O <- N2O_dry30s <- N2O_dry <- CH4_dry <- CO2_dry <- TIME <- DATE <- NH3 <-
    N2Odry_ppb <- CH4dry_ppb <- H2O_ppm <- NH3wet_ppm <- H2O_mmol <-
    POSIX.warning <- N2Odry_30s_ppb <- CO2dry_ppm <- CH4dry_ppm <-
    NH3dry_ppb <- N2Odry_30s_ppm <- N2Odry_ppm <- import.error <- NULL

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

    # Import raw data file from G2508 (.dat)
    data.raw <- try.import %>%
      # Standardize column names
      rename(CO2dry_ppm = CO2_dry, CH4dry_ppm = CH4_dry, N2Odry_ppm = N2O_dry,
             N2Odry_30s_ppm = N2O_dry30s, H2O_mmol = H2O) %>%
      # Convert column class automatically
      type.convert(as.is = TRUE) %>%
      # Convert mmol into ppm for H2O and ppm into ppb for N2O and CH4
      mutate(H2O_ppm = H2O_mmol*1000,
             N2Odry_ppb = N2Odry_ppm*1000,
             N2Odry_30s_ppb = N2Odry_30s_ppm*1000,
             CH4dry_ppb = CH4dry_ppm*1000) %>%
      # Water compensation in NH3
      mutate(NH3wet_ppm = NH3/1000) %>%
      mutate(NH3dry_ppb = (NH3wet_ppm/(1-H2O_ppm/1000000))*1000)

    # Keep only useful columns for gas flux calculation
    if(keep_all == FALSE){
      data.raw <- data.raw %>%
        select(DATE, TIME, CO2dry_ppm, CH4dry_ppb, N2Odry_ppb, N2Odry_30s_ppb,
               NH3dry_ppb, H2O_ppm)}

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
        mutate(CO2_prec = prec[1], CH4_prec = prec[2], N2O_prec = prec[3],
               NH3_prec = prec[4], H2O_prec = prec[5])

      # New function name
      if (as.character(match.call()[[1]]) == "G2508_import") {
        warning(paste("All import functions have changed names in this new version of goFlux.",
                      "\nIn the future, use import.G2508() instead of G2508_import()"), call. = FALSE)
      }

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
#' @rdname G2508_import
import.G2508 <- G2508_import
