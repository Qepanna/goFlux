#' Import function for Los Gatos Research GHG analyzers
#'
#' Imports single raw gas measurement files from the ultra-portable GHG analyzers
#' (UGGA and m-GGA) from Los Gatos Research
#' (\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}) with the extension .txt
#'
#' @param inputfile character string; the name of a file with the extension .txt
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
#'             in the following order: "CO2dry_ppm", CH4dry_ppb" and H2O_ppm".
#'             The default is \code{prec = c(0.2, 1.4, 50)}, which corresponds
#'             to the ultra-portable GGA (GLA132 series). For the micro
#'             ultra-portable GGA (GLA131 series), use
#'             \code{prec = c(0.35, 0.9, 200)}.
#'
#' @returns A data frame containing raw data from LGR GHG analyzer.
#'
#' @include GoFluxYourself-package.R
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument G2508, the date
#' is found in the column "Time".
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#'   \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} and
#'   \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item Torr for pressure
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
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}, or 1 ppb for
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}) to allow for more
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
#'          \code{\link[GoFluxYourself]{GAIA_import}},
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
#' file.path <- system.file("extdata", "LGR/LGR.txt", package = "GoFluxYourself")
#'
#' LGR_imp <- LGR_import(inputfile = file.path)
#'
#' @export
#'
LGR_import <- function(inputfile, date.format = "dmy", timezone = "UTC",
                       save = FALSE, keep_all = FALSE, prec = c(0.2, 1.4, 50)){

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
  POSIX.time <- DATE_TIME <- H2O_ppm <- CH4dry_ppb <- Time <- . <-
    CH4dry_ppm <- CO2dry_ppm <- POSIX.warning <- CO2_ppm <-
    CO2wet_ppm <- CH4_ppm <- CH4wet_ppm <- NULL

  # Load data file
  data.raw <- read.delim(inputfile, skip = 1, sep = ",") %>%
    # Clean column names
    `colnames<-`(gsub("\\.", "",
                      gsub("X.", "",
                           gsub("d_", "dry_",
                                gsub("__", "_", names(.))))))

  # Compensate for water vapor
  if(!any(grepl("CO2dry_ppm", names(data.raw)))){
    data.raw <- data.raw %>%
      rename(CO2wet_ppm = CO2_ppm) %>%
      mutate(CO2dry_ppm = CO2wet_ppm/(1-H2O_ppm/1000000))}
  if(!any(grepl("CH4dry_ppm", names(data.raw)))){
    data.raw <- data.raw %>%
      rename(CH4wet_ppm = CH4_ppm) %>%
      mutate(CH4dry_ppm = CH4wet_ppm/(1-H2O_ppm/1000000))}

  data.raw <- data.raw %>%
    # Remove rows at the end of the file
    drop_na(CO2dry_ppm) %>%
    # Convert ppm into ppb for CH4dry
    mutate(CH4dry_ppb = CH4dry_ppm*1000) %>%
    # Remove NAs and negative gas measurements, if any
    filter(CO2dry_ppm > 0) %>%
    filter(CH4dry_ppb > 0) %>%
    filter(H2O_ppm > 0) %>%
    # Replace characters in Time ("/" -> "-") and remove first space
    mutate(DATE_TIME = gsub("/", "-", sub(" ", "" , Time)))

  # Keep only useful columns for gas flux calculation
  if(keep_all == FALSE){
    data.raw <- data.raw %>%
      select(DATE_TIME, CO2dry_ppm, CH4dry_ppb, H2O_ppm)}

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
    stop(paste("An error occured while converting DATE and TIME into POSIX.time.",
               "Verify that 'date.format' corresponds to the column 'DATE' in",
               "the raw data file. Here is a sample:", data.raw$DATE_TIME[1]))
  } else data.raw$POSIX.time <- try.POSIX

  # Add a column for DATE alone
  data.raw <- data.raw %>% mutate(DATE = substr(POSIX.time, 0, 10))

  # Add instrument precision for each gas
  data.raw <- data.raw %>%
    mutate(CO2_prec = prec[1], CH4_prec = prec[2],  H2O_prec = prec[3])

  # Save cleaned data file
  if(save == TRUE){
    # Create RData folder in working directory
    RData_folder <- paste(getwd(), "RData", sep = "/")
    if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

    # Create output file: change extension to .RData, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.txt", "", inputfile))
    outputfile <- paste("LGR_", file.name, "_imp.RData", sep = "")

    save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
