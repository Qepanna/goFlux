#' Import function for LI-COR GHG analyzer LI-8100
#'
#' Imports single raw gas measurement files from the LI-COR 8100
#' (\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}} GHG analyzer)
#'
#' @param inputfile character string; the name of a file with the extension .81x
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
#'             in the following order: "CO2dry_ppm" and "H2O_ppm". The default
#'             is \code{prec = c(1, 10)}.
#'
#' @returns A data frame containing raw data from LI-COR GHG analyzer LI-8100.
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument LI-8100, the
#' date is found in the column "Date".
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#'   \item mmol/mol for \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item Celsius for temperature}
#' If your LI-COR LI-8100 uses different units, either convert the units after
#' import, change the settings on your instrument, or contact the maintainer of
#' this package for support.
#'
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[goFlux]{k.max}}) in the non-linear flux calculation
#' (\code{\link[goFlux]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm) to allow for more curvature, especially
#' for water vapor fluxes, or very long measurements, that are normally curved.
#' The default values given for instrument precision are the ones provided by
#' the manufacturer upon request, for the latest model of this instrument
#' available at the time of the creation of this function (11-2023).
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
#'          \code{\link[goFlux]{import.LI6400}},
#'          \code{\link[goFlux]{import.LI7810}},
#'          \code{\link[goFlux]{import.LI7820}},
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
#' file.path <- system.file("extdata", "LI8100/LI8100.81x", package = "goFlux")
#'
#' # Run function
#' imp.LI8100 <- import.LI8100(inputfile = file.path)

LI8100_import <- function(inputfile, date.format = "ymd", timezone = "UTC",
                          save = FALSE, keep_all = FALSE, prec = c(1, 10)) {

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
      if(length(prec) != 2) stop("'prec' must be of length 2")}}

  # Assign NULL to variables without binding
  Type <- Etime <- Tcham <- Pressure <- H2O <- . <- Cdry <- V1 <- V2 <- V3 <-
    V4 <- H2O_mmol <- DATE_TIME <- Obs <- cham.close <- cham.open <- plotID <-
    deadband <- start.time <- obs.start <- POSIX.time <- import.error <-
    Date <- CO2dry_ppm <- POSIX.warning <- H2O_ppm <- Pcham <- NULL

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data file
  try.import <- tryCatch(
    {read.delim(inputfile)},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    # Find how many rows need to be skipped
    skip.rows <- as.numeric(which(try.import == "Type"))[1]

    # Import raw data file from LI8100 (.81x)
    data.raw <- read.delim(inputfile, skip = skip.rows) %>%
      # Keep only Type == 1, as everything else is metadata
      filter(Type == "1") %>%
      # Standardize column names
      rename(DATE_TIME = Date, Pcham = Pressure, H2O_mmol = H2O, CO2dry_ppm = Cdry) %>%
      # Convert column class automatically
      type.convert(as.is = TRUE) %>%
      # Convert mmol into ppm for H2O
      mutate(H2O_ppm = H2O_mmol*1000) %>%
      # Detect new observations
      arrange(DATE_TIME) %>%
      mutate(Obs = ifelse(is.na(Etime - lag(Etime)), 0, Etime - lag(Etime))) %>%
      mutate(Obs = rleid(cumsum(Obs < 0)))

    # Keep only useful columns for gas flux calculation
    if(keep_all == FALSE){
      data.raw <- data.raw %>%
        select(Obs, DATE_TIME, Etime, H2O_ppm, CO2dry_ppm,
               Tcham, Pcham, V1, V2, V3, V4)}

    # Create a new column containing date and time (POSIX format)
    tryCatch(
      {if(date.format == "dmy"){
        try.POSIX <- as.POSIXct(dmy_hms(data.raw$DATE_TIME, tz = timezone))
      } else if(date.format == "mdy"){
        try.POSIX <- as.POSIXct(mdy_hms(data.raw$DATE_TIME, tz = timezone))
      } else if(date.format == "ymd"){
        try.POSIX <- as.POSIXct(ymd_hms(data.raw$DATE_TIME, tz = timezone))
      }}, warning = function(w) {POSIX.warning <<- "date.format.error"}
    )

    if(isTRUE(POSIX.warning == "date.format.error")){
      warning("Error occurred in file ", inputfile.name, ":\n",
              "   An error occured while converting DATE and TIME into POSIX.time.\n",
              "   Verify that the 'date.format' you specified (", date.format,
              ") corresponds to the\n",
              "   column 'DATE' in the raw data file. Here is a sample: ",
              data.raw$DATE_TIME[1], "\n", call. = F)
    } else {

      data.raw$POSIX.time <- try.POSIX

      # Import metadata from LI8100 (.81x)
      meta <- read.delim(inputfile, header = F) %>% select(c(1:2)) %>%
        filter(V1 == "Obs#:" | V1 == "Label:" | V1 == "Area:" | V1 == "Vcham:" |
                 V1 == "Offset:" | V1 == "Dead Band:")

      if (nrow(meta)/6 == ceiling(nrow(meta)/6)) {
        metadata <- meta %>% reframe(
          Obs = as.numeric(.[which(.[,1] == "Obs#:"),2]),
          plotID = .[which(.[,1] == "Label:"),2],
          Area = as.numeric(.[which(.[,1] == "Area:"),2]),
          Vcham = as.numeric(.[which(.[,1] == "Vcham:"),2]),
          offset = as.numeric(.[which(.[,1] == "Offset:"),2]),
          deadband = as.numeric(ms(meta[which(meta[,1] == "Dead Band:"),2]), units = "secs"))
      } else {
        metadata <- meta %>% reframe(
          Obs = as.numeric(.[which(.[,1] == "Obs#:"),2]),
          plotID = .[which(.[,1] == "Label:"),2],
          Area = as.numeric(.[which(.[,1] == "Area:"),2]),
          Vcham = as.numeric(.[which(.[,1] == "Vcham:"),2]),
          offset = as.numeric(.[which(.[,1] == "Offset:"),2]),
          deadband = c(as.numeric(ms(meta[which(meta[,1] == "Dead Band:"),2]), units = "secs"),
                       last(as.numeric(ms(meta[which(meta[,1] == "Dead Band:"),2]), units = "secs"))))
      }

      # Add metadata to data.raw
      data.raw <- data.raw %>%
        left_join(metadata, by = "Obs") %>% group_by(Obs) %>%
        # Calculate cham.close, cham.open, flag and correct negative values of Etime
        mutate(cham.close = POSIX.time[which(Etime == 0)],
               cham.open = max(na.omit(POSIX.time)),
               obs.start = min(na.omit(POSIX.time))) %>%
        ungroup() %>%
        mutate(DATE = substr(POSIX.time, 0, 10),
               chamID = paste(plotID, Obs, sep = "_"),
               start.time = cham.close + deadband,
               Etime = as.numeric(POSIX.time - start.time, units = "secs"),
               flag = if_else(between(POSIX.time, start.time, cham.open), 1, 0))

      # Add instrument precision for each gas
      data.raw <- data.raw %>%
        mutate(CO2_prec = prec[1], H2O_prec = prec[2])

      # New function name
      if (as.character(match.call()[[1]]) == "LI8100_import") {
        warning(paste("All import functions have changed names in this new version of goFlux.",
                      "\nIn the future, use import.LI8100() instead of LI8100_import()"), call. = FALSE)
      }

      # Save cleaned data file
      if(save == TRUE){
        # Create RData folder in working directory
        RData_folder <- paste(getwd(), "RData", sep = "/")
        if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

        # Create output file: change extension to .RData, and
        # add instrument name and "imp" for import to file name
        file.name <- gsub(".*/", "", sub("\\.81x", "", inputfile))
        outputfile <- paste("LI8100_", file.name, "_imp.RData", sep = "")

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
#' @rdname LI8100_import
import.LI8100 <- LI8100_import
