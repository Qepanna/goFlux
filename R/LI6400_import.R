#' Import function for LI-COR GHG analyzer LI-6400
#'
#' Imports single raw gas measurement files from the LI-COR 6400
#' (\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}} GHG analyzer)
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
#'             in the following order: "CO2dry_ppm" and "H2O_ppm". The default
#'             is \code{prec = c(0.15, 20)}.
#'
#' @returns A data frame containing raw data from LI-COR GHG analyzer LI-6400.
#'
#' @details
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument LI-6400, the
#' date is found in one of the first lines in a format containing abbreviations,
#' for example "Thr Aug 6 2020", which would be the date format "mdy".
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#'   \item mmol/mol for \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item kPa for pressure
#'   \item \ifelse{html}{\out{cm<sup>3</sup> for volumes}}{\eqn{cm^[3] for volumes}{ASCII}}
#'   \item Celsius for temperature
#'   \item \ifelse{html}{\out{µmol photons m<sup>-2</sup>s<sup>-1</sup> for PAR}}{\eqn{µmol photons m^{-2}s^{-1} for PAR}{ASCII}}}
#' If your LI-COR LI-6400 uses different units, either convert the units after
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
#'          \code{\link[goFlux]{import.GasmetPD}},
#'          \code{\link[goFlux]{import.GT5000}},
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
#' file.path <- system.file("extdata", "LI6400/LI6400.txt", package = "goFlux")
#'
#' # Run function
#' imp.LI6400 <- import.LI6400(inputfile = file.path)

LI6400_import <- function(inputfile, date.format = "mdy", timezone = "UTC",
                          save = FALSE, keep_all = FALSE, prec = c(0.15, 20)){

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
  cham.close <- cham.open <- POSIX.time <- chamID <- DATE <- TIME <- H2O_mmol <-
    Etime <- CO2dry_ppm <- Meas.type <- plotID <- H2OS <- Cdry <- Obs <- PAR <-
    Tair <- Mode <- ETime <- HHMMSS <- Meas.type..NEE.ER. <- Plot. <- Press <-
    V4 <- V1 <- start.time <- met.date.warning <- flag <- offset <- Vcham <-
    Area <- Pcham <- Tcham <- H2O_ppm <- TCham <- PARo <- import.error <- NULL

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data file
  try.import <- tryCatch(
    {read.delim(inputfile, header = F, nrows = 10)},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    # Find how many rows need to be skipped
    skip.rows <- tryapply(seq(1:30), function(i) {
      if (read.delim(inputfile, header = F, skip = i)[1,1] == "$STARTOFDATA$") {
        return(i) }
    }) %>% unlist %>% as.numeric() +1

    # Import metadata from LI6400 (.txt)
    metadata <- read.delim(inputfile, header = F, comment.char = "<") %>%
      filter(V1 == "Const=" | V4 == "")

    # Extract date from metadata
    tryCatch(
      {if (date.format == "dmy") {
        try.met.date <- dmy(substr(metadata[2,1], 5, nchar(metadata[2,1])-9), tz = timezone)
      } else if (date.format == "mdy") {
        try.met.date <- mdy(substr(metadata[2,1], 5, nchar(metadata[2,1])-9), tz = timezone)
      } else if (date.format == "ymd") {
        try.met.date <- ymd(substr(metadata[2,1], 5, nchar(metadata[2,1])-9), tz = timezone)
      }}, warning = function(w) {met.date.warning <<- "date.format.error"}
    )

    if(isTRUE(met.date.warning == "date.format.error")){
      warning("Error occurred in file ", inputfile.name, ":\n",
              "   An error occured while converting DATE and TIME into POSIX.time.\n",
              "   Verify that the 'date.format' you specified (", date.format,
              ") matches the\n",
              "   date format in the raw data file. Here is a sample: ",
              substr(metadata[2,1], 5, nchar(metadata[2,1])-9)[1], "\n", call. = F)
    } else {

      met.date <- try.met.date

      # Import raw data file from LI6400 (.txt)
      data.raw <- read.delim(inputfile, skip = skip.rows) %>%
        # Standardize column names
        rename(plotID = Plot., Meas.type = Meas.type..NEE.ER., TIME = HHMMSS,
               Etime = ETime, Tcham = Tair, Pcham = Press, CO2dry_ppm = Cdry,
               H2O_mmol = H2OS, PAR = PARo) %>% select(!TCham) %>%
        # Remove comments
        filter(!CO2dry_ppm == "") %>% filter(!Obs == "Obs") %>%
        # Convert column class automatically
        type.convert(as.is = TRUE) %>%
        # plotID must be as.character
        mutate(plotID = as.character(plotID)) %>%
        # Remove Mode == 2 (indicates when a measurement ends)
        filter(!Mode == 2) %>% select(!Mode) %>%
        # As the LICOR only saves rows when you have passed all prompts after
        # pressing start, Etime below 4 seconds is not possible.
        filter(Etime > 4) %>%
        # Convert mmol into ppm for H2O
        mutate(H2O_ppm = H2O_mmol*1000) %>%
        # Create new columns containing date and time (POSIX format)
        mutate(DATE = as.character(met.date),
               POSIX.time = as.POSIXct(paste(DATE, TIME), tz = timezone)) %>%
        # Extract other useful information from metadata
        mutate(Area = as.numeric(metadata[which(metadata[,3] == "Area")[1],4]),
               Vcham = as.numeric(metadata[which(metadata[,3] == "Vtot")[1],4]),
               offset = as.numeric(metadata[which(metadata[,3] == "Offset")[1],4])) %>%
        # Convert Vcham from cm3 to L
        mutate(Vcham = Vcham/1000) %>%
        # Detect new observations
        arrange(POSIX.time) %>%
        mutate(Obs = ifelse(is.na(Etime - lag(Etime)), 0, Etime - lag(Etime))) %>%
        mutate(Obs = rleid(cumsum(Obs < 0))) %>%
        # Create a unique chamber ID
        mutate(chamID = paste(Obs, plotID, Meas.type, sep = "_")) %>%
        # Add time related variables (POSIX.time)
        group_by(chamID) %>%
        mutate(cham.close = first(na.omit(POSIX.time)),
               start.time = cham.close,
               cham.open = last(na.omit(POSIX.time)),
               Etime = as.numeric(POSIX.time - start.time, units = "secs")) %>%
        ungroup() %>%
        # Add flag
        mutate(flag = 1)

      # Keep only useful columns for gas flux calculation
      if(keep_all == FALSE){
        data.raw <- data.raw %>%
          select(POSIX.time, DATE, TIME, chamID, Obs, Meas.type, CO2dry_ppm, H2O_ppm,
                 Tcham, Pcham, PAR, Area, Vcham, offset, Obs, plotID, Etime, flag,
                 start.time, cham.close, cham.open)}

      # Add instrument precision for each gas
      data.raw <- data.raw %>%
        mutate(CO2_prec = prec[1], H2O_prec = prec[2])

      # New function name
      if (as.character(match.call()[[1]]) == "LI6400_import") {
        warning(paste("All import functions have changed names in this new version of goFlux.",
                      "\nIn the future, use import.LI6400() instead of LI6400_import()"), call. = FALSE)
      }

      # Save cleaned data file
      if(save == TRUE){
        # Create RData folder in working directory
        RData_folder <- paste(getwd(), "RData", sep = "/")
        if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

        # Create output file: change extension to .RData, and
        # add instrument name and "imp" for import to file name
        file.name <- gsub(".*/", "", sub("\\.txt", "", inputfile))
        outputfile <- paste("LI6400_", file.name, "_imp.RData", sep = "")

        save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

        message(inputfile.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
      }

      if(save == FALSE){
        return(data.raw)
      }
    }
  }
}

#' @export
#' @rdname LI6400_import
import.LI6400 <- LI6400_import
