#' Import function for the automated chamber ECOFlux (GAIA2TECH) synced with
#' multiple chambers and instruments
#'
#' Imports single raw gas measurement files from the automated chamber
#' ECOFlux (GAIA2TECH) with the extension .csv
#' (LI-7810: \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}} / LI7820:
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}})
#'
#' @param inputfile character string; the name of a file with the extension .csv
#' @param date.format character string; specifies the date format found in the
#'                    raw data file. Choose one of the following: "dmy", "ymd",
#'                    or "mdy". Default is "ymd", as it is the date format from
#'                    the example data file provided.
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param pivot character string; either "long" or "wide". If \code{pivot = "long"},
#'              each column containing information about \code{Tsoil},
#'              \code{Tcham}, \code{SWC}, \code{PAR} and operating status
#'              (\code{Op.stat}) will be saved in a single column per parameter.
#'              If \code{pivot = "wide"}, the default display of one column per
#'              chamber per parameter will be used.
#' @param active logical; if \code{active = TRUE}, preserve data for active
#'               chambers only.
#' @param flag numeric vector; indicates the operating status that should be used
#'             for the flux calculation. Default is \code{flag = c(7,11)}, where
#'             7 indicates "Chamber Idle Closed Clear" and 11 indicates
#'             "Chamber Idle Closed Dark".
#' @param background logical; if \code{background = FALSE}, removes all data from
#'                   \code{activ.cham == "Background"}.
#' @param save logical; if \code{save = TRUE}, saves the file as an .RData file
#'             in a RData folder in the current working directory. If
#'             \code{save = FALSE}, returns the file in the Console, or load in
#'             the Environment if assigned to an object.
#' @param prec numerical vector; the precision of the instrument for each gas,
#'             in the following order: "CO2dry_ppm", "CH4dry_ppb", "N2Odry_ppb"
#'             "H2O_ppm_LI7810" and "H2O_ppm_LI7820". The default is
#'             \code{prec = c(3.5, 0.6, 0.4, 45, 45)}.
#' @param Op.stat.col,PAR.col,Tcham.col,Tsoil.col,SWC.col,CH.col character
#'        string; a pattern to match all columns that fit the corresponding
#'        parameter. For example, all columns containing the pattern "3C07_Sunlight"
#'        will be renamed with the pattern "_PAR". Then, if \code{pivot = "long"},
#'        all columns with the pattern "_PAR" will be merged together.
#' @param CO2.col,CH4.col,H2O1.col,N2O.col,H2O2.col character string; a pattern
#'        to match the columns containing the corresponding gas measurements.
#'        \code{H2O1.col} must be the same instrument as \code{CO2.col} and
#'        \code{CH4.col}, and \code{H2O2.col} must be the same instrument as
#'        \code{N2O.col}.
#'
#' @returns A data frame containing raw data from the automated chamber
#'          ECOFlux (GAIA2TECH)
#'
#' @details
#' This function has been designed for the automated chamber ECOFlux (GAIA2TECH)
#' synced with up to 9 chambers and two GHG analyzers from LI-COR (LI-7810:
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}} / LI7820:
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}). If this function
#' could be useful for you, but does not meet your needs, please contact the
#' maintainer of this package for potential adaptations.
#'
#' In \code{date.format}, the date format refers to a date found in the raw data
#' file, not the date format in the file name. For the instrument GAIA the
#' date is found in the column "Titles:".
#'
#' The arguments \code{PAR.col}, \code{Tcham.col},
#' \code{Tsoil.col} and \code{SWC.col} correspond to different types of probes
#' linked to the ECOFlux chamber: PAR, chamber temperature, soil
#' temperature and soil water content volumetric, respectively. The argument
#' \code{Op.stat.col} corresponds to the columns Operating Status of each
#' chamber. The argument \code{CH.col} indicates a character string preceding
#' the chamber number for each column of the raw data. For example, the column
#' "COM5A010C06_OperatingStatus" in the raw data file contains the Operating
#' Status for the chamber 1 if \code{CH.col = "COM5A0"} and
#' \code{Op.stat.col = "0C06_OperatingStatus"}. If columns are absent from the
#' raw data, the arguments are ignored.
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#'   \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item ppb for \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} and
#'   \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}}
#'   \item kPa for pressure
#'   \item volumetric water content (\%) for soil moisture
#'   \item Celsius for temperature
#'   \item \ifelse{html}{\out{µmol photons m<sup>-2</sup>s<sup>-1</sup> for PAR}}{\eqn{µmol photons m^{-2}s^{-1} for PAR}{ASCII}}}
#' If your LI-COR instruments (LI-7810 and LI-7820) use different units, either
#' convert the units after import, change the settings on your instrument, or
#' contact the maintainer of this package for support.
#'
#' As opposed to the other import functions, there is no option to "keep_all" with
#' this instrument. If you would like to import additional data using this
#' function, please contact the maintainer of this package for support.
#'
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[goFlux]{k.max}}) in the non-linear flux calculation
#' (\code{\link[goFlux]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm for
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}, or 1 ppb for
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} and
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}}) to allow for more
#' curvature, especially for water vapor fluxes, or very long measurements, that
#' are normally curved. The default values given for instrument precision are
#' the ones found online for the latest models of the
#' \href{https://www.licor.com/env/products/trace-gas/LI-7810}{LI-7810} and
#' \href{https://www.licor.com/env/products/trace-gas/LI-7820}{LI-7820},
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
#' # Examples on how to use:
#' file.path <- system.file("extdata", "GAIA/GAIA.csv", package = "goFlux")
#'
#' imp.GAIA <- import.GAIA(inputfile = file.path)
#'
#' @export
#'
GAIA_import <- function(inputfile, date.format = "ymd", timezone = "UTC",
                        pivot = "long", active = TRUE, flag = c(7,11),
                        background = FALSE, save = FALSE,
                        prec = c(3.5, 0.6, 0.4, 45, 45),
                        CH.col = "COM5A0",
                        SWC.col = "1C08_Soil.Moisture",
                        Tsoil.col = "1C07_Soil.Temperature",
                        Tcham.col = "2C07_Chamber.Temperature",
                        PAR.col = "3C07_Sunlight",
                        Op.stat.col = "0C06_OperatingStatus",
                        CO2.col = "XT2C05_CO2",
                        CH4.col = "XT2C04_CH4",
                        H2O1.col = "XT2C06_H2O",
                        N2O.col = "XT3C04_N2O",
                        H2O2.col = "XT3C05_H2O"){

  # Check arguments
  if(missing(inputfile)) stop("'inputfile' is required")
  if(!is.character(inputfile)) stop("'inputfile' must be of class character")
  if(length(date.format) != 1) stop("'date.format' must be of length 1")
  if(!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}
  if(!is.character(timezone)) stop("'timezone' must be of class character")
  if(save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")
  if(active != TRUE & active != FALSE) stop("'active' must be TRUE or FALSE")
  if(background != TRUE & background != FALSE) stop("'background' must be TRUE or FALSE")
  if(length(pivot) != 1) stop("'pivot' must be of length 1")
  if(!any(grepl(pivot, c("long", "wide")))) {
    stop("'pivot' must be of class character and one of the following: 'long' or 'wide'")}
  if(is.null(prec)) stop("'prec' is required") else{
    if(!is.numeric(prec)) stop("'prec' must be of class numeric") else{
      if(length(prec) != 5) stop("'prec' must be of length 5")}}
  if(!is.numeric(flag)) stop("'flag' must be of class numeric")

  # Column names
  if(!is.character(H2O2.col)) stop("'H2O2.col' must be of class character")
  if(!is.character(N2O.col)) stop("'N2O.col' must be of class character")
  if(!is.character(H2O1.col)) stop("'H2O1.col' must be of class character")
  if(!is.character(CH4.col)) stop("'CH4.col' must be of class character")
  if(!is.character(CO2.col)) stop("'CO2.col' must be of class character")
  if(!is.character(PAR.col)) stop("'PAR.col' must be of class character")
  if(!is.character(Tcham.col)) stop("'Tcham.col' must be of class character")
  if(!is.character(Tsoil.col)) stop("'Tsoil.col' must be of class character")
  if(!is.character(SWC.col)) stop("'SWC.col' must be of class character")
  if(!is.character(CH.col)) stop("'CH.col' must be of class character")
  if(!is.character(Op.stat.col)) stop("'Op.stat.col' must be of class character")

  # Assign NULL to variables without binding
  POSIX.time <- activ.cham <- DATE_TIME <- start.time <- Obs <- SEQUENCE <-
    Titles. <- cham.probe <- chamID <- obs.start <- rbind.fill <- cham.open <-
    cham.close <- H2O_ppm_LI7820 <- N2Odry_ppb <- import.error <- . <-
    H2O_ppm_LI7810 <- CH4dry_ppb <- CO2dry_ppm <- POSIX.warning <- Op.stat <-
    Tsoil <- Tcham <- SWC <- PAR <- NULL

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data file
  try.import <- tryCatch(
    {read.delim(inputfile, skip = 1, colClasses = "character")},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    # Operating status missing?
    if(!any(grepl(paste("\\<", Op.stat.col, "\\>", sep = ""), names(try.import)))){
      warning(paste("In the file", inputfile.name, "the matching string for Operating",
                    "status (Op.stat.col) was not found in column names. By default,",
                    "Operating Status was set to 2 (Chamber Idle Open) for all measurements."),
              call. = F)}

    # Import raw data file from GAIA (.csv)
    data.raw <- try.import %>%
      # Remove first row containing units
      filter(!Titles. == 'Units:') %>%
      # Modify useful column names
      setNames(gsub(CH.col, "CH", names(.))) %>%
      setNames(gsub(Tsoil.col, "_Tsoil", names(.))) %>%
      setNames(gsub(Tcham.col, "_Tcham", names(.))) %>%
      setNames(gsub(SWC.col, "_SWC", names(.))) %>%
      setNames(gsub(PAR.col, "_PAR", names(.))) %>%
      setNames(gsub(Op.stat.col, "_Op.stat", names(.))) %>%
      # Extract information about light/dark measurements
      mutate(cover = if_else(grepl("Opaque", SEQUENCE), "Dark", if_else(
        grepl("Translucent", SEQUENCE), "Clear", NA))) %>%
      # Extract information about active chamber
      mutate(SEQUENCE = substr(SEQUENCE, 9, 9),
             SEQUENCE = ifelse(SEQUENCE == "", "Background", SEQUENCE),
             SEQUENCE = ifelse(SEQUENCE == "n", "ExecutionPlan", SEQUENCE)) %>%
      dplyr::rename(DATE_TIME = Titles., activ.cham = SEQUENCE) %>%
      # Gas measurements from GHG analyzers need to be renamed manually
      # LI-7810: CO2dry_ppm, CH4dry_ppb, H2O_ppm_LI7810
      setNames(gsub(CO2.col, "CO2dry_ppm", names(.))) %>%
      setNames(gsub(CH4.col, "CH4dry_ppb", names(.))) %>%
      setNames(gsub(H2O1.col, "H2O_ppm_LI7810", names(.))) %>%
      # LI-7820: N2Odry_ppb, H2O_ppm_LI7820
      setNames(gsub(N2O.col, "N2Odry_ppb", names(.))) %>%
      setNames(gsub(H2O2.col, "H2O_ppm_LI7820", names(.))) %>%
      # Detect new observations (Obs) and give a chamber UniqueID (chamID)
      arrange(DATE_TIME) %>%
      mutate(Obs = rleid(activ.cham),
             chamID = ifelse(activ.cham == "Background", paste(activ.cham, "_", Obs, sep = ""),
                             ifelse(activ.cham == "ExecutionPlan", paste(activ.cham, "_", Obs, sep = ""),
                                    paste("CH", activ.cham, "_", Obs, sep = "")))) %>%
      # Select only useful columns
      select(contains(c("DATE_TIME", "ChamID", "activ.cham", "Tsoil", "Tcham",
                        "SWC", "cover", "PAR", "Op.stat", "ppm", "ppb"))) %>%
      # Convert column class automatically
      type.convert(as.is = TRUE) %>%
      # Make sure that all gas data are class numerical
      mutate_at(c("CO2dry_ppm", "CH4dry_ppb", "H2O_ppm_LI7810", "N2Odry_ppb",
                  "H2O_ppm_LI7820"), as.numeric) %>%
      # Remove negative gas measurements, if any
      filter(CO2dry_ppm >= 0 | is.na(CO2dry_ppm)) %>%
      filter(CH4dry_ppb >= 0 | is.na(CH4dry_ppb)) %>%
      filter(H2O_ppm_LI7810 >= 0 | is.na(H2O_ppm_LI7810)) %>%
      filter(N2Odry_ppb >= 0 | is.na(N2Odry_ppb)) %>%
      filter(H2O_ppm_LI7820 >= 0 | is.na(H2O_ppm_LI7820))

    # Group together all columns containing information and merge data
    if(pivot == "long"){ # pivot long: only one column per parameter

      data.pivot <- data.raw

      ### Operating Status from each active chamber
      if(ncol(select(data.raw, contains("Op.stat"))) > 0){
        Op.stat <- data.raw %>% select(DATE_TIME, contains("Op.stat")) %>%
          pivot_longer(contains("Op.stat"), values_to = "Op.stat", names_to = "cham.probe") %>%
          mutate(cham.probe = substr(cham.probe, 3, 3))

        data.pivot <- data.pivot %>%
          # Operating status
          full_join(Op.stat, by = c("DATE_TIME")) %>%
          select(!contains("_Op.stat"))
      }

      ### Soil temperature from each active chamber
      if(ncol(select(data.raw, contains("Tsoil"))) > 0){
        Tsoil <- data.raw %>% select(DATE_TIME, contains("Tsoil")) %>%
          pivot_longer(contains("Tsoil"), values_to = "Tsoil", names_to = "cham.probe") %>%
          mutate(cham.probe = substr(cham.probe, 3, 3))

        data.pivot <- data.pivot %>%
          # Soil temperature
          full_join(Tsoil, by = c("DATE_TIME", "cham.probe")) %>%
          select(!contains("_Tsoil"))
      }

      ### Air temperature from each active chamber
      if(ncol(select(data.raw, contains("Tcham"))) > 0){
        Tcham <- data.raw %>% select(DATE_TIME, contains("Tcham")) %>%
          pivot_longer(contains("Tcham"), values_to = "Tcham", names_to = "cham.probe") %>%
          mutate(cham.probe = substr(cham.probe, 3, 3))

        data.pivot <- data.pivot %>%
          # Air temperature
          full_join(Tcham, by = c("DATE_TIME", "cham.probe")) %>%
          select(!contains("_Tcham"))
      }

      ### Soil water content from each active chamber
      if(ncol(select(data.raw, contains("SWC"))) > 0){
        SWC <- data.raw %>% select(DATE_TIME, contains("SWC")) %>%
          pivot_longer(contains("SWC"), values_to = "SWC", names_to = "cham.probe") %>%
          mutate(cham.probe = substr(cham.probe, 3, 3))

        data.pivot <- data.pivot %>%
          # Soil water content
          full_join(SWC, by = c("DATE_TIME", "cham.probe")) %>%
          select(!contains("_SWC"))
      }

      ### PAR from each active chamber
      if(ncol(select(data.raw, contains("PAR"))) > 0){
        PAR <- data.raw %>% select(DATE_TIME, contains("PAR")) %>%
          pivot_longer(contains("PAR"), values_to = "PAR", names_to = "cham.probe") %>%
          mutate(cham.probe = substr(cham.probe, 3, 3))

        data.pivot <- data.pivot %>%
          # PAR
          full_join(PAR, by = c("DATE_TIME", "cham.probe")) %>%
          select(!contains("_PAR"))
      }

      data.raw <- data.pivot
    }

    # Group together all columns containing information and merge data
    if(pivot == "wide"){ # keep wide: one column per instrument per parameter

      # Operating Status from each active chamber
      if(ncol(select(data.raw, contains("Op.stat"))) > 0){
        Op.stat <- data.raw %>% select(DATE_TIME, contains("Op.stat")) %>%
          pivot_longer(contains("Op.stat"), values_to = "Op.stat", names_to = "cham.probe") %>%
          mutate(cham.probe = substr(cham.probe, 3, 3))

        data.raw <- data.raw %>%
          # Only operating status is pivoted long
          left_join(Op.stat, by = c("DATE_TIME")) %>%
          select(!contains("_Op.stat"))
      }
    }

    # Add Op.stat if it cannot be found
    if(!any(grepl(paste("\\<", Op.stat.col, "\\>", sep = ""), names(try.import)))){
      data.raw$Op.stat <- 2
      data.raw$cham.probe <- NA
    }

    # Remove measurements from non-active chambers
    if(active == TRUE){
      Background <- data.raw %>% filter(grepl("Background", chamID))
      data.raw <- data.raw %>% filter(activ.cham == cham.probe) %>%
        rbind.fill(Background)
    }

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
              "   column 'DATE' in the raw data file. Here is a sample: ",
              data.raw$DATE_TIME[1], "\n", call. = F)
    } else {

      data.raw$POSIX.time <- try.POSIX

      # Add other useful variables (DATE, flag)
      data.raw <- data.raw %>%
        mutate(DATE = substr(POSIX.time, 0, 10),
               flag = ifelse(grepl(paste(flag, collapse = "|"), Op.stat), 1, 0)) %>%
        # Remove flag from Background
        mutate(flag = if_else(grepl("Background", chamID), 0, flag))

      # Calculate chamber closure and chamber opening
      data.time <- data.raw %>% select(chamID, flag, POSIX.time) %>%
        filter(flag == 1) %>% group_by(chamID) %>%
        summarise(cham.close = first(POSIX.time),
                  cham.open = last(POSIX.time)) %>% ungroup()

      # Calculate Etime
      Etime <- data.raw %>% full_join(data.time, by = "chamID") %>%
        select(POSIX.time, chamID, cham.close, cham.open) %>%
        mutate(start.time = cham.close) %>%
        filter(!grepl("Background", chamID)) %>% group_by(chamID) %>%
        mutate(Etime = as.numeric(POSIX.time - start.time, units = "secs")) %>%
        ungroup()

      # Merge data
      data.raw <- data.raw %>% full_join(Etime, by = c("chamID", "POSIX.time"))

      # Remove background
      if(background == FALSE){
        data.raw <- data.raw %>% filter(activ.cham != "Background") %>%
          mutate_at("activ.cham", as.numeric)
      }

      # Add instrument precision for each gas
      data.raw <- data.raw %>%
        mutate(CO2_prec = prec[1], CH4_prec = prec[2], N2O_prec = prec[3],
               H2O_LI7810_prec = prec[4], H2O_LI7810_prec = prec[5])

      # New function name
      if(as.character(match.call()[[1]]) == "GAIA_import") {
        warning(paste("All import functions have changed names in this new version of goFlux.",
                      "\nIn the future, use import.GAIA() instead of GAIA_import()"), call. = FALSE)
      }

      # Save cleaned data file
      if(save == TRUE){
        # Create RData folder in working directory
        RData_folder <- paste(getwd(), "RData", sep = "/")
        if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

        # Create output file: change extension to .RData, and
        # add instrument name and "imp" for import to file name
        file.name <- gsub(".*/", "", sub("\\.csv", "", inputfile))
        outputfile <- paste("GAIA_", file.name, "_imp.RData", sep = "")

        save(data.raw, file = paste(RData_folder, outputfile, sep = "/"))

        message(inputfile.name, " saved as ", outputfile,
                " in RData folder, in working directory\n", sep = "")
      }

      # Warning if file has no data
      if(nrow(data.raw) == 0){
        warning(paste(inputfile.name, "was imported succesfully, but no",
                      "measurements were detected."), call. = F)
      }

      if(save == FALSE){
        return(data.raw)
      }
    }
  }
}

#' @export
#' @rdname GAIA_import
import.GAIA <- GAIA_import
