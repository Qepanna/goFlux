#' Import function for GAIA synced with multiple chambers and instruments
#'
#' Imports single raw gas measurement files from the automated chamber
#' ECOFlux (GAIA2TECH) with the extension .csv
#' (LI-7810: CO2, CH4 and H2O / LI7820: N2O and H2O)
#'
#' @param inputfile character string; the name of a file with the extension .csv,
#'                  including path.
#' @param date.format character string; chose one of the following: "dmy", "ymd",
#'                    or "mdy". Default is "ymd", as it is the date format from
#'                    the example data file provided.
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param pivot character string; either "long" or "wide". If \code{pivot = "long"},
#'              each column containing information about Tsoil, Tcham, SWC, PAR
#'              and operating status (Op.stat) will be saved in a single column
#'              per parameter. If \code{pivot = "wide"}, the default display of
#'              one column per chamber per parameter will be used.
#' @param active logical; if \code{active = TRUE}, preserve data for active
#'               chambers only.
#' @param flag numeric vector; indicates the operating status that should be used
#'             for the flux calculation. Default is \code{flag = c(7,11)}, where
#'             7 indicates "Chamber Idle Closed Clear" and 11 indicates
#'             "Chamber Idle Closed Dark".
#' @param background logical; if \code{background = FALSE}, removes all data from
#'                   \code{activ.cham == "Background"}.
#' @param save logical; if \code{save = TRUE}, saves the file as RData in a RData
#'             folder in the current working directory. If \code{save = FALSE},
#'             returns the file in the Console, or load in the Environment if
#'             assigned to an object.
#' @param Op.stat.col,PAR.col,Tcham.col,Tsoil.col,SWC.col,CH.col character string;
#'        a pattern to match all columns that fit the corresponding parameter. For
#'        example, all columns containing the pattern "3C07_Sunlight" will be
#'        renamed with the pattern "_PAR". Then, if \code{pivot = "long"}, all
#'        columns with the pattern "_PAR" will be merged together.
#' @param CO2.col,CH4.col,H2O1.col,N2O.col,H2O2.col character string; a pattern
#'        to match the columns containing the corresponding gas measurements.
#'        H2O1.col must be the same instrument as CO2.col and CH4.col, and
#'        H2O2.col must be the same instrument as N2O.col.
#'
#' @returns a data frame
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso Use the wrapper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso Import functions for individual instruments:
#'          \code{\link[GoFluxYourself]{G2508_import}},
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
#' file.path <- system.file("extdata", "GAIA/example_GAIA.csv", package = "GoFluxYourself")
#'
#' GAIA.data <- GAIA_import(inputfile = file.path)
#'
#' @export
#'
GAIA_import <- function(inputfile, date.format = "ymd", timezone = "UTC",
                        pivot = "long", active = TRUE, flag = c(7,11),
                        background = FALSE, save = FALSE,
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
  if (missing(inputfile)) stop("'inputfile' is required")
  if (!is.character(inputfile)) stop("'inputfile' must be of class character")
  if (length(date.format) != 1) stop("'date.format' must be of length 1")
  if (!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}
  if (!is.character(timezone)) stop("'timezone' must be of class character")
  if (save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")
  if (active != TRUE & active != FALSE) stop("'active' must be TRUE or FALSE")
  if (background != TRUE & background != FALSE) stop("'background' must be TRUE or FALSE")
  if (length(pivot) != 1) stop("'pivot' must be of length 1")
  if (!any(grepl(pivot, c("long", "wide")))) {
    stop("'pivot' must be of class character and one of the following: 'long' or 'wide'")}
  if (!is.numeric(flag)) stop("'flag' must be of class numeric")
  if (!is.character(H2O2.col)) stop("'H2O2.col' must be of class character")
  if (!is.character(N2O.col)) stop("'N2O.col' must be of class character")
  if (!is.character(H2O1.col)) stop("'H2O1.col' must be of class character")
  if (!is.character(CH4.col)) stop("'CH4.col' must be of class character")
  if (!is.character(CO2.col)) stop("'CO2.col' must be of class character")
  if (!is.character(Op.stat.col)) stop("'Op.stat.col' must be of class character")
  if (!is.character(PAR.col)) stop("'PAR.col' must be of class character")
  if (!is.character(Tcham.col)) stop("'Tcham.col' must be of class character")
  if (!is.character(Tsoil.col)) stop("'Tsoil.col' must be of class character")
  if (!is.character(SWC.col)) stop("'SWC.col' must be of class character")
  if (!is.character(CH.col)) stop("'CH.col' must be of class character")


  # Assign NULL to variables without binding
  POSIX.time <- activ.cham <- DATE_TIME <- start.time <- . <- SEQUENCE <-
    Titles. <- Obs <- cham.probe <- chamID <- obs.start <- rbind.fill <-
    cham.close <- cham.open <- H2O_ppm_LI7820 <- N2Odry_ppb <-
    H2O_ppm_LI7810 <- CH4dry_ppb <- CO2dry_ppm <- NULL

  # Import raw data file from GAIA (.csv)
  data.raw <- read.delim(inputfile, skip = 1, colClasses = "character") %>%
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
           SEQUENCE = ifelse(SEQUENCE == "", "Background", SEQUENCE)) %>%
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
                           paste("CH", activ.cham, "_", Obs, sep = ""))) %>%
    # Select only useful columns
    select(contains(c("DATE_TIME", "ChamID", "activ.cham", "Tsoil", "Tcham", "SWC",
                      "cover", "PAR", "Op.stat", "ppm", "ppb"))) %>%
    # Convert column class automatically
    type.convert(as.is = TRUE) %>%
    # Make sure that all gas data are class numerical
    mutate_at(c("CO2dry_ppm", "CH4dry_ppb", "H2O_ppm_LI7810", "N2Odry_ppb",
                "H2O_ppm_LI7820"), as.numeric) %>%
    # Remove negative gas measurements, if any
    filter(CO2dry_ppm > 0 | is.na(CO2dry_ppm)) %>%
    filter(CH4dry_ppb > 0 | is.na(CH4dry_ppb)) %>%
    filter(H2O_ppm_LI7810 > 0 | is.na(H2O_ppm_LI7810)) %>%
    filter(N2Odry_ppb > 0 | is.na(N2Odry_ppb)) %>%
    filter(H2O_ppm_LI7820 > 0 | is.na(H2O_ppm_LI7820))

  # Group together all columns containing information and merge data
  if (pivot == "long"){ # pivot long: only one column per parameter

    ### Operating Status from each active chamber
    Op.stat <- data.raw %>% select(DATE_TIME, contains("Op.stat")) %>%
      pivot_longer(contains("Op.stat"), values_to = "Op.stat", names_to = "cham.probe") %>%
      mutate(cham.probe = substr(cham.probe, 3, 3))

    ### Soil temperature from each active chamber
    Tsoil <- data.raw %>% select(DATE_TIME, contains("Tsoil")) %>%
      pivot_longer(contains("Tsoil"), values_to = "Tsoil", names_to = "cham.probe") %>%
      mutate(cham.probe = substr(cham.probe, 3, 3))

    ### Air temperature from each active chamber
    Tcham <- data.raw %>% select(DATE_TIME, contains("Tcham")) %>%
      pivot_longer(contains("Tcham"), values_to = "Tcham", names_to = "cham.probe") %>%
      mutate(cham.probe = substr(cham.probe, 3, 3))

    ### Soil water content from each active chamber
    SWC <- data.raw %>% select(DATE_TIME, contains("SWC")) %>%
      pivot_longer(contains("SWC"), values_to = "SWC", names_to = "cham.probe") %>%
      mutate(cham.probe = substr(cham.probe, 3, 3))

    ### PAR from each active chamber
    PAR <- data.raw %>% select(DATE_TIME, contains("PAR")) %>%
      pivot_longer(contains("PAR"), values_to = "PAR", names_to = "cham.probe") %>%
      mutate(cham.probe = substr(cham.probe, 3, 3))

    data.raw <- data.raw %>%
      # Operating status
      full_join(Op.stat, by = c("DATE_TIME")) %>%
      select(!contains("_Op.stat")) %>%
      # Soil temperature
      full_join(Tsoil, by = c("DATE_TIME", "cham.probe")) %>%
      select(!contains("_Tsoil")) %>%
      # Air temperature
      full_join(Tcham, by = c("DATE_TIME", "cham.probe")) %>%
      select(!contains("_Tcham")) %>%
      # Soil water content
      full_join(SWC, by = c("DATE_TIME", "cham.probe")) %>%
      select(!contains("_SWC")) %>%
      # PAR
      full_join(PAR, by = c("DATE_TIME", "cham.probe")) %>%
      select(!contains("_PAR"))
  }

  # Group together all columns containing information and merge data
  if (pivot == "wide"){ # keep wide: one column per instrument per parameter

    # Operating Status from each active chamber
    Op.stat <- data.raw %>% select(DATE_TIME, contains("Op.stat")) %>%
      pivot_longer(contains("Op.stat"), values_to = "Op.stat", names_to = "cham.probe") %>%
      mutate(cham.probe = substr(cham.probe, 3, 3))

    data.raw <- data.raw %>%
      # Only operating status is pivoted long
      left_join(Op.stat, by = c("DATE_TIME")) %>%
      select(!contains("_Op.stat"))
  }

  # Remove measurements from non-active chambers
  if (active == TRUE){
    Background <- data.raw %>% filter(grepl("Background", chamID))
    data.raw <- data.raw %>% filter(activ.cham == cham.probe) %>%
      rbind.fill(Background)
  }

  # Create a new column containing date and time (POSIX format)
  op <- options()
  options(digits.secs=6)
  if(date.format == "dmy"){
    data.raw$POSIX.time <- as.POSIXct(dmy_hms(data.raw$DATE_TIME, tz = timezone),
                                      format = "%Y-%m-%d %H:%M:%OS")
  }
  if(date.format == "mdy"){
    data.raw$POSIX.time <- as.POSIXct(mdy_hms(data.raw$DATE_TIME, tz = timezone),
                                      format = "%Y-%m-%d %H:%M:%OS")
  }
  if(date.format == "ymd"){
    data.raw$POSIX.time <- as.POSIXct(ymd_hms(data.raw$DATE_TIME, tz = timezone),
                                      format = "%Y-%m-%d %H:%M:%OS")
  }
  options(op)

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
  if (background == FALSE){
    data.raw <- data.raw %>% filter(activ.cham != "Background") %>%
      mutate_at("activ.cham", as.numeric)
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

    message(file.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
