#' Import function for LI-COR Multiplexer (LI-8250)
#'
#' Imports single raw data files from the LI-COR Multiplexer (LI-8250)
#'
#' @param inputfile character string; the name of a file with the extension .82z
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
#' @param keep_all logical; if \code{keep_all = TRUE}, keep all columns from the
#'                 raw file. The default is \code{keep_all = FALSE}, and columns
#'                 that are not necessary for gas flux calculation are removed.
#' @param inst1,inst2 character strings; a pattern to match the columns
#'        containing the name of each instrument. For example, with a LI-COR
#'        LI-7810, \code{inst1 = "LI-7810"} and \code{inst2 = NULL}, which
#'        matches the example data file provided with the package.
#' @param gas1,gas2 character vectors; a pattern to match the columns
#'        containing each gas measurement. For example, with a LI-COR LI-7810
#'        \code{gas1 = c("CO2_DRY", "CH4_DRY", "H2O")} and \code{gas2 = NULL},
#'        which matches the example data file provided with the package.
#' @param prec1,prec2 numerical vectors; the precision of the instrument
#'        for each gas mentioned in \code{gas1} and \code{gas2}. For example,
#'        with a LI-COR LI-7810, \code{prec1 = c(3.5, 0.6, 45)} and
#'        \code{prec2 = NULL}, which matches the example data file provided with
#'        the package. Note that the order in the arguments \code{precX} must
#'        match the order of the arguments in \code{gasX}.
#' @param dry1,dry2 logical; are the gas measurements compensated for water
#'        vapor (dry fraction)? If \code{dryX = TRUE} (default), the gases are
#'        compensated for water vapor, and will be named accordingly. For
#'        example, the column "CO2_DRY" will become "CO2dry_ppm".
#' @param Tsoil.col,SWC.col character string; a pattern to match the columns
#'        that fit the corresponding parameter.
#'
#' @returns A data frame containing raw data from the LI-COR Multiplexer LI-8250.
#'
#' @details
#' Files with the format .82z are zip archives that include all measurements
#' that occurred at a given port for one observation. Each .82z file includes
#' two files: a data.csv file and a metadata.json file. Raw files names for
#' .82z files are formatted with the instrument serial number and a time stamp
#' (serial-number-YYYYMMDDHHMMSS.82z). In \code{date.format}, the date format
#' refers to the date format in the file name. Alternatively, the date format
#' can be found in the data.csv file extracted from the .82z file, under the
#' column "Date".
#'
#' Note that this function was designed for the following units in the raw file,
#' according to the data dictionary of the LI-8250 Multiplexer
#' \href{https://www.licor.com/env/support/LI-8250/topics/files-page.html#Datadictionary}{online}.
#' \itemize{
#'   \item \% v/v for soil water content (SWC)
#'   \item litters for volumes
#'   \item kPa for pressure
#'   \item Celsius for temperature}
#' For each gas, the units are taken from the third row in the data.csv file.
#' Currently, the function is adapted for use with LI-COR instruments (LI-870
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}/
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}} Analyzer or LI-78xx
#' Trace Gas Analyzers). If you are using the function with a non-LI-COR
#' instrument, please contact the maintainer of this package for support.
#'
#' Regarding the parameters \code{dryX}, in case of uncertainty, either contact
#' your technical support or assume that gases are compensated for water vapor,
#' which is normally the case.
#'
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[goFlux]{k.max}}) in the non-linear flux calculation
#' (\code{\link[goFlux]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm for
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}, or 1 ppb for
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}) to allow for more
#' curvature, especially for water vapor fluxes, or very long measurements, that
#' are normally curved.
#'
#' According to the data dictionary of the LI-8250 Multiplexer
#' \href{https://www.licor.com/env/support/LI-8250/topics/files-page.html#Datadictionary}{online},
#' the column STATE indicates the chamber state, where, for example, 1 = closing
#' and 5 = closed.
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
#' file.path <- system.file("extdata", "LI8250/LI8250.82z", package = "goFlux")
#'
#' # Run function
#' imp.LI8250 <- import.LI8250(inputfile = file.path, inst1 = "LI-7810",
#'                             gas1 = c("CO2_DRY", "CH4_DRY", "H2O"),
#'                             prec1 = c(3.5, 0.6, 45), SWC.col = "SWC_1",
#'                             Tsoil.col = "TS_1")
#' @export
#'
import.LI8250 <- function(inputfile, date.format = "ymd", timezone = "UTC",
                          save = FALSE, keep_all = FALSE,
                          inst1, inst2 = NULL,
                          gas1, gas2 = NULL,
                          prec1, prec2 = NULL,
                          dry1 = T, dry2 = T,
                          SWC.col, Tsoil.col){

  # Check arguments ####
  if(missing(inputfile)) stop("'inputfile' is required")
  if(!is.character(inputfile)) stop("'inputfile' must be of class character")
  if(missing(inst1)) stop("'inst1' is required")
  if(missing(gas1)) stop("'gas1' is required")
  if(missing(prec1)) stop("'prec1' is required")
  if(length(date.format) != 1) stop("'date.format' must be of length 1")
  if(!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}
  if(!is.character(timezone)) stop("'timezone' must be of class character")
  if(save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")
  if(keep_all != TRUE & keep_all != FALSE) stop("'keep_all' must be TRUE or FALSE")
  if(!missing(SWC.col) & !is.character(SWC.col)) stop("'SWC.col' must be of class character")
  if(!missing(Tsoil.col) & !is.character(Tsoil.col)) stop("'Tsoil.col' must be of class character")

  # inst
  if(!is.null(inst1)) {
    if(!is.character(inst1)) stop("'inst1' must be of class character")
    if(length(inst1) != 1) stop("'inst1' must be of length 1")}

  if(!is.null(inst2)) {
    if(!is.character(inst2)) stop("'inst2' must be of class character")
    if(length(inst2) != 1) stop("'inst2' must be of length 1")}

  # gas
  if(!is.null(inst1)) {
    if(is.null(gas1)) stop("When 'inst1' is not NULL, at least one column name must be passed to 'gas1'")}
  if(!is.null(gas1)) if(!is.character(gas1)) stop("'gas1' must be of class character")

  if(!is.null(inst2)) {
    if(is.null(gas2)) stop("When 'inst2' is not NULL, at least one column name must be passed to 'gas2'")}
  if(!is.null(gas2)) if(!is.character(gas2)) stop("'gas2' must be of class character")

  # prec
  if(!is.null(gas1)) {
    if(is.null(prec1)) stop("When 'gas1' is not NULL, 'prec1' cannot be NULL")}
  if(!is.null(prec1)) {
    if(!is.numeric(prec1)) stop("'prec1' must be of class numeric")
    if(length(gas1) != length(prec1)) stop("'prec1' must be the same length as 'gas1'")}

  if(!is.null(gas2)) {
    if(is.null(prec2)) stop("When 'gas2' is not NULL, 'prec2' cannot be NULL")}
  if(!is.null(prec2)) {
    if(!is.numeric(prec2)) stop("'prec2' must be of class numeric")
    if(length(gas2) != length(prec2)) stop("'prec2' must be the same length as 'gas2'")}

  # Assign NULL to variables without binding ####
  POSIX.warning <- import.error <- inst <- name <- gas.unit <- gas <-
    dry.log <- dry <- new.name <- prec.name <- . <- PA <- TA_cham <-
    STATE_cham <- DATE <- TIME <- H2O_ppm <- chamID <- Pcham <- Tcham <-
    SWC <- Tsoil <- STATE <- POSIX.time <- deadband <- start.time <- NULL

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data.csv file contained in the zip archive ####
  try.import <- tryCatch(
    {read.csv(unz(inputfile, "data.csv"))},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    # Try to load metadata.json file contained in the zip archive ####
    try.metadata <- tryCatch(
      {parse_json(unz(inputfile, "metadata.json"))},
      error = function(e) {import.error <<- e}
    )

    if(inherits(try.metadata, "simpleError")){
      warning("Error occurred in file ", inputfile.name, ":\n", "   ",
              import.error, call. = F)
    } else {

      ## Match column names with arguments ####

      ## instruments
      inst12 <- "NULL"; if(!is.null(inst1)){
        inst12 <- gsub("-", ".", inst1, fixed = T)
        if(!any(grepl(inst12, names(try.import)))){
          stop(paste("Failed to import ", inputfile.name, ". The matching string ",
                     "for inst1 '", inst1, "' was not found in column names.", sep =""))}}

      inst22 <- "NULL"; if(!is.null(inst2)){
        inst22 <- gsub("-", ".", inst2, fixed = T)
        if(!any(grepl(inst22, names(try.import)))) {
          stop(paste("Failed to import ", inputfile.name, ". The matching string ",
                     "for inst2 '", inst2, "' was not found in column names.", sep =""))}}

      ## gases
      gas12 <- NULL; if(!is.null(gas1)){
        for(i in 1:length(gas1)){
          gas12[i] <- gsub(" ", ".", gas1[i], fixed = T)
          if(!is.na(gas1[i])) if(!any(grepl(gas12[i], try.import[1,]))){
            stop(paste("Failed to import ", inputfile.name, ". The matching ",
                       "string for gas1 '", gas1[i], "' was not found in ",
                       "column names.", sep =""))}}}

      gas22 <- NULL; if(!is.null(gas2)){
        for(i in 1:length(gas2)){
          gas22[i] <- gsub(" ", ".", gas2[i], fixed = T)
          if(!is.na(gas2[i])) if(!any(grepl(gas22[i], try.import[1,]))){
            stop(paste("Failed to import ", inputfile.name, ". The matching ",
                       "string for gas2 '", gas2[i], "' was not found in ",
                       "column names.", sep =""))}}}

      ## probes
      SWC.col2 <- "NULL"; if(!missing(SWC.col)){
        SWC.col2 <- gsub(" ", ".", SWC.col, fixed = T)
        if(!any(grepl(SWC.col2, try.import[1,]))){
          stop(paste("Failed to import ", inputfile.name, ". The matching string ",
                     "for SWC.col '", SWC.col, "' was not found in column names.", sep =""))}}

      Tsoil.col2 <- "NULL"; if(!missing(Tsoil.col)){
        Tsoil.col2 <- gsub(" ", ".", Tsoil.col, fixed = T)
        if(!any(grepl(Tsoil.col2, try.import[1,]))) {
          stop(paste("Failed to import ", inputfile.name, ". The matching string ",
                     "for Tsoil.col '", Tsoil.col, "' was not found in column names.", sep =""))}}


      # FUNCTION STARTS ####

      # Define column names
      colnames <- cbind.data.frame(
        # Extract column names and instrument names from try.import
        inst = names(try.import), name = paste(try.import[1,])) %>%
        # Remove numbers added after instrument names
        mutate(inst = if_else(grepl("LI.8250", inst), "LI.8250", inst)) %>%
        mutate(inst = if_else(grepl(inst12, inst), inst12, inst)) %>%
        mutate(inst = if_else(grepl(inst22, inst), inst22, inst)) %>%
        mutate(inst = if_else(grepl("CHAMBER", inst), "cham", inst)) %>%
        # Rename columns with first row + instrument name
        mutate(colnames = if_else(inst == "LI.8250", name,
                                  paste(name, inst, sep = "_"))) %>%
        # Extract units from try.import
        mutate(units = gsub("\\[|]", "", paste(try.import[2,]))) %>%
        # Rename gas columns
        mutate(gas = if_else(grepl(paste(c(gas1, gas2), collapse = "|"), name),
                             gsub("_DRY", "", name), NA)) %>%
        mutate(gas.unit = if_else(units == "umol+1mol-1", "ppm", NA)) %>%
        mutate(gas.unit = if_else(units == "nmol+1mol-1", "ppb", gas.unit)) %>%
        mutate(gas.unit = if_else(units == "mmol+1mol-1", "per.mille", gas.unit)) %>%
        # Rename probes
        mutate(colnames = if_else(grepl(SWC.col, name), "SWC", colnames)) %>%
        mutate(colnames = if_else(grepl(Tsoil.col, name), "Tsoil", colnames))

      # Prepare new names for gas columns
      inst1.rep <- dry1.rep <- NULL; if(!is.null(inst1)) {
        inst1.rep <- rep(inst12, length(gas1))
        dry1.rep <- rep(dry1, length((gas1)))}
      inst2.rep <- dry2.rep <- NULL; if(!is.null(inst2)) {
        inst2.rep <- rep(inst22, length(gas2))
        dry2.rep <- rep(dry2, length((gas2)))}

      gas.col <- cbind.data.frame(gas.col = c(gas12, gas22),
                                  inst = c(inst1.rep, inst2.rep),
                                  prec.col = c(prec1, prec2)) %>%
        mutate(gas = gsub("_DRY", "", gas.col)) %>%
        left_join(select(colnames, gas, gas.unit), by = "gas") %>%
        mutate(dry.log = c(dry1.rep, dry2.rep),
               dry = if_else(dry.log, "dry", "wet")) %>%
        mutate(dry = if_else(grepl("H2O", gas), "", dry),
               new.name = paste(gas, dry, "_", gas.unit, sep = "")) %>%
        mutate(prec.name = paste(gas, "_prec", sep = ""))

      # Control for same gases measured with different instruments
      gas.col.dup <- gas.col %>% group_by(gas) %>% summarise(n = n()) %>%
        filter(n > 1)

      if(nrow(gas.col.dup) > 0){
        # Add inst[i] to duplicated gases
        gas.col.dup.ls <- NULL; for(i in 1:nrow(gas.col.dup)){
          gas.col.dup.ls[[i]] <- filter(gas.col, gas == gas.col.dup$gas[i]) %>%
            mutate(new.name = if_else(grepl(gas.col.dup$gas[i], gas),
                                      paste(new.name, "_", inst, sep = ""), new.name),
                   prec.name = if_else(grepl(gas.col.dup$gas[i], gas),
                                       paste(prec.name, "_", inst, sep = ""), prec.name))}
        # Replace in gas.col2
        gas.col2 <- filter(gas.col, !gas %in% gas.col.dup$gas) %>%
          rbind(bind_rows(gas.col.dup.ls))
      } else gas.col2 <- gas.col

      colnames2 <- colnames %>%
        left_join(select(gas.col2, gas.col, new.name), by = c("name" = "gas.col")) %>%
        mutate(new.name = if_else(is.na(new.name), colnames, new.name))

      data.raw <- try.import %>%
        # Rename columns with first row + instrument name
        `colnames<-`(colnames2$new.name) %>%
        # Remove first two rows containing column names and units
        filter(!row_number() %in% 1:2) %>%
        # Convert column classes
        mutate(across(3:ncol(.), ~ type.convert(., as.is = TRUE))) %>%
        # Make sure that all gas data are class numerical
        mutate_at(gas.col2$new.name, as.numeric) %>%
        # Rename useful columns
        rename(Pcham = PA, Tcham = TA_cham, STATE = STATE_cham) %>%
        # Create chamID
        mutate(chamID = paste(first(DATE), first(TIME), sep = "_"))

      # Convert H2O_per.mille (mmol/mol) to ppm
      if(any(grepl("per.mille", gas.col2$gas.unit))){
        humidity.cols <- select(data.raw, contains("per.mille")) %>%
          mutate_all(~.*1000) %>%
          setNames(gsub("per.mille", "ppm", names(.)))

        data.raw <- cbind.data.frame(data.raw, humidity.cols)}

      # Compensate for water vapor
      if(any(grepl("wet", gas.col2$dry))){
        wet.cols <- select(data.raw, contains("wet"), H2O_ppm) %>%
          mutate(across(contains("wet"), ~./(1-H2O_ppm/1000000))) %>%
          setNames(gsub("wet", "dry", names(.))) %>%
          select(-H2O_ppm)

        data.raw <- cbind.data.frame(data.raw, wet.cols)}

      # Remove unnecessary columns, not used for flux calculation
      if(keep_all == FALSE){
        data.raw <- data.raw %>%
          select(DATE, TIME, chamID, contains(c("ppm", "ppb")), -contains("wet"),
                 Pcham, Tcham, SWC, Tsoil, STATE)
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
                "DATE ", data.raw$DATE[1], "\n", call. = F)
      } else {

        data.raw$POSIX.time <- try.POSIX

        # Extract data from metadata.json file
        data.raw <- data.raw %>%
          mutate(Area = try.metadata$CHAMBER$AREA$VALUE,
                 Offset = try.metadata$CHAMBER$COLLAR_HEIGHT$VALUE,
                 Vcham = try.metadata$CHAMBER$VOLUME$VALUE,
                 Vtot = try.metadata$METADATA$VOLUME_TOTAL$VALUE,
                 deadband = try.metadata$FLUX[[1]]$DEADBAND$VALUE,
                 obs.length = try.metadata$METADATA$OBSERVATION$VALUE)

        # Calcualte cham.close and start.time based on STATE and deadband
        cham.close <- filter(data.raw, STATE == 5) %>% summarise(min(POSIX.time)) %>% .[[1]]
        data.raw <- data.raw %>%
          mutate(cham.close = cham.close) %>%
          mutate(start.time = cham.close + deadband) %>%
          # Calculate Etime from start.time
          mutate(Etime = as.numeric(POSIX.time - start.time, units = "secs"),
                 flag = if_else(POSIX.time >= start.time, 1, 0))

        # Add instrument precision for each gas
        data.raw[gas.col2$prec.name] <- 1
        for(i in 1:nrow(gas.col2)){
          data.raw <- data.raw %>%
            mutate_at(gas.col2$prec.name[i], ~gas.col2$prec.col[i])}

        # Warning about duplicated gases
        if(nrow(gas.col.dup) > 0){
          warning(paste("The same gas was measured by two instruments. The name ",
                        "of each instrument was added to each duplicated ",
                        "gas column as a suffix to avoid confusion.",
                        sep = ""), call. = F)
        }

        # Save cleaned data file
        if(save == TRUE){
          # Create RData folder in working directory
          RData_folder <- paste(getwd(), "RData", sep = "/")
          if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

          # Create output file: change extension to .RData, and
          # add instrument name and "imp" for import to file name
          file.name <- gsub(".*/", "", sub("\\.82z", "", inputfile))
          outputfile <- paste("LI8250_", file.name, "_imp.RData", sep = "")

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
}
