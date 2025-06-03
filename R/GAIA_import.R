#' Import function for the automated chamber ECOFlux (GAIA2TECH) synced with
#' multiple chambers and instruments
#'
#' Imports single raw gas measurement files from the automated chamber
#' ECOFlux (GAIA2TECH) linked with up to three greenhouse gas analyzers.
#' Default settings are set for two LI-COR instruments: LI-7810
#' (\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}) and LI7820
#' (\ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}), to match the
#' example data file provided with the package.
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
#' @param Op.stat.col,CH.col character string; a pattern to match columns that
#'        fit the corresponding parameters: see details below.
#' @param PAR.col,Tcham.col,Tsoil.col,SWC.col,WTD.col character vector; a vector
#'        of patterns to match all columns that fit the corresponding parameters.
#'        See details below for more information.
#' @param inst1,inst2,inst3 character strings; a pattern to match the columns
#'        containing the name of each instrument. By default,
#'        \code{inst1 = "XT2C00_Instrument"}, \code{inst2 = "XT3C00_Instrument"},
#'        and \code{inst3 = NULL}, to match the example data file provided with
#'        the package.
#' @param gas1,gas2,gas3 character vectors; a pattern to match the columns
#'        containing each gas measurement. By default,
#'        \code{gas1 = c("XT2C04_CH4", "XT2C05_CO2", "XT2C06_H2O")},
#'        \code{gas2 = c("XT3C04_N2O", "XT3C05_H2O")}, and \code{gas3 = NULL},
#'        to match the example data file provided with the package.
#' @param prec1,prec2,prec3 numerical vectors; the precision of the instrument
#'        for each gas mentioned in \code{gas1}, \code{gas2} and \code{gas3}.
#'        By default, \code{prec1 = c(0.6, 3.5, 45)}, \code{prec2 = c(0.4, 45)},
#'        and \code{prec3 = NULL}, to match the example data file provided with
#'        the package. Note that the order in the arguments \code{precX} must
#'        match the order of the arguments in \code{gasX}.
#' @param dry1,dry2,dry3 logical; are the gas measurements compensated for water
#'        vapor (dry fraction)? If \code{dryX = TRUE} (default), the gases are
#'        compensated for water vapor, and will be named accordingly. For example,
#'        the column "XT2C05_CO2" will become "CO2dry_ppm".
#' @param manual logical; indicates if measurements were made manually and there
#'               are no records of chamber identification, sequence, etc. If
#'               \code{TRUE}, the arguments \code{Op.stat.col}, \code{CH.col},
#'               \code{active}, \code{background}, \code{flag}, and \code{pivot}
#'               are ignored.
#' @param sep character string defining the field separator character. Values on
#'            each line of the file are separated by this character. By default,
#'            \code{sep = "\t"} for tabulation.
#' @param skip integer; the number of lines of the data file to skip before
#'             beginning to read data. By default, \code{skip = 1}.
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
#' file, not the date format in the file name. For the automated chamber ECOFlux,
#' the date is found in the column "Titles:".
#'
#' The arguments \code{PAR.col}, \code{Tcham.col}, \code{Tsoil.col}, \code{SWC.col},
#' and \code{WTD.col} correspond to different types of probes linked to the ECOFlux
#' chamber: PAR (sunlight), chamber temperature, soil temperature, soil water
#' content and water table depth, respectively. All columns containing the
#' patterns specified in these arguments will be selected and renamed with the
#' corresponding argument. For example, if \code{PAR = "3C07_Sunlight"}, all
#' columns containing the pattern "3C07_Sunlight" will be renamed with the pattern
#' "_PAR". Then, if \code{pivot = "long"}, all columns with the pattern "_PAR"
#' will be merged together. If \code{PAR = c("3C07_Sunlight1", "3C07_Sunlight2")},
#' these patterns will be renamed to PAR1 and PAR2.
#'
#' The argument \code{Op.stat.col}
#' corresponds to the columns Operating Status of each chamber. The argument
#' \code{CH.col} indicates a character string preceding the chamber number for
#' each column of the raw data. For example, the column "COM5A010C06_OperatingStatus"
#' in the raw data file contains the Operating Status for the chamber 1 if
#' \code{CH.col = "COM5A0"} and \code{Op.stat.col = "0C06_OperatingStatus"}. If
#' these columns are absent from the raw data, set arguments to NULL.
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item kPa for atmospheric pressure
#'   \item volumetric water content (\%) for soil moisture
#'   \item Celsius for air temperature
#'   \item \ifelse{html}{\out{µmol photons m<sup>-2</sup>s<sup>-1</sup> for PAR}}{\eqn{µmol photons m^{-2}s^{-1} for PAR}{ASCII}}}
#' For each gas, the units are taken from the second row in the raw file (Units:).
#' If your instruments use different units, either convert the units after
#' import, change the settings on your instruments, or contact the maintainer of
#' this package for support.
#'
#' Regarding the parameters \code{dryX}, in case of uncertainty, either contact
#' your technical support or assume that gases are compensated for water vapor,
#' which is normally the case.
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
#'          \code{\link[goFlux]{import.G2201i}},
#'          \code{\link[goFlux]{import.G2508}},
#'          \code{\link[goFlux]{import.G4301}},
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
#' file.path <- system.file("extdata", "GAIA/GAIA.csv", package = "goFlux")
#'
#' # Run function
#' imp.GAIA <- import.GAIA(inputfile = file.path)

GAIA_import <- function(inputfile, date.format = "ymd", timezone = "UTC",
                        pivot = "long", active = TRUE, flag = c(7,11),
                        background = FALSE, save = FALSE,
                        CH.col = "COM5A0",
                        SWC.col = "1C08_Soil Moisture",
                        Tsoil.col = "1C07_Soil Temperature",
                        Tcham.col = "2C07_Chamber Temperature",
                        PAR.col = "3C07_Sunlight",
                        WTD.col = NULL,
                        Op.stat.col = "0C06_OperatingStatus",
                        inst1 = "XT2C00_Instrument",
                        inst2 = "XT3C00_Instrument",
                        inst3 = NULL,
                        gas1 = c("XT2C04_CH4", "XT2C05_CO2", "XT2C06_H2O"),
                        gas2 = c("XT3C04_N2O", "XT3C05_H2O"),
                        gas3 = NULL,
                        prec1 = c(0.6, 3.5, 45),
                        prec2 = c(0.4, 45),
                        prec3 = NULL,
                        dry1 = T,
                        dry2 = T,
                        dry3 = NULL,
                        manual = FALSE,
                        sep = "\t",
                        skip = 1){

  # Check arguments ####
  if(missing(inputfile)) stop("'inputfile' is required")
  if(!is.character(inputfile)) stop("'inputfile' must be of class character")
  if(length(date.format) != 1) stop("'date.format' must be of length 1")
  if (!is.character(date.format)) stop("'date.format' must be of class character")
  if (!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be one of the following: 'ymd', 'dmy' or 'mdy'")}
  if(!is.character(timezone)) stop("'timezone' must be of class character")
  if(save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")
  if(active != TRUE & active != FALSE) stop("'active' must be TRUE or FALSE")
  if(manual != TRUE & manual != FALSE) stop("'manual' must be TRUE or FALSE")
  if(background != TRUE & background != FALSE) stop("'background' must be TRUE or FALSE")
  if(length(pivot) != 1) stop("'pivot' must be of length 1")
  if(!any(grepl(pivot, c("long", "wide")))) {
    stop("'pivot' must be of class character and one of the following: 'long' or 'wide'")}
  if(!is.null(flag)) if(!is.numeric(flag)) stop("'flag' must be of class numeric")
  if(!is.numeric(skip)) stop("'skip' must be of class numeric")
  if(!is.character(sep)) stop("'sep' must be of class character")

  # Column names
  if(manual == F) {
    if(is.null(CH.col)) stop("'CH.col' is required")
    if(!is.null(CH.col)) if(!is.character(CH.col)) stop("'CH.col' must be of class character")
    if(!is.null(Op.stat.col)) if(!is.character(Op.stat.col)) stop("'Op.stat.col' must be of class character")
  }

  if(!is.null(PAR.col)) if(!is.character(PAR.col)) stop("'PAR.col' must be of class character")
  if(!is.null(Tcham.col)) if(!is.character(Tcham.col)) stop("'Tcham.col' must be of class character")
  if(!is.null(Tsoil.col)) if(!is.character(Tsoil.col)) stop("'Tsoil.col' must be of class character")
  if(!is.null(SWC.col)) if(!is.character(SWC.col)) stop("'SWC.col' must be of class character")
  if(!is.null(WTD.col)) if(!is.character(WTD.col)) stop("'WTD.col' must be of class character")

  # inst
  if(is.null(inst1) & is.null(inst2) & is.null(inst3)) {
    stop("At least one column name must be passed to 'inst1', 'inst2' or 'inst3'")}

  if(!is.null(inst1)) {
    if(!is.character(inst1)) stop("'inst1' must be of class character")
    if(length(inst1) != 1) stop("'inst1' must be of length 1")}

  if(!is.null(inst2)) {
    if(!is.character(inst2)) stop("'inst2' must be of class character")
    if(length(inst2) != 1) stop("'inst2' must be of length 1")}

  if(!is.null(inst3)) {
    if(!is.character(inst3)) stop("'inst3' must be of class character")
    if(length(inst3) != 1) stop("'inst3' must be of length 1")}

  # gas
  if(!is.null(inst1)) {
    if(is.null(gas1)) stop("When 'inst1' is not NULL, at least one column name must be passed to 'gas1'")}
  if(!is.null(gas1)) if(!is.character(gas1)) stop("'gas1' must be of class character")

  if(!is.null(inst2)) {
    if(is.null(gas2)) stop("When 'inst2' is not NULL, at least one column name must be passed to 'gas2'")}
  if(!is.null(gas2)) if(!is.character(gas2)) stop("'gas2' must be of class character")

  if(!is.null(inst3)) {
    if(is.null(gas3)) stop("When 'inst3' is not NULL, at least one column name must be passed to 'gas3'")}
  if(!is.null(gas3)) if(!is.character(gas3)) stop("'gas3' must be of class character")

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

  if(!is.null(gas3)) {
    if(is.null(prec3)) stop("When 'gas3' is not NULL, 'prec3' cannot be NULL")}
  if(!is.null(prec3)) {
    if(!is.numeric(prec3)) stop("'prec3' must be of class numeric")
    if(length(gas3) != length(prec3)) stop("'prec3' must be the same length as 'gas3'")}

  # Assign NULL to variables without binding ####
  POSIX.time <- activ.cham <- DATE_TIME <- start.time <- Obs <- SEQUENCE <-
    Titles. <- cham.probe <- chamID <- obs.start <- rbind.fill <- cham.open <-
    cham.close <- H2O_ppm_LI7820 <- N2Odry_ppb <- import.error <- . <- dry <-
    CH4dry_ppb <- CO2dry_ppm <- POSIX.warning <- Op.stat <- dry.log <- gas <-
    Tsoil <- Tcham <- SWC <- PAR <- new.name <- inst <- prec.name <- NULL

  # Input file name
  inputfile.name <- gsub(".*/", "", inputfile)

  # Try to load data file ####
  try.import <- tryCatch(
    {read.delim(inputfile, skip = skip, sep = sep, colClasses = "character", skipNul = T)},
    error = function(e) {import.error <<- e}
  )

  if(inherits(try.import, "simpleError")){
    warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {

    # Replace spaces in column names with a dot to match imported data frame
    if(!is.null(SWC.col)){
      SWC.col2 <- gsub(" ", ".", SWC.col, fixed = T)} else {SWC.col2 <- "NA"}
    if(!is.null(Tcham.col)){
      Tcham.col2 <- gsub(" ", ".", Tcham.col, fixed = T)} else {Tcham.col2 <- "NA"}
    if(!is.null(Tsoil.col)){
      Tsoil.col2 <- gsub(" ", ".", Tsoil.col, fixed = T)} else {Tsoil.col2 <- "NA"}
    if(!is.null(PAR.col)){
      PAR.col2 <- gsub(" ", ".", PAR.col, fixed = T)} else {PAR.col2 <- "NA"}
    if(!is.null(WTD.col)){
      WTD.col2 <- gsub(" ", ".", WTD.col, fixed = T)} else {WTD.col2 <- "NA"}

    # Column names match CH.col?
    if(manual == F){
      CH.col2 <- gsub(" ", ".", CH.col, fixed = T)
      if(!any(grepl(CH.col2, names(try.import)))) {
        stop(paste("Failed to import ", inputfile.name, ". The matching string ",
                   "for chamber ID (CH.col) was not found in column names.", sep =""))}
      } else CH.col2 <- "NA"

    # Operating status missing?
    if(manual == F){
      if(!is.null(Op.stat.col)) Op.stat.col2 = gsub(" ", ".", Op.stat.col, fixed = T)
      if(!any(grepl(Op.stat.col2, names(try.import)))){
        warning(paste("In the file", inputfile.name, "the matching string for Operating",
                      "status (Op.stat.col) was not found in column names. By default,",
                      "Operating Status was set to 2 (Chamber Idle Open) for all measurements."),
                call. = F)}} else Op.stat.col2 <- "NA"

    ## Match column names with instruments and gases? ####

    ## instruments
    if(!is.null(inst1)){
      inst12 <- gsub(" ", ".", inst1, fixed = T)
      if(!any(grepl(inst12, names(try.import)))){
        stop(paste("Failed to import ", inputfile.name, ". The matching string ",
                   "for inst1 '", inst1, "' was not found in column names.", sep =""))}}

    if(!is.null(inst2)){
      inst22 <- gsub(" ", ".", inst2, fixed = T)
      if(!any(grepl(inst22, names(try.import)))) {
        stop(paste("Failed to import ", inputfile.name, ". The matching string ",
                   "for inst2 '", inst2, "' was not found in column names.", sep =""))}}

    if(!is.null(inst3)){
      inst32 <- gsub(" ", ".", inst3, fixed = T)
      if(!any(grepl(inst3, names(try.import)))) {
        stop(paste("Failed to import ", inputfile.name, ". The matching string ",
                   "for inst3 '", inst3, "' was not found in column names.", sep =""))}}

    ## gas1
    gas12 <- NULL; if(!is.null(gas1)){
      for(i in 1:length(gas1)){
        gas12[i] <- gsub(" ", ".", gas1[i], fixed = T)
        if(!is.na(gas1[i])) if(!any(grepl(gas12[i], names(try.import)))){
          stop(paste("Failed to import ", inputfile.name, ". The matching ",
                     "string for gas1 '", gas1[i], "' was not found in ",
                     "column names.", sep =""))}}}

    ## gas2
    gas22 <- NULL; if(!is.null(gas2)){
      for(i in 1:length(gas2)){
        gas22[i] <- gsub(" ", ".", gas2[i], fixed = T)
        if(!is.na(gas2[i])) if(!any(grepl(gas22[i], names(try.import)))){
          stop(paste("Failed to import ", inputfile.name, ". The matching ",
                     "string for gas2 '", gas2[i], "' was not found in ",
                     "column names.", sep =""))}}}

    ## gas3
    gas32 <- NULL; if(!is.null(gas3)){
      for(i in 1:length(gas3)){
        gas32[i] <- gsub(" ", ".", gas3[i], fixed = T)
        if(!is.na(gas3[i])) if(!any(grepl(gas32[i], names(try.import)))){
          stop(paste("Failed to import ", inputfile.name, ". The matching ",
                     "string for gas3 '", gas3[i], "' was not found in ",
                     "column names.", sep =""))}}}

    # FUNCTION STARTS ####

    # Rename gas columns
    instr1 <- instr2 <- instr3 <- "NULL"
    inst1.rep <- inst2.rep <- inst3.rep <- NULL
    dry1.rep <- dry2.rep <- dry3.rep <- NULL
    if(!is.null(inst1)) {
      instr1 <- select(try.import, all_of(inst12))[[1]][1]
      inst1.rep <- rep("inst1", length(gas1))
      dry1.rep <- rep(dry1, length((gas1)))}
    if(!is.null(inst2)) {
      instr2 <- select(try.import, all_of(inst22))[[1]][1]
      inst2.rep <- rep("inst2", length(gas2))
      dry2.rep <- rep(dry2, length((gas2)))}
    if(!is.null(inst3)) {
      instr3 <- select(try.import, all_of(inst32))[[1]][1]
      inst3.rep <- rep("inst3", length(gas3))
      dry3.rep <- rep(dry3, length((gas3)))}
    units.ls <- list()
    for(i in 1:length(c(gas1, gas2, gas3))){
      units.ls[[i]] <- select(try.import, all_of(c(gas12, gas22, gas32)[i]))[[1]][1]}

    gas.col <- cbind.data.frame(gas.col = c(gas12, gas22, gas32),
                                inst = c(inst1.rep, inst2.rep, inst3.rep),
                                units = unlist(units.ls),
                                prec.col = c(prec1, prec2, prec3)) %>%
      mutate(gas = gsub(".*_", "", gas.col)) %>%
      mutate(gas = if_else(grepl("Humidity", gas), "H2O", gas)) %>%
      mutate(dry.log = c(dry1.rep, dry2.rep, dry3.rep),
             dry = if_else(dry.log, "dry", "wet")) %>%
      mutate(dry = if_else(grepl("H2O", gas), "", dry),
             new.name = paste(gas, dry, "_", units, sep = "")) %>%
      mutate(prec.name = paste(gas, "_prec", sep = ""))

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

    # Rename gas columns
    data.raw <- try.import
    for(i in 1:nrow(gas.col2)){
      data.raw <- data.raw %>%
        setNames(gsub(gas.col2$gas.col[i], gas.col2$new.name[i], names(.)))}

    # Are there multiple columns for Tsoil, Tcham, SWC, PAR and WTD?
    Tsoil.col3 <- Tsoil.col2; Tsoil.col3.1 <- Tsoil.col3.2 <- "NULL"
    if(length(Tsoil.col2) > 1){
      if(length(Tsoil.col2) > 2) stop(paste(
        "The function is not adapted for more than two Tsoil columns. Contact the",
        "maintainer of the package for help."))
      Tsoil.col3.1 <- Tsoil.col2[1]
      Tsoil.col3.2 <- Tsoil.col2[2]
      Tsoil.col3 <- "NULL"}

    Tcham.col3 <- Tcham.col2; Tcham.col3.1 <- Tcham.col3.2 <- "NULL"
    if(length(Tcham.col2) > 1){
      if(length(Tcham.col2) > 2) stop(paste(
        "The function is not adapted for more than two Tcham columns. Contact the",
        "maintainer of the package for help."))
      Tcham.col3.1 <- Tcham.col2[1]
      Tcham.col3.2 <- Tcham.col2[2]
      Tcham.col3 <- "NULL"}

    SWC.col3 <- SWC.col2; SWC.col3.1 <- SWC.col3.2 <- "NULL"
    if(length(SWC.col2) > 1){
      if(length(SWC.col2) > 2) stop(paste(
        "The function is not adapted for more than two SWC columns. Contact the",
        "maintainer of the package for help."))
      SWC.col3.1 <- SWC.col2[1]
      SWC.col3.2 <- SWC.col2[2]
      SWC.col3 <- "NULL"}

    PAR.col3 <- PAR.col2; PAR.col3.1 <- PAR.col3.2 <- "NULL"
    if(length(PAR.col2) > 1){
      if(length(PAR.col2) > 2) stop(paste(
        "The function is not adapted for more than two PAR columns. Contact the",
        "maintainer of the package for help."))
      PAR.col3.1 <- PAR.col2[1]
      PAR.col3.2 <- PAR.col2[2]
      PAR.col3 <- "NULL"}

    WTD.col3 <- WTD.col2; WTD.col3.1 <- WTD.col3.2 <- "NULL"
    if(length(WTD.col2) > 1){
      if(length(WTD.col2) > 2) stop(paste(
        "The function is not adapted for more than two WTD columns. Contact the",
        "maintainer of the package for help."))
      WTD.col3.1 <- WTD.col2[1]
      WTD.col3.2 <- WTD.col2[2]
      WTD.col3 <- "NULL"}

    # Import raw data file from GAIA (.csv)
    data.raw <- data.raw %>%
      # Remove first row containing units
      filter(!Titles. == 'Units:') %>%
      # Modify useful column names
      setNames(gsub(CH.col2, "CH", names(.))) %>%
      setNames(gsub(Tsoil.col3, "_Tsoil", names(.))) %>%
      setNames(gsub(Tsoil.col3.1, "_Tsoil1", names(.))) %>%
      setNames(gsub(Tsoil.col3.2, "_Tsoil2", names(.))) %>%
      setNames(gsub(Tcham.col3, "_Tcham", names(.))) %>%
      setNames(gsub(Tcham.col3.1, "_Tcham1", names(.))) %>%
      setNames(gsub(Tcham.col3.2, "_Tcham2", names(.))) %>%
      setNames(gsub(SWC.col3, "_SWC", names(.))) %>%
      setNames(gsub(SWC.col3.1, "_SWC1", names(.))) %>%
      setNames(gsub(SWC.col3.2, "_SWC2", names(.))) %>%
      setNames(gsub(PAR.col3, "_PAR", names(.))) %>%
      setNames(gsub(PAR.col3.1, "_PAR1", names(.))) %>%
      setNames(gsub(PAR.col3.2, "_PAR2", names(.))) %>%
      setNames(gsub(WTD.col3, "_WTD", names(.))) %>%
      setNames(gsub(WTD.col3.1, "_WTD1", names(.))) %>%
      setNames(gsub(WTD.col3.2, "_WTD2", names(.))) %>%
      setNames(gsub(Op.stat.col2, "_Op.stat", names(.))) %>%
      # Extract information about light/dark measurements
      mutate(cover = if_else(grepl("Opaque", SEQUENCE), "Dark", if_else(
        grepl("Translucent", SEQUENCE), "Clear", NA))) %>%
      # Extract information about active chamber
      mutate(SEQUENCE = substr(SEQUENCE, 9, 9),
             SEQUENCE = ifelse(SEQUENCE == "", "Background", SEQUENCE),
             SEQUENCE = ifelse(SEQUENCE == "n", "ExecutionPlan", SEQUENCE)) %>%
      dplyr::rename(DATE_TIME = Titles., activ.cham = SEQUENCE) %>%
      # Detect new observations (Obs) and give a chamber UniqueID (chamID)
      arrange(DATE_TIME) %>%
      mutate(Obs = rleid(activ.cham),
             chamID = ifelse(activ.cham == "Background", paste(activ.cham, "_", Obs, sep = ""),
                             ifelse(activ.cham == "ExecutionPlan", paste(activ.cham, "_", Obs, sep = ""),
                                    paste("CH", activ.cham, "_", Obs, sep = "")))) %>%
      # Select only useful columns
      select(contains(c("DATE_TIME", "ChamID", "activ.cham", "cover", "Tsoil", "Tcham",
                        "SWC", "PAR", "WTD", "Op.stat", "ppb", "ppm", "RH%"))) %>%
      # Convert column class automatically
      type.convert(as.is = TRUE) %>%
      # Make sure that all gas data are class numerical
      mutate_at(gas.col2$new.name, as.numeric)

    # Remove useless columns when manual == TRUE
    if(manual == T){data.raw <- data.raw %>%
      select(contains(c("DATE_TIME", "Tsoil", "Tcham", "WTD",
                        "SWC", "PAR", "ppb", "ppm", "RH%")))}

    # Convert Humidity Sensor RH% to ppm
    if(any(grepl("Humidity", gas.col2$gas.col))){
      humidity.cols <- select(data.raw, contains("RH%")) %>%
        mutate_all(~.*10000) %>%
        setNames(gsub("RH%", "ppm", names(.)))

      data.raw <- cbind.data.frame(data.raw, humidity.cols)}

    # Group together all columns containing information and merge data
    if(manual == F) if(pivot == "long"){ # pivot long: only one column per parameter

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
        if(length(Tsoil.col) > 1){
          Tsoil1 <- data.raw %>% select(DATE_TIME, contains("Tsoil1")) %>%
            pivot_longer(contains("Tsoil1"), values_to = "Tsoil1", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          Tsoil2 <- data.raw %>% select(DATE_TIME, contains("Tsoil2")) %>%
            pivot_longer(contains("Tsoil2"), values_to = "Tsoil2", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(Tsoil1, by = c("DATE_TIME", "cham.probe")) %>%
            full_join(Tsoil2, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_Tsoil"))
        } else {
          Tsoil <- data.raw %>% select(DATE_TIME, contains("Tsoil")) %>%
            pivot_longer(contains("Tsoil"), values_to = "Tsoil", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(Tsoil, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_Tsoil"))
        }
      }
      ### Air temperature from each active chamber
      if(ncol(select(data.raw, contains("Tcham"))) > 0){
        if(length(Tcham.col) > 1){
          Tcham1 <- data.raw %>% select(DATE_TIME, contains("Tcham1")) %>%
            pivot_longer(contains("Tcham1"), values_to = "Tcham1", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          Tcham2 <- data.raw %>% select(DATE_TIME, contains("Tcham2")) %>%
            pivot_longer(contains("Tcham2"), values_to = "Tcham2", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(Tcham1, by = c("DATE_TIME", "cham.probe")) %>%
            full_join(Tcham2, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_Tcham"))
        } else {
          Tcham <- data.raw %>% select(DATE_TIME, contains("Tcham")) %>%
            pivot_longer(contains("Tcham"), values_to = "Tcham", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(Tcham, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_Tcham"))
        }
      }
      ### Soil water content from each active chamber
      if(ncol(select(data.raw, contains("SWC"))) > 0){
        if(length(SWC.col) > 1){
          SWC1 <- data.raw %>% select(DATE_TIME, contains("SWC1")) %>%
            pivot_longer(contains("SWC1"), values_to = "SWC1", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          SWC2 <- data.raw %>% select(DATE_TIME, contains("SWC2")) %>%
            pivot_longer(contains("SWC2"), values_to = "SWC2", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(SWC1, by = c("DATE_TIME", "cham.probe")) %>%
            full_join(SWC2, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_SWC"))
        } else {
          SWC <- data.raw %>% select(DATE_TIME, contains("SWC")) %>%
            pivot_longer(contains("SWC"), values_to = "SWC", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(SWC, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_SWC"))
        }
      }
      ### PAR from each active chamber
      if(ncol(select(data.raw, contains("PAR"))) > 0){
        if(length(PAR.col) > 1){
          PAR1 <- data.raw %>% select(DATE_TIME, contains("PAR1")) %>%
            pivot_longer(contains("PAR1"), values_to = "PAR1", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          PAR2 <- data.raw %>% select(DATE_TIME, contains("PAR2")) %>%
            pivot_longer(contains("PAR2"), values_to = "PAR2", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(PAR1, by = c("DATE_TIME", "cham.probe")) %>%
            full_join(PAR2, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_PAR"))
        } else {
          PAR <- data.raw %>% select(DATE_TIME, contains("PAR")) %>%
            pivot_longer(contains("PAR"), values_to = "PAR", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(PAR, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_PAR"))
        }
      }
      ### Water table depth from each active chamber
      if(ncol(select(data.raw, contains("WTD"))) > 0){
        if(length(WTD.col) > 1){
          WTD1 <- data.raw %>% select(DATE_TIME, contains("WTD1")) %>%
            pivot_longer(contains("WTD1"), values_to = "WTD1", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          WTD2 <- data.raw %>% select(DATE_TIME, contains("WTD2")) %>%
            pivot_longer(contains("WTD2"), values_to = "WTD2", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(WTD1, by = c("DATE_TIME", "cham.probe")) %>%
            full_join(WTD2, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_WTD"))
        } else {
          WTD <- data.raw %>% select(DATE_TIME, contains("WTD")) %>%
            pivot_longer(contains("WTD"), values_to = "WTD", names_to = "cham.probe") %>%
            mutate(cham.probe = substr(cham.probe, 3, 3))

          data.pivot <- data.pivot %>%
            full_join(WTD, by = c("DATE_TIME", "cham.probe")) %>%
            select(!contains("_WTD"))
        }
      }

      data.raw <- data.pivot
    }

    # Group together all columns containing information and merge data
    if(manual == F) if(pivot == "wide"){ # keep wide: one column per instrument per parameter

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

    # No pivot if manual = T, but adjust column names
    if(manual == T){
     data.raw <- data.raw %>%
       setNames(gsub(".*Tsoil", "Tsoil", names(.))) %>%
       setNames(gsub(".*Tcham", "Tcham", names(.))) %>%
       setNames(gsub(".*SWC", "SWC", names(.))) %>%
       setNames(gsub(".*PAR", "PAR", names(.))) %>%
       setNames(gsub(".*WTD", "WTD", names(.)))
    }

    # Add Op.stat if it cannot be found
    if(manual == F) if(!any(grepl("Op.stat", names(data.raw)))){
      data.raw$Op.stat <- 2
      data.raw$cham.probe <- NA
    }

    # Remove measurements from non-active chambers
    if(manual == F) if(active == TRUE){
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
      data.raw <- data.raw %>% mutate(DATE = substr(POSIX.time, 0, 10))

      if(manual == F){

        # Add flag
        data.raw <- data.raw %>%
          mutate(flag = ifelse(grepl(paste(flag, collapse = "|"), Op.stat), 1, 0)) %>%
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

      }

      # Add instrument precision for each gas
      data.raw[gas.col2$prec.name] <- 1
      for(i in 1:nrow(gas.col2)){
        data.raw <- data.raw %>%
          mutate_at(gas.col2$prec.name[i], ~gas.col2$prec.col[i])}

      # Add columns for instruments
      if(!is.null(inst1)) data.raw$inst1 <- instr1
      if(!is.null(inst2)) data.raw$inst2 <- instr2
      if(!is.null(inst3)) data.raw$inst3 <- instr3

      # Remove background
      if(manual == F) if(background == FALSE){
        data.raw <- data.raw %>% filter(activ.cham != "Background") %>%
          mutate_at("activ.cham", as.numeric)
      }

      # New function name
      if(as.character(match.call()[[1]]) == "GAIA_import") {
        warning(paste("All import functions have changed names in this new version of goFlux.",
                      "\nIn the future, use import.GAIA() instead of GAIA_import()"), call. = FALSE)
      }

      ## Warn if there is no match with unnecessary columns ####
      # SWC
      if(!any(grepl(SWC.col2[1], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "SWC.col '", SWC.col[1], "' was not found in column names.",
                      sep = ""), call. = F)}

      if(length(SWC.col2) > 1) if(!any(grepl(SWC.col2[2], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "SWC.col '", SWC.col[2], "' was not found in column names.",
                      sep = ""), call. = F)}

      # Tcham
      if(!any(grepl(Tcham.col2[1], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "Tcham.col '", Tcham.col[1], "' was not found in column names.",
                      sep = ""), call. = F)}

      if(length(Tcham.col2) > 1) if(!any(grepl(Tcham.col2[2], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "Tcham.col '", Tcham.col[2], "' was not found in column names.",
                      sep = ""), call. = F)}

      # Tsoil
      if(!any(grepl(Tsoil.col2[1], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "Tsoil.col '", Tsoil.col2[1], "' was not found in column names.",
                      sep = ""), call. = F)}

      if(length(Tsoil.col2) > 1) if(!any(grepl(Tsoil.col2[2], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "Tsoil.col '", Tsoil.col2[2], "' was not found in column names.",
                      sep = ""), call. = F)}

      # PAR
      if(!any(grepl(PAR.col2[1], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "PAR.col '", PAR.col[1], "' was not found in column names.",
                      sep = ""), call. = F)}

      if(length(PAR.col2) > 1) if(!any(grepl(PAR.col2[2], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "PAR.col '", PAR.col[2], "' was not found in column names.",
                      sep = ""), call. = F)}

      # WTD
      if(!any(grepl(WTD.col2[1], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "WTD.col '", WTD.col[1], "' was not found in column names.",
                      sep = ""), call. = F)}

      if(length(WTD.col2) > 1) if(!any(grepl(WTD.col2[2], names(try.import)))){
        warning(paste("In the file ", inputfile.name, " the matching string for ",
                      "WTD.col '", WTD.col[2], "' was not found in column names.",
                      sep = ""), call. = F)}

      # Duplicated gas columns
      if(nrow(gas.col.dup) > 0){
        warning(paste("The same gas was measured by two instruments. The suffix ",
                      "'inst1', 'inst2' or 'inst3' was added to each duplicated ",
                      "gas column to avoid confusion. 'inst1' = ", instr1, ", ",
                      "'inst2' = ", instr2, " and 'inst3' = ", instr3,
                      sep = ""), call. = F)
      }

      # Warning if file has no data
      if(nrow(data.raw) == 0){
        warning(paste(inputfile.name, "was imported succesfully, but no",
                      "measurements were detected."), call. = F)
      }

      # Save cleaned data file
      if(save == TRUE){
        # Create RData folder in working directory
        RData_folder <- paste(getwd(), "RData", sep = "/")
        if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}

        # Create output file: change extension to .RData, and
        # add instrument name and "imp" for import to file name
        if(grepl("\\.csv", inputfile.name)){
          file.name <- gsub(".*/", "", sub("\\.csv", "", inputfile))
        } else if(grepl("\\.txt", inputfile.name)){
          file.name <- gsub(".*/", "", sub("\\.txt", "", inputfile))
        } else file.name <- inputfile.name
        outputfile <- paste("GAIA_", file.name, "_imp.RData", sep = "")

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
#' @rdname GAIA_import
import.GAIA <- GAIA_import
