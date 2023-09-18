#' Import function for LI-COR smart chamber (LI-8200)
#'
#' Imports single raw data files from the LI-COR smart chamber (LI-8200)
#'
#' @param inputfile the name of a file with the extension .json
#' @param save logical. If save = TRUE, save the file as Rdata in a Rdata folder
#'             in the current working directory. If save = FALSE, return the file
#'             in the Console, or load in the Environment if assigned to an object.
#' @param timezone a time zone in which to import the data to POSIXct format.
#'                 Default is "UTC". Note about time zone: I recommend using
#'                 the time zone "UTC" to avoid any issue related to summer
#'                 time and winter time changes.
#' @returns a data frame
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' # Load file from downloaded package
#' file.path <- system.file("extdata", "LI8200/example_LI8200.json", package = "GoFluxYourself")
#'
#' # Run function
#' LI8200.data <- LI8200_import(inputfile = file.path)
#'
#' @export
#'
LI8200_import <- function(inputfile, timezone = "UTC", save = FALSE){

  # Assign NULL to variables without binding
  h2o <- cham.close <- deadband <- POSIX.time <- plot.ID <- n2o <- Etime <-
    ch4 <- co2 <- H2O_ppm <- chamber_t <- chamber_p <- Vcham <- Area <-
    soilp_m <- . <- soilp_t <- DATE <- cham.open <- start.time <- NULL

  # Load data file
  data.raw.ls <- fromJSON(file = inputfile)

  # Create empty lists to extract objects from lists of data.raw.ls
  df.ls <- list()
  plot.ID.ls <- list()
  cham.close.ls <- list()
  deadband.ls <- list()
  Vcham.ls <- list()
  offset.ls <- list()
  Area.ls <- list()

  # Loop through data.raw.ls
  for (i in 1:length(data.raw.ls$datasets)) {
    # loop through all measurements reps
    all.reps <- data.raw.ls$datasets[[i]][[1]]$reps
    # Extract plot.ID from list "datasets"
    plot.ID.ls[[i]] <- names(data.raw.ls$datasets[[i]])
    if(length(all.reps) > 0) {
      rep.ls <- list()
      for (j in 1:length(all.reps)) {
        # Extract gas measurements from "data"
        rep.ls[[j]] <- all.reps[[j]]$data
        # Extract chamber closure time from "Date"
        cham.close.ls[[i]] <- all.reps[[j]]$header$Date
        # Extract deadband before measurement start from "DeadBand"
        deadband.ls[[i]] <- all.reps[[j]]$header$DeadBand
        # Extract chamber volume from "Vcham"
        Vcham.ls[[i]] <- all.reps[[j]]$header$TotalVolume
        # Extract chamber offset (collar height) from "Offset"
        offset.ls[[i]] <- all.reps[[j]]$header$Offset
        # Extract chamber Area (collar inner Area) from "Area"
        Area.ls[[i]] <- all.reps[[j]]$header$Area
      }
      # Convert list of data into a dataframe
      df.ls[[i]] <- map_df(rep.ls, ~as.data.frame(.x), .id="rep")

    } else {
      cham.close.ls[[i]] <- NA
      deadband.ls[[i]] <- NA
      Vcham.ls[[i]] <- NA
      offset.ls[[i]] <- NA
      Area.ls[[i]] <- NA
    }
  }

  # Convert lists of metadata into matrix
  metadata <- cbind.data.frame(
    Obs = as.character(c(1:length(data.raw.ls$datasets))),
    plot.ID = unlist(plot.ID.ls),
    cham.close = unlist(cham.close.ls),
    deadband = unlist(deadband.ls),
    Vcham = unlist(Vcham.ls),
    offset = unlist(offset.ls),
    Area = unlist(Area.ls))

  # Create extra columns for CO2, CH4 or N2O, if missing
  cols <- c(co2 = NA_real_, ch4 = NA_real_, n2o = NA_real_)

  # Convert list of dataframe into a dataframe
  data.raw <- map_df(df.ls, ~as.data.frame(.x), .id="Obs") %>%
    # Add metadata and extra columns for additional gases
    left_join(as.data.frame(metadata), by = "Obs")%>%
    add_column(!!!cols[!names(cols) %in% names(.)]) %>%
    # Convert H2O_mmol/mol into H2O_ppm
    mutate(H2O_ppm = h2o*1000) %>%
    # Create a column for POSIX time
    mutate(cham.close = as.POSIXct(cham.close, tz = timezone),
           POSIX.time = cham.close + timestamp,
           Etime = timestamp - deadband,
           DATE = substr(POSIX.time, 0, 10)) %>%
    # Select and rename useful columns
    select(POSIX.time, plot.ID, rep, cham.close, deadband, Etime,
           N2Odry_ppb = n2o, CH4dry_ppb = ch4, CO2dry_ppm = co2, H2O_ppm,
           Tcham = chamber_t, Pcham = chamber_p, Vcham, Area,
           SWC = soilp_m, Tsoil = soilp_t, DATE) %>%
    # Add start.time and cham.open (POSIX.time)
    group_by(plot.ID, rep) %>%
    mutate(start.time = cham.close + deadband,
           cham.open = last(POSIX.time),
           obs.length = as.numeric(cham.open - cham.close, units = "secs")+1) %>%
    ungroup() %>%
    # Create chamID and flag
    mutate(chamID = paste(plot.ID, rep, sep = "_"),
           flag = if_else(between(POSIX.time, start.time, cham.open), 1, 0))

  # Save cleaned data file
  if(save == TRUE){
    # Create Rdata folder in working directory
    Rdata_folder <- paste(getwd(), "Rdata", sep = "/")
    if(dir.exists(Rdata_folder) == FALSE){dir.create(Rdata_folder)}

    # Create output file: change extension to .RData, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.json", "", inputfile))
    outputfile <- paste("LI8200_", file.name, "_imp.RData", sep = "")

    save(data.raw, file = paste(Rdata_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in Rdata folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
