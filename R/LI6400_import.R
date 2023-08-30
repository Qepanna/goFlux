#' Import function for LI-COR GHG analyzer LI-6400
#'
#' Imports single raw gas measurement files from the LI-COR 6400
#' (CO2 and H2O GHG analyzer)
#'
#' @param inputfile the name of a file with the extension .txt
#' @param date.format Date format. Chose one of the following: "dmy", "ymd", or "mdy".
#'                    Default is "mdy" as it is the date format from the example
#'                    data file provided.
#' @param timezone a time zone in which to import the data to POSIXct format.
#'                 Default is "UTC". Note about time zone: I recommend using
#'                 the time zone "UTC" to avoid any issue related to summer
#'                 time and winter time changes.
#' @param save logical. If save = TRUE, save the file as Rdata in the current
#'             working directory. If save = FALSE, return the file in the Console,
#'             or load in the Environment if assigned to an object.
#' @returns a data frame
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' # Examples on how to use:
#' file.path <- system.file("extdata", "LI6400/example_LI6400.txt", package = "GoFluxYourself")
#'
#' # 1. If you want to import the data and work with it directly
#' LI6400.data <- LI6400_import(inputfile = file.path)
#'
#' # 2. Or if you want to import the data and save it as Rdata
#' LI6400_import(inputfile = file.path, save = TRUE)
#' @export
#'
LI6400_import <- function(inputfile, date.format = "mdy",
                          timezone = "UTC", save = FALSE){

  # Assign NULL to variables without binding
  start.time <- end.time <- POSIX.time <- chamID <- DATE <- TIME <- H2O_mmol <-
    Etime <- CO2dry_ppm <- Meas.type <- plot.ID <- H2OS <- Cdry <- Press <-
    Tair <- Mode <- ETime <- HHMMSS <- Meas.type..NEE.ER. <- Plot. <- Obs <-
    V4 <- V1 <- NULL

  # Find how many rows need to be skipped
  skip.rows <- tryapply(seq(1:30), function(i) {
    if (read.delim(inputfile, header = F, skip = i)[1,1] == "$STARTOFDATA$") {
      return(i) }
  }) %>% unlist %>% as.numeric() +1

  # Import metadata from LI6400 (.txt)
  metadata <- read.delim(inputfile, header = F, comment.char = "<") %>%
    filter(V1 == "Const=" | V4 == "")

  # Extract date from metadata
  if (date.format == "dmy") {
    met.date <- dmy(substr(metadata[2,1], 5, nchar(metadata[2,1])-9), tz = timezone)
  }
  if (date.format == "mdy") {
    met.date <- mdy(substr(metadata[2,1], 5, nchar(metadata[2,1])-9), tz = timezone)
  }
  if (date.format == "ymd") {
    met.date <- ymd(substr(metadata[2,1], 5, nchar(metadata[2,1])-9), tz = timezone)
  }

  # Import raw data file from LI6400 (.txt)
  data.raw <- read.delim(inputfile, skip = skip.rows) %>%
    # Select useful columns and standardize column names
    select(Obs, plot.ID = Plot., Meas.type = Meas.type..NEE.ER.,
           TIME = HHMMSS, Etime = ETime, Mode,
           TA_cham = Tair, pressure_cham = Press,
           CO2dry_ppm = Cdry, H2O_mmol = H2OS) %>%
    # Create a unique chamber ID
    mutate(chamID = paste(plot.ID, Meas.type, sep = "_")) %>%
    # Remove comments
    filter(!CO2dry_ppm == "") %>% filter(!Obs == "Obs") %>%
    # Convert column class automatically
    type.convert(as.is = TRUE) %>%
    # Remove Mode == 2 (indicates when a measurement ends)
    filter(!Mode == 2) %>%
    # As the LICOR only saves rows when you have passed all promts after
    # pressing start, Etime below 4 seconds is not possible.
    filter(Etime > 4) %>%
    # Convert mmol into ppm for H2O
    mutate(H2O_ppm = H2O_mmol*1000) %>%
    # Remove rows with duplicated times
    distinct(TIME, .keep_all = TRUE) %>%
    # Create new columns containing date and time (POSIX format)
    mutate(DATE = met.date,
           POSIX.time = as.POSIXct(paste(DATE, TIME), tz = timezone)) %>%
    # Remove unnecessary columns
    select(!c(Obs, Mode)) %>%
    # Extract other useful information from metadata
    mutate(Area = as.numeric(metadata[which(metadata[,3] == "Area")[1],4]),
           Vcham = as.numeric(metadata[which(metadata[,3] == "Vtot")[1],4]),
           offset = as.numeric(metadata[which(metadata[,3] == "Offset")[1],4])) %>%
    # Add start.time and end.time (POSIX.time)
    group_by(chamID) %>%
    mutate(start.time = first(POSIX.time),
           end.time = last(POSIX.time),
           Etime = seq(0, length(end.time - start.time)-1, 1)) %>% ungroup()

  # Save cleaned data file
  if(save == TRUE){
    save(data.raw, file = paste(sub("\\.txt", "\\.Rdata", inputfile), sep = "/"))
    message("'data.raw' was saved as ", getwd(), "/",
            paste(sub("\\.txt", "\\.Rdata", inputfile), sep = "/"), sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
