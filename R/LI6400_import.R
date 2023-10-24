#' Import function for LI-COR GHG analyzer LI-6400
#'
#' Imports single raw gas measurement files from the LI-COR 6400
#' (CO2 and H2O GHG analyzer)
#'
#' @param inputfile character string; the name of a file with the extension .txt
#' @param date.format character string; chose one of the following: "dmy", "ymd",
#'                    or "mdy". Default is "ymd", as it is the date format from
#'                    the example data file provided.
#' @param timezone character string; a time zone in which to import the data to
#'                 POSIXct format. Default is "UTC". Note about time zone: it is
#'                 recommended to use the time zone "UTC" to avoid any issue
#'                 related to summer time and winter time changes.
#' @param save logical; if save = TRUE, saves the file as RData in a RData folder
#'             in the current working directory. If save = FALSE, returns the file
#'             in the Console, or load in the Environment if assigned to an object.
#' @returns a data frame
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso Use the wrapper function \code{\link[GoFluxYourself]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso Import functions for individual instruments:
#'          \code{\link[GoFluxYourself]{G2508_import}},
#'          \code{\link[GoFluxYourself]{GAIA_import}},
#'          \code{\link[GoFluxYourself]{LGR_import}},
#'          \code{\link[GoFluxYourself]{LI7810_import}},
#'          \code{\link[GoFluxYourself]{LI7820_import}},
#'          \code{\link[GoFluxYourself]{LI8100_import}},
#'          \code{\link[GoFluxYourself]{LI8200_import}}
#' @seealso See \code{\link[base]{timezones}} for a description of the underlying
#'          timezone attribute.
#'
#' @examples
#' # Load file from downloaded package
#' file.path <- system.file("extdata", "LI6400/example_LI6400.txt", package = "GoFluxYourself")
#'
#' # Run function
#' LI6400.data <- LI6400_import(inputfile = file.path)
#'
#' @export
#'
LI6400_import <- function(inputfile, date.format = "mdy",
                          timezone = "UTC", save = FALSE){

  # Check arguments
  if (missing(inputfile)) stop("'inputfile' is required")
  if (!is.character(inputfile)) stop("'inputfile' must be of class character")
  if (length(date.format) != 1) stop("'date.format' must be of length 1")
  if (!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
    stop("'date.format' must be of class character and one of the following: 'ymd', 'dmy' or 'mdy'")}
  if (!is.character(timezone)) stop("'timezone' must be of class character")
  if (save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")

  # Assign NULL to variables without binding
  cham.close <- cham.open <- POSIX.time <- chamID <- DATE <- TIME <- H2O_mmol <-
    Etime <- CO2dry_ppm <- Meas.type <- plotID <- H2OS <- Cdry <- Press <-
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
    select(Obs, plotID = Plot., Meas.type = Meas.type..NEE.ER.,
           TIME = HHMMSS, Etime = ETime, Mode,
           Tcham = Tair, Pcham = Press,
           CO2dry_ppm = Cdry, H2O_mmol = H2OS) %>%
    # Create a unique chamber ID
    mutate(chamID = paste(plotID, Meas.type, sep = "_")) %>%
    # Remove comments
    filter(!CO2dry_ppm == "") %>% filter(!Obs == "Obs") %>%
    # Convert column class automatically
    type.convert(as.is = TRUE) %>%
    # Remove Mode == 2 (indicates when a measurement ends)
    filter(!Mode == 2) %>%
    # As the LICOR only saves rows when you have passed all promts after
    # pressing start, Etime below 4 seconds is not possible.
    filter(Etime > 4) %>%
    # Remove NAs and negative gas measurements, if any
    filter(CO2dry_ppm > 0) %>%
    filter(H2O_mmol > 0) %>%
    # Convert mmol into ppm for H2O
    mutate(H2O_ppm = H2O_mmol*1000) %>%
    # Remove rows with duplicated times
    # distinct(TIME, .keep_all = TRUE) %>%
    # Create new columns containing date and time (POSIX format)
    mutate(DATE = met.date,
           POSIX.time = as.POSIXct(paste(DATE, TIME), tz = timezone)) %>%
    # Remove unnecessary columns
    select(!c(Obs, Mode)) %>%
    # Extract other useful information from metadata
    mutate(Area = as.numeric(metadata[which(metadata[,3] == "Area")[1],4]),
           Vcham = as.numeric(metadata[which(metadata[,3] == "Vtot")[1],4]),
           offset = as.numeric(metadata[which(metadata[,3] == "Offset")[1],4])) %>%
    # Add time related variables (POSIX.time)
    group_by(chamID) %>%
    mutate(cham.close = first(POSIX.time),
           start.time = cham.close,
           cham.open = last(POSIX.time),
           Etime = as.numeric(POSIX.time - start.time, units = "secs")) %>%
    ungroup() %>%
    # Add flag
    mutate(flag = 1)

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

    message(file.name, " saved as ", outputfile, " in RData folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
