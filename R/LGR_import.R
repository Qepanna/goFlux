#' Import function for Los Gatos Research GHG analyzers
#'
#' Imports single raw gas measurement files from the ultra-portable GHG analyzers
#' (UGGA and m-GGA) from Los Gatos Research (CO2, CH4 and H2O) with the extension .txt
#'
#' @param inputfile the name of a file with the extension .txt
#' @param date.format Date format. Chose one of the following: "dmy", "ymd", or "mdy".
#'                    Default is "dmy" as it is the date format from the example
#'                    data file provided.
#' @param timezone a time zone in which to import the data to POSIXct format.
#'                 Default is "UTC". Note about time zone: I recommend using
#'                 the time zone "UTC" to avoid any issue related to summer
#'                 time and winter time changes.
#' @param save logical. If save = TRUE, save the file as Rdata in a Rdata folder
#'             in the current working directory. If save = FALSE, return the file
#'             in the Console, or load in the Environment if assigned to an object.
#' @returns a data frame
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' # Examples on how to use:
#' file.path <- system.file("extdata", "LGR/example_LGR.txt", package = "GoFluxYourself")
#'
#' LGR.data <- LGR_import(inputfile = file.path)
#'
#' @export
#'
LGR_import <- function(inputfile, date.format = "dmy",
                       timezone = "UTC", save = FALSE){

  # Assign NULL to variables without binding
  POSIX.time <- DATE_TIME <- H2O_ppm <- CH4dry_ppb <- Time <- . <-
    CH4dry_ppm <- CO2dry_ppm <- NULL

  # Load data file
  data.raw <- read.delim(inputfile, skip = 1, sep = ",") %>%
    # Clean column names
    `colnames<-`(gsub("\\.", "",
                      gsub("X.", "",
                           gsub("d_", "dry_",
                                gsub("__", "_", names(.)))))) %>%
    # Remove rows at the end of the file
    drop_na(CO2dry_ppm) %>%
    # Convert ppm into ppb for CH4dry
    mutate(CH4dry_ppb = CH4dry_ppm*1000) %>%
    # Keep only useful columns
    select(DATE_TIME = Time, CO2dry_ppm, CH4dry_ppb, H2O_ppm) %>%
    # Replace characters in Time ("/" -> "-") and remove first space
    mutate(DATE_TIME = gsub("/", "-", sub(" ", "" , DATE_TIME)))

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

  # Add a column for DATE alone
  data.raw <- data.raw %>% mutate(DATE = substr(POSIX.time, 0, 10))

  # Save cleaned data file
  if(save == TRUE){
    # Create Rdata folder in working directory
    Rdata_folder <- paste(getwd(), "Rdata", sep = "/")
    if(dir.exists(Rdata_folder) == FALSE){dir.create(Rdata_folder)}

    # Create output file: change extension to .Rdata, and
    # add instrument name and "imp" for import to file name
    file.name <- gsub(".*/", "", sub("\\.txt", "", inputfile))
    outputfile <- paste("LGR_", file.name, "_imp.Rdata", sep = "")

    save(data.raw, file = paste(Rdata_folder, outputfile, sep = "/"))

    message(file.name, " saved as ", outputfile, " in Rdata folder, in working directory", sep = "")
  }

  if(save == FALSE){
    return(data.raw)
  }

}
