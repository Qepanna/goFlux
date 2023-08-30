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
#' @param save logical. If save = TRUE, save the file as Rdata in the current
#'             working directory. If save = FALSE, return the file in the Console,
#'             or load in the Environment if assigned to an object.
#' @returns a data frame
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' # Examples on how to use:
#' file.path <- system.file("extdata", "LGR/example_LGR.txt", package = "GoFluxYourself")
#'
#' # 1. If you want to import the data and work with it directly
#' LGR.data <- LGR_import(inputfile = file.path)
#'
#' # 2. Or if you want to import the data and save it as Rdata
#' LGR_import(inputfile = file.path, save = TRUE)
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
    save(data.raw, file = paste(sub("\\.txt", "\\.Rdata", inputfile), sep = "/"))
    message("'data.raw' was saved as ", getwd(), "/",
            paste(sub("\\.txt", "\\.Rdata", inputfile), sep = "/"), sep = "")

  }

  if(save == FALSE){
    return(data.raw)
  }

}
