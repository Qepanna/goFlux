#' Import function for data from several gas analyzers (LI7810, LI7820, LI8100) saved on the Campbell Scientific CR1000X data logger
#'
#' Imports single raw gas measurement files from the Campbell Scientific CR1000X data logger
#' (\ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{CO[2]}{ASCII}} and
#' (\ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{CH[4]}{ASCII}} and
#' (\ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}})
#'
#' @instrumentlink Campbell Scientific CR1000X data logger|https://www.campbellsci.com/cr1000x
#' @param inputfile character string; the name of a file with the extension
#'                  .data or .txt
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
#'
#' @returns List of data frames containing raw data from each of the gas analyzers.
#'
#' @details
#'
#' Note that this function was designed for the following units in the raw file:
#' \itemize{
#'   \item ppm for \ifelse{html}{\out{CO<sub>2<}}{\eqn{CO[2]}{ASCII}}
#'   \item ppb for \ifelse{html}{\out{CH<sub>4<}}{\eqn{CH[4]}{ASCII}}
#'   \item ppb for \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}}
#'   \item ppm for \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'   \item kPa for pressure
#'   \item Celsius for temperature}
#' If your gas analyzers uses different units, either convert the units after
#' import, change the settings on your instrument, or contact the maintainer of
#' this package for support.
#'
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[goFlux]{k.max}}) in the non-linear flux calculation
#' (\code{\link[goFlux]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm for
#' #' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}, or 1 ppb for
#' #' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}}) and
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}}) to allow for more
#' curvature, especially for water vapor fluxes, or very long measurements, that
#' are normally curved. The default values given for instrument precision are
#' the ones found \href{https://www.licor.com/products/trace-gas/LI-7820}{online}
#' for the latest model of this instrument available at the time of the
#' creation of this function (11-2023).
#'
#' @include goFlux-package.R
#'
#' @seealso Use the wrapper function \code{\link[goFlux]{import2RData}}
#'          to import multiple files from the same folder path using any instrument.
#' @seealso See also, import functions for other instruments:
#'          \code{\link[goFlux]{import.DX4015}},
#'          \code{\link[goFlux]{import.eosMX12}},
#'          \code{\link[goFlux]{import.EGM5}},
#'          \code{\link[goFlux]{import.G2201i}},
#'          \code{\link[goFlux]{import.G2508}},
#'          \code{\link[goFlux]{import.G4301}},
#'          \code{\link[goFlux]{import.GAIA}},
#'          \code{\link[goFlux]{import.GasmetPD}},
#'          \code{\link[goFlux]{import.GT5000}},
#'          \code{\link[goFlux]{import.HT8850}},
#'          \code{\link[goFlux]{import.LI6400}},
#'          \code{\link[goFlux]{import.LI7810}},
#'          \code{\link[goFlux]{import.LI8100}},
#'          \code{\link[goFlux]{import.LI8150}},
#'          \code{\link[goFlux]{import.LI8200}},
#'          \code{\link[goFlux]{import.LI8250}},
#'          \code{\link[goFlux]{import.N2OM1}},
#'          \code{\link[goFlux]{import.N2Oi2}},
#'          \code{\link[goFlux]{import.skyline}},
#'          \code{\link[goFlux]{import.uCH4}},
#'          \code{\link[goFlux]{import.uN2O}},
#'          \code{\link[goFlux]{import.UGGA}},
#'          \code{\link[goFlux]{import.PS3010}}
#'
#' @seealso See \code{\link[base]{timezones}} for a description of the underlying
#'          timezone attribute.
#'
##' @examples
##' # Load file from downloaded package
##' file.path <- system.file("extdata", "Cr1000X/CR1000X.data", package = "goFlux")
##'
##' # Run function
##' imp.CR1000X <- import.CR1000X(inputfile = file.path)
#' @export

import.CR1000X <- function(inputfile, timezone = "UTC",
                          save = FALSE, keep_all = FALSE){
  
  # Check arguments
  if (missing(inputfile)) stop("'inputfile' is required")
  if (!is.character(inputfile)) stop("'inputfile' must be of class character")
  #if (length(date.format) != 1) stop("'date.format' must be of length 1")
  #if (!is.character(date.format)) stop("'date.format' must be of class character")
  #if (!any(grepl(date.format, c("ymd", "dmy", "mdy")))) {
  #  stop("'date.format' must be one of the following: 'ymd', 'dmy' or 'mdy'")}
  if (!is.character(timezone)) stop("'timezone' must be of class character")
  if (save != TRUE & save != FALSE) stop("'save' must be TRUE or FALSE")
  if (keep_all != TRUE & keep_all != FALSE) stop("'keep_all' must be TRUE or FALSE")
  #if(is.null(prec)) stop("'prec' is required") else{
  #  if(!is.numeric(prec)) stop("'prec' must be of class numeric") else{
  #    if(length(prec) != 2) stop("'prec' must be of length 2")}}
  
  ## Assign NULL to variables without binding
  #H2O_ppm <- H2O <- N2O <- TIME <- DATE <- DATAH <- N2Odry_ppb <-
  #  REMARK <- POSIX.warning <- import.error <- NULL
  
  # Input file name
  library(dplyr)
  #inputfile<-"C:/Users/au787802/OneDrive - Aarhus universitet/Desktop/Chambers-sync_Soil_flux.dat"
  inputfile.name <- gsub(".*/", "", inputfile)
  
  # Try to load data file
  try.import <- tryCatch(
    {read.table(inputfile, sep = ",",nrows = 20,skip=1,header=T)},
    error = function(e) {import.error <<- e}
  )
  
  
  if(inherits(try.import, "simpleError")){
   warning("Error occurred in file ", inputfile.name, ":\n", "   ",
            import.error, call. = F)
  } else {
    
    
    # Import raw data file from LI7820 (.data or .txt)
    data.raw <- read.table(inputfile, sep = ",",skip = 1,header=T) %>%
      # Remove the row "DATAU"
      #filter(!DATAH == 'DATAU') %>% select(!DATAH) %>%
      # Convert column class automatically
      type.convert(as.is = TRUE) %>%
      mutate(LI7810_Remark = as.character(LI7810_Remark),
             LI7820_Remark = as.character(LI7820_Remark)) %>%
      #remove first two rows
      slice(-(1:2))

      
    
    # Keep only useful columns for gas flux calculation
    if(keep_all == FALSE){
      data.raw <- data.raw %>%
        select(TIMESTAMP, LI7810_H2O, LI7810_CO2, LI7810_CH4, LI7820_H2O, LI7820_N20, LI8100_H2O, LI8100_CO2)}
    
    
    # Create a new column containing date and time (POSIX format)
    #timezone="UTC"
    data.raw$POSIX.time <- as.POSIXct(data.raw$TIMESTAMP,format = "%Y-%m-%d %H:%M:%S" , tz = timezone)


    #convert into long format
    library(tidyr)
    library(dplyr)
    data.raw <- data.raw %>%
      pivot_longer(
        cols = starts_with("LI"),
        names_to = c("GAmodel", ".value"),
        names_sep = "_"
      )
      
    # Standardize column names
    data.raw<-data.raw %>%
    rename(CO2dry_ppm = CO2, CH4dry_ppb = CH4, N2Odry_ppb = N20, H2O_ppm = H2O)
    
    #split into separate files by gas analyzer
    data.raw.LI7810<-data.raw[data.raw$GAmodel=="LI7810",]
    data.raw.LI7810$GAmodel <- NULL
    data.raw.LI7820<-data.raw[data.raw$GAmodel=="LI7820",]
    data.raw.LI7820$GAmodel <- NULL
    data.raw.LI8100<-data.raw[data.raw$GAmodel=="LI8100",]
    data.raw.LI8100$GAmodel <- NULL
      
      # Add instrument precision for each gas
      data.raw.LI7810 <- data.raw.LI7810 %>%
        mutate(H2O_prec = 45, CO2_prec = 3.5,CH4_prec=0.6)
      data.raw.LI7820 <- data.raw.LI7820 %>%
        mutate(H2O_prec = 45,N2O_prec=0.4)
      data.raw.LI8100 <- data.raw.LI8100 %>%
        mutate(H2O_prec = 10, CO2_prec = 1)
      
      
      # New function name
      if (as.character(match.call()[[1]]) == "CR1000X_import") {
        warning(paste("All import functions have changed names in this new version of goFlux.",
                      "\nIn the future, use import.CR1000X() instead of CR1000X_import()"), call. = FALSE)
      }
      
      # Save cleaned data file
      #save=TRUE
      if(save == TRUE){
        # Create RData folder in working directory
        RData_folder <- paste(getwd(), "RData", sep = "/")
        if(dir.exists(RData_folder) == FALSE){dir.create(RData_folder)}
        
        # Create output file: change extension to .RData, and
        # add instrument name and "imp" for import to file name
        file.name <- gsub(".*/", "", sub("\\.data|\\.txt", "", inputfile))
        outputfile1 <- paste("LI7810_", file.name, "_imp.RData", sep = "")
        outputfile2 <- paste("LI7820_", file.name, "_imp.RData", sep = "")
        outputfile3 <- paste("LI8100_", file.name, "_imp.RData", sep = "")
        
        save(data.raw.LI7810, file = paste(RData_folder, outputfile1, sep = "/"))
        
        message(inputfile.name, " saved as ", outputfile1,
                " in RData folder, in working directory\n", sep = "")
        
        save(data.raw.LI7820, file = paste(RData_folder, outputfile2, sep = "/"))
        
        message(inputfile.name, " saved as ", outputfile2,
                " in RData folder, in working directory\n", sep = "")
        
        save(data.raw.LI8100, file = paste(RData_folder, outputfile3, sep = "/"))
        
        message(inputfile.name, " saved as ", outputfile3,
                " in RData folder, in working directory\n", sep = "")
      }
      # saves a list of data frames containing the raw measurements from the individual gas analyzers 
      if(save == FALSE){
        return(list(imp.LI7810 = data.raw.LI7810,
                    imp.LI7820 = data.raw.LI7820,
                    imp.LI8100 = data.raw.LI8100))
      }
    }
}