#' Determine the isotopic composition of
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} measurements
#'
#' Extracts the y-intercept of the regression between the isotope ratios plotted
#' againts the inverse of the \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#' concentration
#'
#' @param dataframe a data.frame containing measurements of isotopic fractions of
#'                  \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#'                  and a \code{UniqueID} (or \code{chamID}) per measurements.
#' @param CO2_12,delta_13C character string; specifies which columns should be
#'                         used for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#'                         measurements, and the delta 13C, respectively.
#'
#' @returns A data frame of the isotopic composition per measurement (UniqueID)
#'
#' @include goFlux-package.R
#'
#' @seealso \code{\link[goFlux]{import.G2201i}}
#'
#' @examples
#' data(manID.G2201i)
#' my.iso.comp <- iso.comp(manID.G2201i)
#' @export

iso.comp <- function(dataframe, CO2_12 = "CO2dry_ppm", delta_13C = "Delta_Raw_iCO2"){

  # Check arguments
  if(missing(dataframe)) stop("'dataframe' is required")
  if(!is.null(dataframe) & !is.data.frame(dataframe)) stop("'dataframe' must be of class data.frame")

  if(is.null(CO2_12)) stop("'CO2_12' is required") else {
    if(!is.character(CO2_12)) stop("'CO2_12' must be of class character")}

  if(!any(grepl(CO2_12, names(dataframe)))){
    stop(paste("The matching string for CO2_12 '", CO2_12,
               "' was not found in dataframe.", sep = ""))}

  if(is.null(delta_13C)) stop("'delta_13C' is required") else {
    if(!is.character(delta_13C)) stop("'delta_13C' must be of class character")}

  if(!any(grepl(delta_13C, names(dataframe)))){
    stop(paste("The matching string for delta_13C '", delta_13C,
               "' was not found in dataframe.", sep = ""))}

  # Split dataframe into a list of dataframes for each UniqueID
  data_split <- dataframe %>%
    # Split dataset by UniqueID
    group_by(UniqueID) %>% group_split() %>% as.list()

  # Create an empty list to store results
  res.ls <- list()

  for(i in 1:length(data_split)){

    # Extract data
    CO2_12_meas <- Reduce("c", data_split[[i]][, CO2_12])
    delta_13C_meas <- Reduce("c", data_split[[i]][, delta_13C])
    UniqueID <- unique(na.omit(data_split[[i]]$UniqueID))

    # Inverse of CO2 concentration
    inv.CO2 <- 1/CO2_12_meas

    # Linear regression
    LM <- lm(delta_13C_meas ~ inv.CO2)

    # Extract values from the linear fit
    iso.comp <- summary(LM)[[4]][1,1]

    # Results
    res.ls[[i]] <- cbind.data.frame(UniqueID, iso.comp)
  }

  # Unlist results
  results <- map_df(res.ls,  ~as.data.frame(.x))

  # Return results
  return(results)
}
