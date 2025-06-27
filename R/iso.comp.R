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
#' @param CO2dry_ppm,delta_13C character string; specifies which columns should be
#'                  used for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#'                  measurements, and the delta 13C, respectively.
#' @param save.plots character string; a file path with the extension .pdf to
#'                   save Keeling plots. By default plots are not saved.
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

iso.comp <- function(dataframe, CO2dry_ppm = "CO2dry_ppm",
                     delta_13C = "Delta_Raw_iCO2", save.plots){

  # Check arguments
  if(missing(dataframe)) stop("'dataframe' is required")
  if(!is.null(dataframe) & !is.data.frame(dataframe)) stop("'dataframe' must be of class data.frame")

  if(is.null(CO2dry_ppm)) stop("'CO2dry_ppm' is required") else {
    if(!is.character(CO2dry_ppm)) stop("'CO2dry_ppm' must be of class character")}

  if(!any(grepl(CO2dry_ppm, names(dataframe)))){
    stop(paste("The matching string for CO2dry_ppm '", CO2dry_ppm,
               "' was not found in dataframe.", sep = ""))}

  if(is.null(delta_13C)) stop("'delta_13C' is required") else {
    if(!is.character(delta_13C)) stop("'delta_13C' must be of class character")}

  if(!any(grepl(delta_13C, names(dataframe)))){
    stop(paste("The matching string for delta_13C '", delta_13C,
               "' was not found in dataframe.", sep = ""))}

  if(!missing(save.plots)){
    if(!is.character(save.plots)) stop("'save.plot' must be a character string")}

  # Split dataframe into a list of dataframes for each UniqueID
  data_split <- dataframe %>%
    # Split dataset by UniqueID
    group_by(UniqueID) %>% group_split() %>% as.list()

  # Create an empty list to store results
  res.ls <- list()
  plot.ls <- list()

  for(i in 1:length(data_split)){

    # Extract data
    CO2_meas <- Reduce("c", data_split[[i]][, CO2dry_ppm])
    delta_13C_meas <- Reduce("c", data_split[[i]][, delta_13C])
    UniqueID <- unique(na.omit(data_split[[i]]$UniqueID))

    # Inverse of CO2 concentration
    inv.CO2 <- 1/CO2_meas

    # Linear regression
    LM <- lm(delta_13C_meas ~ inv.CO2)

    # Extract values from the linear fit
    iso.comp <- summary(LM)[[4]][1,1]

    # Results
    res.ls[[i]] <- cbind.data.frame(UniqueID, iso.comp)

    # Plot results
    if(!missing(save.plots)){
      data <- cbind.data.frame(delta_13C_meas, inv.CO2)
      if(iso.comp < 0) direction <- " - " else direction <- " + "
      slope <- round(summary(LM)$coefficients[2])
      intercept <- signif(iso.comp,3)
      r2 <- round(as.numeric(summary(lm(fitted(LM) ~ delta_13C_meas))[9])[1],3)
      plot.ls[[i]] <-
        ggplot(data, aes(inv.CO2, delta_13C_meas)) +
        geom_abline(slope = summary(LM)$coefficients[2],
                    intercept = summary(LM)$coefficients[1],
                    col = "red", linewidth = 1) +
        geom_point(col = "black") +
        labs(title = paste("UniqueID:", UniqueID,
                           "\nIsotopic composition (intercept) = ",
                           intercept, "\u2030"),
             subtitle = bquote(Equation:~delta^13*"C = "*.(slope)*"/"*CO["2"]*
                                 .(paste(sep = "", direction, abs(intercept)))
                               ~~~~~~~~R^2~"= "*.(r2))) +
        ylab(expression(delta^13*"C of "*CO["2"]*" (\u2030)")) +
        xlab(expression(1/CO["2"]*" ("*ppm^-1*")")) +
        theme_bw() +
        theme(plot.title = element_text(size = 11))
    }
  }

  # Save plots as pdf
  if(length(plot.ls) > 0){
    pdf(file = save.plots, width = 11.6, height = 8.2)
    for (p in 1:length(plot.ls)){
      if(!is.null(plot.ls[[p]])) print(plot.ls[[p]])
    }
    dev.off()
  }

  # Unlist results
  results <- map_df(res.ls,  ~as.data.frame(.x))

  # Return results
  return(results)
}
