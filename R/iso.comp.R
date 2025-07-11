#' Determine the isotopic composition of
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} and
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} measurements
#'
#' Extracts the y-intercept of the regression between the isotope ratios plotted
#' against the inverse of the gas concentration
#' (\ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} or
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}})
#'
#' @param dataframe a data.frame containing measurements of isotopic fractions of
#'                  \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}} or
#'                  \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}}
#'                  and a \code{UniqueID} (or \code{chamID}) per measurements.
#' @param CO2dry_ppm,delta_13C character string; specifies which columns should be
#'                  used for \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}}
#'                  measurements, and the delta 13C, respectively.
#' @param N2Odry_ppb,delta_15N,delta_18O character string; specifies which columns
#'                  should be used for \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}}
#'                  measurements, the delta 15N and the delta 18O, respectively.
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

iso.comp <- function(dataframe,
                     CO2dry_ppm = "CO2dry_ppm", delta_13C = "Delta_Raw_iCO2",
                     N2Odry_ppb = "N2Odry_ppb", delta_15N = "d15N", delta_18O = "d18O",
                     save.plots){

  # Check arguments
  if(missing(dataframe)) stop("'dataframe' is required")
  if(!is.null(dataframe) & !is.data.frame(dataframe)) stop("'dataframe' must be of class data.frame")

  if(!is.null(CO2dry_ppm) & !is.character(CO2dry_ppm)) stop("'CO2dry_ppm' must be of class character")
  if(!is.null(delta_13C) & !is.character(delta_13C)) stop("'delta_13C' must be of class character")
  if(!is.null(N2Odry_ppb) & !is.character(N2Odry_ppb)) stop("'N2Odry_ppb' must be of class character")
  if(!is.null(delta_15N) & !is.character(delta_15N)) stop("'delta_15N' must be of class character")
  if(!is.null(delta_18O) & !is.character(delta_18O)) stop("'delta_18O' must be of class character")

  if(any(grepl(CO2dry_ppm, names(dataframe)))){
    if(!any(grepl(delta_13C, names(dataframe)))){
      stop(paste("The matching string for delta_13C '", delta_13C,
                 "' was not found in dataframe.", sep = ""))}
  }

  if(any(grepl(N2Odry_ppb, names(dataframe)))){
    if(!any(grepl(delta_15N, names(dataframe)))){
      stop(paste("The matching string for delta_15N '", delta_15N,
                 "' was not found in dataframe.", sep = ""))}
    if(!any(grepl(delta_18O, names(dataframe)))){
      stop(paste("The matching string for delta_18O '", delta_18O,
                 "' was not found in dataframe.", sep = ""))}
  }

  if(!missing(save.plots)){
    if(!is.character(save.plots)) stop("'save.plot' must be a character string")}

  # Split dataframe into a list of dataframes for each UniqueID
  data_split <- dataframe %>%
    # Split dataset by UniqueID
    group_by(UniqueID) %>% group_split() %>% as.list()

  # Create an empty list to store results
  res.ls <- list()
  plot.ls <- list()

  # Save plots as pdf
  if(!missing(save.plots)) pdf(file = save.plots, width = 11.6, height = 8.2)

  ## CO2dry_ppm ####
  if(any(grepl(CO2dry_ppm, names(dataframe)))){
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

        plot_13C <-
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
        print(plot_13C)
      }
    }
  }

  ## N2Odry_ppb ####
  if(any(grepl(N2Odry_ppb, names(dataframe)))){
    for(i in 1:length(data_split)){

      # Extract data
      N2O_meas <- Reduce("c", data_split[[i]][, N2Odry_ppb])
      delta_15N_meas <- Reduce("c", data_split[[i]][, delta_15N])
      delta_18O_meas <- Reduce("c", data_split[[i]][, delta_18O])
      UniqueID <- unique(na.omit(data_split[[i]]$UniqueID))

      # Inverse of N2O concentration
      inv.N2O <- 1/N2O_meas

      # Linear regressions
      LM_15N <- lm(delta_15N_meas ~ inv.N2O)
      LM_18O <- lm(delta_18O_meas ~ inv.N2O)

      # Extract values from the linear fit
      iso.comp_15N <- summary(LM_15N)[[4]][1,1]
      iso.comp_18O <- summary(LM_18O)[[4]][1,1]

      # Results
      res.ls[[i]] <- cbind.data.frame(UniqueID, iso.comp_15N, iso.comp_18O)

      # Plot results
      if(!missing(save.plots)){

        data_15N <- cbind.data.frame(delta_15N_meas, inv.N2O)
        data_18O <- cbind.data.frame(delta_18O_meas, inv.N2O)

        if(iso.comp_15N < 0) direction_15N <- " - " else direction_15N <- " + "
        if(iso.comp_18O < 0) direction_18O <- " - " else direction_18O <- " + "

        slope_15N <- round(summary(LM_15N)$coefficients[2])
        intercept_15N <- signif(iso.comp_15N,3)
        r2_15N <- round(as.numeric(summary(lm(fitted(LM_15N) ~ delta_15N_meas))[9])[1],3)

        slope_18O <- round(summary(LM_18O)$coefficients[2])
        intercept_18O <- signif(iso.comp_18O,3)
        r2_18O <- round(as.numeric(summary(lm(fitted(LM_18O) ~ delta_18O_meas))[9])[1],3)

        plot.15N <-
          ggplot(data_15N, aes(inv.N2O, delta_15N_meas)) +
          geom_abline(slope = summary(LM_15N)$coefficients[2],
                      intercept = summary(LM_15N)$coefficients[1],
                      col = "red", linewidth = 1) +
          geom_point(col = "black") +
          labs(title = paste("UniqueID:", UniqueID,
                             "\nIsotopic composition (intercept) = ",
                             intercept_15N, "\u2030"),
               subtitle = bquote(Equation:~delta^15*"N = "*.(slope_15N)*"/"*N["2"]*O*
                                   .(paste(sep = "", direction_15N, abs(intercept_15N)))
                                 ~~~~~~~~R^2~"= "*.(r2_15N))) +
          ylab(expression(delta^15*"N of "*N["2"]*O*" (\u2030)")) +
          xlab(expression(1/N["2"]*O*" ("*ppb^-1*")")) +
          theme_bw() +
          theme(plot.title = element_text(size = 11))

        plot.18O <-
          ggplot(data_18O, aes(inv.N2O, delta_18O_meas)) +
          geom_abline(slope = summary(LM_18O)$coefficients[2],
                      intercept = summary(LM_18O)$coefficients[1],
                      col = "red", linewidth = 1) +
          geom_point(col = "black") +
          labs(title = paste("UniqueID:", UniqueID,
                             "\nIsotopic composition (intercept) = ",
                             intercept_18O, "\u2030"),
               subtitle = bquote(Equation:~delta^18*"O = "*.(slope_18O)*"/"*N["2"]*O*
                                   .(paste(sep = "", direction_18O, abs(intercept_18O)))
                                 ~~~~~~~~R^2~"= "*.(r2_18O))) +
          ylab(expression(delta^18*"O of "*N["2"]*O*" (\u2030)")) +
          xlab(expression(1/N["2"]*O*" ("*ppb^-1*")")) +
          theme_bw() +
          theme(plot.title = element_text(size = 11))

        grid.arrange(plot.15N, plot.18O, nrow = 1, ncol = 2)
      }
    }
  }

  if(!missing(save.plots)) dev.off()

  # Unlist results
  results <- map_df(res.ls,  ~as.data.frame(.x))

  # Return results
  return(results)
}
