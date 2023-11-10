#' goFlux (without kappa-max): a user-friendly GHG fluxes calculation tool
#'
#' A wrapper function to calculate GHG fluxes from static chamber measurements.
#' Calculates linear (\code{\link[GoFluxYourself]{LM.flux}}) and non-linear fluxes
#' (Hutchinson and Mosier model; \code{\link[GoFluxYourself]{HM.flux}}).
#'
#' @param dataframe a data.frame containing gas measurements (see \code{gastype}
#'                  below), water vapor measurements (see \code{H2O_col} below)
#'                  and the following columns: \code{UniqueID}, \code{Etime},
#'                  \code{Vtot}, \code{Area}, \code{Pcham}, \code{Tcham} and
#'                  \code{flag} (see the parameters \code{Vtot}, \code{Area},
#'                  \code{Pcham} and \code{Tcham} below for more details).
#'                  \code{chamID} may be used instead of \code{UniqueID}.
#' @param gastype character string; specifies which column should be used for the
#'                flux calculations. Must be one of the following: "CO2dry_ppm",
#'                "CH4dry_ppb", "COdry_ppb", "N2Odry_ppb", "NH3dry_ppb" or "H2O_ppm".
#' @param H2O_col character string; specifies which column should be used to
#'                subtract the effect of water vapor in the chamber space.
#' @param prec numerical value; precision of the instruments. Units must be the
#'             same as \code{gastype}. With the default \code{prec = NULL},
#'             instrument precision for each gas must be provided in
#'             \code{dataframe}.
#' @param Area numerical value; area of the soil surface inside the chamber
#'             \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}}.
#'             Alternatively, provide the column \code{Area} in \code{dataframe}
#'             if \code{Area} is different between samples.
#' @param Vtot numerical value; total volume inside the chamber, tubes, instruments,
#'             etc. (L). Alternatively, provide the column \code{Vtot} in
#'             \code{dataframe} if \code{Vtot} is different between samples. If
#'             \code{Vtot} is missing, the function will calculate it from
#'             \code{Area}, \code{Vcham} and \code{offset}.
#' @param Vcham (optional) numerical value; volume inside the chamber, tubes and
#'              instruments (L). Alternatively, provide the column \code{Vcham}
#'              in \code{dataframe} if \code{Vcham} is different between samples.
#'              \code{Vhcam} is only used if \code{Vtot} is missing.
#' @param offset (optional) numerical value; height between the soil surface and
#'               the chamber (cm). Alternatively, provide the column \code{offset}
#'               in \code{dataframe} if \code{offset} is different between samples.
#'               \code{offset} is only used if \code{Vtot} is missing.
#' @param Pcham numerical value; pressure inside the chamber (kPa).
#'              Alternatively, provide the column \code{Pcham} in \code{dataframe}
#'              if \code{Pcham} is different between samples. If \code{Pcham} is
#'              not provided, normal atmospheric pressure (101.325 kPa) is used.
#' @param Tcham numerical value; temperature inside the chamber (Celsius).
#'              Alternatively, provide the column \code{Tcham} in \code{dataframe}
#'              if \code{Tcham} is different between samples. If \code{Tcham} is
#'              not provided, normal air temperature (15°C) is used.
#' @param warn.length numerical; minimum amount of observations accepted (number
#'                    of data points). With nowadays portable greenhouse gas
#'                    analyzers, the frequency of measurement is usually one
#'                    measurement per second. Therefore, for a default setting
#'                    of \code{warn.length = 60}, the chamber closure time
#'                    should be approximately one minute (60 seconds).
#'
#' @details
#' This function differs from the original version
#' \code{\link[GoFluxYourself]{goFlux}}. In this version, the non-linear model
#' is calculated without kappa-max (\code{\link[GoFluxYourself]{k.max}}). In the
#' original version, the kappa-max is estimated using the
#' \code{\link[GoFluxYourself]{MDF}}.
#'
#' Flux estimate units are
#' \ifelse{html}{\out{µmol/m<sup>2</sup>s}}{\eqn{µmol/m^{2}s}{ASCII}}
#' (if initial concentration is ppm, e.g. CO2dry_ppm) and
#' \ifelse{html}{\out{nmol/m<sup>2</sup>s}}{\eqn{nmol/m^{2}s}{ASCII}}
#' (if initial concentration is ppb, e.g. CH4dry_ppb).
#'
#' The function \code{\link[GoFluxYourself]{k.max}} calculates the maximal
#' curvature (kappa) of the non-linear model (Hutchinson and Mosier;
#' \code{\link[GoFluxYourself]{HM.flux}}) allowed for each flux measurements.
#' kappa-max is calculated based on the minimal detectable flux
#' (\code{\link[GoFluxYourself]{MDF}}), the linear flux estimate
#' \code{\link[GoFluxYourself]{LM.flux}}
#' and the measurement time. The unit of kappa-max is
#' \ifelse{html}{\out{s<sup>-1</sup>}}{\eqn{s^{-1}}{ASCII}}.
#'
#' The function \code{\link[GoFluxYourself]{MDF}} calculates the minimal detectable
#' flux (MDF) based on instrument precision and measurement time.
#'
#' The argument \code{Area} is in \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}},
#' but the output units from \code{goFlux()} are in
#' \ifelse{html}{\out{(m<sup>2</sup>)}}{\eqn{(m^2)}{ASCII}}. This means that there is a factor
#' of 10000 to convert from \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}}
#' to \ifelse{html}{\out{(m<sup>2</sup>)}}{\eqn{(m^2)}{ASCII}}. This is important
#' to take into account if one would provide something else than an Area in
#' \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}} to the function.
#' For example, with incubated soil samples, one may provide an amount of soil
#' (kg) instead of an Area. To get the right units in that case, multiply the
#' kilograms of soil by 10000 to remove the conversion from
#' \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}} to
#' \ifelse{html}{\out{(m<sup>2</sup>)}}{\eqn{(m^2)}{ASCII}}.
#'
#' @returns Returns a data frame with 26 columns: a UniqueID per measurement,
#'          10 columns for the linear model results (linear flux estimate
#'          (\code{LM.flux}), initial gas concentration (\code{LM.C0}), final
#'          gas concentration (\code{LM.Ct}), slope of linear regression
#'          (\code{LM.slope}), mean absolute error ((\code{LM.MAE})), root mean
#'          square error ((\code{LM.RMSE})), standard error ((\code{LM.se})),
#'          relative se ((\code{LM.se.rel})),
#'          \ifelse{html}{\out{r<sup>2</sup>}}{\eqn{r^2}{ASCII}}, and p-value
#'          ((\code{LM.p.val}))), 10 columns for the non-linear model results
#'          (non-linear flux estimate (\code{HM.flux}), initial gas concentration
#'          (\code{HM.C0}), final gas concentration (\code{HM.Ci}), slope
#'          at \code{t=0} (\code{HM.slope}), mean absolute error (\code{HM.MAE}),
#'          root mean square error (\code{HM.RMSE}), standard error (\code{HM.se}),
#'          relative se (\code{HM.se.rel}),
#'          \ifelse{html}{\out{r<sup>2</sup>}}{\eqn{r^2}{ASCII}}, and curvature
#'          (kappa; \code{HM.k}), as well as the minimal detectable flux
#'          (\code{\link[GoFluxYourself]{MDF}}), the precision of the instrument
#'          (\code{prec}), the flux term (\code{\link[GoFluxYourself]{flux.term}}),
#'          kappa-max (\code{\link[GoFluxYourself]{k.max}}), the g factor
#'          (\code{\link[GoFluxYourself]{g.factor}}), the number of observations
#'          used (\code{nb.obs}) and the true initial gas concentration
#'          (\code{C0}) and final gas concentration (\code{Ct}).
#'
#' @include GoFluxYourself-package.R
#' @include flux.term.R
#' @include MDF.R
#' @include LM.flux.R
#' @include HM.flux.R
#' @include g.factor.R
#' @include k.max.R
#'
#' @seealso Look up the functions \code{\link[GoFluxYourself]{MDF}},
#'          \code{\link[GoFluxYourself]{flux.term}},
#'          \code{\link[GoFluxYourself]{g.factor}},
#'          \code{\link[GoFluxYourself]{k.max}},
#'          \code{\link[GoFluxYourself]{HM.flux}} and
#'          \code{\link[GoFluxYourself]{LM.flux}} of this package for more
#'          information about these parameters.
#'
#' @seealso See also the import functions to find the
#'          default instrument precision for each instrument:
#'          \code{\link[GoFluxYourself]{DX4015_import}},
#'          \code{\link[GoFluxYourself]{G2508_import}},
#'          \code{\link[GoFluxYourself]{GAIA_import}},
#'          \code{\link[GoFluxYourself]{LGR_import}},
#'          \code{\link[GoFluxYourself]{LI6400_import}},
#'          \code{\link[GoFluxYourself]{LI7810_import}},
#'          \code{\link[GoFluxYourself]{LI7820_import}},
#'          \code{\link[GoFluxYourself]{LI8100_import}},
#'          \code{\link[GoFluxYourself]{LI8200_import}}
#'
#' @examples
#' data(LGR_manID)
#' CO2_flux <- goFlux_nokmax(LGR_manID, "CO2dry_ppm")
#' CH4_flux <- goFlux_nokmax(LGR_manID, "CH4dry_ppb")
#' H2O_flux <- goFlux_nokmax(LGR_manID, "H2O_ppm")
#'
#' @export
#'
goFlux_nokmax <- function(dataframe, gastype, H2O_col = "H2O_ppm", prec = NULL,
                          Area = NULL, offset = NULL, Vtot = NULL, Vcham = NULL,
                          Pcham = NULL, Tcham = NULL, warn.length = 60){

  # Check arguments ####
  if(!is.numeric(warn.length)){stop("'warn.length' must be of class numeric")
  } else {if(warn.length <= 0) stop("'warn.length' must be greater than 0")}

  ## Check dataframe ####
  if(missing(dataframe)) stop("'dataframe' is required")
  if(!is.null(dataframe) & !is.data.frame(dataframe)){
    stop("'dataframe' must be of class data.frame")}

  ### gastype and match in dataframe ####
  if(missing(gastype)) stop("'gastype' is required")
  if(!is.null(gastype) & !is.character(gastype)) stop("'gastype' must be a character string")
  if(!any(grepl(paste("\\<", gastype, "\\>", sep = ""),
                c("CO2dry_ppm", "CH4dry_ppb", "COdry_ppb", "N2Odry_ppb", "NH3dry_ppb", "H2O_ppm")))){
    stop("'gastype' must be one of the following: 'CO2dry_ppm', 'CH4dry_ppb', 'COdry_ppb', 'N2Odry_ppb', 'NH3dry_ppb' or 'H2O_ppm'")}
  if(!any(grepl(paste("\\<", gastype, "\\>", sep = ""), names(dataframe)))){
    stop("'dataframe' must contain a column that matches 'gastype'")}
  if(any(grepl(paste("\\<", gastype, "\\>", sep = ""), names(dataframe))) &
     !is.numeric(dataframe[,gastype][[1]])){
    stop("The column that matches 'gastype' in 'dataframe' must be of class numeric")}

  ### prec and match in dataframe ####
  if(!is.null(prec) & !is.numeric(prec)) stop("'prec' must be of class numeric")
  if(is.null(prec)){
    if(gastype == "CO2dry_ppm" &
       !any(grepl(paste("\\<CO2_prec\\>", sep = ""), names(dataframe)))){
      stop("'dataframe' must contain the column 'CO2_prec' if prec = NULL")}
    if(gastype == "H2O_ppm" &
       !any(grepl(paste("\\<H2O_prec\\>", sep = ""), names(dataframe)))){
      stop("'dataframe' must contain the column 'H2O_prec' if prec = NULL")}
    if(gastype == "CH4dry_ppb" &
       !any(grepl(paste("\\<CH4_prec\\>", sep = ""), names(dataframe)))){
      stop("'dataframe' must contain the column 'CH4_prec' if prec = NULL")}
    if(gastype == "COdry_ppb" &
       !any(grepl(paste("\\<CO_prec\\>", sep = ""), names(dataframe)))){
      stop("'dataframe' must contain the column 'CO_prec' if prec = NULL")}
    if(gastype == "NH3dry_ppb" &
       !any(grepl(paste("\\<NH3_prec\\>", sep = ""), names(dataframe)))){
      stop("'dataframe' must contain the column 'NH3_prec' if prec = NULL")}
    if(gastype == "N2Odry_ppb" &
       !any(grepl(paste("\\<N2O_prec\\>", sep = ""), names(dataframe)))){
      stop("'dataframe' must contain the column 'N2O_prec' if prec = NULL")}
  }

  ### H2O_col and match in dataframe ####
  if(is.null(H2O_col)) stop("'H2O_col' is required")
  if(!is.null(H2O_col) & !is.character(H2O_col)) stop("'H2O_col' must be a character string")
  if(!any(grepl(paste("\\<", H2O_col, "\\>", sep = ""), names(dataframe)))){
    stop("'dataframe' must contain a column that matches 'H2O_col'")}
  if(any(grepl(paste("\\<", H2O_col, "\\>", sep = ""), names(dataframe))) &
     !is.numeric(dataframe[,H2O_col][[1]])){
    stop("The column that matches 'H2O_col' in 'dataframe' must be of class numeric")}

  ### UniqueID (or chamID) ####
  if(!any(grepl(paste(c("\\<UniqueID\\>", "\\<chamID\\>"), collapse = "|"), names(dataframe)))){
    stop("'dataframe' must contain 'UniqueID'")}

  ### Etime and flag ####
  if(!any(grepl("\\<Etime\\>", names(dataframe)))) stop("'dataframe' must contain 'Etime'")
  if(any(grepl("\\<Etime\\>", names(dataframe))) & !is.numeric(dataframe$Etime)){
    stop("'Etime' in 'dataframe' must be of class numeric (or integer)")}
  if(!any(grepl("\\<flag\\>", names(dataframe)))) stop("'dataframe' must contain 'flag'")
  if(any(grepl("\\<flag\\>", names(dataframe))) & !is.numeric(dataframe$flag)){
    stop("'flag' in 'dataframe' must be of class numeric (or integer)")}

  ### Vtot (or Vcham + offset) ####
  ### if Vtot is an argument
  if(!is.null(Vtot)){
    if(!is.numeric(Vtot)) stop("'Vtot' must be of class numeric")
    ### if Vtot is not an argument
  } else {
    #### look for it in dataframe
    if(!any(grepl("\\<Vtot\\>", names(dataframe)))){
      #### if not found, look for alternative arguments
      ##### Vcham
      if(!is.null(Vcham)){ # if Vcham is an argument
        if(!is.numeric(Vcham)) stop("'Vcham' must be of class numeric")
      } else { # if Vcham is not an argument, look in dataframe
        if(!any(grepl("\\<Vcham\\>", names(dataframe)))){
          stop("'Vtot' missing. Alternative argument 'Vcham' also missing.")
        } else { # Vcham in dataframe must be numeric
            if(!is.numeric(dataframe$Vcham)){
              stop("'Vcham' in 'dataframe' must be of class numeric")}
          }
      }
      ##### offset
      if(!is.null(offset)){ # if offset is an argument
        if(!is.numeric(offset)) stop("'offset' must be of class numeric")
      } else { # if offset is not an argument, look in dataframe
        if(!any(grepl("\\<offset\\>", names(dataframe)))){
          stop("'Vtot' missing. Alternative argument 'offset' also missing.")
        } else { # offset in dataframe must be numeric
          if(!is.numeric(dataframe$offset)){
            stop("'offset' in 'dataframe' must be of class numeric")}
        }
      }
      # if found in dataframe
    } else {
      if(!is.numeric(dataframe$Vtot)){
        stop("'Vtot' in 'dataframe' must be of class numeric")}
    }
  }

  ### Area ####
  ### if Area is an argument
  if(!is.null(Area)){
    if(!is.numeric(Area)) stop("'Area' must be of class numeric")
    ### if Area is not an argument
  } else {
    #### look for it in dataframe
    if(any(grepl("\\<Area\\>", names(dataframe)))){
      if(!is.numeric(dataframe$Area)){
        stop("'Area' in 'dataframe' must be of class numeric")}
    } else stop("'Area' missing")
  }

  ### Pcham ####
  ### if Pcham is an argument
  if(!is.null(Pcham)){
    if(!is.numeric(Pcham)) stop("'Pcham' must be of class numeric")
    ### if Pcham is not an argument
  } else {
    #### look for it in dataframe
    if(any(grepl("\\<Pcham\\>", names(dataframe)))){
      if(!is.numeric(dataframe$Pcham)){
        stop("'Pcham' in 'dataframe' must be of class numeric")}
    }
  }

  ### Tcham ####
  ### if Tcham is an argument
  if(!is.null(Tcham)){
    if(!is.numeric(Tcham)) stop("'Tcham' must be of class numeric")
    ### if Tcham is not an argument
  } else {
    #### look for it in dataframe
    if(any(grepl("\\<Tcham\\>", names(dataframe)))){
      if(!is.numeric(dataframe$Tcham)){
        stop("'Tcham' in 'dataframe' must be of class numeric")}
    }
  }

  # Assign NULL to variables without binding ####
  H2O_ppm <- H2O_mol <- Etime <- flag <- chamID <- DATE <- NULL

  # FUNCTION STARTS ####

  # Use provided values for Area, offset, Vcham, Vtot, Pcham and Tcham
  # if they are missing from dataframe
  if(!is.null(Area)){
    dataframe <- dataframe %>% mutate(Area = Area)
  }
  if(!is.null(offset)){
    dataframe <- dataframe %>% mutate(offset = offset)
  }
  if(!is.null(Vcham)){
    dataframe <- dataframe %>% mutate(Vcham = Vcham)
  }
  if(!is.null(Vtot)){
    dataframe <- dataframe %>% mutate(Vtot = Vtot)
  }
  if(!is.null(Pcham)){
    dataframe <- dataframe %>% mutate(Pcham = Pcham)
  }
  if((!is.null(Tcham))){
    dataframe <- dataframe %>% mutate(Tcham = Tcham)
  }

  # Calculate Vtot if absent from dataframe
  if(!any(grepl("\\<Vtot\\>", names(dataframe)))){
    dataframe <- dataframe %>% mutate(Vtot = Vcham + (Area * offset))
  }

  # Use normal atmospheric pressure and ambient temperature
  # if Pcham and Tcham are missing from dataframe
  if(!any(grepl("\\<Pcham\\>", names(dataframe)))){
    dataframe <- dataframe %>% mutate(Pcham = 101.325)
  }
  if(!any(grepl("\\<Tcham\\>", names(dataframe)))){
    dataframe <- dataframe %>% mutate(Tcham = 15)
  }

  # Rename chamID to UniqueID
  if(!any(grepl("\\<UniqueID\\>", names(dataframe)))){
    if(any(grepl("\\<chamID\\>", names(dataframe)))){
      dataframe <- dataframe %>% mutate(UniqueID = paste(chamID, DATE, sep = "_"))}
  }

  # Clean and subset data (per gastype)
  if(gastype != "H2O_ppm"){
    data_split <- dataframe %>%
      # Rename H2O_col
      rename(H2O_ppm = all_of(H2O_col)) %>%
      # Use mutate() to convert H2O_ppm into H2O_mol
      mutate(H2O_mol = H2O_ppm / (1000*1000)) %>%
      select(UniqueID, H2O_mol, Etime, Vtot, Pcham, Area, Tcham,
             flag, matches(gastype)) %>%
      # Filter flag == 1
      filter(flag == 1) %>%
      # Use drop_na() to remove NAs in gastype
      drop_na(matches(gastype)) %>%
      # Split dataset by UniqueID
      group_by(UniqueID) %>% group_split() %>% as.list()
  } else if(gastype == "H2O_ppm"){
    data_split <- dataframe %>%
      select(UniqueID, Etime, Vtot, Pcham, Area, Tcham,
             flag, all_of(H2O_col)) %>%
      # Rename H2O_col
      rename(H2O_ppm = all_of(H2O_col)) %>%
      # Filter flag == 1
      filter(flag == 1) %>%
      # Use drop_na() to remove NAs in gastype
      drop_na(matches(gastype)) %>%
      # Split dataset by UniqueID
      group_by(UniqueID) %>% group_split() %>% as.list()
  }

  # Instrument precision (by gastype)
  # If prec = NULL, the instrument precision must be provided in 'dataframe'
  if(is.null(prec)){
    if(gastype == "CO2dry_ppm") prec <- unique(na.omit(dataframe$CO2_prec))
    if(gastype == "CH4dry_ppb") prec <- unique(na.omit(dataframe$CH4_prec))
    if(gastype == "COdry_ppb") prec <- unique(na.omit(dataframe$CO_prec))
    if(gastype == "N2Odry_ppb") prec <- unique(na.omit(dataframe$N2O_prec))
    if(gastype == "NH3dry_ppb") prec <- unique(na.omit(dataframe$NH3_prec))
    if(gastype == "H2O_ppm") prec <- unique(na.omit(dataframe$H2O_prec))
  }

  # Calculate auxiliary variables: flux term and minimal detectable flux
  for (f in 1:length(data_split)){

    H2O_flux.term <- ifelse(gastype == "H2O_ppm", 0, first(data_split[[f]]$H2O_mol))

    data_split[[f]] <- data_split[[f]] %>%
      mutate(flux.term = flux.term(first(na.omit(Vtot)), first(na.omit(Pcham)),
                                   first(na.omit(Area)), first(na.omit(Tcham)),
                                   H2O_flux.term),
             MDF = MDF(prec, (max(Etime)+1), flux.term))
  }

  # Create an empty list to store results
  flux.res.ls <- list()

  # Print a progress bar
  pb = txtProgressBar(min = 0, max = length(data_split), initial = 0, style = 3)

  # Flux calculation
  for (f in 1:length(data_split)){

    # Extract auxiliary variables
    UniqueID <- unique(na.omit(data_split[[f]]$UniqueID))
    flux.term <- unique(na.omit(data_split[[f]]$flux.term))
    MDF <- unique(na.omit(data_split[[f]]$MDF))
    nb.obs <- nrow(na.omit(data_split[[f]][, gastype]))

    # Extract gas measurement (by gastype)
    gas.meas <- Reduce("c", data_split[[f]][, gastype])

    # Linear model
    LM.res <- LM.flux(gas.meas = gas.meas,
                      time.meas = data_split[[f]]$Etime,
                      flux.term = flux.term)

    # Calculate C0 and Ct and their boundaries based on LM.flux
    C0.flux <- LM.res$LM.C0
    Ct.flux <- LM.res$LM.Ct
    C.diff.flux <- abs(Ct.flux-C0.flux)
    C0.lim.flux <- c(C0.flux-C.diff.flux*0.2, C0.flux+C.diff.flux*0.2)
    Ct.lim.flux <- c(Ct.flux-C.diff.flux*0.2, Ct.flux+C.diff.flux*0.2)

    # Calculate C0 and Ct and their boundaries based on raw data
    C0 <- first(gas.meas)
    Ct <- last(gas.meas)

    # Chose the right C0 and Ct
    Ct.best <- if_else(between(Ct, Ct.lim.flux[1], Ct.lim.flux[2]), Ct, Ct.flux)
    C0.best <- if_else(between(C0, C0.lim.flux[1], C0.lim.flux[2]), C0, C0.flux)

    # Calculate kappa thresholds based on MDF, LM.flux and Etime
    kappa.max <- k.max(MDF, LM.res$LM.flux, (max(data_split[[f]]$Etime)+1))

    # Hutchinson and Mosier
    HM.res <- HM.flux(gas.meas = gas.meas, time.meas = data_split[[f]]$Etime,
                      flux.term = flux.term, Ct = Ct.best, C0 = C0.best,
                      k.max = kappa.max*Inf)

    # Flux results
    flux.res.ls[[f]] <- cbind.data.frame(
      UniqueID, LM.res, HM.res, C0, Ct, MDF, prec,
      flux.term, nb.obs, g.fact = g.factor(HM.res$HM.flux, LM.res$LM.flux))

    # Update progress bar
    setTxtProgressBar(pb, f)

  }

  # Unlist flux results
  flux_results <- map_df(flux.res.ls,  ~as.data.frame(.x))

  # Close progress bar
  close(pb)

  for (f in 1:nrow(flux_results)){
    if(flux_results$nb.obs[f] < warn.length){
      warning("Number of observations for UniqueID: ", flux_results$UniqueID[f],
              " is ", flux_results$nb.obs[f], " observations", call. = FALSE)}
  }

  # Return results
  return(flux_results)

}
