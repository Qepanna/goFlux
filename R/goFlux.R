#' goFlux: a user-friendly GHG fluxes calculation tool
#'
#' A wrapper function to calculate GHG fluxes from static chamber measurements.
#' Calculates linear (\code{\link[goFlux]{LM.flux}}) and non-linear
#' fluxes (Hutchinson and Mosier model; \code{\link[goFlux]{HM.flux}}),
#' from a variety of greenhouse gasses (
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}},
#' \ifelse{html}{\out{NH<sub>3</sub>}}{\eqn{NH[3]}{ASCII}}, CO, and
#' \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}).
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
#'                Default is \code{H2O_col = "H2O_ppm"}.
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
#'              not provided, 15°C is used as default.
#' @param k.mult numerical value; a multiplier for the allowed kappa-max.
#'               Default setting is no multiplier (\code{k.mult = 1}).
#'               \code{k.mult} cannot be negative and must be smaller or
#'               equal to 10.
#' @param warn.length numerical value; limit under which a measurement is
#'                    flagged for being too short (\code{nb.obs < warn.length}).
#' @param k.min numerical value; a lower boundary value for kappa in the HM model.
#'              Default is \code{k.min = 0}
#'
#' @details
#' Flux estimate units are
#' \ifelse{html}{\out{µmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{µmol m^{-2}s^{-1}}{ASCII}}
#' (if initial concentration is ppm, e.g. CO2dry_ppm) and
#' \ifelse{html}{\out{nmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{nmol m^{-2}s^{-1}}{ASCII}}
#' (if initial concentration is ppb, e.g. CH4dry_ppb).
#'
#' The \code{\link[goFlux]{goFlux}} function calculates flux estimates
#' from the linear model (LM) and the Hutchinson and Mosier model (HM). The HM
#' model is a non-linear model, whose curvature is controlled by the parameter
#' kappa. A large kappa returns a strong curvature. A maximum threshold for this
#' parameter, kappa-max (\code{\link[goFlux]{k.max}}), can be calculated
#' from the linear flux estimate (\code{\link[goFlux]{LM.flux}}), the
#' minimal detectable flux (\code{\link[goFlux]{MDF}}) and the time of
#' chamber closure. This limit of kappa-max is included in the
#' \code{\link[goFlux]{goFlux}} function, so that the non-linear flux
#' estimate cannot exceed this maximum curvature. Inversely, one can set a
#' minimal threshold for kappa: to allow for a log-like curvature, set
#' \code{k.min} below 0 (ex. -1).
#'
#' All flux estimates, including the \code{\link[goFlux]{MDF}}, are
#' obtained from the multiplication of the slope and the
#' \code{\link[goFlux]{flux.term}}, which is used to correct the
#' pressure inside the chamber (due to water vapor), as well as convert the
#' units to obtain a term in nmol or
#' \ifelse{html}{\out{µmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{µmol m^{-2}s^{-1}}{ASCII}}.
#'
#' The argument \code{Area} is in \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}},
#' but the output units from \code{\link[goFlux]{goFlux}} are in
#' \ifelse{html}{\out{(m<sup>2</sup>)}}{\eqn{(m^2)}{ASCII}}. This is due to the
#' conversion from \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}}
#' to \ifelse{html}{\out{(m<sup>2</sup>)}}{\eqn{(m^2)}{ASCII}} within the
#' function. This means that there is a factor of 10,000 to convert from
#' \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}}
#' to \ifelse{html}{\out{(m<sup>2</sup>)}}{\eqn{(m^2)}{ASCII}}. This is important
#' to take into account if one would provide something else than an \code{Area}
#' in \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}} to the function.
#' For example, with incubated soil samples, one may provide an amount of soil
#' (kg) instead of an area in the column \code{Area}. To get the right units in
#' that case, multiply the kilograms of soil by 10,000 to remove the conversion
#' from \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}} to
#' \ifelse{html}{\out{(m<sup>2</sup>)}}{\eqn{(m^2)}{ASCII}}.
#'
#' In \code{gastype}, the gas species listed are the ones for which this package
#' has been adapted. Please write to the maintainer of this package for
#' adaptation of additional gases.
#'
#' \code{warn.length} is the limit below which the chamber closure time is
#' flagged for being too short (\code{nb.obs < warn.length}). Portable
#' greenhouse gas analyzers typically measure at a frequency of 1 Hz. Therefore,
#' for the default setting of \code{warn.length = 60}, the chamber closure time
#' should be approximately one minute (60 seconds). If the number of
#' observations is smaller than the threshold, a warning is printed: e.g. "Low
#' number of observations: UniqueID X has 59 observations".
#'
#' @references Hüppi et al. (2018). Restricting the nonlinearity parameter in
#' soil greenhouse gas flux calculation for more reliable flux estimates.
#' \emph{PloS one}, 13(7), e0200876.
#'
#' @references Hutchinson and Mosier (1981). Improved soil cover method for
#' field measurement of nitrous oxide fluxes.
#' \emph{Soil Science Society of America Journal}, 45(2), 311-316.
#'
#' @returns Returns a data frame with 28 columns: a \code{UniqueID} per
#' measurement, 11 columns for the linear model results (linear flux estimate
#' (\code{\link[goFlux]{LM.flux}}), initial gas concentration
#' (\code{LM.C0}), final gas concentration (\code{LM.Ct}), slope of linear
#' regression (\code{LM.slope}), mean absolute error (\code{LM.MAE}), root mean
#' square error (\code{LM.RMSE}), Akaike's information criterion corrected for
#' small sample size (\code{LM.AICc}), standard error (\code{LM.SE}), relative
#' standard error (\code{LM.se.rel}), coefficient of determination (\code{LM.r2}),
#' and \emph{p-value} (\code{LM.p.val})), 11 columns for the non-linear model
#' results (non-linear flux estimate (\code{\link[goFlux]{HM.flux}}),
#' initial gas concentration (\code{HM.C0}), the assumed concentration of
#' constant gas source below the surface (\code{HM.Ci}), slope at \code{t=0}
#' (\code{HM.slope}), mean absolute error (\code{HM.MAE}), root mean square error
#' (\code{HM.RMSE}), Akaike's information criterion corrected for small sample
#' size (\code{HM.AICc}), standard error (\code{HM.SE}), relative standard error
#' (\code{HM.se.rel}), coefficient of determination (\code{HM.r2}), and curvature
#' (kappa; \code{HM.k}), as well as the minimal detectable flux
#' (\code{\link[goFlux]{MDF}}), the precision of the instrument
#' (\code{prec}), the flux term (\code{\link[goFlux]{flux.term}}),
#' kappa-max (\code{\link[goFlux]{k.max}}), the g factor (g.fact;
#' \code{\link[goFlux]{g.factor}}), the number of observations used
#' (\code{nb.obs}) and the true initial gas concentration (\code{C0}) and final
#' gas concentration (\code{Ct}).
#'
#' @include goFlux-package.R
#' @include flux.term.R
#' @include MDF.R
#' @include LM.flux.R
#' @include HM.flux.R
#' @include g.factor.R
#' @include k.max.R
#'
#' @seealso Look up the functions \code{\link[goFlux]{MDF}},
#'          \code{\link[goFlux]{flux.term}},
#'          \code{\link[goFlux]{g.factor}},
#'          \code{\link[goFlux]{k.max}},
#'          \code{\link[goFlux]{HM.flux}} and
#'          \code{\link[goFlux]{LM.flux}} of this package for more
#'          information about these parameters.
#'
#' @seealso See also the import functions to find the
#'          default instrument precision for each instrument:
#'          \code{\link[goFlux]{import.DX4015}},
#'          \code{\link[goFlux]{import.EGM5}},
#'          \code{\link[goFlux]{import.G2508}},
#'          \code{\link[goFlux]{import.G4301}},
#'          \code{\link[goFlux]{import.GAIA}},
#'          \code{\link[goFlux]{import.LI6400}},
#'          \code{\link[goFlux]{import.LI7810}},
#'          \code{\link[goFlux]{import.LI7820}},
#'          \code{\link[goFlux]{import.LI8100}},
#'          \code{\link[goFlux]{import.LI8200}},
#'          \code{\link[goFlux]{import.N2OM1}},
#'          \code{\link[goFlux]{import.uCH4}},
#'          \code{\link[goFlux]{import.uN2O}},
#'          \code{\link[goFlux]{import.UGGA}}
#'
#' @examples
#' data(manID.UGGA)
#' CO2_flux <- goFlux(manID.UGGA, "CO2dry_ppm")
#' CH4_flux <- goFlux(manID.UGGA, "CH4dry_ppb")
#' H2O_flux <- goFlux(manID.UGGA, "H2O_ppm")
#'
#' @export
#'
goFlux <- function(dataframe, gastype, H2O_col = "H2O_ppm", prec = NULL,
                   Area = NULL, offset = NULL, Vtot = NULL, Vcham = NULL,
                   Pcham = NULL, Tcham = NULL, k.mult = 1,
                   warn.length = 60, k.min = 0){

  # Check arguments ####
  if(!is.numeric(k.mult)) stop("'k.mult' must be of class numeric")
  if(!dplyr::between(k.mult, 0, 10) | k.mult <= 0){
    stop("'k.mult' cannot be negative and must be smaller or equal to 10")}
  if(!is.numeric(warn.length)){stop("'warn.length' must be of class numeric")
  } else {if(warn.length <= 0) stop("'warn.length' must be greater than 0")}
  if(!is.numeric(k.min)) stop("'k.min' must be of class numeric")

  ## Check dataframe ####
  if(missing(dataframe)) stop("'dataframe' is required")
  if(!is.null(dataframe) & !is.data.frame(dataframe)){
    stop("'dataframe' must be of class data.frame")}

  ### gastype and match in dataframe ####
  if(missing(gastype)){
    stop("'gastype' must be one of the following: 'CO2dry_ppm', 'CH4dry_ppb', 'COdry_ppb', 'N2Odry_ppb', 'NH3dry_ppb' or 'H2O_ppm'")}
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
    dataframe <- dataframe %>% mutate(Vtot = Vcham + (Area * offset)/1000)
  }

  # Use normal atmospheric pressure and ambient temperature
  # if Pcham and Tcham are missing from dataframe
  if(!any(grepl("\\<Pcham\\>", names(dataframe)))){
    dataframe <- dataframe %>% mutate(Pcham = 101.325)
  }
  if(!any(grepl("\\<Tcham\\>", names(dataframe)))){
    dataframe <- dataframe %>% mutate(Tcham = 15)
  }

  # Create UniqueID from chamID, if missing
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

    # Are all Pcham or Tcham NAs?
    if(is.na(first(na.omit(data_split[[f]]$Pcham)))) {
      data_split[[f]]$Pcham <- 101.325
      data_split[[f]]$warn.Pcham <- TRUE
      } else data_split[[f]]$warn.Pcham <- FALSE
    if(is.na(first(na.omit(data_split[[f]]$Tcham)))) {
      data_split[[f]]$Tcham <- 15
      data_split[[f]]$warn.Tcham <- TRUE
    } else data_split[[f]]$warn.Tcham <- FALSE

    data_split[[f]] <- data_split[[f]] %>%
      mutate(flux.term = flux.term(first(na.omit(Vtot)), first(na.omit(Pcham)),
                                   first(na.omit(Area)), first(na.omit(Tcham)),
                                   H2O_flux.term)) %>%
      mutate(MDF = MDF(prec, (max(Etime)+1), flux.term))
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

    # Skip if there is less than 3 data points
    if(nb.obs < 3){

      # LM.res and HM.res <- NA
      LM.res <- cbind.data.frame(LM.flux = NA, LM.C0 = NA, LM.Ct = NA,
                                 LM.slope = NA, LM.MAE = NA, LM.RMSE = NA,
                                 LM.AICc = NA, LM.SE = NA, LM.se.rel = NA,
                                 LM.r2 = NA, LM.p.val = NA)

      HM.res <- cbind.data.frame(HM.flux = NA, HM.C0 = NA, HM.Ci = NA,
                                 HM.slope = NA, HM.MAE = NA, HM.RMSE = NA,
                                 HM.AICc = NA, HM.SE = NA, HM.se.rel = NA,
                                 HM.r2 = NA, HM.k = NA)

      # Extract gas measurement (by gastype)
      gas.meas <- Reduce("c", data_split[[f]][, gastype])

      # Get C0 and Ct from raw data
      C0 <- first(gas.meas)
      Ct <- last(gas.meas)

      # HM derived variables
      kappa.max <- g.fact <- NA

    } else {

      # Extract gas measurement (by gastype)
      gas.meas <- Reduce("c", data_split[[f]][, gastype])

      # Linear model
      LM.res <- suppressWarnings(
        LM.flux(gas.meas = gas.meas,
                time.meas = data_split[[f]]$Etime,
                flux.term = flux.term))

      # Calculate C0 and Ct and their boundaries based on LM.flux
      C0.flux <- LM.res$LM.C0
      Ct.flux <- LM.res$LM.Ct
      C.diff.flux <- abs(Ct.flux-C0.flux)
      C0.lim.flux <- c(C0.flux-C.diff.flux*0.2, C0.flux+C.diff.flux*0.2)
      Ct.lim.flux <- c(Ct.flux-C.diff.flux*0.2, Ct.flux+C.diff.flux*0.2)

      # Get C0 and Ct from raw data
      C0 <- first(gas.meas)
      Ct <- last(gas.meas)

      # Choose the right C0 and Ct
      Ct.best <- if_else(between(Ct, Ct.lim.flux[1], Ct.lim.flux[2]), Ct, Ct.flux)
      C0.best <- if_else(between(C0, C0.lim.flux[1], C0.lim.flux[2]), C0, C0.flux)

      # Adjust C0 and Ct if the different between them is smaller than 1
      if(abs(C.diff.flux) < 1){
        Ct.best <- floor(Ct.best) - 1
        C0.best <- ceiling(C0.best) + 1
      }

      # Calculate kappa thresholds based on MDF, LM.flux and Etime
      kappa.max <- abs(k.max(MDF, LM.res$LM.flux, (max(data_split[[f]]$Etime)+1)))

      # Try to catch errors and warnings from HM calculation
      HM.catch <- HM.flux(gas.meas = gas.meas, time.meas = data_split[[f]]$Etime,
                          flux.term = flux.term, Ct = Ct.best, C0 = C0.best,
                          k.max = kappa.max*Inf, k.min = k.min)

      # If there is an error with singular gradient
      if(inherits(HM.catch[[2]], "simpleError")){

        # Print warning
        if(isTRUE(grepl("singular gradient", HM.catch[[2]]$message))){
          warning("Flux estimate is too close to zero to estimate HM flux in UniqueID ",
                  UniqueID, ". NAs produced.", call. = F)
        } else {
          message("Error in UniqueID ", UniqueID, ": ", HM.catch[[2]]$message)
        }

        # Return data frame
        HM.res <- HM.catch[[1]]
      }

      # If there is no error
      if(!inherits(HM.catch[[2]], "simpleError")){

        # But there is a warning with HM
        if(inherits(HM.catch[[3]], "simpleWarning")){

          # Print warning
          message("Error in UniqueID ", UniqueID, ": ", HM.catch[[3]]$message)
        }

        # Or there is a warning with AICc
        if(inherits(HM.catch[[4]], "simpleWarning")){

          # Print warning
          if(isTRUE(grepl("sample size", HM.catch[[4]]$message))){
            warning("Sample size is too small for UniqueID ", UniqueID,
                    ". Results may be meanignless or missing.", call. = F)
          } else {
            message("Error in UniqueID ", UniqueID, ": ", HM.catch[[4]]$message)
          }
        }

        # Hutchison and Mosier without kappa max
        HM.noK <- HM.flux(gas.meas = gas.meas, time.meas = data_split[[f]]$Etime,
                          flux.term = flux.term, Ct = Ct.best, C0 = C0.best,
                          k.max = kappa.max*Inf, k.min = k.min)[[1]]

        # Hutchinson and Mosier with kappa max
        HM.K <- HM.flux(gas.meas = gas.meas, time.meas = data_split[[f]]$Etime,
                        flux.term = flux.term, Ct = Ct.best, C0 = C0.best,
                        k.max = kappa.max, k.mult = k.mult, k.min = k.min)[[1]]

        # Compare results, with and without kappa max.
        # Select the result with the smallest curvature.
        if(abs(HM.K$HM.k) <= abs(HM.noK$HM.k)) HM.res <- HM.K else HM.res <- HM.noK
      }
    }

    # Flux results
    flux.res.ls[[f]] <- cbind.data.frame(
      UniqueID, LM.res, HM.res, C0, Ct, MDF, prec,
      flux.term, nb.obs, k.max = kappa.max, k.mult,
      g.fact = g.factor(HM.res$HM.flux, LM.res$LM.flux))

    # Update progress bar
    setTxtProgressBar(pb, f)

  }

  # Unlist flux results
  flux_results <- map_df(flux.res.ls,  ~as.data.frame(.x))

  # Close progress bar
  close(pb)

  for (f in 1:nrow(flux_results)){
    if(flux_results$nb.obs[f] < warn.length){
      warning("Low number of observations: UniqueID ", flux_results$UniqueID[f],
              " has ", flux_results$nb.obs[f], " observations", call. = FALSE)}
  }

  # Warn about NAs in Pcham or Tcham
  for (f in 1:length(data_split)){
    if(first(data_split[[f]]$warn.Tcham) == TRUE){
      warning("Tcham missing in UniqueID ", first(data_split[[f]]$UniqueID),
              ". 15 degrees Celsius was used as default.", call. = FALSE)}
  }
  for (f in 1:length(data_split)){
    if(first(data_split[[f]]$warn.Pcham) == TRUE){
      warning("Pcham missing in UniqueID ", first(data_split[[f]]$UniqueID),
              ". 101.325 kPa was used as default.", call. = FALSE)}
  }

  # Warn about measurements with less than 3 data points
  for (f in 1:nrow(flux_results)){
    if(flux_results$nb.obs[f] < 3){
      warning("Error in UniqueID ", flux_results$UniqueID[f], ": cannot calculate ",
              "flux estimates with less than 3 data points. NAs produced.", call. = FALSE)}
  }

  # Return results
  return(flux_results)

}
