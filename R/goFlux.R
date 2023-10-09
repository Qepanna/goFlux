#' goFlux: a user-friendly GHG fluxes calculation tool
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
#' @param gastype character string; specifies which column should be used for the
#'                flux calculations. Must be one of the following: "CO2dry_ppm",
#'                "CH4dry_ppb", "N2Odry_ppb" or "H2O_ppm".
#' @param H2O_col character string; specifies which column should be used to
#'                subtract the effect of water vapor in the chamber space.
#' @param prec numerical value; precision of the instruments. Units must be the
#'             same as \code{gastype}. Default values for
#'             \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#'             \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#'             \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}} and
#'             \ifelse{html}{\out{H<sub>2</sub>O}}{\eqn{H[2]O}{ASCII}}
#'             are based on the LI-COR instruments:
#'             \itemize{
#'               \item \ifelse{html}{\out{CO<sub>2</sub> = 3.5 ppm;}}{\eqn{CO[2] = 3.5 ppm;}{ASCII}}
#'               \item \ifelse{html}{\out{CH<sub>4</sub> = 0.6 ppb;}}{\eqn{CH[4] = 0.6 ppb;}{ASCII}}
#'               \item \ifelse{html}{\out{N<sub>2</sub>O = 0.4 ppb;}}{\eqn{N[2]O = 0.4 ppb;}{ASCII}}
#'               \item \ifelse{html}{\out{H<sub>2</sub>O = 45 ppm.}}{\eqn{H[2]O = 45 ppm.}{ASCII}}
#'             }
#'             For other instruments, you must manually specify the precision of
#'             the instrument. If using the ultra-portable GGA (GLA132 series):
#'             \itemize{
#'               \item \ifelse{html}{\out{CO<sub>2</sub> = 0.3 ppm;}}{\eqn{CO[2] = 0.3 ppm;}{ASCII}}
#'               \item \ifelse{html}{\out{CH<sub>4</sub> = 0.3 ppm;}}{\eqn{CH[4] = 0.3 ppm;}{ASCII}} = 1.4 ppb;
#'               \item \ifelse{html}{\out{H<sub>2</sub>O = 50 ppm.}}{\eqn{H[2]O = 50 ppm.}{ASCII}}
#'             }
#'             If using the micro  ultra-portable GGA (GLA131 series):
#'             \itemize{
#'               \item \ifelse{html}{\out{CO<sub>2</sub> = 0.35 ppm;}}{\eqn{CO[2] = 0.35 ppm;}{ASCII}}
#'               \item \ifelse{html}{\out{CH<sub>4</sub> = 0.9 ppb;}}{\eqn{CH[4] = 0.9 ppb;}{ASCII}}
#'               \item \ifelse{html}{\out{H<sub>2</sub>O = 200 ppm.}}{\eqn{H[2]O = 200 ppm.}{ASCII}}
#'             }
#' @param Area numerical value; area of the soil surface inside the chamber
#'             \ifelse{html}{\out{(cm<sup>2</sup>)}}{\eqn{(cm^2)}{ASCII}}.
#'             Alternatively, provide the column \code{Area} in dataframe if
#'             \code{Area} is different between samples.
#' @param Vtot numerical value; total volume inside the chamber, tubes, instruments,
#'             etc. (L). Alternatively, provide the column \code{Vtot} in dataframe
#'             if \code{Vtot} is different between samples. If \code{Vtot} is
#'             missing, the function will calculate it from \code{Area},
#'             \code{Vcham} and \code{offset}.
#' @param Vcham (optional) numerical value; volume inside the chamber, tubes and
#'              instruments (L). Alternatively, provide the column \code{Vcham}
#'              in dataframe if \code{Vcham} is different between samples.
#'              \code{Vhcam} is only used if \code{Vtot} is missing.
#' @param offset (optional) numerical value; height between the soil surface and
#'               the chamber (cm). Alternatively, provide the column \code{offset}
#'               in dataframe if \code{offset} is different between samples.
#'               \code{offset} is only used if \code{Vtot} is missing.
#' @param Pcham numerical value; pressure inside the chamber (kPa).
#'              Alternatively, provide the column \code{Pcham} in dataframe if
#'              \code{Pcham} is different between samples. If \code{Pcham} is
#'              not provided, normal atmospheric pressure (101.325 kPa) is used.
#' @param Tcham numerical value; temperature inside the chamber (Celcius).
#'              Alternatively, provide the column \code{Tcham} in dataframe if
#'              \code{Tcham} is different between samples. If \code{Tcham} is
#'              not provided, normal air temperature (15 \u00b0 C) is used.
#' @param k.mult numerical value; a multiplier for the allowed kappa-max.
#'               kappa-max is the maximal curvature (kappa) of the non-linear
#'               regression (Hutchinson and Mosier model) allowed for a each
#'               flux measurement. See the functions
#'               \code{\link[GoFluxYourself]{k.max}} and
#'               \code{\link[GoFluxYourself]{HM.flux}}
#'               for more information. Default setting is \code{k.mult = 1}.
#'
#' @details
#' Flux estimate units are
#' \ifelse{html}{\out{nmol/m<sup>2</sup>s}}{\eqn{nmol/m^{2}s}{ASCII}}
#' (if initial concentration is ppm, e.g. CO2dry_ppm) and
#' \ifelse{html}{\out{µmol/m<sup>2</sup>s}}{\eqn{µmol/m^{2}s}{ASCII}}
#' (if initial concentration is ppb, e.g. CH4dry_ppm).
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
#' @returns Returns a data frame with 24 columns: a UniqueID per measurement,
#'          9 columns for the linear model results (slope, C0, Ci, flux, root
#'          mean square error (RMSE), standard error (se), relative se (se.rel),
#'          \ifelse{html}{\out{r<sup>2</sup>}}{\eqn{2^2}{ASCII}}, and p-value),
#'          9 columns for the non-linear model results (slope, C0, Ci, flux, root
#'          mean square error (RMSE), standard error (se), relative se (se.rel),
#'          \ifelse{html}{\out{r<sup>2</sup>}}{\eqn{2^2}{ASCII}}, and kappa), as
#'          well as the minimal detectable flux (f.min; \code{\link[GoFluxYourself]{MDF}}),
#'          the precision of the instrument (prec), the flux term
#'          (\code{\link[GoFluxYourself]{flux.term}}), kappa-max
#'          (\code{\link[GoFluxYourself]{k.max}}) and the g factor
#'          (\code{\link[GoFluxYourself]{g.factor}}).
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
#' @examples
#' data(example_LGR_manID)
#' CO2_flux <- goFlux(example_LGR_manID, "CO2dry_ppm")
#' CH4_flux <- goFlux(example_LGR_manID, "CH4dry_ppb")
#' H2O_flux <- goFlux(example_LGR_manID, "H2O_ppm")
#'
#' @export
#'
goFlux <- function(dataframe, gastype, H2O_col = "H2O_ppm", prec = NULL,
                   Area = NULL, offset = NULL, Vtot = NULL, Vcham = NULL,
                   Pcham = NULL, Tcham = NULL, k.mult = 1) {

  # Assign NULL to variables without binding
  H2O_ppm <- H2O_mol <- Etime <- flag <- NULL

  # Use provided values for Area, offset, Vcham, Vtot, Pcham and Tcham
  # if they are missing from dataframe
  if (!is.null(Area)) {
    dataframe <- dataframe %>% mutate(Area = Area)
  }
  if (!is.null(offset)) {
    dataframe <- dataframe %>% mutate(offset = offset)
  }
  if (!is.null(Vcham)) {
    dataframe <- dataframe %>% mutate(Vcham = Vcham)
  }
  if (!is.null(Vtot)) {
    dataframe <- dataframe %>% mutate(Vtot = Vtot)
  }
  if (!is.null(Pcham)) {
    dataframe <- dataframe %>% mutate(Pcham = Pcham)
  }
  if ((!is.null(Tcham))) {
    dataframe <- dataframe %>% mutate(Tcham = Tcham)
  }

  # Calculate Vtot if absent from dataframe
  if (!any(grepl("Vtot", names(dataframe)))) {
    dataframe <- dataframe %>% mutate(Vtot = Vcham + (Area * offset))
  }

  # Use normal atmospheric pressure and ambient temperature
  # if Pcham and Tcham are missing from dataframe
  if (!any(grepl("Pcham", names(dataframe)))) {
    dataframe <- dataframe %>% mutate(Pcham = 101.325)
  }
  if (!any(grepl("Tcham", names(dataframe)))) {
    dataframe <- dataframe %>% mutate(Tcham = 15)
  }

  # Clean and subset data (per gastype)
  if (gastype != "H2O_ppm") {
    data_split <- dataframe %>%
      # Rename H2O_col
      rename(H2O_ppm = all_of(H2O_col)) %>%
      # Use mutate() to convert H2O_ppm into H2O_mol
      mutate(H2O_mol = H2O_ppm / (1000*1000)) %>%
      select(UniqueID, H2O_mol, Etime, Vtot, Pcham, Area, Tcham,
             flag, matches(gastype)) %>%
      # Remove bad measurements (flag == 0)
      filter(flag == 1) %>%
      # Use drop_na() to remove NAs
      drop_na(matches(gastype)) %>% group_by(UniqueID) %>%
      # Interpolate missing values for chamber pressure and temperature
      fill(Pcham, Tcham, .direction = "up") %>%
      # Remove duplicates of Etime
      ungroup() %>% distinct(UniqueID, Etime, .keep_all = TRUE) %>%
      # Split dataset by UniqueID
      group_split(UniqueID) %>% as.list()
  } else

    if (gastype == "H2O_ppm") {
      data_split <- dataframe %>%
        select(UniqueID, Etime, Vtot, Pcham, Area, Tcham,
               flag, all_of(H2O_col)) %>%
        # Rename H2O_col
        rename(H2O_ppm = all_of(H2O_col)) %>%
        # Remove bad measurements (flag == 0)
        filter(flag == 1) %>%
        # Use drop_na() to remove NAs
        drop_na(matches(gastype)) %>% group_by(UniqueID) %>%
        # Interpolate missing values for chamber pressure and temperature
        fill(Pcham, Tcham, .direction = "up") %>%
        # Remove duplicates of Etime
        ungroup() %>% distinct(UniqueID, Etime, .keep_all = TRUE) %>%
        # Split dataset by UniqueID
        group_split(UniqueID) %>% as.list()
    }

  # Instrument precision (by gastype)
  # If prec = NULL, the default parameters are set to the LI-7810 for CH4 and CO2,
  # or the LI-7820 for N2O. Both instruments have the same precision for H2O.
  if (is.null(prec)) {
    prec <- ifelse(gastype == "CO2dry_ppm", 3.5,
                   ifelse(gastype == "CH4dry_ppb", 0.6,
                          ifelse(gastype == "N2Odry_ppb", 0.4,
                                 ifelse(gastype == "H2O_ppm", 45, NA))))
  } else { prec = prec }

  # Calculate auxiliary variables: flux term and minimal detectable flux
  for (f in 1:length(data_split)) {

    H2O_flux.term <- ifelse(gastype == "H2O_ppm", 0, first(data_split[[f]]$H2O_mol))

    data_split[[f]] <- data_split[[f]] %>%
      mutate(flux.term = flux.term(first(Vtot), first(Pcham), first(Area),
                                   first(Tcham), H2O_flux.term),
             f.min = MDF(prec, (max(Etime)+1), flux.term))
  }

  # Create an empty list to store results
  flux.res.ls <- list()

  # Print a progress bar
  pb = txtProgressBar(min = 0, max = length(data_split), initial = 0, style = 3)

  # Flux calculation
  for (f in 1:length(data_split)) {

    # Extract auxiliary variables: flux term, minimal detectable flux and UniqueID
    UniqueID <- unique(data_split[[f]]$UniqueID)
    flux.term <- first(data_split[[f]]$flux.term)
    f.min <- first(data_split[[f]]$f.min)

    # Extract gas measurement (by gastype)
    gas.meas <- Reduce("c", data_split[[f]][, gastype])

    # Linear model
    LM.res <- LM.flux(gas.meas = gas.meas,
                      time.meas = data_split[[f]]$Etime,
                      flux.term = flux.term)

    # Calculate C0 and Ci and their boundaries based on LM.flux
    C0.flux <- LM.res$LM.C0
    Ci.flux <- LM.res$LM.Ci
    C.diff.flux <- abs(Ci.flux-C0.flux)
    C0.lim.flux <- c(C0.flux-C.diff.flux*0.2, C0.flux+C.diff.flux*0.2)
    Ci.lim.flux <- c(Ci.flux-C.diff.flux*0.2, Ci.flux+C.diff.flux*0.2)

    # Calculate C0 and Ci and their boundaries based on raw data
    C0.raw <- first(gas.meas)
    Ci.raw <- last(gas.meas)

    # Chose the right C0 and Ci
    Ci <- if_else(between(Ci.raw, Ci.lim.flux[1], Ci.lim.flux[2]), Ci.raw, Ci.flux)
    C0 <- if_else(between(C0.raw, C0.lim.flux[1], C0.lim.flux[2]), C0.raw, C0.flux)

    # Calculate kappa thresholds based on MDF, LM.flux and Etime
    kappa.max <- k.max(f.min, LM.res$LM.flux, (max(data_split[[f]]$Etime)+1))

    # Hutchinson and Mosier
    HM.res <- HM.flux(gas.meas = gas.meas, time.meas = data_split[[f]]$Etime,
                      flux.term = flux.term, Ci = Ci, C0 = C0, k.max = kappa.max)

    # Flux results and G factor
    flux.res.ls[[f]] <- cbind.data.frame(
      UniqueID, LM.res, HM.res, f.min, prec, flux.term, k.max = kappa.max*k.mult,
      g.fact = g.factor(HM.res$HM.flux, LM.res$LM.flux))

    # Update progress bar
    setTxtProgressBar(pb, f)
  }

  # Unlist flux results
  flux_results <- map_df(flux.res.ls,  ~as.data.frame(.x))

  # Close progress bar
  close(pb)

  # Return results
  return(flux_results)

}
