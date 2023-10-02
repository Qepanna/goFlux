#' goFlux: a user-friendly GHG fluxes calculation tool
#'
#' A wrapper function to calculate GHG fluxes from static chamber measurements.
#' Calculates linear and non-linear fluxes (Hutchinson and Mosier model; HM).
#'
#' @param dataframe a data.frame containing gas measurements (see gastype below),
#'                  water vapor measurements (see H2O_col below) and the following:
#'                  UniqueID, Etime, Vtot, Area, Pcham, Tcham, flag
#' @param gastype character string; specifies which column should be used for the
#'                flux calculations. Must be one of the following: "CO2dry_ppm",
#'                "CH4dry_ppb", "N2Odry_ppb" or "H2O_ppm".
#' @param H2O_col character string; specifies which column should be used to
#'                subtract the effect of water vapor in the chamber space.
#' @param prec numerical value; precision of the instruments. Default values for
#'             CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O and H<sub>2</sub>O
#'             are based on the LI-COR instruments: CO<sub>2</sub> = 3.5 ppm;
#'             CH<sub>4</sub> = 0.6 ppb; N<sub>2</sub>O = 0.4 ppb; H<sub>2</sub>O = 45 ppm.
#'             Units must be the same as gastype. For Los Gatos Research instruments,
#'             you must manually specify the precision of the instruments.
#'             If using the ultra-portable GGA (GLA132 series): prec = CO<sub>2</sub>:
#'             0.3 ppm; CH<sub>4</sub>: 1.4 ppb; H<sub>2</sub>O: 50 ppm.
#'             If using the micro  ultra-portable GGA (GLA131 series): prec =
#'             CO<sub>2</sub>: 0.35 ppm; CH<sub>4</sub>: 0.9 ppb; H<sub>2</sub>O: 200 ppm.
#' @param Area numerical value; area of the soil surface inside the chamber
#'             (cm~2~). Alternatively, provide the column Area in
#'             dataframe if Area is different between samples.
#' @param offset numerical value; height between the soil surface and the chamber
#'               (cm). Alternatively, provide the column offset in dataframe if
#'               offset is different between samples.
#' @param Vtot numerical value; total volume inside the chamber, tubes, instruments,
#'             etc. (L). Alternatively, provide the column Vtot in dataframe if
#'             Vtot is different between samples.
#' @param Vcham numerical value; volume inside the chamber, tubes and instruments
#'              (L). Alternatively, provide the column Vcham in dataframe if
#'              Vcham is different between samples.
#' @param Pcham numerical value; pressure inside the chamber (kPa).
#'              Alternatively, provide the column Pcham in dataframe if
#'              Pcham is different between samples.
#' @param Tcham numerical value; temperature inside the chamber (Celcius).
#'              Alternatively, provide the column Tcham in dataframe if
#'              Tcham is different between samples.
#'
#' @details
#' Flux estimate units are nmol/m~2~*s (if initial concentration is ppm,
#' e.g. CO2dry_ppm) and µmol/m~2~*s (if initial concentration is ppb,
#' e.g. CH4dry_ppm).
#'
#' The function `k.max()` calculates the maximal curvature (kappa) of the non-linear
#' model (Hutchinson and Mosier) allowed for each flux measurements. k.max
#' is calculated based on the minimal detectable flux (MDF), the linear
#' flux estimate and the measurement time. The unit of the kappa-max is s-1.
#'
#' The function `MDF()` calculates the minimal detectable flux (MDF) based on
#' instrument precision, measurements time, and the number of measurement points.
#'
#' @returns a data frame
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
#' goFlux(example_LGR_manID, "CO2dry_ppm")
#' goFlux(example_LGR_manID, "CH4dry_ppb")
#' goFlux(example_LGR_manID, "H2O_ppm")
#'
#' @export
#'
goFlux <- function(dataframe, gastype, H2O_col = "H2O_ppm", prec = NULL,
                   Area = NULL, offset = NULL, Vtot = NULL, Vcham = NULL,
                   Pcham = NULL, Tcham = NULL) {

  # Assign NULL to variables without binding
  H2O_ppm <- H2O_mol <- Etime <- flag <- NULL

  # Modify global options for warning: break for-loop after warning message
  options(warn = 2)

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
      UniqueID, LM.res, HM.res, f.min, prec, flux.term, k.max = kappa.max,
      g.fact = g.factor(HM.res$HM.flux, LM.res$LM.flux))

    # Update progress bar
    setTxtProgressBar(pb, f)
  }

  # Unlist flux results
  flux_results <- map_df(flux.res.ls,  ~as.data.frame(.x))

  # Close progress bar
  close(pb)

  # Modify global options for warning: set option back to default
  options(warn = 0)

  # Return results
  return(flux_results)

}
