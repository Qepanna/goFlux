#' goFlux: a user-friendly GHG fluxes calculation tool
#'
#' A wrapper function to calculate GHG fluxes from static chamber measurements.
#' Calculates linear (\code{\link[goFlux]{LM.flux}}) and non-linear
#' fluxes (Hutchinson and Mosier model; \code{\link[goFlux]{HM.flux}}),
#' from a variety of greenhouse gases (
#' \ifelse{html}{\out{CO<sub>2</sub>}}{\eqn{CO[2]}{ASCII}},
#' \ifelse{html}{\out{CH<sub>4</sub>}}{\eqn{CH[4]}{ASCII}},
#' \ifelse{html}{\out{N<sub>2</sub>O}}{\eqn{N[2]O}{ASCII}},
#' \ifelse{html}{\out{NO<sub>2</sub>}}{\eqn{NO[2]}{ASCII}}, NO,
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
#'                "CH4dry_ppb", "COdry_ppb", "N2Odry_ppb", "NH3dry_ppb",
#'                "NOdry_ppb", "NO2dry_ppb" or "H2O_ppm".
#' @param H2O_col character string; specifies which column should be used to
#'                subtract the effect of water vapor in the chamber space.
#'                Default is \code{H2O_col = "H2O_ppm"}. If water vapor was not
#'                measured, set to \code{H2O_col = NULL}.
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
#'             \code{Vtot} is missing, the function will calculate it as
#'             \code{Vcham + Area*offset/1000}.
#' @param Vcham (optional) numerical value; volume inside the chamber, tubes and
#'              instruments (L). Alternatively, provide the column \code{Vcham}
#'              in \code{dataframe} if \code{Vcham} is different between samples.
#'              \code{Vcham} is only used if \code{Vtot} is missing.
#' @param offset (optional) numerical value; height between the soil surface and
#'               the chamber (cm). Alternatively, provide the column \code{offset}
#'               in \code{dataframe} if \code{offset} is different between samples.
#'               \code{offset} is only used if \code{Vtot} is missing.
#' @param Pcham numerical value; pressure inside the chamber (kPa).
#'              Alternatively, provide the column \code{Pcham} in \code{dataframe}
#'              if \code{Pcham} is different between samples. If \code{Pcham} is
#'              not provided, normal atmospheric pressure (101.325 kPa) is used.
#' @param Tcham numerical value; temperature inside the chamber (°C).
#'              Alternatively, provide the column \code{Tcham} in \code{dataframe}
#'              if \code{Tcham} is different between samples. If \code{Tcham} is
#'              not provided, 15°C is used as default.
#' @param k.mult numerical value; a multiplier for the allowed kappa-max.
#'               Default setting is no multiplier (\code{k.mult = 1}).
#'               \code{k.mult} must be > 0 and =< 10.
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
#' Flux estimates and the \code{\link[goFlux]{MDF}} are calculated from
#' \code{Etime} (in seconds). Make sure that chamber closure is indicated by
#' \code{Etime == 0} and the first instance of \code{flag == 1} per UniqueID.
#'
#' All flux estimates, including the \code{\link[goFlux]{MDF}}, are obtained
#' from the multiplication of the slope and the \code{\link[goFlux]{flux.term}},
#' which is used to correct for the dilution effect of water vapor and convert
#' the flux units to obtain a term in nmol or
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
#' @returns Returns a data frame with 32 columns: a \code{UniqueID} per
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
#' kappa-max (\code{\link[goFlux]{k.max}}) and its multiplier (\code{k.mult}),
#' the g-factor (g.fact; \code{\link[goFlux]{g.factor}}), the number of
#' observations used (\code{nb.obs}) and the true initial gas concentration
#' (\code{C0}) and final gas concentration (\code{Ct}).
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
  is_scalar_num <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)}
  has_col <- function(nm) {nm %in% names(dataframe)}


  # k.mult
  if (!is_scalar_num(k.mult)) {
    stop("'k.mult' must be a finite numeric scalar.", call. = FALSE)}
  if (k.mult <= 0 || k.mult > 10) {
    stop("'k.mult' must be > 0 and <= 10.", call. = FALSE)}


  # warn.length
  if (!is_scalar_num(warn.length)) {
    stop("'warn.length' must be a finite numeric scalar.", call. = FALSE)}
  if (warn.length <= 0) {
    stop("'warn.length' must be > 0.", call. = FALSE)}
  if (warn.length %% 1 != 0) {
    stop("'warn.length' must be an integer.", call. = FALSE)}


  # k.min
  if (!is_scalar_num(k.min)) {
    stop("'k.min' must be a finite numeric scalar.", call. = FALSE)}


  ## Check dataframe ####
  if (missing(dataframe)) {
    stop("'dataframe' is required.", call. = FALSE)}
  if (!is.data.frame(dataframe)) {
    stop("'dataframe' must be a data.frame.", call. = FALSE)}
  if (nrow(dataframe) == 0) {
    stop("'dataframe' is empty.", call. = FALSE)}


  ### gastype and match in dataframe ####
  .allowed_gastype <- c(
    "CO2dry_ppm", "CH4dry_ppb", "COdry_ppb", "N2Odry_ppb",
    "NH3dry_ppb", "NO2dry_ppb", "NOdry_ppb", "H2O_ppm"
  )
  if (missing(gastype)) {
    stop("'gastype' is required and must be one of: ",
         paste0("'", .allowed_gastype, "'", collapse = ", "), call. = FALSE)}
  if (!is.character(gastype) || length(gastype) != 1L || is.na(gastype)) {
    stop("'gastype' must be a character string of length 1.", call. = FALSE)}
  if (!(gastype %in% .allowed_gastype)) {
    stop("'gastype' must be one of: ",
         paste0("'", .allowed_gastype, "'", collapse = ", "), call. = FALSE)}
  if (!has_col(gastype)) {
    stop("'dataframe' must contain a column that matches 'gastype'", call. = FALSE)}
  if (!is.numeric(dataframe[[gastype]])) {
    stop("Column '", gastype, "' in 'dataframe' must be numeric.", call. = FALSE)}


  ### prec and match in dataframe ####
  if (!is.null(prec)) {
    if (!is_scalar_num(prec) || prec <= 0) {
      stop("'prec' must be a finite numeric scalar greater than 0.", call. = FALSE)}

  } else {

    prec_col <- switch(gastype,
                       "CO2dry_ppm" = "CO2_prec",
                       "CH4dry_ppb" = "CH4_prec",
                       "COdry_ppb"  = "CO_prec",
                       "N2Odry_ppb" = "N2O_prec",
                       "NO2dry_ppb" = "NO2_prec",
                       "NOdry_ppb"  = "NO_prec",
                       "NH3dry_ppb" = "NH3_prec",
                       "H2O_ppm"    = "H2O_prec")

    if (!has_col(prec_col)) {
      stop("'dataframe' must contain the column '", prec_col,
           "' when prec = NULL.", call. = FALSE)}
    if (!is.numeric(dataframe[[prec_col]])) {
      stop("Column '", prec_col, "' in 'dataframe' must be numeric.",
           call. = FALSE)}
  }


  ### H2O_col and match in dataframe ####
  if (!is.null(H2O_col)) {

    if (!is.character(H2O_col) || length(H2O_col) != 1L ||
        is.na(H2O_col) || H2O_col == "") {
      stop("'H2O_col' must be a non-missing, non-empty character string",
           "of length 1, or NULL.", call. = FALSE)}

    if (!has_col(H2O_col)) {
      stop("'dataframe' must contain a column that matches 'H2O_col'.",
           call. = FALSE)}

    if (!is.numeric(dataframe[[H2O_col]])) {
      stop("Column '", H2O_col, "' in 'dataframe' must be numeric.",
           call. = FALSE)}

  } else {

    warning("H2O_col is NULL: water vapour dilution correction is disabled ",
            "(H2O_ppm assumed 0).", call. = FALSE)}


  ### UniqueID (or chamID) ####
  if (!has_col("UniqueID") && !has_col("chamID")) {
    stop("'dataframe' must contain column 'UniqueID' or 'chamID'.", call. = FALSE)}
  if (has_col("UniqueID") && all(is.na(dataframe$UniqueID))) {
    stop("'UniqueID' in 'dataframe' contains only NA values.", call. = FALSE)}
  if (has_col("UniqueID") &&
      !(is.character(dataframe$UniqueID) || is.factor(dataframe$UniqueID))) {
    stop("'UniqueID' in 'dataframe' must be character or factor.", call. = FALSE)}

  # Construct UniqueID from chamID + DATE, if missing
  if (!has_col("UniqueID") && has_col("chamID") && !has_col("DATE")) {
    stop("'dataframe' must contain 'DATE' to construct 'UniqueID' from 'chamID'.",
         call. = FALSE)}

  if (has_col("chamID") && all(is.na(dataframe$chamID))) {
    stop("'chamID' in 'dataframe' contains only NA values.", call. = FALSE)}
  if (has_col("chamID") &&
      !(is.character(dataframe$chamID) || is.factor(dataframe$chamID))) {
    stop("'chamID' in 'dataframe' must be character or factor.", call. = FALSE)
  }

  if (!has_col("UniqueID")){
    dataframe$UniqueID <- paste(dataframe$chamID, dataframe$DATE, sep = "_")}


  ### Etime ####
  if (!has_col("Etime")) {
    stop("'dataframe' must contain the column 'Etime'.", call. = FALSE)}
  if (!is.numeric(dataframe$Etime)) {
    stop("'Etime' in 'dataframe' must be numeric (or integer).", call. = FALSE)}
  if (all(is.na(dataframe$Etime))) {
    stop("'Etime' in 'dataframe' contains only NA values.", call. = FALSE)}


  ### flag ####
  if (!has_col("flag")) {
    stop("'dataframe' must contain the column 'flag'.", call. = FALSE)}
  if (!is.numeric(dataframe$flag)) {
    stop("'flag' in 'dataframe' must be numeric (or integer).", call. = FALSE)}
  if (all(is.na(dataframe$flag))) {
    stop("'flag' in 'dataframe' contains only NA values.", call. = FALSE)}


  ### Area ####
  if (!is.null(Area)) {
    # Area is an argument
    if (!is_scalar_num(Area) || Area <= 0) {
      stop("'Area' must be a finite numeric scalar greater than 0.",
           call. = FALSE)}

  } else {
    # Area must be in dataframe
    if (!has_col("Area")) {
      stop("'Area' missing: provide 'Area' as an argument or as a column in ",
           "'dataframe'.", call. = FALSE)}
    if (!is.numeric(dataframe$Area)) {
      stop("'Area' in 'dataframe' must be numeric.", call. = FALSE)}
    if (all(is.na(dataframe$Area))) {
      stop("'Area' in 'dataframe' contains only NA values.", call. = FALSE)}
    if (any(na.omit(dataframe$Area) <= 0)) {
      stop("'Area' in 'dataframe' must be greater than 0.", call. = FALSE)}
  }

  # Add Area to dataframe if provided
  if (!is.null(Area)) dataframe$Area <- Area


  ### Vtot (or Vcham + offset) ####
  # 1) If Vtot provided as argument, validate it
  if (!is.null(Vtot)) {
    if (!is_scalar_num(Vtot) || Vtot <= 0) {
      stop("'Vtot' must be a finite numeric scalar greater than 0.", call. = FALSE)
    }

  } else if (has_col("Vtot")) {
    # 2) Else: Vtot must be present as a numeric column (and not all NA)
    if (!is.numeric(dataframe$Vtot)) {
      stop("'Vtot' in 'dataframe' must be numeric.", call. = FALSE)}
    if (all(is.na(dataframe$Vtot))) {
      stop("'Vtot' in 'dataframe' contains only NA values.", call. = FALSE)}

  } else {
    # 3) Else: must be able to compute Vtot from Vcham + Area*offset (both required)

    # Vcham is an argument
    if (!is.null(Vcham)) {
      if (!is_scalar_num(Vcham) || Vcham <= 0) {
        stop("'Vcham' must be a finite numeric scalar greater than 0.",
             call. = FALSE)}

    } else {
      # Vcham must be in dataframe
      if (!has_col("Vcham")) {
        stop("'Vtot' missing: provide 'Vtot' as an argument or as a column in ",
             "'dataframe'. Alternatively, provide 'Vcham' (arg/column), ",
             "'Area' (arg/column) and 'offset' (arg/column).", call. = FALSE)
      }
      if (!is.numeric(dataframe$Vcham)) {
        stop("'Vcham' in 'dataframe' must be numeric.", call. = FALSE)}
      if (all(is.na(dataframe$Vcham))) {
        stop("'Vcham' in 'dataframe' contains only NA values.", call. = FALSE)}
    }

    # offset is an argument
    if (!is.null(offset)) {
      if (!is_scalar_num(offset) || offset < 0) {
        stop("'offset' must be a finite numeric scalar greater or equal to 0.",
             call. = FALSE)}

    } else {
      # offset must be in dataframe
      if (!has_col("offset")) {
        stop("'Vtot' missing: provide 'Vtot' as an argument or as a column in ",
             "'dataframe'. Alternatively, provide 'Vcham' (arg/column), ",
             "'Area' (arg/column) and 'offset' (arg/column).", call. = FALSE)
      }
      if (!is.numeric(dataframe$offset)) {
        stop("'offset' in 'dataframe' must be numeric.", call. = FALSE)}
      if (all(is.na(dataframe$offset))) {
        stop("'offset' in 'dataframe' contains only NA values.", call. = FALSE)}
    }
  }

  # Add Vtot, offset and Vcham to dataframe if provided
  if (!is.null(Vtot)) dataframe$Vtot <- Vtot
  if (!is.null(offset)) dataframe$offset <- offset
  if (!is.null(Vcham)) dataframe$Vcham <- Vcham

  # Calculate Vtot if absent from dataframe
  if (is.null(Vtot) && !has_col("Vtot")){
    dataframe$Vtot <- dataframe$Vcham + (dataframe$Area * dataframe$offset)/1000
  }


  ### Pcham ####
  if (!is.null(Pcham)) {

    # Pcham is an argument
    if (!is_scalar_num(Pcham) || Pcham <= 0) {
      stop("'Pcham' must be a finite numeric scalar greater than 0.",
           call. = FALSE)}

  } else if (has_col("Pcham")) {

    # Check Pcham in dataframe
    if (!is.numeric(dataframe$Pcham)) {
      stop("'Pcham' in 'dataframe' must be numeric.", call. = FALSE)}
    if (all(is.na(dataframe$Pcham))) {
      stop("'Pcham' in 'dataframe' contains only NA values.", call. = FALSE)}
    if (any(na.omit(dataframe$Pcham) <= 0)) {
      stop("'Pcham' in 'dataframe' must be greater than 0.", call. = FALSE)}

  } else {

    # Use normal atmospheric pressure if Pcham is not provided
    dataframe$Pcham <- 101.325
    warning("Normal atmospheric pressure (101.325kPa) is used when Pcham ",
            "is not provided.", call. = FALSE)
  }

  # Add Pcham to dataframe if provided
  if (!is.null(Pcham)) dataframe$Pcham <- Pcham


  ### Tcham ####
  if (!is.null(Tcham)) {

    # Tcham is an argument
    if (!is_scalar_num(Tcham)) {
      stop("'Tcham' must be a finite numeric scalar.", call. = FALSE)}
    if (Tcham < -273.15) {
      stop("'Tcham' cannot be smaller than -273.15 Celsius (physical limit).",
           call. = FALSE)}

  } else if (has_col("Tcham")) {

    # Check Tcham in dataframe
    if (!is.numeric(dataframe$Tcham)) {
      stop("'Tcham' in 'dataframe' must be numeric.", call. = FALSE)}
    if (all(is.na(dataframe$Tcham))) {
      stop("'Tcham' in 'dataframe' contains only NA values.", call. = FALSE)}
    if (any(na.omit(dataframe$Tcham) < -273.15)) {
      stop("Values under the physical limits of temperature (-273.15 Celsius) ",
           "were detected in 'Tcham' in 'dataframe'.", call. = FALSE)}

  } else {

    # Use air temperature if Tcham is not provided
    dataframe$Tcham <- 15
    warning("Normal ambient temperature (15 Celsius) is used when ",
            "Tcham is not provided.", call. = FALSE)
  }

  # Add Tcham to dataframe if provided
  if (!is.null(Tcham)) dataframe$Tcham <- Tcham


  # Assign NULL to variables without binding ####
  H2O_ppm_select <- H2O_mol <- Etime <- flag <- NULL


  # FUNCTION STARTS ####

  ## Clean and split data ####
  if (gastype != "H2O_ppm"){

    # If water vapor is missing, set H2O_ppm = 0
    if (is.null(H2O_col)) {
      dataframe$H2O_ppm <- 0
      H2O_col <- "H2O_ppm"
    }

    data_split <- dataframe %>%
      # Rename H2O_col
      rename(H2O_ppm_select = all_of(H2O_col)) %>%
      # Convert H2O_ppm into H2O_mol
      mutate(H2O_mol = H2O_ppm_select / (1000*1000)) %>%
      select(UniqueID, any_of(c("chamID", "DATE")), Etime, flag, all_of(gastype),
             contains("_prec"), H2O_mol, Vtot, Area, Pcham, Tcham) %>%
      # Filter flag == 1
      filter(flag == 1) %>%
      # Remove NAs in gastype
      tidyr::drop_na(all_of(gastype)) %>%
      tidyr::drop_na(Etime) %>%
      tidyr::drop_na(UniqueID) %>%
      # Split dataset by UniqueID
      group_by(UniqueID) %>% group_split() %>% as.list()
  }

  if (gastype == "H2O_ppm"){

    data_split <- dataframe %>%
      select(UniqueID, any_of(c("chamID", "DATE")), Etime, flag,
             all_of(gastype), contains("_prec"), Vtot, Area, Pcham, Tcham) %>%
      # Filter flag == 1
      filter(flag == 1) %>%
      # Remove NAs in gastype and UniqueID
      tidyr::drop_na(all_of(gastype)) %>%
      tidyr::drop_na(Etime) %>%
      tidyr::drop_na(UniqueID) %>%
      # Split dataset by UniqueID
      group_by(UniqueID) %>% group_split() %>% as.list()
  }

  # Ensure data_split is not empty
  if (length(data_split) == 0L) {
    stop("No valid observations after filtering (flag == 1) and removing NAs in '",
         gastype, "'.", call. = FALSE)}

  # Ensure Etime is ordered within each UniqueID
  data_split <- lapply(data_split, function(df) {df %>% arrange(Etime)})

  ## Calculate auxiliary variables: flux term and minimal detectable flux ####
  for (f in 1:length(data_split)){

    # Instrument precision (by gastype)
    # If prec = NULL, the instrument precision must be provided in 'dataframe'
    if (is.null(prec)) {

      prec_col <- switch(gastype,
                         "CO2dry_ppm" = "CO2_prec",
                         "CH4dry_ppb" = "CH4_prec",
                         "COdry_ppb"  = "CO_prec",
                         "N2Odry_ppb" = "N2O_prec",
                         "NO2dry_ppb" = "NO2_prec",
                         "NOdry_ppb"  = "NO_prec",
                         "NH3dry_ppb" = "NH3_prec",
                         "H2O_ppm"    = "H2O_prec"
      )

      prec_vals <- unique(na.omit(data_split[[f]][[prec_col]]))
      uid <- unique(data_split[[f]]$UniqueID)

      if (length(prec_vals) != 1) {
        stop("'", prec_col, "' in 'dataframe' must contain exactly one non-missing ",
             "value per UniqueID. Problem detected for UniqueID: ",
             uid, ".", call. = FALSE)
      }

      data_split[[f]]$prec_f <- prec_vals

    } else { data_split[[f]]$prec_f <- prec }

    # Extract water vapor concentration at the start of the measurement
    if (gastype == "H2O_ppm") {
      # Assign 0 if gastype == "H2O_ppm"
      H2O_flux.term <- 0
      data_split[[f]]$warn.H2O_mol <- FALSE
    }
    if (gastype != "H2O_ppm") {
      # If H2O_mol is all NAs, default to 0
      if (all(is.na(data_split[[f]]$H2O_mol))) {
        H2O_flux.term <- 0
        data_split[[f]]$warn.H2O_mol <- TRUE
      } else {
        H2O_flux.term <- first(na.omit(data_split[[f]]$H2O_mol))
        data_split[[f]]$warn.H2O_mol <- FALSE}
    }

    # First flagged time must be chamber closure (Etime == 0)
    if (is.na(data_split[[f]]$Etime[1]) || data_split[[f]]$Etime[1] != 0) {
      stop("Invalid Etime origin: for each UniqueID, the first row with ",
           "flag == 1 must have Etime == 0. Problem detected for UniqueID: ",
           data_split[[f]]$UniqueID[1], ".", call. = FALSE)}

    # Ensure values are available and unique per UniqueID for Vtot and Area
    if (all(is.na(data_split[[f]]$Vtot))) {
      stop("Vtot missing and could not be calculated for UniqueID: ",
           data_split[[f]]$UniqueID[1], call. = FALSE)}
    if (length(unique(na.omit(data_split[[f]]$Vtot))) != 1) {
      stop("'Vtot' in 'dataframe' must contain exactly one value ",
           "per UniqueID. Problem detected for UniqueID: ",
           data_split[[f]]$UniqueID[1], ".", call. = FALSE)}

    if (all(is.na(data_split[[f]]$Area))) {
      stop("Area missing for UniqueID: ",
           data_split[[f]]$UniqueID[1], call. = FALSE)}
    if (length(unique(na.omit(data_split[[f]]$Area))) != 1) {
      stop("'Area' in 'dataframe' must contain exactly one value ",
           "per UniqueID. Problem detected for UniqueID: ",
           data_split[[f]]$UniqueID[1], ".", call. = FALSE)}

    # If Pcham and Tcham are all NAs, default to normal P and T
    if (all(is.na(data_split[[f]]$Pcham))) {
      data_split[[f]]$Pcham <- 101.325
      data_split[[f]]$warn.Pcham <- TRUE
    } else data_split[[f]]$warn.Pcham <- FALSE

    if (all(is.na(data_split[[f]]$Tcham))) {
      data_split[[f]]$Tcham <- 15
      data_split[[f]]$warn.Tcham <- TRUE
    } else data_split[[f]]$warn.Tcham <- FALSE

    # Calculate flux.term and MDF
    flux_term_f <- flux.term(first(na.omit(data_split[[f]]$Vtot)),
                             first(na.omit(data_split[[f]]$Pcham)),
                             first(na.omit(data_split[[f]]$Area)),
                             first(na.omit(data_split[[f]]$Tcham)),
                             H2O_flux.term)
    data_split[[f]]$flux_term <- flux_term_f

    MDF_f <- MDF(data_split[[f]]$prec_f[1],
                 (max(data_split[[f]]$Etime)+1), flux_term_f)
    data_split[[f]]$MDF <- MDF_f
  }

  ##  Flux calculation ####

  # Create an empty list to store results
  flux.res.ls <- list()

  # Print a progress bar
  pb = txtProgressBar(min = 0, max = length(data_split), initial = 0, style = 3)

  for (f in 1:length(data_split)){

    # Extract auxiliary variables
    UniqueID <- data_split[[f]]$UniqueID[1]
    flux.term_f <- data_split[[f]]$flux_term[1]
    MDF <- data_split[[f]]$MDF[1]
    nb.obs <- length(data_split[[f]][[gastype]])
    prec_f <- data_split[[f]]$prec_f[1]

    # Skip if there is less than 3 data points
    if (nb.obs < 3){

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
      gas.meas <- data_split[[f]][[gastype]]

      # Get C0 and Ct from raw data
      C0 <- first(gas.meas)
      Ct <- last(gas.meas)

      # HM derived variables
      kappa.max <- g.fact <- NA

    } else {

      # Extract gas measurement (by gastype)
      gas.meas <- data_split[[f]][[gastype]]

      # Linear model
      LM.res <- suppressWarnings(
        LM.flux(gas.meas = gas.meas,
                time.meas = data_split[[f]]$Etime,
                flux.term = flux.term_f))

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

      # Adjust C0 and Ct if the difference between them is smaller than 1
      if (abs(C.diff.flux) < 1) {
        if (Ct.best < C0.best){
          Ct.best <- floor(Ct.best) - 1
          C0.best <- ceiling(C0.best) + 1
        } else {
          Ct.best <- ceiling(Ct.best) + 1
          C0.best <- floor(C0.best) - 1
        }
      }

      # Calculate kappa thresholds based on MDF, LM.flux and Etime
      kappa.max <- abs(k.max(MDF, LM.res$LM.flux, (max(data_split[[f]]$Etime)+1)))

      # Try to catch errors and warnings from HM calculation
      HM.catch <- HM.flux(gas.meas = gas.meas, time.meas = data_split[[f]]$Etime,
                          flux.term = flux.term_f, Ct = Ct.best, C0 = C0.best,
                          k.max = Inf, k.min = k.min)

      # If there is an error with singular gradient
      if (inherits(HM.catch[[2]], "simpleError")){

        # Print warning
        if (isTRUE(grepl("singular gradient", HM.catch[[2]]$message))){
          warning("Flux estimate is too close to zero to estimate HM flux in UniqueID ",
                  UniqueID, ". NAs produced.", call. = FALSE)
        } else {
          message("Warning in UniqueID ", UniqueID, ": ", HM.catch[[2]]$message)
        }

        # Return data frame
        HM.res <- HM.catch[[1]]
      }

      # If there is no error
      if (!inherits(HM.catch[[2]], "simpleError")){

        # But there is a warning with HM
        if (inherits(HM.catch[[3]], "simpleWarning")){

          # Print warning
          message("Warning in UniqueID ", UniqueID, ": ", HM.catch[[3]]$message)
        }

        # Or there is a warning with AICc
        if (inherits(HM.catch[[4]], "simpleWarning")){

          # Print warning
          if (isTRUE(grepl("sample size", HM.catch[[4]]$message))){
            warning("Sample size is too small for UniqueID ", UniqueID,
                    ". Results may be meaningless or missing.", call. = FALSE)
          } else {
            message("Warning in UniqueID ", UniqueID, ": ", HM.catch[[4]]$message)
          }
        }

        # Hutchison and Mosier without kappa max
        HM.noK <- HM.catch[[1]]

        # Hutchinson and Mosier with kappa max
        HM.K <- HM.flux(gas.meas = gas.meas, time.meas = data_split[[f]]$Etime,
                        flux.term = flux.term_f, Ct = Ct.best, C0 = C0.best,
                        k.max = kappa.max, k.mult = k.mult, k.min = k.min)[[1]]

        # Compare results, with and without kappa max.
        # Select the result with the smallest curvature.
        if (abs(HM.K$HM.k) <= abs(HM.noK$HM.k)) HM.res <- HM.K else HM.res <- HM.noK
      }
    }

    # Flux results
    flux.res.ls[[f]] <- cbind.data.frame(
      UniqueID, LM.res, HM.res, C0, Ct, MDF, prec = prec_f,
      flux.term = flux.term_f, nb.obs, k.max = kappa.max, k.mult,
      g.fact = g.factor(HM.res$HM.flux, LM.res$LM.flux))

    # Update progress bar
    setTxtProgressBar(pb, f)

  }

  # Unlist flux results
  flux_results <- purrr::map_df(flux.res.ls,  ~as.data.frame(.x))

  # Close progress bar
  close(pb)

  for (f in 1:nrow(flux_results)){
    if (flux_results$nb.obs[f] < warn.length){
      warning("Low number of observations: UniqueID ", flux_results$UniqueID[f],
              " has ", flux_results$nb.obs[f], " observations", call. = FALSE)}
  }

  # Warn about NAs in Pcham, Tcham or H2O_mol
  for (f in 1:length(data_split)){
    if (first(data_split[[f]]$warn.Tcham) == TRUE){
      warning("Tcham missing in UniqueID ", first(data_split[[f]]$UniqueID),
              ". 15 Celsius was used as default.", call. = FALSE)}
  }
  for (f in 1:length(data_split)){
    if (first(data_split[[f]]$warn.Pcham) == TRUE){
      warning("Pcham missing in UniqueID ", first(data_split[[f]]$UniqueID),
              ". 101.325 kPa was used as default.", call. = FALSE)}
  }
  for (f in 1:length(data_split)){
    if (first(data_split[[f]]$warn.H2O_mol) == TRUE){
      warning("All NAs found in '", H2O_col, "' for UniqueID ",
              first(data_split[[f]]$UniqueID), ". 0 ppm was used as default,",
              "and water correction was suppressed.", call. = FALSE)}
  }

  # Warn about measurements with less than 3 data points
  for (f in 1:nrow(flux_results)){
    if (flux_results$nb.obs[f] < 3){
      warning("Warning in UniqueID ", flux_results$UniqueID[f], ": cannot ",
              "calculate flux estimates with less than 3 data points. ",
              "NAs produced.", call. = FALSE)}
  }

  # Return results
  return(flux_results)

}
