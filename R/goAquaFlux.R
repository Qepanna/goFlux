#' Compute gas fluxes from aquatic chamber measurements
#'
#' \code{goAquaFlux} computes gas fluxes from chamber-based aquatic
#' incubations. The function partitions total flux into diffusive and
#' ebullitive components when bubbling events are detected. Bubbling
#' events are identified using high-frequency concentration variability
#' (typically CH4), after which the time series is separated into
#' diffusive and ebullitive segments.
#'
##' The workflow includes: (1) data validation and cleaning; (2) calculation of
#' auxiliary variables (flux conversion term and MDF); (3) bubble detection
#' (typically using CH4 concentration); (4) estimation of ebullition flux based
#' on detected bubble magnitudes; (5) estimation of diffusive flux using model
#' selection; and (6) combination of the diffusive and ebullitive components
#' into a total flux.
#'
#' The function operates on datasets containing multiple chamber
#' incubations, which are automatically split using the \code{UniqueID}
#' column.
#'
#' @param dataframe A \code{data.frame} containing time series observations
#'   for one or more chamber incubations.
#'
#' @param gastype Character string indicating the gas concentration column
#'   to use for flux calculation. Allowed values include:
#'   \code{"CO2dry_ppm"}, \code{"CH4dry_ppb"}, \code{"COdry_ppb"},
#'   \code{"N2Odry_ppb"}, \code{"NH3dry_ppb"}, \code{"NO2dry_ppb"},
#'   \code{"NOdry_ppb"}, and \code{"H2O_ppm"}.
#'
#' @param H2O_col Character string specifying the column containing water
#'   vapor concentration used for dilution correction. Default is
#'   \code{"H2O_ppm"}. If \code{NULL}, water vapor correction is disabled.
#'
#' @param prec Numeric scalar specifying instrument precision for the gas
#'   analyzer. If \code{NULL}, precision is retrieved from the corresponding
#'   precision column in \code{dataframe} (e.g. \code{CH4_prec}).
#'
#' @param criteria Character vector specifying model selection criteria used
#'   by \code{goFlux}. Default criteria include \code{"MAE"}, \code{"RMSE"},
#'   \code{"AICc"}, \code{"SE"}, \code{"g.factor"}, \code{"kappa"},
#'   \code{"MDF"}, \code{"nb.obs"}, \code{"intercept"}, and \code{"p-value"}.
#'
#' @param Area Numeric scalar; chamber base area in
#'   \ifelse{html}{\out{cm<sup>2</sup>}}{\eqn{cm^2}{ASCII}} (as in
#'   \code{\link[goFlux]{goFlux}}; note the internal factor of 10,000 to
#'   \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}{ASCII}}). If \code{NULL}, the
#'   value is retrieved from the \code{Area} column in \code{dataframe}.
#'
#' @param offset Numeric scalar; height between the water surface and the
#'   chamber top in cm, used to compute \code{Vtot} as
#'   \code{Vcham + Area * offset / 1000} when \code{Vtot} is not provided.
#'
#' @param Vtot Numeric scalar specifying total chamber volume (L). If
#'   \code{NULL}, it is calculated as:
#'   \deqn{Vtot = Vcham + (Area * offset) / 1000}
#'
#' @param Vcham Numeric scalar representing chamber headspace volume (L).
#'   Used only when \code{Vtot} is not provided.
#'
#' @param Pcham Numeric scalar representing chamber pressure (kPa). If
#'   \code{NULL}, atmospheric pressure (101.325 kPa) is assumed.
#'
#' @param Tcham Numeric scalar representing chamber temperature (°C).
#'   If \code{NULL}, a default temperature of 15 °C is assumed.
#'
#' @param use_bubble_detection Logical; if \code{TRUE} (default) bubbling events
#'   are detected using \code{bubble_gas} and, when \code{gastype == bubble_gas},
#'   the flux is partitioned into diffusive and ebullitive components. If
#'   \code{FALSE}, no bubble detection is performed and only a diffusive flux is
#'   returned.
#'
#' @param bubble.window.size Integer specifying the rolling window size
#'   (number of observations) used for bubble detection.
#'
#' @param bubble_gas Character string specifying the gas used to detect
#'   bubbling events. Default is \code{"CH4dry_ppb"}.
#'
#' @param bubble.method Character; dispersion metric passed to
#'   \code{\link{find.bubbles}}: \code{"mad"} (default), \code{"variance"} or
#'   \code{"diff"} (rolling variance of increments, robust to a diffusive trend).
#' @param bubble.args Named list of additional arguments forwarded to
#'   \code{\link{find.bubbles}} (e.g. \code{list(k = 5, min_magnitude = 10)}).
#'   Values here override the defaults for advanced tuning.
#'
#' @param ebullition.final_window_min Minimum time window (seconds) required
#'   after the last bubble event to define the incubation end for ebullition
#'   flux calculation.
#'
#' @param ebullition.window_C0Cf Time window (seconds) used to compute
#'   initial and final concentrations for the endpoint flux estimate.
#'
#' @param diffusion.minimum_window Minimum number of observations required
#'   to compute diffusive flux before the first bubble event.
#'
#' @param return_df Logical. If \code{TRUE} (default) the function returns a
#'   tidy list of three data frames (see \strong{Value}). If \code{FALSE}, the
#'   raw per-incubation results list is returned instead, which is convenient
#'   for advanced users who want the untidied intermediate objects.
#'
#' @return
#' If \code{return_df = TRUE} (default), a named list of three data frames:
#' \describe{
#'   \item{\code{flux_summary}}{One row per incubation, with \code{UniqueID},
#'     \code{gastype}, \code{flux_total}, \code{SE_total}, \code{flux_diffusive},
#'     \code{SE_diffusive}, \code{n_obs.diffusion}, \code{flux_ebullition},
#'     \code{SE_ebullition} and \code{first_bubble_time}.}
#'   \item{\code{bubbles}}{All detected bubbling events across incubations
#'     (\code{NULL} if none), each tagged with its \code{UniqueID}.}
#'   \item{\code{diffusive}}{The selected diffusive-model row (from
#'     \code{\link[goFlux]{best.flux}}) per incubation, tagged with
#'     \code{UniqueID}.}
#' }
#'
#' If \code{return_df = FALSE}, the raw per-incubation results list is returned.
#'
#' Flux units follow \code{\link[goFlux]{goFlux}}:
#' \ifelse{html}{\out{µmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{µmol m^{-2}s^{-1}}{ASCII}}
#' for ppm gases and
#' \ifelse{html}{\out{nmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{nmol m^{-2}s^{-1}}{ASCII}}
#' for ppb gases.
#'
#' @details
#' Bubble detection is performed using a rolling window approach applied
#' to concentration variability of the selected gas (typically CH4).
#' Detected bubbling events are used to estimate ebullition fluxes and
#' restrict the time window used for diffusive flux estimation.
#'
#' Diffusive fluxes are calculated using the \code{goFlux} framework,
#' which evaluates multiple regression models and selects the best model
#' according to user-defined criteria.
#'
#' @references
#' Rheault, K., Christiansen, J. R., & Larsen, K. S. (2024). goFlux: A
#' user-friendly way to calculate GHG fluxes yourself, regardless of user
#' experience. *Journal of Open Source Software*, 9(96), 6393. [@rheault2024]
#'
#' @include goFlux-package.R
#' @include flux.term.R
#' @include MDF.R
#' @include LM.flux.R
#' @include HM.flux.R
#' @include g.factor.R
#' @include k.max.R
#' @include find.bubbles.R
#' @include goFlux.R
#' @include goAquaFlux.ebullition.R
#' @include goAquaFlux.diffusive.R
#' @include goAquaFlux.total.R
#'
#' @seealso
#' \code{\link{find.bubbles}},
#' \code{\link{goAquaFlux.ebullition}},
#' \code{\link{goAquaFlux.diffusive}},
#' \code{\link{goAquaFlux.total}},
#' \code{\link{goFlux}}
#'
#' @examples
#' \dontrun{
#' results <- goAquaFlux(
#'   dataframe = chamber_data,
#'   gastype = "CH4dry_ppb"
#' )
#' }
#'
#' @export
#'
## NOTE: the roxygen block above documents goAquaFlux(). The `.bind_with_id`
## helper was moved to the END of this file: when it sat here (between the
## roxygen block and goAquaFlux), roxygen2 attached the documentation and
## @export to the helper instead of to goAquaFlux().
goAquaFlux <- function(dataframe,
                       gastype,
                       H2O_col = "H2O_ppm",
                       prec = NULL,
                       criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor",
                                    "kappa", "MDF", "nb.obs", "intercept", "p-value"),

                       # Auxiliary information derived from obs.win()
                       Area = NULL, offset = NULL, Vtot = NULL, Vcham = NULL,
                       Pcham = NULL, Tcham = NULL,

                       # Bubble detection
                       use_bubble_detection = TRUE,
                       bubble.window.size = 30,
                       bubble_gas = "CH4dry_ppb",
                       bubble.method = "mad",   ## "mad" (default), "variance" or "diff"; passed to find.bubbles().
                       bubble.args = list(),    ## named list of extra find.bubbles() args (e.g. list(k = 5, min_magnitude = 10)).

                       # Ebullition flux
                       ebullition.final_window_min = 30,
                       ebullition.window_C0Cf = 10,

                       # Diffusive flux
                       diffusion.minimum_window = 30,

                       # Do you want results as dataframe? Default is list.
                       return_df = TRUE) {


  # ------------------- Check arguments -------------------

  is_scalar_num <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)}

  has_col <- function(nm) {nm %in% names(dataframe)}


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




  # ------------------------------------
  # FUNCTION STARTS ####
  # ------------------------------------


  ## Clean and split data ####

  gas_cols <- unique(c(gastype, bubble_gas))

  if (gastype != "H2O_ppm"){

    # If water vapor is missing, set H2O_ppm = 0
    if (is.null(H2O_col)) {
      dataframe$H2O_ppm <- 0
      H2O_col <- "H2O_ppm"
    }

    data_split <- dataframe %>%
      rename(H2O_ppm_select = all_of(H2O_col)) %>%
      ## Keep the ORIGINAL water vapour in ppm as `H2O_ppm` as well as the
      ## mole fraction `H2O_mol`. goAquaFlux uses H2O_mol for its own flux.term,
      ## while goAquaFlux.diffusive() passes H2O_ppm to goFlux() (which does its
      ## own ppm -> mole-fraction conversion). Without this, goFlux would divide
      ## an already-converted H2O_mol by 1e6 a second time, silently disabling
      ## the water-vapour dilution correction.
      mutate(H2O_mol = H2O_ppm_select / (1000*1000),
             H2O_ppm = H2O_ppm_select) %>%
      select(
        UniqueID,
        any_of(c("chamID","DATE")),
        Etime,
        flag,
        any_of(gas_cols),              # <-- keep both gases
        contains("_prec"),
        H2O_mol, H2O_ppm,
        Vtot, Area, Pcham, Tcham
      ) %>%
      filter(flag == 1) %>%
      tidyr::drop_na(all_of(gastype)) %>%
      tidyr::drop_na(Etime) %>%
      tidyr::drop_na(UniqueID) %>%
      group_by(UniqueID) %>%
      group_split() %>%
      as.list()
  }


  if (gastype == "H2O_ppm"){

    data_split <- dataframe %>%
      select(
        UniqueID,
        any_of(c("chamID","DATE")),
        Etime,
        flag,
        any_of(gas_cols),              # <-- keep both gases
        contains("_prec"),
        Vtot, Area, Pcham, Tcham
      ) %>%
      filter(flag == 1) %>%
      tidyr::drop_na(all_of(gastype)) %>%
      tidyr::drop_na(Etime) %>%
      tidyr::drop_na(UniqueID) %>%
      group_by(UniqueID) %>%
      group_split() %>%
      as.list()
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
    if (is.na(data_split[[f]]$Etime[1])) {
      stop("Invalid Etime origin: the first row with ",
           "flag == 1 is NA. Problem detected for UniqueID: ",
           data_split[[f]]$UniqueID[1], ".", call. = FALSE)}
    if(data_split[[f]]$Etime[1] != 0){
      warning("Etime origin is not 0 but ",data_split[[f]]$Etime[1],". Resetting origin")
      # making sure Etime starts at 0
      Etimestart = min(data_split[[f]]$Etime)
      data_split[[f]]$Etime <- data_split[[f]]$Etime - Etimestart
    }

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



  # -------------------------------------------------
  # ---------- FLUX CALCULATION
  # -------------------------------------------------


  # Create an empty list to store results
  flux.res.ls <- list()

  # Print a progress bar
  pb = txtProgressBar(min = 0, max = length(data_split), initial = 0, style = 3)


  # ---------- Loop through incubations
  for (f in seq_along(data_split)){

    df <- data_split[[f]]

    # Extract auxiliary variables
    UniqueID <- data_split[[f]]$UniqueID[1]
    flux.term_f <- data_split[[f]]$flux_term[1]
    MDF <- data_split[[f]]$MDF[1]
    nb.obs <- length(data_split[[f]][[gastype]])
    prec_f <- data_split[[f]]$prec_f[1]


    # ----------------------------
    # 1. Check if ebullition needs to be computed
    # ----------------------------

    if(use_bubble_detection){
      if(bubble_gas %in% names(df)){
        compute_ebullition <- TRUE
      } else {
        compute_ebullition <- FALSE
        warning(paste0("Cannot compute ebullition because ",bubble_gas, " doesn't appear in provided dataframe"))
      }
    } else {
      compute_ebullition <- FALSE
    }

    # ----------------------------
    # 2. Detect bubbles if possible
    # ----------------------------

    if (compute_ebullition && bubble_gas %in% names(df)) {

      ## forward the detection method and any extra tuning args, while
      ## keeping simple defaults for beginners. `bubble.args` wins on conflicts.
      .bubble_call <- utils::modifyList(
        list(df = df, bubble_source = bubble_gas,
             window.size = bubble.window.size, method = bubble.method),
        bubble.args)
      bubbles <- do.call(find.bubbles, .bubble_call)

    } else {

      bubbles <- NULL

    }

    # ----------------------------
    # 5. Ebullition flux
    # ----------------------------

    # we compute ebullition only if possible and if gastype = bubble_gas
    if (bubble_gas == gastype) {

      ebullition_flux <- goAquaFlux.ebullition(
        df = df,
        gastype = gastype,
        bubbles = bubbles,
        flux.term = flux.term_f,
        final_window.min = ebullition.final_window_min,
        window_C0Cf = ebullition.window_C0Cf
      )

    } else {

      ## Use the same field names the function actually returns
      ## (flag_inconsistent + message), so the ebullition object has a
      ## consistent shape whether or not it was computed.
      ebullition_flux <- list(
        flux = NA_real_,
        SE = NA_real_,
        F_tot2pts = NA_real_,
        F_tot2pts.SE = NA_real_,
        n_bubbles = NA_integer_,
        deltaC_bubbles = NA_real_,
        deltaC_total = NA_real_,
        bubble_ratio = NA_real_,
        flag_inconsistent = NA,
        message = "Ebullition not computed (gastype is not the bubble gas)"
      )
    }

    # ----------------------------
    # 4. Diffusive flux (restricted by CH4 bubbling if available)
    # ----------------------------

    diffusive_flux <- goAquaFlux.diffusive(
      df = df,
      gastype = gastype,
      criteria = criteria,
      bubble_gas = bubble_gas,   ## Needed so the diffusive window is
                                 ## truncated correctly for the bubble gas and,
                                 ## for other gases, only on an abrupt change.
      bubbles = bubbles,
      minimum_window = diffusion.minimum_window
    )


    # ----------------------------
    # 3. Total flux
    # ----------------------------
    # we compute total flux only if ebullition was computed. If not, total = diffusion

    if (bubble_gas == gastype) {

      total_flux <- goAquaFlux.total(
        ebullition_flux = ebullition_flux,
        diffusive_flux = diffusive_flux
      )

    } else {

      total_flux <- list(
        flux = diffusive_flux$flux,
        SE = diffusive_flux$SE,
        ratio = NA,
        flag_suspicious = FALSE,
        message = NA
      )

    }


    # ---- combine outputs ----

    flux_summary <- data.frame(
      UniqueID = df$UniqueID[1],
      gastype = gastype,

      flux_total = total_flux$flux,
      SE_total = total_flux$SE,

      flux_diffusive = diffusive_flux$flux,
      SE_diffusive = diffusive_flux$SE,
      n_obs.diffusion = diffusive_flux$n_used,

      flux_ebullition = ebullition_flux$flux,
      SE_ebullition = ebullition_flux$SE,

      first_bubble_time = diffusive_flux$first_bubble_time
    )

    flux.res.ls[[f]] <- list(
      flux_summary = flux_summary,
      bubbles = bubbles,
      best.diffusive.flux = diffusive_flux$best.flux.output
    )

  }

  ## Close the progress bar opened above.
  close(pb)

  ## When FALSE, return the raw per-incubation results list (advanced use).
  if (!isTRUE(return_df)) return(flux.res.ls)

  # reorganize flux.res.ls by UniqueID
  df_flux_summary <- .bind_with_id(flux.res.ls, "flux_summary")

  df_bubbles <- .bind_with_id(flux.res.ls, "bubbles")

  df_diffusive <- .bind_with_id(flux.res.ls, "best.diffusive.flux")

  # order by UniqueID
  df_flux_summary <- df_flux_summary[order(df_flux_summary$UniqueID), ]
  if(!is.null(df_bubbles)){
    df_bubbles <- df_bubbles[order(df_bubbles$UniqueID), ]
    }

  if(!is.null(df_diffusive)){
    df_diffusive <- df_diffusive[order(df_diffusive$UniqueID), ]
    }

  return(list(
    flux_summary = df_flux_summary,
    bubbles = df_bubbles,
    diffusive = df_diffusive
  ))
}




# --- Internal helper: bind a named element across the per-incubation results --
# For each incubation in `lst`, extract `element_name` (a data.frame), tag it
# with the incubation's UniqueID (taken from its flux_summary when the element
# itself lacks one, e.g. the bubbles table), and row-bind everything together.
# Returns NULL if no incubation contributed rows.
.bind_with_id <- function(lst, element_name) {
  do.call(rbind, lapply(seq_along(lst), function(i) {

    x <- lst[[i]][[element_name]]

    # Skip NULL or empty elements safely
    if (is.null(x) || nrow(x) == 0) return(NULL)

    x <- as.data.frame(x)

    # Add UniqueID if missing (e.g. the bubbles data.frame has no UniqueID)
    if (!"UniqueID" %in% names(x)) {
      if ("flux_summary" %in% names(lst[[i]])) {
        x$UniqueID <- lst[[i]]$flux_summary$UniqueID
      } else {
        x$UniqueID <- paste0("ID_", i)
      }
    }

    return(x)
  }))
}
