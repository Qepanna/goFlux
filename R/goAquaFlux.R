#' Compute gas fluxes from aquatic chamber measurements
#'
#' \code{goAquaFlux} computes gas fluxes from chamber-based aquatic
#' incubations. The function partitions total flux into diffusive and
#' ebullitive components when bubbling events are detected. Bubbling
#' events are identified using high-frequency concentration variability
#' (typically CH4), after which the time series is separated into
#' diffusive and ebullitive segments.
#'
#' The workflow includes:
#' \enumerate{
#'   \item Data validation and cleaning
#'   \item Calculation of auxiliary variables (flux conversion term and MDF)
#'   \item Bubble detection (typically using CH4 concentration)
#'   \item Estimation of ebullition flux based on detected bubble magnitudes
#'   \item Estimation of diffusive flux using model selection
#'   \item Combination of diffusive and ebullitive components into total flux
#' }
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
#' @param Area Numeric scalar representing chamber base area (m²). If
#'   \code{NULL}, the value is retrieved from the \code{Area} column in
#'   \code{dataframe}.
#'
#' @param offset Numeric scalar representing the vertical chamber offset
#'   (mm) used to compute total chamber volume when \code{Vtot} is not
#'   directly provided.
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
#' @param bubble.window.size Integer specifying the rolling window size
#'   (number of observations) used for bubble detection.
#'
#' @param bubble_gas Character string specifying the gas used to detect
#'   bubbling events. Default is \code{"CH4dry_ppb"}.
#'
#' @param ebullition.final_window_min Minimum time window (minutes) required
#'   after the last bubble event to define the incubation end for ebullition
#'   flux calculation.
#'
#' @param ebullition.window_C0Cf Time window (minutes) used to compute
#'   initial and final concentrations for the endpoint flux estimate.
#'
#' @param diffusion.minimum_window Minimum number of observations required
#'   to compute diffusive flux before the first bubble event.
#'
#' @param return_df Logical indicating whether results should be returned
#'   as a single \code{data.frame}. If \code{FALSE}, results are returned
#'   as a list of data.frames (one per incubation).
#'
#' @return
#' If \code{return_df = TRUE}, a data frame containing one row per
#' incubation with the following columns:
#' \itemize{
#'   \item \code{UniqueID} – incubation identifier
#'   \item \code{flux_total} – total gas flux
#'   \item \code{SE_total} – standard error of total flux
#'   \item \code{flux_diffusive} – diffusive flux component
#'   \item \code{SE_diffusive} – standard error of diffusive flux
#'   \item \code{flux_ebullition} – ebullition flux component
#'   \item \code{SE_ebullition} – standard error of ebullition flux
#'   \item \code{first_bubble_time} – time of first detected bubbling event
#' }
#'
#' If \code{return_df = FALSE}, a list containing one data frame per
#' incubation is returned.
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
      mutate(H2O_mol = H2O_ppm_select / (1000*1000)) %>%
      select(
        UniqueID,
        any_of(c("chamID","DATE")),
        Etime,
        flag,
        any_of(gas_cols),              # <-- keep both gases
        contains("_prec"),
        H2O_mol,
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



  # -------------------------------------------------
  # ---------- FLUX CALCULATION
  # -------------------------------------------------

  # check if ebullition needs to be computed
  compute_ebullition <- !is.null(bubble_gas) && gastype == bubble_gas
  if (gastype != bubble_gas) {
    message("Ebullition not computed because gastype != bubble_gas.")
  }

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
    # 1. Determine which gas to use for bubble detection
    # ----------------------------

    bubble_source <- NA

    if (gastype == bubble_gas) {

      # CH4 flux calculation
      bubble_source <- gastype

    } else if (bubble_gas %in% names(df)) {

      # Non-CH4 flux but bubble_gas (e.g. CH4) available for bubble detection
      bubble_source <- bubble_gas
    }


    # ----------------------------
    # 2. Detect bubbles if possible
    # ----------------------------

    if (use_bubble_detection && compute_ebullition && bubble_gas %in% names(df)) {

      bubbles <- find.bubbles(
        df = df,
        bubble_source = bubble_gas,
        window.size = bubble.window.size
      )

    } else {

      bubbles <- NULL

    }


    # ----------------------------
    # 5. Ebullition flux
    # ----------------------------

    # we compute ebullition only if possible and if gastype = bubble_gas
    if (compute_ebullition) {

      ebullition_flux <- goAquaFlux.ebullition(
        df = df,
        gastype = gastype,
        bubbles = bubbles,
        flux.term = flux.term_f,
        final_window.min = ebullition.final_window_min,
        window_C0Cf = ebullition.window_C0Cf
      )

    } else {

      ebullition_flux <- list(
        flux = NA,
        SE = NA,
        F_tot2pts = NA,
        F_tot2pts.SE = NA,
        n_bubbles = NA,
        deltaC_bubbles = NA,
        deltaC_total = NA,
        bubble_ratio = NA,
        inconsistent = NA
      )
    }

    # ----------------------------
    # 4. Diffusive flux (restricted by CH4 bubbling if available)
    # ----------------------------

    diffusive_flux <- goAquaFlux.diffusive(
      df = df,
      gastype = gastype,
      criteria = criteria,
      bubbles = bubbles,
      minimum_window = diffusion.minimum_window
    )


    # ----------------------------
    # 3. Total flux
    # ----------------------------
    # we compute total flux only if ebullition was computed. If not, total = diffusion

    if (compute_ebullition) {

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
    flux.res.ls[[f]] <- data.frame(
      gastype = gastype,
      UniqueID = df$UniqueID[1],

      flux_total = total_flux$flux,
      SE_total = total_flux$SE,

      flux_diffusive = diffusive_flux$flux,
      SE_diffusive = diffusive_flux$SE,
      n_obs.diffusion = diffusive_flux$n_used,

      flux_ebullition = ebullition_flux$flux,
      SE_ebullition = ebullition_flux$SE,

      first_bubble_time = diffusive_flux$first_bubble_time
    )

  }

  # -------------------------------------------------
  # 6. Return format
  # -------------------------------------------------

  if (return_df) {
    return(dplyr::bind_rows(flux.res.ls))
  } else {
    return(flux.res.ls)
  }
}


