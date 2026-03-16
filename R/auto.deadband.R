#' Automatic selection of deadband per measurement
#'
#' Loops through multiple gas measurements and automatically define a suitable
#' deadband per measurement.
#'
#' @param dataframe data.frame; output from import or ID functions.
#' @param gastype character string; specifies which gas column should be used to
#'                detect inflection point. Must be one of the following:
#'                "CO2dry_ppm", "CH4dry_ppb", "COdry_ppb", "N2Odry_ppb",
#'                "NH3dry_ppb", "NOdry_ppb", "NO2dry_ppb" or "H2O_ppm".
#' @param db.min numerical value (seconds); minimal deadband per measurement.
#' @param db.max numerical value (seconds); maximal deadband per measurement.
#' @param obs.win numerical value (seconds); maximal observation window to
#'                include in the linear models. If possible, keept at least 30
#'                seconds between \code{db.max} and \code{obs.win} for more
#'                accurate results.
#' @param mov.win numerical value (seconds); time increment between
#'                \code{obs.win}, from chamber closure to \code{db.max}.
#' @param min.r2 numerical value; minimum increase in
#'               \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^{2}}{ASCII}} required
#'               to accept dropping more data points.
#'
#' @returns A data.frame of the automatically selected \code{deadband} per
#'          \code{UniqueID}.
#'
#' @details
#' For each measurement, a sequence of observation window is defined from
#' chamber closure (+ \code{mov.win}) to \code{obs.win}. For each observation
#' window, a linear model is calculated, from which the
#' \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^{2}}{ASCII}} is extracted. The
#' 'best' deadband is then selected at the highest
#' \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^{2}}{ASCII}}.
#'
#' In \code{db.min} and \code{db.max}, negative values are accepted. This is
#' especially applicable in cases where \code{start.time} already includes a
#' deadband after chamber closure, and \code{cham.close} is not provided.
#'
#' Note that, \code{obs.win} should represent the most linear part of the
#' measurement, at the start of the measurement. For long chamber enclosures
#' (more than a few minutes) select a longer \code{obs.win} more representative
#' of your measurements. The default value of \code{obs.win = 90s} is based on
#' previous observations that the most linear part of a measurement is often
#' found within a short period (<2 minutes) at the start of the measurement, for
#' small chamber sizes (~4L) (Johannesson et al., 2024). Similarly, the
#' argument \code{mov.win} could be increased for very long chamber enclosures.
#' Using a short \code{mov.win} (e.g., 1s) increases computational time, and is
#' not recommended for very large data sets. The default value of
#' \code{db.max = 60} is based on dark chamber measurements on, e.g., soils.
#' For Reco, a longer \code{db.max} (e.g., 90 seconds) is recommended to take
#' into account a recovery phase of residual photosynthesis at the start of the
#' measurement.
#'
#' \code{min.r2} can be used to restrict longer deadbands (or minimize dropping
#' data points). For example, if an \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^{2}}{ASCII}}
#' of 0.990 is found with a deadband of 30 seconds, and an
#' \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^{2}}{ASCII}} of 0.992 with a
#' deadband of 40 seconds (considering \code{mov.win = 10}), the best deadband
#' is defined at 30 seconds if \code{min.r2 = 0.005}. By default, the highest
#' \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^{2}}{ASCII}} selects the best
#' deadband (\code{min.r2 = 0}), but this may result in unnecessary loss of
#' data points.
#'
#' In \code{gastype}, the gas species listed are the ones for which this package
#' has been adapted. Please write to the maintainer of this package for
#' adaptation of additional gases.
#'
#' @references Johannesson, C. F., Nordén, J., Lange, H., Silvennoinen, H., &
#' Larsen, K. S. (2024). Optimizing the closure period for improved accuracy of
#' chamber-based greenhouse gas flux estimates.
#' \emph{Agricultural and Forest Meteorology}, 359, 110289.
#'
#' @include goFlux-package.R
#'
#' @seealso \code{\link[goFlux]{crop.meas}} to correct measurements based on
#' this newly defined deadband per measurement.
#'
#' @examples
#' data("manID.UGGA")
#' auto_db <- auto.deadband(manID.UGGA, "CO2dry_ppm")
#' crop_UGGA <- crop.meas(manID.UGGA, auto_db, deadband = "aux")
#'
#' @export
#'
auto.deadband <- function(dataframe, gastype, db.min = 0, db.max = 60,
                          obs.win = 90, mov.win = 10, min.r2 = 0){

  # Check arguments ####
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
    dataframe$UniqueID <- paste(
      as.character(dataframe$chamID), as.character(dataframe$DATE), sep = "_")}

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

  ### POSIX.time ####
  if (!has_col("POSIX.time")) {
    stop("'dataframe' must contain the column 'POSIX.time'. ", call. = FALSE)}
  if(!lubridate::is.POSIXct(dataframe$POSIX.time)){
    stop("'POSIX.time' in 'dataframe' must be of class POSIXct", call. = FALSE)}
  if (all(is.na(dataframe$POSIX.time))) {
    stop("'POSIX.time' in 'dataframe' contains all NAs", call. = FALSE)}

  ### start.time or cham.close ####
  if (!has_col("start.time") && !has_col("cham.close")) {
    stop("'dataframe' must contain the column 'cham.close'. ",
         "Alternatively, provide the column 'start.time'.", call. = FALSE)}

  if (has_col("start.time")) {
    if(!lubridate::is.POSIXct(dataframe$start.time)){
      stop("'start.time' in 'dataframe' must be of class POSIXct", call. = FALSE)
    } else {
      if(attr(dataframe$start.time, "tzone") != attr(dataframe$POSIX.time, "tzone")) {
        stop("'start.time' in 'dataframe' must be in the same time zone as ",
             "'POSIX.time'", call. = FALSE)}
    }
  }
  if (has_col("cham.close")) {
    if(!lubridate::is.POSIXct(dataframe$cham.close)){
      stop("'cham.close' in 'dataframe' must be of class POSIXct", call. = FALSE)
    } else {
      if(attr(dataframe$cham.close, "tzone") != attr(dataframe$POSIX.time, "tzone")) {
        stop("'cham.close' in 'dataframe' must be in the same time zone as ",
             "'POSIX.time'", call. = FALSE)}
    }
  }

  # Define start.time as cham.close if cham.close is missing.
  if (!has_col("cham.close")) dataframe$cham.close <- dataframe$start.time

  ## db.min & db.max & mov.win & obs.win ####
  if (!is_scalar_num(obs.win)) {
    stop("'obs.win' must be a finite numeric scalar.", call. = FALSE)}
  if (!is_scalar_num(mov.win)) {
    stop("'mov.win' must be a finite numeric scalar.", call. = FALSE)}
  if (!is_scalar_num(db.min)) {
    stop("'db.min' must be a finite numeric scalar.", call. = FALSE)}
  if (!is_scalar_num(db.max)) {
    stop("'db.max' must be a finite numeric scalar.", call. = FALSE)}

  if (obs.win <= 0) {
    stop("'obs.win' must be > 0.", call. = FALSE)}

  if (mov.win <= 0) {
    stop("'mov.win' must be > 0.", call. = FALSE)}
  if (mov.win > (db.max-db.min)/2) {
    stop("'mov.win' must be < '(db.max-db.min)/2' to allow for at least two ",
         "tested deadband", call. = FALSE)}

  if (db.min >= db.max-mov.win) {
    stop("'db.min' must be < 'db.max - mov.win' to allow for at least two ",
         "tested deadband", call. = FALSE)}
  if (db.min >= db.max) {
    stop("'db.min' must be < 'db.max'", call. = FALSE)}

  if (db.max <= db.min+mov.win) {
    stop("'db.max' must be > 'db.min + mov.win' to allow for at least two ",
         "tested deadband", call. = FALSE)}
  if (db.max >= obs.win) {
    stop("'db.max' must be < 'obs.win'. If possible, keep at least 30 seconds ",
         "between 'db.max' and 'obs.win'.", call. = FALSE)}

  ## min.r2 ####
  if (!is_scalar_num(min.r2)) {
    stop("'min.r2' must be a finite numeric scalar.", call. = FALSE)}
  if (min.r2 < 0 || min.r2 > 1) {
    stop("'min.r2' must be contained between 0 and 1.", call. = FALSE)}

  # auto.deadband function per measurement
  auto.deadband_internal <- function(dat,
                                     db.min = db.min, db.max = db.max,
                                     obs.win = obs.win, mov.win = mov.win,
                                     min.r2 = min.r2){

    # Create a sequence of observation window from chamber closed in Xs intervals (mov.win)
    db.seq <- seq(db.min, db.max, mov.win)

    # Do LM for each and make a vector of all the R2 from all linear regressions
    for(p in 1:length(db.seq)){
      dat2 <- dat %>% filter(time.meas >= db.seq[p])
      if (nrow(dat2) < 3) {
        stop("Too few data points to perform LM regression. Problem found in ",
             "UniqueID: '", unique(dat$UniqueID), "' for deadband ",
             db.seq[p], " and higher.", call. = FALSE)}

      LM <- lm(dat2$gas.meas ~ dat2$time.meas)
      if (p == 1) R2_db <- cbind.data.frame(deadband = db.seq[p],
                                            r2 = summary(LM)$r.squared)
      if (p > 1) R2_db <- rbind(
        R2_db, cbind.data.frame(deadband = db.seq[p],
                                r2 = summary(LM)$r.squared))
    }
    R2_db$inc <- c(0, diff(R2_db$r2))

    # Filter all deadband with an improvement in R2 larger than min.r2
    step_r2 <- R2_db %>%
      mutate(keep = (deadband == db.min) | (inc >= min.r2)) %>%
      filter(keep)

    # Select the shortest deadband with the highest R2
    auto_db <- step_r2$deadband[which.max(step_r2$r2)]

    return(auto_db)
  }

  # Assign NULL to variables without binding ####
  deadband <- inc <- keep <- cham.close <- UniqueID <- POSIX.time <-
    Etime <- time.meas <- NULL

  # FUNCTION STARTS ####

  # Create a list of dataframe split by UniqueID
  data_split <- dataframe %>%
    select(UniqueID, cham.close, POSIX.time, all_of(gastype)) %>%
    # Remove NAs
    tidyr::drop_na(all_of(gastype)) %>%
    tidyr::drop_na(UniqueID) %>%
    # Split dataset by UniqueID
    group_by(UniqueID) %>% group_split() %>% as.list()

  # Ensure data_split is not empty
  if (length(data_split) == 0L) {
    stop("No valid observations after removing NAs in '",
         gastype, "'.", call. = FALSE)}

  # Ensure POSIX.time is ordered within each UniqueID
  data_split <- lapply(data_split, function(df) {df %>% arrange(POSIX.time)})

  ## Estimate best deadband per UniqueID ####
  auto_db <- data.frame(UniqueID=character(), deadband=numeric())
  for (f in 1:length(data_split)){

    # Define Etime == 0 at 'cham.close'
    df <- data_split[[f]] %>%
      mutate(Etime = as.numeric(POSIX.time - cham.close, units = "secs")) %>%
      # Filter NAs
      filter(!is.na(Etime)) %>%
      # Keep only data point below obs.win
      filter(Etime <= obs.win)

    # Extract gas measurements and time stamps
    UniqueID_f <- unique(df$UniqueID)
    time.meas_f <- df[["Etime"]]
    gas.meas_f <- df[[gastype]]

    # Construct data frame with UniqueID, time.meas and gas.meas
    dat <- data.frame(UniqueID = UniqueID_f,
                      time.meas = time.meas_f,
                      gas.meas = gas.meas_f)

    # Find auto deadband
    auto_db <- rbind(
      auto_db,
      cbind.data.frame(
        UniqueID = UniqueID_f,
        deadband = auto.deadband_internal(dat,
                                          db.min = db.min, db.max = db.max,
                                          obs.win = obs.win, mov.win = mov.win,
                                          min.r2 = min.r2)))
  }
  return(auto_db)
}
