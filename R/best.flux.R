#' Automatic selection of best flux estimate
#'
#' This automatic selection of the best flux estimate (linear or non-linear) is
#' based on objective criteria and non-arbitrary thresholds.
#'
#' @param flux.result data.frame; output from the function \code{\link[GoFluxYourself]{goFlux}}.
#' @param criteria character vector; criteria used to asses the goodness of fit of the
#'                 linear and non-linear flux estimates. Must be at least one the following:
#'                 \code{criteria = c("MAE", "RMSE", "g.factor", "kappa", "MDF",
#'                 "nb.obs", "p-value", "intercept", "SErel")}. "MAE" and "RMSE"
#'                 cannot both be selected. RMSE is more sensitive to outliers
#'                 than MAE. Select RMSE to identify more measurements with small
#'                 deviations (a warning is given in the column \code{quality.check}
#'                 saying "Noisy measurement").
#' @param intercept.lim numerical vector of length 2; inferior and superior
#'                      limits of the intercept (initial concentration;
#'                      \ifelse{html}{\out{C<sub>0</sub>}}{\eqn{C[0]}{ASCII}}).
#'                      Must be the same units as \code{gastype} in the
#'                      \code{\link[GoFluxYourself]{goFlux}} function. If
#'                      \code{intercept.lim = NULL}, the limits are calculated
#'                      from the true values of
#'                      \ifelse{html}{\out{C<sub>0</sub>}}{\eqn{C[0]}{ASCII}} and
#'                      \ifelse{html}{\out{C<sub>i</sub>}}{\eqn{C[i]}{ASCII}}
#'                      for each measurement.
#' @param SErel numerical value; maximal relative standard error accepted (\%)
#'               on flux estimate. The default is 5\%.
#' @param g.limit numerical value; maximal limit of the the g-factor, which is the
#'                ratio between the non-linear flux estimate
#'                (\code{\link[GoFluxYourself]{HM.flux}}) and the linear flux
#'                estimate (\code{\link[GoFluxYourself]{LM.flux}}). Recommended
#'                thresholds for the g-factor are 4 (flexible), 2 (medium), or
#'                1.25 (conservative). The default limit is \code{g.limit = 2}.
#' @param k.ratio numerical value; maximal limit of the ratio between kappa and
#'                the kappa-max. kappa-max is the maximal curvature (kappa)
#'                of the non-linear regression (Hutchinson and Mosier model)
#'                allowed for a each flux measurements. With \code{k.ratio}, one
#'                can chose to select the LM flux estimate instead of the
#'                non-linear flux estimate by giving a percentage of kappa-max.
#'                Default is \code{k.ratio = 1}.
#' @param p.val numerical value; a limit for a statistically non-zero flux.
#'              The default threshold is p-value < 0.05.
#' @param warn.length numerical; minimum amount of observations accepted (nb.obs).
#'                    With nowadays portable greenhouse gas analyzers, the
#'                    frequency of measurement is usually one measurement per
#'                    second. Therefore, for a default setting of
#'                    \code{warn.length = 60}, the chamber closure time
#'                    should be approximately one minute (60 seconds).
#'
#' @details
#' The function \code{\link[GoFluxYourself]{k.max}} calculates the maximal
#' curvature (kappa) of the non-linear model (Hutchinson and Mosier) allowed for
#' each flux measurements. k.max is calculated based on the minimal detectable
#' flux (MDF), the linear flux estimate (LM.flux) and the measurement time. The
#' unit of kappa-max is \ifelse{html}{\out{s<sup>-1</sup>}}{\eqn{s^{-1}}{ASCII}}.
#'
#' The function \code{\link[GoFluxYourself]{MDF}} calculates the minimal
#' detectable flux (MDF) based on instrument precision and measurements time.
#'
#' Another criteria, which is calculated automatically with the function
#' \code{\link[GoFluxYourself]{best.flux}}, is the g-factor
#' (\code{\link[GoFluxYourself]{g.factor}}), which is the ratio between the HM
#' flux estimate and the LM flux estimate. The threshold for the g-factor
#' should be 4 (flexible), 2 (medium), or 1.25 (conservative).
#'
#' @returns a data.frame identical to the input \code{flux.result} with the
#'          additional columns \code{HM.diagnose}, \code{LM.diagnose},
#'          \code{best.flux}, \code{model} and \code{quality.check}.
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' data(LGR_manID)
#' LGR_flux <- goFlux(LGR_manID, "CO2dry_ppm")
#' criteria <- c("MAE", "g.factor", "MDF", "SErel")
#' LGR_res <- best.flux(LGR_flux, criteria)
#'
#' @seealso Look up the functions \code{\link[GoFluxYourself]{MDF}},
#'          \code{\link[GoFluxYourself]{flux.term}},
#'          \code{\link[GoFluxYourself]{g.factor}},
#'          \code{\link[GoFluxYourself]{k.max}},
#'          \code{\link[GoFluxYourself]{HM.flux}} and
#'          \code{\link[GoFluxYourself]{LM.flux}}
#'          of this package for more information about these parameters.
#' @seealso See also the function \code{\link[GoFluxYourself]{goFlux}}
#'          for more information about usage.
#'
#' @export
#'
best.flux <- function(flux.result,
                      criteria = c("MAE", "g.factor", "kappa", "MDF", "nb.obs",
                                   "intercept", "SErel"),
                      intercept.lim = NULL, SErel = 5, g.limit = 2,
                      p.val = 0.05, k.ratio = 1, warn.length = 60) {

  # Check arguments ####
  if(missing(flux.result)) stop("'flux.result' is required")
  if(!is.null(flux.result) & !is.data.frame(flux.result)){
    stop("'flux.result' must be of class 'dataframe'")}
  if(!any(grepl(paste(c("\\<MAE\\>", "\\<RMSE\\>", "\\<g.factor\\>", "\\<kappa\\>",
                        "\\<MDF\\>", "\\<nb.obs\\>", "\\<p-value\\>",
                        "intercept", "SErel"), collapse = "|"), criteria))){
    stop("'criteria' must contain at least one of the following: 'MAE', 'RMSE', 'g.factor', 'kappa', 'MDF', 'nb.obs', 'p-value', 'intercept', 'SErel'")}

  ## Check intercept.lim ####
  if(any(grepl("\\<intercept\\>", criteria))){
    # Required columns in flux.result
    intercept.require <- c("\\<HM.C0\\>", "\\<LM.C0\\>")
    if(length(grep(paste(intercept.require, collapse = "|"), names(flux.result))) != 2){
      if(!any(grepl("\\<HM.C0\\>", names(flux.result)))){
        stop("'HM.C0' required in 'flux.result'")
      } else if(!is.numeric(flux.result$HM.C0[[1]])){
        stop("'HM.C0' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<LM.C0\\>", names(flux.result)))){
        stop("'LM.C0' required in 'flux.result'")
      } else if(!is.numeric(flux.result$LM.C0[[1]])){
        stop("'LM.C0' in 'flux.result' must be of class numeric")}
    }
    # Requirements with intercept.lim
    if(!is.null(intercept.lim)){
      if(length(intercept.lim) != 2) stop("'intercept.lim' must have length = 2")
      if(!is.numeric(intercept.lim)) stop("'intercept.lim' must be of class numeric")
    } else {
      if(!any(grepl("\\<C0\\>", names(flux.result)))){
        stop("'C0' required in 'flux.result'")
      } else if(!is.numeric(flux.result$C0[[1]])){
        stop("'C0' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<Ct\\>", names(flux.result)))){
        stop("'Ct' required in 'flux.result'")
      } else if(!is.numeric(flux.result$Ct[[1]])){
        stop("'Ct' in 'flux.result' must be of class numeric")}
    }
  }
  ## Check SErel ####
  if(any(grepl("\\<SErel\\>", criteria)) & !is.null(SErel)){
    if(!is.numeric(SErel)) stop("'SErel' must be of class numeric")
    if(is.numeric(SErel) & !between(SErel, 0, 100)) stop("'SErel' must be between 0% and 100%")
    if(is.numeric(SErel) & SErel <= 0) stop("'SErel' must be higher than 0%")

    SErel.require <- c("\\<LM.se.rel\\>", "\\<HM.se.rel\\>")
    if(length(grep(paste(SErel.require, collapse = "|"), names(flux.result))) != 2){
      if(!any(grepl("\\<LM.se.rel\\>", names(flux.result)))){
        stop("'LM.se.rel' required in 'flux.result'")}
      if(any(grepl("\\<LM.se.rel\\>", names(flux.result))) & !is.numeric(flux.result$LM.se.rel)){
        stop("'LM.se.rel' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<HM.se.rel\\>", names(flux.result)))){
        stop("'HM.se.rel' required in 'flux.result'")}
      if(any(grepl("\\<HM.se.rel\\>", names(flux.result))) & !is.numeric(flux.result$HM.se.rel)){
        stop("'HM.se.rel' in 'flux.result' must be of class numeric")}
    }
  } else if(any(grepl("\\<SErel\\>", criteria)) & is.null(SErel)){
    stop("'SErel' is mentionned in 'criteria', but the argument 'SErel' is NULL")}

  ## Check g.factor and g.limit ####
  if(any(grepl("\\<g.factor\\>", criteria)) & !is.null(g.limit)){
    if(!is.numeric(g.limit)) stop("'g.limit' must be of class numeric")
    if(is.numeric(g.limit) & !between(g.limit, 1, 100)) {
      stop("Recommended thresholds for 'g.limit' are 4 (flexible), 2 (medium), or 1.25 (conservative). 'g.limit' cannot exceed 100 or be smaller than 1.")}

    g.limit.require <- c("\\<g.fact\\>", "\\<LM.flux\\>")
    if(length(grep(paste(g.limit.require, collapse = "|"), names(flux.result))) != 2){
      if(!any(grepl("\\<g.fact\\>", names(flux.result)))){
        stop("'g.fact' required in 'flux.result'")}
      if(any(grepl("\\<g.fact\\>", names(flux.result))) & !is.numeric(flux.result$g.fact)){
        stop("'g.fact' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<LM.flux\\>", names(flux.result)))){
        stop("'LM.flux' required in 'flux.result'")}
      if(any(grepl("\\<LM.flux\\>", names(flux.result))) & !is.numeric(flux.result$LM.flux)){
        stop("'LM.flux' in 'flux.result' must be of class numeric")}
    }
  } else if(any(grepl("\\<g.factor\\>", criteria)) & is.null(g.limit)){
    stop("'g.factor' is mentionned in 'criteria', but the argument 'g.limit' is NULL")}

  ## Check p-value ####
  if(any(grepl("\\<p-value\\>", criteria)) & !is.null(p.val)){
    if(!is.numeric(p.val)) stop("'p.val' must be of class numeric")
    if(is.numeric(p.val) & !between(p.val, 0, 1)) stop("'p.val' must be between 0 and 1")

    p.val.require <- c("\\<LM.p.val\\>", "\\<LM.flux\\>")
    if(length(grep(paste(p.val.require, collapse = "|"), names(flux.result))) != 2){
      if(!any(grepl("\\<LM.p.val\\>", names(flux.result)))){
        stop("'LM.p.val' required in 'flux.result'")}
      if(any(grepl("\\<LM.p.val\\>", names(flux.result))) & !is.numeric(flux.result$LM.p.val)){
        stop("'LM.p.val' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<LM.flux\\>", names(flux.result)))){
        stop("'LM.flux' required in 'flux.result'")}
      if(any(grepl("\\<LM.flux\\>", names(flux.result))) & !is.numeric(flux.result$LM.flux)){
        stop("'LM.flux' in 'flux.result' must be of class numeric")}
    }
  } else if(any(grepl("\\<p-value\\>", criteria)) & is.null(p.val)){
    stop("'p-value' is mentionned in 'criteria', but the argument 'p.val' is NULL")}

  ## Check kappa and k.ratio ####
  if(any(grepl("\\<kappa\\>", criteria)) & !is.null(k.ratio)){
    if(!is.numeric(k.ratio)) stop("'k.ratio' must be of class numeric")
    if(is.numeric(k.ratio) & !between(k.ratio, 0, 100)) stop("'k.ratio' must be between 0% and 100%")

    k.ratio.require <- c("\\<k.max\\>", "\\<HM.k\\>", "\\<LM.flux\\>")
    if(length(grep(paste(k.ratio.require, collapse = "|"), names(flux.result))) != 3){
      if(!any(grepl("\\<k.max\\>", names(flux.result)))){
        stop("'k.max' required in 'flux.result'")}
      if(any(grepl("\\<k.max\\>", names(flux.result))) & !is.numeric(flux.result$k.max)){
        stop("'k.max' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<HM.k\\>", names(flux.result)))){
        stop("'HM.k' required in 'flux.result'")}
      if(any(grepl("\\<HM.k\\>", names(flux.result))) & !is.numeric(flux.result$HM.k)){
        stop("'HM.k' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<LM.flux\\>", names(flux.result)))){
        stop("'LM.flux' required in 'flux.result'")}
      if(any(grepl("\\<LM.flux\\>", names(flux.result))) & !is.numeric(flux.result$LM.flux)){
        stop("'LM.flux' in 'flux.result' must be of class numeric")}
    }
  } else if(any(grepl("\\<kappa\\>", criteria)) & is.null(k.ratio)){
    stop("'kappa' is mentionned in 'criteria', but the argument 'k.ratio' is NULL")}

  ## Check nb.obs and warn.length ####
  if(any(grepl("\\<nb.obs\\>", criteria)) & !is.null(warn.length)){
    if(!is.numeric(warn.length)) stop("'warn.length' must be of class numeric")
    if(is.numeric(warn.length) & warn.length <= 0) stop("'warn.length' must be larger than 0")

    if(!any(grepl("\\<nb.obs\\>", names(flux.result)))){
      stop("'nb.obs' required in 'flux.result'")
    } else if(!is.numeric(flux.result$nb.obs)){
      stop("'nb.obs' in 'flux.result' must be of class numeric")}

  } else if(any(grepl("\\<nb.obs\\>", criteria)) & is.null(warn.length)){
    stop("'nb.obs' is mentionned in 'criteria', but the argument 'warn.length' is NULL")}

  ## Check MDF ####
  if(any(grepl("\\<MDF\\>", criteria))){
    MDF.require <- c("\\<MDF\\>", "\\<LM.flux\\>", "\\<LM.se\\>", "\\<HM.flux\\>", "\\<HM.se\\>")
    if(length(grep(paste(MDF.require, collapse = "|"), names(flux.result))) != 5){
      if(!any(grepl("\\<MDF\\>", names(flux.result)))){
        stop("'MDF' required in 'flux.result'")
      } else if(!is.numeric(flux.result$MDF)){
        stop("'MDF' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<LM.flux\\>", names(flux.result)))){
        stop("'LM.flux' required in 'flux.result'")
      } else if(!is.numeric(flux.result$LM.flux)){
        stop("'LM.flux' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<LM.se\\>", names(flux.result)))){
        stop("'LM.se' required in 'flux.result'")
      } else if(!is.numeric(flux.result$LM.se)){
        stop("'LM.se' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<HM.flux\\>", names(flux.result)))){
        stop("'HM.flux' required in 'flux.result'")
      } else if(!is.numeric(flux.result$HM.flux)){
        stop("'HM.flux' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<HM.se\\>", names(flux.result)))){
        stop("'HM.se' required in 'flux.result'")
      } else if(!is.numeric(flux.result$HM.se)){
        stop("'HM.se' in 'flux.result' must be of class numeric")}
    }
  }
  ## Check MAE / RMSE ####
  if(any(grepl("\\<MAE\\>", criteria)) & any(grepl("\\<RMSE\\>", criteria))) {
    stop("'MAE' and 'RMSE' cannot both be selected.")
  } else {
    if(any(grepl("\\<MAE\\>", criteria))){
      MAE.require <- c("\\<LM.flux\\>", "\\<HM.MAE\\>", "\\<prec\\>")
      if(length(grep(paste(MAE.require, collapse = "|"), names(flux.result))) != 3){
        if(!any(grepl("\\<LM.flux\\>", names(flux.result)))){
          stop("'LM.flux' required in 'flux.result'")
        } else if(!is.numeric(flux.result$LM.flux)){
          stop("'LM.flux' in 'flux.result' must be of class numeric")}
        if(!any(grepl("\\<HM.MAE\\>", names(flux.result)))){
          stop("'HM.MAE' required in 'flux.result'")
        } else if(!is.numeric(flux.result$HM.MAE)){
          stop("'HM.MAE' in 'flux.result' must be of class numeric")}
        if(!any(grepl("\\<prec\\>", names(flux.result)))){
          stop("'prec' required in 'flux.result'")
        } else if(!is.numeric(flux.result$prec)){
          stop("'prec' in 'flux.result' must be of class numeric")}
      }
    } else if(any(grepl("\\<RMSE\\>", criteria))){
      RMSE.require <- c("\\<LM.flux\\>", "\\<HM.RMSE\\>", "\\<prec\\>")
      if(length(grep(paste(RMSE.require, collapse = "|"), names(flux.result))) != 3){
        if(!any(grepl("\\<LM.flux\\>", names(flux.result)))){
          stop("'LM.flux' required in 'flux.result'")
        } else if(!is.numeric(flux.result$LM.flux)){
          stop("'LM.flux' in 'flux.result' must be of class numeric")}
        if(!any(grepl("\\<HM.RMSE\\>", names(flux.result)))){
          stop("'HM.RMSE' required in 'flux.result'")
        } else if(!is.numeric(flux.result$HM.RMSE)){
          stop("'HM.RMSE' in 'flux.result' must be of class numeric")}
        if(!any(grepl("\\<prec\\>", names(flux.result)))){
          stop("'prec' required in 'flux.result'")
        } else if(!is.numeric(flux.result$prec)){
          stop("'prec' in 'flux.result' must be of class numeric")}
      }
    }
  }
  # Assign NULL to variables without binding ####
  g.fact <- HM.diagnose <- HM.RMSE <- prec <- model <- quality.check <- HM.k <-
    LM.p.val <- LM.diagnose <- HM.se.rel <- LM.se.rel <- HM.C0 <- LM.C0 <-
    LM.Ci <- LM.se <- HM.se <- MDF <- HM.MAE <- nb.obs <- LM.RMSE <- Ct <-
    LM.MAE <- UniqueID <- . <- C0 <- Ct <- C0.min <- C0.max <- RMSE.lim <- NULL

  # FUNCTION START ####

  # Assume that the best flux is HM.flux and leave *.diagnose empty
  best.flux.df <- flux.result %>%
    mutate(HM.diagnose = "", LM.diagnose = "", best.flux = HM.flux,
           model = "HM", quality.check = "")

  ## RMSE ####
  # Reflects the instrument precision. Sensitive to outliers.
  if(any(grepl("\\<RMSE\\>", criteria))) {

    RMSE.quality <- "Noise (LM.RMSE & HM.RMSE)"
    RMSE.LM.diagnostic <- "Noisy measurement (LM.RMSE)"
    RMSE.HM.diagnostic <- "Noisy measurement (HM.RMSE)"

    best.flux.df <- best.flux.df %>%
      # Calculate RMSE threshold: prec + 0.05% of reading
      mutate(RMSE.lim = prec + 0.05*max(c(Ct, C0))/100) %>%
      # If HM.RMSE <= RMSE.lim, use HM.flux
      # If HM.RMSE > RMSE.lim, but HM.RMSE <= LM.RMSE, still use HM.flux
      # else, use LM.flux
      mutate(best.flux = if_else(HM.RMSE > RMSE.lim & HM.RMSE > LM.RMSE,
                                 LM.flux, best.flux),
             model = if_else(HM.RMSE > RMSE.lim & HM.RMSE > LM.RMSE,
                             "LM", model)) %>%
      # If LM.RMSE > RMSE.lim
      mutate(LM.diagnose = if_else(LM.RMSE > RMSE.lim, ifelse(
        LM.diagnose == "", RMSE.LM.diagnostic,
        paste(LM.diagnose, RMSE.LM.diagnostic, sep = " | ")), LM.diagnose)) %>%
      # If HM.RMSE > RMSE.lim
      mutate(HM.diagnose = if_else(HM.RMSE > RMSE.lim, ifelse(
        HM.diagnose == "", RMSE.HM.diagnostic,
        paste(HM.diagnose, RMSE.HM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If LM.RMSE > RMSE.lim & HM.RMSE > RMSE.lim
      mutate(quality.check = if_else(
        LM.RMSE > RMSE.lim & HM.RMSE > RMSE.lim, ifelse(
          quality.check == "", RMSE.quality,
          paste(quality.check, RMSE.quality, sep = " | ")), quality.check)) %>%
      # If LM.RMSE > RMSE.lim & HM.RMSE <= RMSE.lim
      mutate(HM.diagnose = if_else(
        LM.RMSE > RMSE.lim & HM.RMSE <= RMSE.lim, ifelse(
          HM.diagnose == "", RMSE.LM.diagnostic,
          paste(HM.diagnose, RMSE.LM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If LM.RMSE <= RMSE.lim & HM.RMSE > RMSE.lim
      mutate(HM.diagnose = if_else(
        LM.RMSE <= RMSE.lim & HM.RMSE > RMSE.lim, ifelse(
          HM.diagnose == "", RMSE.HM.diagnostic,
          paste(HM.diagnose, RMSE.HM.diagnostic, sep = " | ")), HM.diagnose))
  }

  ## MAE ####
  # Reflects the instrument precision.
  if(any(grepl("\\<MAE\\>", criteria))) {

    MAE.quality <- "Noise (LM.MAE & HM.MAE)"
    MAE.LM.diagnostic <- "Noisy measurement (LM.MAE)"
    MAE.HM.diagnostic <- "Noisy measurement (HM.MAE)"

    best.flux.df <- best.flux.df %>%
      # Calculate MAE threshold: prec + 0.05% of reading
      mutate(MAE.lim = prec + 0.05*max(c(Ct, C0))/100) %>%
      # If HM.MAE <= prec, use HM.flux
      # else, if HM.MAE > prec, but HM.MAE <= LM.MAE, still use HM.flux
      # else, use LM.flux
      mutate(best.flux = if_else(HM.MAE > prec & HM.MAE > LM.MAE,
                                 LM.flux, best.flux),
             model = if_else(HM.MAE > prec & HM.MAE > LM.MAE,
                             "LM", model)) %>%
      # If LM.MAE > prec
      mutate(LM.diagnose = if_else(LM.MAE > prec, ifelse(
        LM.diagnose == "", MAE.LM.diagnostic,
        paste(LM.diagnose, MAE.LM.diagnostic, sep = " | ")), LM.diagnose)) %>%
      # If HM.MAE > prec
      mutate(HM.diagnose = if_else(HM.MAE > prec, ifelse(
        HM.diagnose == "", MAE.HM.diagnostic,
        paste(HM.diagnose, MAE.HM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If LM.MAE > prec & HM.MAE > prec
      mutate(quality.check = if_else(
        LM.MAE > prec & HM.MAE > prec, ifelse(
          quality.check == "", MAE.quality,
          paste(quality.check, MAE.quality, sep = " | ")), quality.check)) %>%
      # If LM.MAE > prec & HM.MAE <= prec
      mutate(HM.diagnose = if_else(
        LM.MAE > prec & HM.MAE <= prec, ifelse(
          HM.diagnose == "", MAE.LM.diagnostic,
          paste(HM.diagnose, MAE.LM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If LM.MAE <= prec & HM.MAE > prec
      mutate(HM.diagnose = if_else(
        LM.MAE <= prec & HM.MAE > prec, ifelse(
          HM.diagnose == "", MAE.HM.diagnostic,
          paste(HM.diagnose, MAE.HM.diagnostic, sep = " | ")), HM.diagnose))
  }

  ## SErel ####
  # Standard Error Relative (%). Calculated with deltamethod().
  # Default is 5%.

  if(any(grepl("\\<SErel\\>", criteria))) {

    SErel.quality <- paste("SE rel. > ", SErel, "%", sep = "")
    SErel.LM.diagnostic <- paste("Noisy meas. (LM.se.rel > ", SErel, "%)", sep = "")
    SErel.HM.diagnostic <- paste("Noisy meas. (HM.se.rel > ", SErel, "%)", sep = "")

    best.flux.df <- best.flux.df %>%
      # If HM.se.rel <= SErel, use HM.flux
      # else, if HM.se.rel > SErel, but HM.se.rel <= LM.se.rel, still use HM.flux
      # else, use LM.flux
      mutate(best.flux = if_else(HM.se.rel > SErel & HM.se.rel > LM.se.rel,
                                 LM.flux, best.flux),
             model = if_else(HM.se.rel > SErel & HM.se.rel > LM.se.rel,
                             "LM", model)) %>%
      # If LM.se.rel > SErel
      mutate(LM.diagnose = if_else(LM.se.rel > SErel, ifelse(
        LM.diagnose == "", SErel.LM.diagnostic,
        paste(LM.diagnose, SErel.LM.diagnostic, sep = " | ")), LM.diagnose)) %>%
      # If HM.se.rel > SErel
      mutate(HM.diagnose = if_else(HM.se.rel > SErel, ifelse(
        HM.diagnose == "", SErel.HM.diagnostic,
        paste(HM.diagnose, SErel.HM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If LM.se.rel > SErel & HM.se.rel > SErel
      mutate(quality.check = if_else(
        LM.se.rel > SErel & HM.se.rel > SErel, ifelse(
          quality.check == "", SErel.quality,
          paste(quality.check, SErel.quality, sep = " | ")), quality.check)) %>%
      # If LM.se.rel > SErel & HM.se.rel <= SErel
      mutate(HM.diagnose = if_else(
        LM.se.rel > SErel & HM.se.rel <= SErel, ifelse(
          HM.diagnose == "", SErel.LM.diagnostic,
          paste(HM.diagnose, SErel.LM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If LM.se.rel <= SErel & HM.se.rel > SErel
      mutate(HM.diagnose = if_else(
        LM.se.rel <= SErel & HM.se.rel > SErel, ifelse(
          HM.diagnose == "", SErel.HM.diagnostic,
          paste(HM.diagnose, SErel.HM.diagnostic, sep = " | ")), HM.diagnose))
  }

  ## G factor ####
  # Ratio between HM/LM. Default is 2
  if(any(grepl("\\<g.factor\\>", criteria))) {

    g.quality <- paste("g-factor > ", g.limit, sep = "")
    g.diagnostic <- paste("Overestimation of flux (g-factor > ", g.limit, ")", sep = "")

    best.flux.df <- best.flux.df %>%
      # If g.fact <= g.limit, use HM.flux
      # else, use LM.flux
      mutate(best.flux = if_else(g.fact > g.limit, LM.flux, best.flux),
             model = if_else(g.fact > g.limit, "LM", model)) %>%
      # If g.fact > g.limit
      mutate(HM.diagnose = if_else(g.fact > g.limit, ifelse(
        HM.diagnose == "", g.diagnostic,
        paste(HM.diagnose, g.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If g.fact > g.limit
      mutate(quality.check = if_else(g.fact > g.limit, ifelse(
        quality.check == "", g.quality,
        paste(quality.check, g.quality, sep = " | ")), quality.check))
  }

  ## kappa max ####
  # Maximal curvature allowed in non-linear regression (Hutchinson and Mosier)
  if(any(grepl("\\<kappa\\>", criteria))) {

    k.quality <- paste("kappa ratio > ", k.ratio*100, "%", sep = "")
    k.diagnostic <- paste("Exaggerated curvature (kappa ratio > ",
                          k.ratio, sep = "")

    best.flux.df <- best.flux.df %>%
      # If abs(HM.k/k.max) <= k.ratio, use HM.flux
      # else, use LM.flux
      mutate(best.flux = if_else(abs(HM.k/k.max) > k.ratio, LM.flux, best.flux),
             model = if_else(abs(HM.k/k.max) > k.ratio, "LM", model)) %>%
      # If abs(HM.k/k.max) > k.ratio
      mutate(HM.diagnose = if_else(abs(HM.k/k.max) > k.ratio, ifelse(
        HM.diagnose == "", k.diagnostic,
        paste(HM.diagnose, k.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If abs(HM.k/k.max) > k.ratio
      mutate(quality.check = if_else(abs(HM.k/k.max) > k.ratio, ifelse(
        quality.check == "", k.quality,
        paste(quality.check, k.quality, sep = " | ")), quality.check))
  }

  ## p-value ####
  # Statistical limit of detectable flux. Default is p-value < 0.05
  if(any(grepl("\\<p-value\\>", criteria))) {

    p.quality <- "No detect. LM.flux (p-val)"
    p.diagnostic <- paste("No detectable flux (p-value > ", p.val, ")", sep = "")

    best.flux.df <- best.flux.df %>%
      # If LM.p.val > p.val
      mutate(LM.diagnose = if_else(LM.p.val > p.val, ifelse(
        LM.diagnose == "", p.diagnostic,
        paste(LM.diagnose, p.diagnostic, sep = " | ")), LM.diagnose)) %>%
      # If LM.p.val > p.val
      mutate(quality.check = if_else(LM.p.val > p.val, ifelse(
        quality.check == "", p.quality,
        paste(quality.check, p.quality, sep = " | ")), quality.check))
  }

  ## MDF ####
  # Minimal detectable flux. Calculated with function MDF().
  # Based on instrument precision.
  if(any(grepl("\\<MDF\\>", criteria))) {

    MDF.quality <- "No detectable flux (MDF)"
    MDF.LM.diagnostic <- "No detectable LM.flux (MDF)"
    MDF.HM.diagnostic <- "No detectable HM.flux (MDF)"

    best.flux.df <- best.flux.df %>%
      # If HM.flux => MDF, use HM.flux
      # else, if HM.flux < MDF, but LM.flux < MDF also, still use HM.flux
      # else, use LM.flux
      mutate(best.flux = if_else(abs(HM.flux) < MDF & abs(LM.flux) >= MDF,
                                 LM.flux, best.flux),
             model = if_else(abs(HM.flux) < MDF & abs(LM.flux) >= MDF,
                             "LM", model)) %>%
      # If abs(LM.flux) < MDF
      mutate(LM.diagnose = if_else(abs(LM.flux) < MDF, ifelse(
        LM.diagnose == "", MDF.LM.diagnostic,
        paste(LM.diagnose, MDF.LM.diagnostic, sep = " | ")), LM.diagnose)) %>%
      # If abs(HM.flux) < MDF
      mutate(HM.diagnose = if_else(abs(HM.flux) < MDF, ifelse(
        HM.diagnose == "", MDF.HM.diagnostic,
        paste(HM.diagnose, MDF.HM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If abs(HM.flux) < MDF & abs(LM.flux) < MDF
      mutate(quality.check = if_else(abs(HM.flux) < MDF & abs(LM.flux) < MDF, ifelse(
        quality.check == "", MDF.quality,
        paste(quality.check, MDF.quality, sep = " | ")), quality.check)) %>%
      # If abs(HM.flux) < MDF & abs(LM.flux) >= MDF
      mutate(quality.check = if_else(abs(HM.flux) < MDF & abs(LM.flux) >= MDF, ifelse(
        quality.check == "", MDF.HM.diagnostic,
        paste(quality.check, MDF.HM.diagnostic, sep = " | ")), quality.check))
  }

  ## Intercept ####
  # Limits must have a minimum and a maximum value
  if(any(grepl("\\<intercept\\>", criteria))){

    C0.quality <- "Intercept (LM & HM)"
    C0.LM.diagnostic <- "Intercept out of bounds (LM)"
    C0.HM.diagnostic <- "Intercept out of bounds (HM)"

    if(!is.null(intercept.lim)){
      best.flux.df <- best.flux.df %>%
        mutate(C0.min = intercept.lim[1],
               C0.max = intercept.lim[2])
    } else {
      best.flux.df <- best.flux.df %>%
        mutate(C0.min = C0-(abs(Ct-C0)*0.1),
               C0.max = C0+(abs(Ct-C0)*0.1))
    }

    best.flux.df <- best.flux.df %>%
      # If only HM.C0 is out of bounds
      mutate(
        HM.diagnose = if_else(
          !between(HM.C0, C0.min, C0.max) &
            between(LM.C0, C0.min, C0.max), ifelse(
              HM.diagnose == "", C0.HM.diagnostic,
              paste(HM.diagnose, C0.HM.diagnostic, sep = " | ")), HM.diagnose),
        quality.check = if_else(
          !between(HM.C0, C0.min, C0.max) &
            between(LM.C0, C0.min, C0.max), ifelse(
              quality.check == "", C0.HM.diagnostic,
              paste(quality.check, C0.HM.diagnostic, sep = " | ")), quality.check)) %>%
      # If only LM.C0 is out of bounds
      mutate(
        LM.diagnose = if_else(
          !between(LM.C0, C0.min, C0.max) &
            between(HM.C0, C0.min, C0.max), ifelse(
              LM.diagnose == "", C0.LM.diagnostic,
              paste(LM.diagnose, C0.LM.diagnostic, sep = " | ")), LM.diagnose),
        quality.check = if_else(
          !between(LM.C0, C0.min, C0.max) &
            between(HM.C0, C0.min, C0.max), ifelse(
              quality.check == "", C0.LM.diagnostic,
              paste(quality.check, C0.LM.diagnostic, sep = " | ")), quality.check)) %>%
      # If both intercepts are out of bounds
      mutate(quality.check = if_else(
        !between(LM.C0, C0.min, C0.max) &
          !between(HM.C0, C0.min, C0.max), ifelse(
            quality.check == "", C0.quality,
            paste(quality.check, C0.quality, sep = " | ")), quality.check)) %>%
      # Remove intercept limits from best.flux.df
      select(!c(C0.min, C0.max))
  }

  ## Number of observations ####
  if(any(grepl("\\<nb.obs\\>", criteria))) {

    obs.quality <- paste("nb.obs <", warn.length)

    best.flux.df <- best.flux.df %>%
      # If nb.obs < warn.length
      mutate(quality.check = if_else(nb.obs < warn.length, ifelse(
        quality.check == "", obs.quality,
        paste(quality.check, obs.quality, sep = " | ")), quality.check))
  }

  ## NA ####
  NA.quality <- "fluxes are NA"
  NA.LM.diagnostic <- "LM.flux is NA"
  NA.HM.diagnostic <- "HM.flux is NA"

  best.flux.clean <- best.flux.df %>%
    # If HM.flux is NA, use LM.flux
    mutate(best.flux = if_else(is.na(HM.flux) & !is.na(LM.flux),
                               LM.flux, best.flux),
           model = if_else(is.na(HM.flux) & !is.na(LM.flux),
                           "LM", model)) %>%
    # If LM.flux is also NA, return NA
    mutate(best.flux = if_else(is.na(HM.flux) & is.na(LM.flux),
                               NA, best.flux),
           model = if_else(is.na(HM.flux) & is.na(LM.flux),
                           NA, model)) %>%
    # If HM.flux is NA
    mutate(HM.diagnose = if_else(is.na(HM.flux), NA.HM.diagnostic, HM.diagnose),
           quality.check = if_else(is.na(HM.flux) & !is.na(LM.flux),
                                   NA.HM.diagnostic, quality.check)) %>%
    # If LM.flux is NA
    mutate(LM.diagnose = if_else(is.na(LM.flux), NA.LM.diagnostic, LM.diagnose)) %>%
    # If both are NA
    mutate(quality.check = if_else(is.na(LM.flux) & is.na(HM.flux),
                                   NA.quality, quality.check))

  return(best.flux.clean)
}
