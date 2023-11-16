#' Automatic selection of best flux estimate
#'
#' This automatic selection of the best flux estimate (linear or non-linear) is
#' based on objective criteria and non-arbitrary thresholds.
#'
#' @param flux.result data.frame; output from the function
#'                    \code{\link[GoFluxYourself]{goFlux}}.
#' @param criteria character vector; criteria used to asses the goodness of fit
#'                 of the linear and non-linear flux estimates. Must be at least
#'                 one the following: "MAE", "RMSE", "AICc", "SE", "g.factor",
#'                 "kappa", "MDF", "nb.obs", "p-value", or "intercept". The
#'                 default is all of them.
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
#' @param g.limit numerical value; maximal limit of the the g-factor (ratio
#'                between \code{\link[GoFluxYourself]{HM.flux}} and
#'                \code{\link[GoFluxYourself]{LM.flux}}). Recommended
#'                thresholds for the g-factor are 4 (flexible), 2 (medium), or
#'                1.25 (conservative). The default limit is \code{g.limit = 2}.
#' @param k.ratio numerical value; maximal limit of the ratio between kappa and
#'                the kappa-max. Default is \code{k.ratio = 1}.
#' @param p.val numerical value; a limit for a statistically detectable flux.
#'              The default threshold is \emph{p-value} < 0.05.
#' @param warn.length numerical; limit under which a measurement is flagged for
#'                    being too short (\code{nb.obs < warn.length}).
#'
#' @details
#' In \code{criteria}, the indices of model fit "MAE", "RMSE" and "SE" all
#' have a threshold. For MAE and RMSE, the threshold is instrument precision
#' (1\sigma). For SE, the threshold is the instrument accuracy
#' (1\sigma/\eqn{\sqrt{n}}{ASCII}. These indices also compare the two models
#' (linear, LM, and non-linear, HM). The selection of the best model based on
#' indices of model fit ("MAE", "RMSE", "AICc" and "SE") is based on a scoring
#' system. Both models start with a score of 0. For each criteria, whichever
#' model performs worst is given +1. After all selected criteria have been
#' evaluated, the model with the lowest score wins. In case of a tie, the
#' non-linear flux estimate is always chosen by default, as non-linearity is assumed.
#'
#' The \code{g.limit} indicates a threshold for the
#' \code{\link[GoFluxYourself]{g.factor}}, which is the ratio between a
#' non-linear flux estimate and a linear flux estimate. With the
#' \code{\link[GoFluxYourself]{best.flux}} function, one can chose a limit at
#' which the HM model is deemed to overestimate
#' \ifelse{html}{\out{f<sub>0</sub>}}{\eqn{f[0]}{ASCII}}. Recommended thresholds
#' for the g-factor are <4 for a flexible threshold, <2 for a medium threshold,
#' or <1.25 for a more conservative threshold. The default threshold is
#' \code{g.limit = 2}. If the g-factor is above the specified threshold, the
#' best flux estimate will switch to LM instead of HM and give a warning in the
#' columns \code{quality.check} and \code{HM.diagnose}.
#'
#' The minimal detectable flux (\code{\link[GoFluxYourself]{MDF}}) is calculated
#' from the instrument precision and measurement time. Below the MDF, the flux
#' estimate is considered under the detection limit, but not null. Therefore,
#' the function will not return a 0. There will simply be a warning in the
#' columns \code{quality.check}, \code{LM.diagnose} or \code{HM.diagnose} to
#' warn of a flux estimate under the detectable limit. No best flux estimate
#' is chosen based on MDF.
#'
#' The parameter kappa determines the curvature of the non-linear regression in
#' the Hutchinson and Mosier model. A maximal limit of kappa,
#' \code{\link[GoFluxYourself]{k.max}} is included in the
#' \code{\link[GoFluxYourself]{goFlux}} function, so that the non-linear flux
#' estimate cannot exceed this maximum curvature. In the function
#' \code{best.flux()}, one can choose the linear flux estimate over the
#' non-linear flux estimate based on the ratio between kappa and kappa-max
#' (\code{k.ratio}). The ratio is expressed as a percentage, where 1 indicates
#' \code{HM.k = k.max}, and 0.5 indicates \code{HM.k = 0.5*k.max}. The default
#' setting is \code{k.ratio = 1}. If \code{HM.k/k.max} is larger than k.ratio,
#' a warning is issued in the columns HM.diagnose and quality.check.
#'
#' If the initial gas concentration
#' (\ifelse{html}{\out{C<sub>0</sub>}}{\eqn{C[0]}{ASCII}}) calculated for the
#' flux estimates are more or less than 10% of the difference between
#' \ifelse{html}{\out{C<sub>0</sub>}}{\eqn{C[0]}{ASCII}} and the final gas
#' concentration at the end of the measurement
#' (\ifelse{html}{\out{C<sub>t</sub>}}{\eqn{C[t]}{ASCII}}), a warning is issued
#' in the columns \code{quality.check}, \code{LM.diagnose} or \code{HM.diagnose}
#' to warn of an intercept out of bounds. Alternatively, one can provide
#' boundaries for the intercept, for example: \code{intercept.lim = c(380, 420)}
#' for a true \ifelse{html}{\out{C<sub>0</sub>}}{\eqn{C[0]}{ASCII}} of 400 ppm.
#'
#' The argument \code{p.val} is only applicable to the linear flux. Under the
#' defined \emph{p-value}, the linear flux estimate is deemed non-significant,
#' i. e., flux under the detectable limit. The default threshold is
#' \code{p.val = 0.05}. No best flux estimate is chosen based on \emph{p-value}.
#' If \code{LM.p.val < p.val}, a warning is given in the columns
#' \code{quality.check} and \code{LM.diagnose} to warn of an estimated flux
#' under the detection limit.
#'
#' \code{warn.length} is the limit under which a measurement is flagged for
#' being too short (\code{nb.obs < warn.length}). With nowadays' portable
#' greenhouse gas analyzers, the frequency of measurement is usually one
#' observation per second. Therefore, for the default setting of
#' \code{warn.length = 60}, the chamber closure time should be approximately
#' one minute (60 seconds). If the number of observations is smaller than the
#' threshold, a warning is issued in the column \code{quality.check}.
#'
#' @returns a data.frame identical to the input \code{flux.result} with the
#'          additional columns \code{HM.diagnose}, \code{LM.diagnose},
#'          \code{best.flux}, \code{model} and \code{quality.check}. For each
#'          criteria selected, an additional column is also added to specify
#'          the limits used for those criteria (e.g. \code{RMSE.lim},
#'          \code{p.val.lim}, etc.)
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' data(LGR_manID)
#' LGR_flux <- goFlux(LGR_manID, "CO2dry_ppm")
#' LGR_res <- best.flux(LGR_flux)
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
                      criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor",
                                   "kappa", "MDF", "nb.obs", "intercept", "p-value"),
                      intercept.lim = NULL, g.limit = 2,
                      p.val = 0.05, k.ratio = 1, warn.length = 60) {

  # Check arguments ####
  if(missing(flux.result)) stop("'flux.result' is required")
  if(!is.null(flux.result) & !is.data.frame(flux.result)){
    stop("'flux.result' must be of class 'dataframe'")}
  if(!any(grepl(paste(c("\\<MAE\\>", "\\<RMSE\\>", "\\<g.factor\\>", "\\<kappa\\>",
                        "\\<MDF\\>", "\\<nb.obs\\>", "\\<p-value\\>", "\\<AICc\\>",
                        "\\<intercept\\>", "\\<SE\\>"), collapse = "|"), criteria))){
    stop("'criteria' must contain at least one of the following: 'MAE', 'RMSE', 'AICc', 'g.factor', 'kappa', 'MDF', 'nb.obs', 'p-value', 'intercept', 'SE'")}

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
    MDF.require <- c("\\<MDF\\>", "\\<LM.flux\\>", "\\<HM.flux\\>")
    if(length(grep(paste(MDF.require, collapse = "|"), names(flux.result))) != 3){
      if(!any(grepl("\\<MDF\\>", names(flux.result)))){
        stop("'MDF' required in 'flux.result'")
      } else if(!is.numeric(flux.result$MDF)){
        stop("'MDF' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<LM.flux\\>", names(flux.result)))){
        stop("'LM.flux' required in 'flux.result'")
      } else if(!is.numeric(flux.result$LM.flux)){
        stop("'LM.flux' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<HM.flux\\>", names(flux.result)))){
        stop("'HM.flux' required in 'flux.result'")
      } else if(!is.numeric(flux.result$HM.flux)){
        stop("'HM.flux' in 'flux.result' must be of class numeric")}
    }
  }
  ## Check MAE ####
  if(any(grepl("\\<MAE\\>", criteria))){
    MAE.require <- c("\\<LM.MAE\\>", "\\<HM.MAE\\>", "\\<prec\\>")
    if(length(grep(paste(MAE.require, collapse = "|"), names(flux.result))) != 3){
      if(!any(grepl("\\<LM.MAE\\>", names(flux.result)))){
        stop("'LM.MAE' required in 'flux.result'")
      } else if(!is.numeric(flux.result$LM.MAE)){
        stop("'LM.MAE' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<HM.MAE\\>", names(flux.result)))){
        stop("'HM.MAE' required in 'flux.result'")
      } else if(!is.numeric(flux.result$HM.MAE)){
        stop("'HM.MAE' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<prec\\>", names(flux.result)))){
        stop("'prec' required in 'flux.result'")
      } else if(!is.numeric(flux.result$prec)){
        stop("'prec' in 'flux.result' must be of class numeric")}
    }
  }
  ## Check RMSE ####
  if(any(grepl("\\<RMSE\\>", criteria))){
    RMSE.require <- c("\\<LM.RMSE\\>", "\\<HM.RMSE\\>", "\\<prec\\>")
    if(length(grep(paste(RMSE.require, collapse = "|"), names(flux.result))) != 3){
      if(!any(grepl("\\<LM.RMSE\\>", names(flux.result)))){
        stop("'LM.RMSE' required in 'flux.result'")
      } else if(!is.numeric(flux.result$LM.RMSE)){
        stop("'LM.RMSE' in 'flux.result' must be of class numeric")}
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
  ## Check SE ####
  if(any(grepl("\\<SE\\>", criteria))){
    SE.require <- c("\\<LM.SE\\>", "\\<HM.SE\\>", "\\<prec\\>")
    if(length(grep(paste(SE.require, collapse = "|"), names(flux.result))) != 3){
      if(!any(grepl("\\<LM.SE\\>", names(flux.result)))){
        stop("'LM.SE' required in 'flux.result'")
      } else if(!is.numeric(flux.result$LM.SE)){
        stop("'LM.SE' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<HM.SE\\>", names(flux.result)))){
        stop("'HM.SE' required in 'flux.result'")
      } else if(!is.numeric(flux.result$HM.SE)){
        stop("'HM.SE' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<prec\\>", names(flux.result)))){
        stop("'prec' required in 'flux.result'")
      } else if(!is.numeric(flux.result$prec)){
        stop("'prec' in 'flux.result' must be of class numeric")}
    }
  }

  # Assign NULL to variables without binding ####
  g.fact <- HM.diagnose <- HM.RMSE <- prec <- model <- quality.check <- HM.k <-
    LM.p.val <- LM.diagnose <- HM.SE <- LM.SE <- HM.C0 <- LM.C0 <-
    LM.Ci <- LM.SE <- HM.SE <- MDF <- HM.MAE <- nb.obs <- LM.RMSE <- Ct <-
    LM.MAE <- UniqueID <- . <- C0 <- Ct <- C0.min <- C0.max <- RMSE.lim <-
    MAE.lim <- MDF.lim <- HM.score <- LM.score <- HM.AICc <- LM.AICc <-  NULL

  # Function to find decimal places
  nb.decimal = function(x) {
    # length zero input
    if (length(x) == 0) return(numeric())

    # count decimals
    x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
    x_int = floor(x) %>% abs() %>% nchar()
    x_nchr = x_nchr - 1 - x_int
    x_nchr[x_nchr < 0] = 0

    x_nchr
  }

  # FUNCTION START ####

  # Assume that the best flux is HM.flux and leave *.diagnose empty
  best.flux.df <- flux.result %>%
    mutate(HM.diagnose = "", LM.diagnose = "", best.flux = HM.flux,
           model = "HM", quality.check = "", HM.score = 0, LM.score = 0)

  ## RMSE ####
  # Reflects the instrument precision. Sensitive to outliers.
  if(any(grepl("\\<RMSE\\>", criteria))) {

    RMSE.quality <- "RMSE"
    RMSE.LM.diagnostic <- "Noise (LM.RMSE)"
    RMSE.HM.diagnostic <- "Noise (HM.RMSE)"

    best.flux.df <- best.flux.df %>%
      # RMSE threshold: instrument precision
      mutate(RMSE.lim = prec) %>%
      # Update score for HM and LM
      # If HM.RMSE <= RMSE.lim, use HM.flux
      # If HM.RMSE > RMSE.lim, but HM.RMSE <= LM.RMSE, still use HM.flux
      # else, use LM.flux
      mutate(HM.score = if_else(
        round(HM.RMSE, nb.decimal(prec)) > RMSE.lim &
          round(HM.RMSE, nb.decimal(prec)) >
          round(LM.RMSE, nb.decimal(prec)), HM.score+1, HM.score)) %>%
      mutate(LM.score = if_else(
        round(HM.RMSE, nb.decimal(prec)) > RMSE.lim &
          round(HM.RMSE, nb.decimal(prec)) >
          round(LM.RMSE, nb.decimal(prec)), LM.score, LM.score+1)) %>%
      # Update LM.diagnose
      mutate(LM.diagnose = if_else(
        round(LM.RMSE, nb.decimal(prec)) >
          round(RMSE.lim, nb.decimal(prec)), ifelse(
            LM.diagnose == "", RMSE.LM.diagnostic,
            paste(LM.diagnose, RMSE.LM.diagnostic, sep = " | ")), LM.diagnose)) %>%
      # Update HM.diagnose
      mutate(HM.diagnose = if_else(
        round(HM.RMSE, nb.decimal(prec)) > round(RMSE.lim, nb.decimal(prec)), ifelse(
          HM.diagnose == "", RMSE.HM.diagnostic,
          paste(HM.diagnose, RMSE.HM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # Update quality check
      mutate(quality.check = if_else(
        round(LM.RMSE, nb.decimal(prec)) > round(RMSE.lim, nb.decimal(prec)) &
          round(HM.RMSE, nb.decimal(prec)) > round(RMSE.lim, nb.decimal(prec)), ifelse(
            quality.check == "", RMSE.quality,
            paste(quality.check, RMSE.quality, sep = " | ")), quality.check))
  }

  ## MAE ####
  # Reflects the instrument precision.
  if(any(grepl("\\<MAE\\>", criteria))) {

    MAE.quality <- "MAE"
    MAE.LM.diagnostic <- "Noise (LM.MAE)"
    MAE.HM.diagnostic <- "Noise (HM.MAE)"

    best.flux.df <- best.flux.df %>%
      # MAE threshold: instrument precision
      mutate(MAE.lim = prec) %>%
      # Update score for HM and LM
      # If HM.MAE <= MAE.lim, use HM.flux
      # If HM.MAE > MAE.lim, but HM.MAE <= LM.MAE, still use HM.flux
      # else, use LM.flux
      mutate(HM.score = if_else(
        round(HM.MAE, nb.decimal(prec)) > MAE.lim &
          round(HM.MAE, nb.decimal(prec)) >
          round(LM.MAE, nb.decimal(prec)), HM.score+1, HM.score)) %>%
      mutate(LM.score = if_else(
        round(HM.MAE, nb.decimal(prec)) > MAE.lim &
          round(HM.MAE, nb.decimal(prec)) >
          round(LM.MAE, nb.decimal(prec)), LM.score, LM.score+1)) %>%
      # Update LM.diagnose
      mutate(LM.diagnose = if_else(
        round(LM.MAE, nb.decimal(prec)) >
          round(MAE.lim, nb.decimal(prec)), ifelse(
            LM.diagnose == "", MAE.LM.diagnostic,
            paste(LM.diagnose, MAE.LM.diagnostic, sep = " | ")), LM.diagnose)) %>%
      # Update HM.diagnose
      mutate(HM.diagnose = if_else(
        round(HM.MAE, nb.decimal(prec)) > round(MAE.lim, nb.decimal(prec)), ifelse(
          HM.diagnose == "", MAE.HM.diagnostic,
          paste(HM.diagnose, MAE.HM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # Update quality check
      mutate(quality.check = if_else(
        round(LM.MAE, nb.decimal(prec)) > round(MAE.lim, nb.decimal(prec)) &
          round(HM.MAE, nb.decimal(prec)) > round(MAE.lim, nb.decimal(prec)), ifelse(
            quality.check == "", MAE.quality,
            paste(quality.check, MAE.quality, sep = " | ")), quality.check))
  }

  ## SE ####
  # Standard Error. Calculated with deltamethod().
  # Instrument SE is prec/sqrt(nb.obs)

  if(any(grepl("\\<SE\\>", criteria))) {

    SE.quality <- "SE"
    SE.LM.diagnostic <- "Noise (LM.SE)"
    SE.HM.diagnostic <- "Noise (HM.SE)"

    best.flux.df <- best.flux.df %>%
      # SE threshold: instrument precision
      mutate(SE.lim = prec/sqrt(nb.obs)) %>%
      # Update score for HM and LM
      # If HM.SE <= SE.lim, use HM.flux
      # If HM.SE > SE.lim, but HM.SE <= LM.SE, still use HM.flux
      # else, use LM.flux
      mutate(HM.score = if_else(
        round(HM.SE, nb.decimal(prec)) > SE.lim &
          round(HM.SE, nb.decimal(prec)) >
          round(LM.SE, nb.decimal(prec)), HM.score+1, HM.score)) %>%
      mutate(LM.score = if_else(
        round(HM.SE, nb.decimal(prec)) > SE.lim &
          round(HM.SE, nb.decimal(prec)) >
          round(LM.SE, nb.decimal(prec)), LM.score, LM.score+1)) %>%
      # Update LM.diagnose
      mutate(LM.diagnose = if_else(
        round(LM.SE, nb.decimal(prec)) >
          round(SE.lim, nb.decimal(prec)), ifelse(
            LM.diagnose == "", SE.LM.diagnostic,
            paste(LM.diagnose, SE.LM.diagnostic, sep = " | ")), LM.diagnose)) %>%
      # Update HM.diagnose
      mutate(HM.diagnose = if_else(
        round(HM.SE, nb.decimal(prec)) > round(SE.lim, nb.decimal(prec)), ifelse(
          HM.diagnose == "", SE.HM.diagnostic,
          paste(HM.diagnose, SE.HM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # Update quality check
      mutate(quality.check = if_else(
        round(LM.SE, nb.decimal(prec)) > round(SE.lim, nb.decimal(prec)) &
          round(HM.SE, nb.decimal(prec)) > round(SE.lim, nb.decimal(prec)), ifelse(
            quality.check == "", SE.quality,
            paste(quality.check, SE.quality, sep = " | ")), quality.check))
  }

  ## AICc ####
  # Index of model fit that corrects for the number of parameters in the
  # model and corrects for the number of observations
  if(any(grepl("\\<AICc\\>", criteria))) {

    AICc.quality <- "AICc"

    best.flux.df <- best.flux.df %>%
      # Update score for HM and LM
      # If HM.AICc <= LM.AICc, use HM.flux else, use LM.flux
      mutate(HM.score = if_else(HM.AICc > LM.AICc, HM.score+1, HM.score)) %>%
      mutate(LM.score = if_else(HM.AICc > LM.AICc, LM.score, LM.score+1)) %>%
      # Update quality check
      mutate(quality.check = if_else(HM.AICc > LM.AICc, ifelse(
        quality.check == "", AICc.quality,
        paste(quality.check, AICc.quality, sep = " | ")), quality.check))
  }

  ## Choose best model based on HM and LM scores ####
  if(any(grepl(paste(c("\\<MAE\\>", "\\<RMSE\\>", "\\<AICc\\>", "\\<SE\\>"),
                    collapse = "|"), criteria))){

    best.flux.df <- best.flux.df %>%
      mutate(best.flux = if_else(HM.score > LM.score, HM.flux, LM.flux),
             model = if_else(HM.score > LM.score, "LM", model))
  }

  ## G factor ####
  # Ratio between HM/LM. Default is 2
  if(any(grepl("\\<g.factor\\>", criteria))) {

    g.quality <- paste("g-fact. > ", g.limit, sep = "")
    g.diagnostic <- "Overestimated flux (g-fact)"

    best.flux.df <- best.flux.df %>%
      mutate(g.limit = g.limit) %>%
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

  ## kappa ratio ####
  # Maximal curvature allowed in non-linear regression (Hutchinson and Mosier)
  if(any(grepl("\\<kappa\\>", criteria))) {

    k.quality <- "kappa max"
    k.diagnostic <- "Exaggerated curvature (k.ratio)"

    best.flux.df <- best.flux.df %>%
      mutate(k.ratio.lim = k.ratio) %>%
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

    p.quality <- "p-value"
    p.diagnostic <- "No detect. flux (p-val.)"

    best.flux.df <- best.flux.df %>%
      mutate(p.val.lim = p.val) %>%
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

    MDF.quality <- "MDF"
    MDF.LM.diagnostic <- "MDF (LM)"
    MDF.HM.diagnostic <- "MDF (HM)"

    best.flux.df <- best.flux.df %>%
      mutate(MDF.lim = ifelse(nb.decimal(signif(MDF, 1)) != 0, signif(MDF, 2),
                              round(MDF, 1))) %>%
      # If HM.flux => MDF.lim, use HM.flux
      # else, if HM.flux < MDF.lim, but LM.flux < MDF.lim also, still use HM.flux
      # else, use LM.flux
      mutate(best.flux = if_else(round(abs(HM.flux), nb.decimal(MDF.lim)) < MDF.lim &
                                   round(abs(LM.flux), nb.decimal(MDF.lim)) >= MDF.lim,
                                 LM.flux, best.flux),
             model = if_else(round(abs(HM.flux), nb.decimal(MDF.lim)) < MDF.lim &
                               round(abs(LM.flux), nb.decimal(MDF.lim)) >= MDF.lim,
                             "LM", model)) %>%
      # If round(abs(LM.flux), nb.decimal(MDF.lim)) < MDF.lim
      mutate(LM.diagnose = if_else(
        round(abs(LM.flux), nb.decimal(MDF.lim)) < MDF.lim, ifelse(
          LM.diagnose == "", MDF.LM.diagnostic,
          paste(LM.diagnose, MDF.LM.diagnostic, sep = " | ")), LM.diagnose)) %>%
      # If round(abs(HM.flux), nb.decimal(MDF.lim)) < MDF.lim
      mutate(HM.diagnose = if_else(
        round(abs(HM.flux), nb.decimal(MDF.lim)) < MDF.lim, ifelse(
          HM.diagnose == "", MDF.HM.diagnostic,
          paste(HM.diagnose, MDF.HM.diagnostic, sep = " | ")), HM.diagnose)) %>%
      # If round(abs(HM.flux), nb.decimal(MDF.lim)) < MDF.lim &
      #    round(abs(LM.flux), nb.decimal(MDF.lim)) < MDF.lim
      mutate(quality.check = if_else(
        round(abs(HM.flux), nb.decimal(MDF.lim)) < MDF.lim &
          round(abs(LM.flux), nb.decimal(MDF.lim)) < MDF.lim, ifelse(
            quality.check == "", MDF.quality,
            paste(quality.check, MDF.quality, sep = " | ")), quality.check)) %>%
      # If round(abs(HM.flux), nb.decimal(MDF.lim)) < MDF.lim &
      #    round(abs(LM.flux), nb.decimal(MDF.lim)) >= MDF.lim
      mutate(quality.check = if_else(
        round(abs(HM.flux), nb.decimal(MDF.lim)) < MDF.lim &
          round(abs(LM.flux), nb.decimal(MDF.lim)) >= MDF.lim, ifelse(
            quality.check == "", MDF.HM.diagnostic,
            paste(quality.check, MDF.HM.diagnostic, sep = " | ")), quality.check))
  }

  ## Intercept ####
  # Limits must have a minimum and a maximum value
  if(any(grepl("\\<intercept\\>", criteria))){

    C0.quality <- "Intercept"
    C0.LM.diagnostic <- "Intercept (LM)"
    C0.HM.diagnostic <- "Intercept (HM)"

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
      mutate(C0.min = C0.min, C0.max = C0.max) %>%
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

    obs.quality <- "nb.obs"

    best.flux.df <- best.flux.df %>%
      mutate(warn.nb.obs = warn.length) %>%
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
