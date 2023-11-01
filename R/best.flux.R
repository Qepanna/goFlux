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
#'                      limits of the intercept (initial concentration; C0).
#'                      Must be the same units as \code{gastype}.
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
#' data(example_LGR_manID)
#' example_LGR_flux <- goFlux(example_LGR_manID, "CO2dry_ppm")
#' criteria <- c("MAE", "g.factor", "MDF", "SErel", "intercept")
#' example_LGR_res <- best.flux(example_LGR_flux, criteria)
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
      } else if(!is.numeric(flux.result$HM.C0)){
        stop("'HM.C0' in 'flux.result' must be of class numeric")}
      if(!any(grepl("\\<LM.C0\\>", names(flux.result)))){
        stop("'LM.C0' required in 'flux.result'")
      } else if(!is.numeric(flux.result$LM.C0)){
        stop("'LM.C0' in 'flux.result' must be of class numeric")}
    }
    # Requirements with intercept.lim
    if(!is.null(intercept.lim)){
      if(length(intercept.lim) != 2) stop("'intercept.lim' must have length = 2")
      if(!is.numeric(intercept.lim)) stop("'intercept.lim' must be of class numeric")
    } else {
      intercept.require2 <- c("\\<C0\\>", "\\<Ci\\>")
      if(length(grep(paste(intercept.require2, collapse = "|"), names(flux.result))) != 2){
        if(!any(grepl("\\<C0\\>", names(flux.result)))){
          stop("'C0' required in 'flux.result'")
        } else if(!is.numeric(flux.result$C0)){
          stop("'C0' in 'flux.result' must be of class numeric")}
        if(!any(grepl("\\<Ci\\>", names(flux.result)))){
          stop("'Ci' required in 'flux.result'")
        } else if(!is.numeric(flux.result$Ci)){
          stop("'Ci' in 'flux.result' must be of class numeric")}
      }
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
    LM.Ci <- LM.se <- HM.se <- MDF <- HM.MAE <- nb.obs <- LM.RMSE <-
    LM.MAE <- UniqueID <- . <- NULL

  # FUNCTION START ####

  # Assume that the best flux is HM.flux and leave *.diagnose empty
  best.flux.df <- flux.result %>%
    mutate(HM.diagnose = "", LM.diagnose = "", best.flux = HM.flux,
           model = "HM", quality.check = "") %>%
    group_by(UniqueID) %>% group_split()

  ## NA ####
  # If HM.flux is NA, use LM.flux
  best.flux.NA <- function(x, best.flux.df) {

    # best.flux variables
    LM.flux <- best.flux.df[[x]]$LM.flux
    HM.flux <- best.flux.df[[x]]$HM.flux
    best.flux <- best.flux.df[[x]]$best.flux
    model <- best.flux.df[[x]]$model
    HM.diagnose <- best.flux.df[[x]]$HM.diagnose
    LM.diagnose <- best.flux.df[[x]]$LM.diagnose
    quality.check <- best.flux.df[[x]]$quality.check

    # If HM.flux is NA, use LM.flux
    if(is.na(HM.flux)){
      if(!is.na(LM.flux)){
        best.flux <- LM.flux
        model <- "LM"
      }
      # IF LM.flux is also NA, return NA
      if(is.na(LM.flux)){
        best.flux <- NA
        model <- NA
      }
    }
    # diagnostic & quality
    quality <- "flux is NA"
    LM.diagnostic <- "LM.flux is NA"
    HM.diagnostic <- "HM.flux is NA"

    if(is.na(LM.flux)){
      LM.diagnose <- ifelse(
        LM.diagnose == "", LM.diagnostic,
        paste(LM.diagnose, LM.diagnostic, sep = " | "))
    }
    if(is.na(HM.flux)){
      HM.diagnose <- ifelse(
        HM.diagnose == "", HM.diagnostic,
        paste(HM.diagnose, HM.diagnostic, sep = " | "))
    }
    if(is.na(LM.flux) & is.na(HM.flux)){
      quality.check <- ifelse(
        quality.check == "", quality,
        paste(quality.check, quality, sep = " | "))
    }

    # Update best.flux.df
    best.flux.df.NA <- best.flux.df[[x]] %>%
      mutate(HM.diagnose = HM.diagnose, LM.diagnose = LM.diagnose,
             best.flux = best.flux, model = model,
             quality.check = quality.check)

    return(best.flux.df.NA)
  }

  best.flux.df <- lapply(seq_along(best.flux.df), best.flux.NA, best.flux.df)

  ## RMSE ####
  # Reflects the instrument precision. Sensitive to outliers.
  if(any(grepl("\\<RMSE\\>", criteria))) {

    best.flux.RMSE <- function(x, best.flux.df) {

      # best.flux variables
      LM.flux <- best.flux.df[[x]]$LM.flux
      best.flux <- best.flux.df[[x]]$best.flux
      model <- best.flux.df[[x]]$model
      HM.diagnose <- best.flux.df[[x]]$HM.diagnose
      LM.diagnose <- best.flux.df[[x]]$LM.diagnose
      quality.check <- best.flux.df[[x]]$quality.check

      # RMSE variables
      HM.RMSE <- signif(best.flux.df[[x]]$HM.RMSE, 1)
      LM.RMSE <- signif(best.flux.df[[x]]$LM.RMSE, 1)
      prec <- best.flux.df[[x]]$prec

      # If HM.RMSE <= prec, use HM.flux
      # else, if HM.RMSE > prec, but HM.RMSE <= LM.RMSE, still use HM.flux
      # else, use LM.flux
      if(isTRUE(HM.RMSE > prec & HM.RMSE > LM.RMSE)){
        best.flux <- LM.flux
        model <- "LM"
      }
      # diagnostic & quality
      quality <- "Noise (LM.RMSE & HM.RMSE)"
      LM.diagnostic <- "Noisy measurement (LM.RMSE)"
      HM.diagnostic <- "Noisy measurement (HM.RMSE)"

      if(isTRUE(LM.RMSE > prec)){
        LM.diagnose <- ifelse(
          LM.diagnose == "", LM.diagnostic,
          paste(LM.diagnose, LM.diagnostic, sep = " | "))
      }
      if(isTRUE(HM.RMSE > prec)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", HM.diagnostic,
          paste(HM.diagnose, HM.diagnostic, sep = " | "))
      }
      if(isTRUE(LM.RMSE > prec & HM.RMSE > prec)){
        quality.check <- ifelse(
          quality.check == "", quality,
          paste(quality.check, quality, sep = " | "))
      }
      if(isTRUE(LM.RMSE > prec & HM.RMSE <= prec)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", LM.diagnostic,
          paste(HM.diagnose, LM.diagnostic, sep = " | "))
      }
      if(isTRUE(LM.RMSE <= prec & HM.RMSE > prec)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", HM.diagnostic,
          paste(HM.diagnose, HM.diagnostic, sep = " | "))
      }

      # Update best.flux.df
      best.flux.df.RMSE <- best.flux.df[[x]] %>%
        mutate(HM.diagnose = HM.diagnose, LM.diagnose = LM.diagnose,
               best.flux = best.flux, model = model,
               quality.check = quality.check)

      return(best.flux.df.RMSE)
    }

    best.flux.df <- lapply(seq_along(best.flux.df), best.flux.RMSE, best.flux.df)
  }

  ## MAE ####
  # Reflects the instrument precision.
  if(any(grepl("\\<MAE\\>", criteria))) {

    best.flux.MAE <- function(x, best.flux.df) {

      # best.flux variables
      LM.flux <- best.flux.df[[x]]$LM.flux
      best.flux <- best.flux.df[[x]]$best.flux
      model <- best.flux.df[[x]]$model
      HM.diagnose <- best.flux.df[[x]]$HM.diagnose
      LM.diagnose <- best.flux.df[[x]]$LM.diagnose
      quality.check <- best.flux.df[[x]]$quality.check

      # MAE variables
      HM.MAE <- signif(best.flux.df[[x]]$HM.MAE, 1)
      LM.MAE <- signif(best.flux.df[[x]]$LM.MAE, 1)
      prec <- best.flux.df[[x]]$prec

      # If HM.MAE <= prec, use HM.flux
      # else, if HM.MAE > prec, but HM.MAE <= LM.MAE, still use HM.flux
      # else, use LM.flux
      if(isTRUE(HM.MAE > prec & HM.MAE > LM.MAE)){
        best.flux <- LM.flux
        model <- "LM"
      }
      # diagnostic & quality
      quality <- "Noise (LM.MAE & HM.MAE)"
      LM.diagnostic <- "Noisy measurement (LM.MAE)"
      HM.diagnostic <- "Noisy measurement (HM.MAE)"

      if(isTRUE(LM.MAE > prec)){
        LM.diagnose <- ifelse(
          LM.diagnose == "", LM.diagnostic,
          paste(LM.diagnose, LM.diagnostic, sep = " | "))
      }
      if(isTRUE(HM.MAE > prec)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", HM.diagnostic,
          paste(HM.diagnose, HM.diagnostic, sep = " | "))
      }
      if(isTRUE(LM.MAE > prec & HM.MAE > prec)){
        quality.check <- ifelse(
          quality.check == "", quality,
          paste(quality.check, quality, sep = " | "))
      }
      if(isTRUE(LM.MAE > prec & HM.MAE <= prec)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", LM.diagnostic,
          paste(HM.diagnose, LM.diagnostic, sep = " | "))
      }
      if(isTRUE(LM.MAE <= prec & HM.MAE > prec)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", HM.diagnostic,
          paste(HM.diagnose, HM.diagnostic, sep = " | "))
      }

      # Update best.flux.df
      best.flux.df.MAE <- best.flux.df[[x]] %>%
        mutate(HM.diagnose = HM.diagnose, LM.diagnose = LM.diagnose,
               best.flux = best.flux, model = model,
               quality.check = quality.check)

      return(best.flux.df.MAE)
    }

    best.flux.df <- lapply(seq_along(best.flux.df), best.flux.MAE, best.flux.df)
  }

  ## SErel ####
  # Standard Error Relative (%). Calculated with deltamethod().
  # Default is 5%.

  if(any(grepl("\\<SErel\\>", criteria))) {

    best.flux.SErel <- function(x, best.flux.df, SErel) {

      # best.flux variables
      LM.flux <- best.flux.df[[x]]$LM.flux
      best.flux <- best.flux.df[[x]]$best.flux
      model <- best.flux.df[[x]]$model
      HM.diagnose <- best.flux.df[[x]]$HM.diagnose
      LM.diagnose <- best.flux.df[[x]]$LM.diagnose
      quality.check <- best.flux.df[[x]]$quality.check

      # SErel variables
      HM.se.rel <- signif(best.flux.df[[x]]$HM.se.rel, 1)
      LM.se.rel <- signif(best.flux.df[[x]]$LM.se.rel, 1)

      # If HM.se.rel <= SErel, use HM.flux
      # else, if HM.se.rel > SErel, but HM.se.rel <= LM.se.rel, still use HM.flux
      # else, use LM.flux
      if(isTRUE(HM.se.rel > SErel & HM.se.rel > LM.se.rel)){
        best.flux <- LM.flux
        model <- "LM"
      }
      # diagnostic & quality
      quality <- paste("SE rel. > ", SErel, "%)", sep = "")
      LM.diagnostic <- paste("Noisy meas. (LM.se.rel > ", SErel, "%)", sep = "")
      HM.diagnostic <- paste("Noisy meas. (HM.se.rel > ", SErel, "%)", sep = "")

      if(isTRUE(LM.se.rel > SErel)){
        LM.diagnose <- ifelse(
          LM.diagnose == "", LM.diagnostic,
          paste(LM.diagnose, LM.diagnostic, sep = " | "))
      }
      if(isTRUE(HM.se.rel > SErel)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", HM.diagnostic,
          paste(HM.diagnose, HM.diagnostic, sep = " | "))
      }
      if(isTRUE(LM.se.rel > SErel & HM.se.rel > SErel)){
        quality.check <- ifelse(
          quality.check == "", quality,
          paste(quality.check, quality, sep = " | "))
      }
      if(isTRUE(LM.se.rel > SErel & HM.se.rel <= SErel)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", LM.diagnostic,
          paste(HM.diagnose, LM.diagnostic, sep = " | "))
      }
      if(isTRUE(LM.se.rel <= SErel & HM.se.rel > SErel)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", HM.diagnostic,
          paste(HM.diagnose, HM.diagnostic, sep = " | "))
      }

      # Update best.flux.df
      best.flux.df.SErel <- best.flux.df[[x]] %>%
        mutate(HM.diagnose = HM.diagnose, LM.diagnose = LM.diagnose,
               best.flux = best.flux, model = model,
               quality.check = quality.check)

      return(best.flux.df.SErel)
    }

    best.flux.df <- lapply(seq_along(best.flux.df), best.flux.SErel,
                           best.flux.df, SErel)
  }

  ## G factor ####
  # Ratio between HM/LM. Default is 2
  if(any(grepl("\\<g.factor\\>", criteria))) {

    best.flux.g.fact <- function(x, best.flux.df, g.limit) {

      # best.flux variables
      LM.flux <- best.flux.df[[x]]$LM.flux
      best.flux <- best.flux.df[[x]]$best.flux
      model <- best.flux.df[[x]]$model
      HM.diagnose <- best.flux.df[[x]]$HM.diagnose
      quality.check <- best.flux.df[[x]]$quality.check

      # g-factor variables
      g.fact <- best.flux.df[[x]]$g.fact

      # If g.fact <= g.limit, use HM.flux
      # else, use LM.flux
      if(isTRUE(g.fact > g.limit)){
        best.flux <- LM.flux
        model <- "LM"
      }
      # diagnostic & quality
      quality <- paste("g-factor > ", g.limit, sep = "")
      diagnostic <- paste("Overestimation of flux (g-factor > ", g.limit, ")", sep = "")

      if(isTRUE(g.fact > g.limit)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", diagnostic,
          paste(HM.diagnose, diagnostic, sep = " | "))
        quality.check <- ifelse(
          quality.check == "", quality,
          paste(quality.check, quality, sep = " | "))
      }

      # Update best.flux.df
      best.flux.df.g <- best.flux.df[[x]] %>%
        mutate(HM.diagnose = HM.diagnose, best.flux = best.flux,
               model = model, quality.check = quality.check)

      return(best.flux.df.g)
    }

    best.flux.df <- lapply(seq_along(best.flux.df), best.flux.g.fact,
                           best.flux.df, g.limit)
  }

  ## kappa max ####
  # Maximal curvature allowed in non-linear regression (Hutchinson and Mosier)
  if(any(grepl("\\<kappa\\>", criteria))) {

    best.flux.kappa <- function(x, best.flux.df, k.ratio) {

      # best.flux variables
      LM.flux <- best.flux.df[[x]]$LM.flux
      best.flux <- best.flux.df[[x]]$best.flux
      model <- best.flux.df[[x]]$model
      HM.diagnose <- best.flux.df[[x]]$HM.diagnose
      quality.check <- best.flux.df[[x]]$quality.check

      # kappa variables
      HM.k <- best.flux.df[[x]]$HM.k
      k.max <- best.flux.df[[x]]$k.max

      # If abs(HM.k/k.max) <= k.ratio, use HM.flux
      # else, use LM.flux
      if(isTRUE(abs(HM.k/k.max) > k.ratio)){
        best.flux <- LM.flux
        model <- "LM"
      }
      # diagnostic & quality
      quality <- paste("kappa ratio > ", k.ratio*100, "%", sep = "")
      diagnostic <- paste("Exaggerated curvature (kappa ratio > ",
                          k.ratio*100, "%)", sep = "")

      if(isTRUE(abs(HM.k/k.max) > k.ratio)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", diagnostic,
          paste(HM.diagnose, diagnostic, sep = " | "))
        quality.check <- ifelse(
          quality.check == "", quality,
          paste(quality.check, quality, sep = " | "))
      }

      # Update best.flux.df
      best.flux.df.k <- best.flux.df[[x]] %>%
        mutate(HM.diagnose = HM.diagnose, best.flux = best.flux,
               model = model, quality.check = quality.check)

      return(best.flux.df.k)
    }

    best.flux.df <- lapply(seq_along(best.flux.df), best.flux.kappa,
                           best.flux.df, k.ratio)
  }

  ## p-value ####
  # Statistical limit of detectable flux. Default is p-value < 0.05
  if(any(grepl("\\<p-value\\>", criteria))) {

    best.flux.p.val <- function(x, best.flux.df, p.val) {

      # best.flux variables
      LM.diagnose <- best.flux.df[[x]]$LM.diagnose
      quality.check <- best.flux.df[[x]]$quality.check

      # LM p-value
      LM.p.val <- best.flux.df[[x]]$LM.p.val

      # diagnostic & quality
      quality <- "No detect. LM.flux (p-val)"
      diagnostic <- paste("No detectable flux (p-value > ", p.val, ")", sep = "")

      if(isTRUE(LM.p.val > p.val)){
        LM.diagnose <- ifelse(
          LM.diagnose == "", diagnostic,
          paste(LM.diagnose, diagnostic, sep = " | "))
        quality.check <- ifelse(
          quality.check == "", quality,
          paste(quality.check, quality, sep = " | "))
      }

      # Update best.flux.df
      best.flux.df.p <- best.flux.df[[x]] %>%
        mutate(LM.diagnose = LM.diagnose, quality.check = quality.check)

      return(best.flux.df.p)
    }

    best.flux.df <- lapply(seq_along(best.flux.df), best.flux.p.val,
                           best.flux.df, p.val)
  }

  ## MDF ####
  # Minimal detectable flux. Calculated with function MDF().
  # Based on instrument precision.
  if(any(grepl("\\<MDF\\>", criteria))) {

    best.flux.MDF <- function(x, best.flux.df) {

      # best.flux variables
      LM.flux <- best.flux.df[[x]]$LM.flux
      HM.flux <- best.flux.df[[x]]$HM.flux
      best.flux <- best.flux.df[[x]]$best.flux
      model <- best.flux.df[[x]]$model
      HM.diagnose <- best.flux.df[[x]]$HM.diagnose
      LM.diagnose <- best.flux.df[[x]]$LM.diagnose
      quality.check <- best.flux.df[[x]]$quality.check

      # MDF
      MDF <- best.flux.df[[x]]$MDF

      # If HM.flux => MDF, use HM.flux
      # else, if HM.flux < MDF, but LM.flux < MDF also, still use HM.flux
      # else, use LM.flux
      if(isTRUE(abs(HM.flux) < MDF & abs(LM.flux) >= MDF)){
        best.flux <- LM.flux
        model <- "LM"
      }
      # diagnostic & quality
      quality <- "No detectable flux (MDF)"
      LM.diagnostic <- "No detectable LM.flux (MDF)"
      HM.diagnostic <- "No detectable HM.flux (MDF)"

      if(isTRUE(abs(LM.flux) < MDF)){
        LM.diagnose <- ifelse(
          LM.diagnose == "", LM.diagnostic,
          paste(LM.diagnose, LM.diagnostic, sep = " | "))
      }
      if(isTRUE(abs(HM.flux) < MDF)){
        HM.diagnose <- ifelse(
          HM.diagnose == "", HM.diagnostic,
          paste(HM.diagnose, HM.diagnostic, sep = " | "))
      }
      if(isTRUE(abs(HM.flux) < MDF & abs(LM.flux) < MDF)){
        quality.check <- ifelse(
          quality.check == "", quality,
          paste(quality.check, quality, sep = " | "))
      }
      if(isTRUE(abs(HM.flux) < MDF & abs(LM.flux) >= MDF)){
        quality.check <- ifelse(
          quality.check == "", HM.diagnostic,
          paste(quality.check, HM.diagnostic, sep = " | "))
      }

      # Update best.flux.df
      best.flux.df.MDF <- best.flux.df[[x]] %>%
        mutate(HM.diagnose = HM.diagnose, LM.diagnose = LM.diagnose,
               best.flux = best.flux, model = model,
               quality.check = quality.check)

      return(best.flux.df.MDF)
    }

    best.flux.df <- lapply(seq_along(best.flux.df), best.flux.MDF, best.flux.df)
  }

  ## Intercept ####
  # Limits must have a minimum and a maximum value
  if(any(grepl("\\<intercept\\>", criteria))){

    best.flux.C0 <- function(x, best.flux.df, intercept.lim) {

      # best.flux variables
      HM.diagnose <- best.flux.df[[x]]$HM.diagnose
      LM.diagnose <- best.flux.df[[x]]$LM.diagnose
      quality.check <- best.flux.df[[x]]$quality.check

      # intercept variables
      HM.C0 <- best.flux.df[[x]]$HM.C0
      LM.C0 <- best.flux.df[[x]]$LM.C0
      if(is.null(intercept.lim)){
        C0 <- best.flux.df[[x]]$C0
        Ci <- best.flux.df[[x]]$Ci
        intercept.lim <- c(C0-(abs(Ci-C0)*0.2), C0+(abs(Ci-C0)*0.2))}

      # diagnostic & quality
      quality <- "Intercept (LM & HM)"
      LM.diagnostic <- "Intercept out of bounds (LM)"
      HM.diagnostic <- "Intercept out of bounds (HM)"

      if(!isTRUE(between(HM.C0, intercept.lim[1], intercept.lim[2]))){
        HM.diagnose <- ifelse(
          HM.diagnose == "", HM.diagnostic,
          paste(HM.diagnose, HM.diagnostic, sep = " | "))
        if(!isTRUE(between(LM.C0, intercept.lim[1], intercept.lim[2]))){
          quality.check <- ifelse(
            quality.check == "", quality,
            paste(quality.check, quality, sep = " | "))
        } else {
          quality.check <- ifelse(
            quality.check == "", HM.diagnostic,
            paste(quality.check, HM.diagnostic, sep = " | "))
        }
      }
      if(!isTRUE(between(LM.C0, intercept.lim[1], intercept.lim[2]))){
        LM.diagnose <- ifelse(
          LM.diagnose == "", LM.diagnostic,
          paste(LM.diagnose, LM.diagnostic, sep = " | "))
        if(isTRUE(between(HM.C0, intercept.lim[1], intercept.lim[2]))){
          quality.check <- ifelse(
            quality.check == "", LM.diagnostic,
            paste(quality.check, LM.diagnostic, sep = " | "))
        }
      }

      # Update best.flux.df
      best.flux.df.C0 <- best.flux.df[[x]] %>%
        mutate(HM.diagnose = HM.diagnose, LM.diagnose = LM.diagnose,
               quality.check = quality.check)

      return(best.flux.df.C0)
    }

    best.flux.df <- lapply(seq_along(best.flux.df), best.flux.C0,
                           best.flux.df, intercept.lim)
  }

  ## Number of observations ####
  if(any(grepl("\\<nb.obs\\>", criteria))) {

    best.flux.obs <- function(x, best.flux.df, warn.length) {

      # best.flux variables
      quality.check <- best.flux.df[[x]]$quality.check

      # nb.obs
      nb.obs <- best.flux.df[[x]]$nb.obs

      # diagnostic & quality
      quality <- paste("nb.obs <", warn.length)

      if(isTRUE(nb.obs < warn.length)){
        quality.check <- ifelse(
          quality.check == "", quality,
          paste(quality.check, quality, sep = " | "))
      }

      # Update best.flux.df
      best.flux.df.obs <- best.flux.df[[x]] %>%
        mutate(quality.check = quality.check)

      return(best.flux.df.obs)
    }

    best.flux.df <- lapply(seq_along(best.flux.df), best.flux.obs,
                           best.flux.df, warn.length)
  }

  best.flux.df <- best.flux.df %>% map_df(., ~as.data.frame(.x))

  return(best.flux.df)
}
