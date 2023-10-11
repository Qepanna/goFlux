#' Automatic selection of best flux estimate
#'
#' This automatic selection of the best flux estimate (linear or non-linear) is
#' based on objective criteria and non-arbitrary thresholds.
#'
#' @param flux.result data.frame; output from the function `goFlux()`.
#' @param criteria character vector; criteria used to asses the goodness of fit of the
#'                 linear and non-linear flux estimates. Must be at least one the following:
#'                 `criteria = c("RMSE", "g.factor", "kappa", "MDF", "p-value", "r2", "intercept", "SE.rel")`.
#'                 Default is all of them.
#' @param intercept.lim numerical vector of length 2; inferior and superior
#'                      limits of the intercept (initial concentration). Same
#'                      units as gastype.
#' @param SE.rel numerical value; maximal standard error accepted (\%) on flux
#'               estimate. The default setting is 5\%.
#' @param g.limit numerical value; maximal limit of the the g-factor, which is the
#'                ratio between the non-linear flux estimate (`HM.flux()`) and the
#'                linear flux estimate (`LM.flux()`). Recommended thresholds for
#'                the g-factor are 4 (flexible), 2 (medium), or 1.25 (conservative).
#'                Default limit `g.limit = 2`.
#' @param k.ratio numerical value; maximal limit of the ratio between kappa and
#'                the kappa-max. kappa-max is the maximal curvature (kappa)
#'                of the non-linear regression (Hutchinson and Mosier model)
#'                allowed for a each flux measurements. With `k.ratio`, one can
#'                chose to select the LM flux estimate instead of the non-linear
#'                flux estimate by giving a percentage of kappa-max. Default
#'                setting is `k.ratio = 1`.
#' @param p.val numerical value; a limit for a statistically non-zero flux.
#'              The default threshold is p-value < 0.05.
#' @param r2 numerical value; a maximal limit under which a warning is returned
#'           for quality checking. The default value is `r2 < 0.6`.
#'
#' @details
#' The function `k.max()` calculates the maximal curvature (kappa) of the non-linear
#' model (Hutchinson and Mosier) allowed for each flux measurements. k.max
#' is calculated based on the minimal detectable flux (MDF), the linear
#' flux estimate and the measurement time. The unit of the kappa-max is s-1.
#'
#' The function `MDF()` calculates the minimal detectable flux (MDF) based on
#' instrument precision, measurements time, and the number of measurement points.
#'
#' Another criteria, which is calculated automatically with the function `best.flux()`,
#' is the g-factor (`g.factor()`), which is the ratio between the HM flux estimate
#' and the LM flux estimate. The threshold for the g-factor should be 4 (flexible),
#' 2 (medium), or 1.25 (conservative). Default limit is set to 2.
#'
#' @returns a data.frame
#'
#' @include GoFluxYourself-package.R
#'
#' @examples
#' data(example_LGR_manID)
#' example_LGR_flux <- goFlux(example_LGR_manID, "CO2dry_ppm")
#' criteria <- c("g.factor", "kappa", "MDF", "r2", "SE.rel")
#' example_LGR_res <- best.flux(example_LGR_flux, "CO2dry_ppm", criteria)
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
                      criteria = c("RMSE", "g.factor", "kappa", "MDF", "p-value",
                                   "r2", "intercept", "SE.rel"),
                      intercept.lim = NULL, SE.rel = 5, g.limit = 2,
                      p.val = 0.05, r2 = 0.6, k.ratio = 1) {

  def.criteria <- c("RMSE", "g.factor", "kappa", "MDF", "p-value", "r2", "intercept", "SE.rel")
  if (!any(grepl(paste(def.criteria, collapse = "|"), criteria))) {
    warning("'criteria' must contain one of the following: 'g.factor', 'kappa', 'MDF', 'p-value', 'r2', 'intercept', 'SE.rel'",
            call. = F)
  }

  # Assign NULL to variables without binding
  g.fact <- HM.diagnose <- HM.RMSE <- prec <- model <- quality.check <- HM.k <- LM.p.val <-
    LM.diagnose <- HM.se.rel <- LM.se.rel <- HM.C0 <- LM.C0 <- HM.r2 <-
    LM.r2 <- LM.Ci <- LM.se <- HM.se <- MDF <- NULL

  # Assume that the best flux is HM.flux and leave *.diagnose empty
  best.flux <- flux.result %>%
    mutate(HM.diagnose = "", LM.diagnose = "", best.flux = HM.flux,
           model = "HM", quality.check = "")

  ## RMSE ####
  # Reflects the instrument precision. Use that as a threshold?
  if (any(grepl("RMSE", criteria))) {

    rmse.diagnostic <- paste("Noisy measurement (RMSE)")

    best.flux <- best.flux %>%
      mutate(HM.diagnose = ifelse(HM.RMSE > prec, ifelse(
        HM.diagnose == "", rmse.diagnostic,
        paste(HM.diagnose, rmse.diagnostic, sep = " | ")),
        HM.diagnose)) %>%
      mutate(best.flux = ifelse(HM.RMSE > prec, LM.flux, best.flux),
             model = ifelse(HM.RMSE > prec, "LM", model),
             quality.check = ifelse(HM.RMSE > prec, ifelse(
               quality.check == "", "RMSE",
               paste(quality.check, "RMSE", sep = " | ")),
               quality.check))
  }

  # G factor ####
  # Ratio between HM/LM. Default is 2
  if (any(grepl("g.factor", criteria))) {

    g.diagnostic <- paste("Overestimation of flux (g-factor > ", g.limit, ")", sep = "")

    best.flux <- best.flux %>%
      mutate(HM.diagnose = ifelse(g.fact > g.limit, ifelse(
        HM.diagnose == "", g.diagnostic,
        paste(HM.diagnose, g.diagnostic, sep = " | ")),
        HM.diagnose)) %>%
      mutate(best.flux = ifelse(g.fact > g.limit, LM.flux, best.flux),
             model = ifelse(g.fact > g.limit, "LM", model),
             quality.check = ifelse(g.fact > g.limit, ifelse(
               quality.check == "", "g.fact",
               paste(quality.check, "g.fact", sep = " | ")),
               quality.check))
  }

  # kappa max ####
  # Maximal curvature allowed in non-linear regression (Hutchinson and Mosier)
  if (any(grepl("kappa", criteria))) {

    k.diagnostic <- paste("Exaggerated curvature (kappa ratio > ",
                          k.ratio*100, "%)", sep = "")

    best.flux <- best.flux %>% mutate(
      HM.diagnose = ifelse(abs(HM.k/k.max) > k.ratio,
                           ifelse(HM.diagnose == "", k.diagnostic,
                                  paste(HM.diagnose, k.diagnostic, sep = " | ")),
                           HM.diagnose)) %>%
      mutate(best.flux = ifelse(abs(HM.k/k.max) > k.ratio, LM.flux, best.flux),
             model = ifelse(abs(HM.k/k.max) > k.ratio, "LM", model),
             quality.check = ifelse(abs(HM.k/k.max) > k.ratio,
                                    ifelse(quality.check == "", "kappa",
                                           paste(quality.check, "kappa", sep = " | ")),
                                    quality.check))
  }

  # p-value ####
  # Statistical limit of detectable flux. Default is p-value < 0.05
  if (any(grepl("p-value", criteria))) {

    p.diagnostic <- paste("No detectable flux (p-value > ", p.val, ")", sep = "")

    best.flux <- best.flux %>% mutate(
      LM.diagnose = ifelse(LM.p.val > p.val,
                           ifelse(LM.diagnose == "", p.diagnostic,
                                  paste(LM.diagnose, p.diagnostic, sep = " | ")),
                           LM.diagnose)) %>%
      mutate(best.flux = ifelse(LM.p.val > p.val, LM.flux, best.flux),
             model = ifelse(LM.p.val > p.val, "LM", model),
             quality.check = ifelse(LM.p.val > p.val,
                                    ifelse(quality.check == "", "No flux (p-val)",
                                           paste(quality.check, "No flux (p-val)", sep = " | ")),
                                    quality.check))
  }

  # MDF ####
  # Minimal detectable flux. Calculated with function MDF().
  # Based on instrument precision.
  if (any(grepl("MDF", criteria))) {

    mdf.diagnostic = "No detectable flux (MDF)"

    best.flux <- best.flux %>% mutate(
      # LM
      LM.diagnose = ifelse(abs(LM.flux) < (MDF + abs(LM.se)),
                           ifelse(LM.diagnose == "", mdf.diagnostic,
                                  paste(LM.diagnose, mdf.diagnostic, sep = " | ")),
                           LM.diagnose)) %>%
      mutate(best.flux = ifelse(abs(LM.flux) < (MDF + abs(LM.se)), LM.flux, best.flux),
             model = ifelse(abs(LM.flux) < (MDF + abs(LM.se)), "LM", model),
             quality.check = ifelse(abs(LM.flux) < (MDF + abs(LM.se)),
                                    ifelse(quality.check == "", "No flux (MDF)",
                                           paste(quality.check, "No flux (MDF)", sep = " | ")),
                                    quality.check)) %>%
      # HM
      mutate(HM.diagnose = ifelse(abs(HM.flux) < (MDF + abs(HM.se)),
                                  ifelse(HM.diagnose == "", mdf.diagnostic,
                                         paste(HM.diagnose, mdf.diagnostic, sep = " | ")),
                                  HM.diagnose)) %>%
      mutate(best.flux = ifelse(abs(HM.flux) < (MDF + abs(HM.se)), LM.flux, best.flux),
             model = ifelse(abs(HM.flux) < (MDF + abs(HM.se)), "LM", model),
             quality.check = ifelse(abs(HM.flux) < (MDF + abs(HM.se)),
                                    ifelse(quality.check == "", "No flux (MDF)",
                                           paste(quality.check, "No flux (MDF)", sep = " | ")),
                                    quality.check))
  }

  # Quality check ####
  # The quality of the measurements needs to be verified graphically

  ## r2 ####
  if (any(grepl("r2", criteria))) {

    LM.r2.quality = paste("Check plot (LM.r2 < ", r2, ")", sep = "")
    HM.r2.quality = paste("Check plot (HM.r2 < ", r2, ")", sep = "")

    best.flux <- best.flux %>%
      mutate(quality.check = ifelse(abs(LM.r2) < r2,
                                    ifelse(quality.check == "", LM.r2.quality,
                                           paste(quality.check, LM.r2.quality, sep = " | ")),
                                    quality.check),
             quality.check = ifelse(abs(HM.r2) < r2,
                                    ifelse(quality.check == "", HM.r2.quality,
                                           paste(quality.check, HM.r2.quality, sep = " | ")),
                                    quality.check))
  }

  ## Intercept ####
  # Limits must have a minimum and a maximum value
  LM.C0.quality <- "Intercept out of bounds (LM)"
  HM.C0.quality <- "Intercept out of bounds (HM)"

  if (any(grepl("intercept", criteria)) & !is.null(intercept.lim)) {
    best.flux <- best.flux %>%
      mutate(quality.check = ifelse(between(LM.C0, intercept.lim[1], intercept.lim[2]),
                                    quality.check, LM.C0.quality)) %>%
      mutate(quality.check = ifelse(between(HM.C0, intercept.lim[1], intercept.lim[2]),
                                    quality.check, HM.C0.quality))
  }

  if (any(grepl("intercept", criteria)) & is.null(intercept.lim)) {

    best.flux <- best.flux %>%
      mutate(quality.check = ifelse(
        between(HM.C0, LM.C0-(abs(LM.Ci-LM.C0)*0.2), LM.C0+(abs(LM.Ci-LM.C0)*0.2)),
        quality.check, HM.C0.quality))
  }

  ## SE.rel ####
  # Standard Error Relative (%). Calculated with deltamethod().
  # Default is 5%. Check up plot if SE.rel is above threshold.

  if (any(grepl("SE.rel", criteria))) {

    LM.SE.quality = paste("Check plot (LM.SE > ", SE.rel, "%)", sep = "")
    HM.SE.quality = paste("Check plot (HM.SE > ", SE.rel, "%)", sep = "")

    best.flux <- best.flux %>%
      mutate(quality.check = ifelse(abs(LM.se.rel) > SE.rel,
                                    ifelse(quality.check == "", LM.SE.quality,
                                           paste(quality.check, LM.SE.quality, sep = " | ")),
                                    quality.check),
             quality.check = ifelse(abs(HM.se.rel) > SE.rel,
                                    ifelse(quality.check == "", HM.SE.quality,
                                           paste(quality.check, HM.SE.quality, sep = " | ")),
                                    quality.check))
    }

  return(best.flux)
}
