#' Automatic selection of best flux estimate
#'
#' This automatic selection of the best flux estimate (linear or non-linear) is
#' based on objective criteria and non-arbitrary thresholds.
#'
#' @param flux.result data.frame; output from goFlux()
#' @param gastype character string; same as used in goFlux. Must be one of the
#'                following: "CO2dry_ppm", "CH4dry_ppb", "N2Odry_ppb" or "H2O_ppm".
#' @param criteria character vector; criteria used to asses the goodness of fit
#'                 of the linear and non-linear flux estimates. Must be at least
#'                 one the following: criteria = c("g.factor", "kappa", "MDF",
#'                 "p-value", "R2", "intercept", "SE.rel"). Default is all of them.
#' @param intercept.lim numerical vector of length 2; inferior and superior
#'                      limits of the intercept (initial concentration). Same
#'                      units as gastype.
#' @param SE.rel numerical value; maximal standard error accepted (\%) on flux
#'               estimate. The default setting is 5\%.
#' @param g.limit numerical value; maximal limit of the the G-factor: the ratio
#'                between the result of a non-linear flux calculation model
#'                (e.g. Hutchinson and Mosier; HM) and the result of a linear
#'                flux calculation model. The threshold for the G-factor should
#'                be 4 (flexible), 2 (medium), or 1.25 (conservative). Default
#'                limit is set to 2.
#' @param p.val numerical value; a limit for a statistically non-zero flux.
#'              The default threshold is p-value < 0.05.
#' @param r2 numerical value; a maximal limit under which a warning is issued
#'           for quality checking. The default value is r2 < 0.6.
#'
#' @returns a data.frame
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso [goFlux()]
#' @seealso [flux.plot()]
#' @seealso [flux2pdf()]
#'
#' @examples
#' data(example_LGR_manID)
#' example_LGR_flux <- goFlux(example_LGR_manID, "CO2dry_ppm")
#' criteria <- c("g.factor", "kappa", "MDF", "R2", "SE.rel")
#' example_LGR_res <- best.flux(example_LGR_flux, "CO2dry_ppm", criteria)
#'
#' @export
#'
best.flux <- function(flux.result, gastype,
                      criteria = c("g.factor", "kappa", "MDF", "p-value",
                                   "R2", "intercept", "SE.rel"),
                      intercept.lim = NULL, SE.rel = 5, g.limit = 2,
                      p.val = 0.05, r2 = 0.6) {

  # Assign NULL to variables without binding
  g.fact <- HM.diagnose <- model <- quality.check <- HM.k <- LM.p.val <-
    LM.diagnose <- HM.se.rel <- LM.se.rel <- HM.C0 <- LM.intercept <- HM.R2 <-
    LM.R2 <- LM.se <- HM.se <- f.min <- NULL

  best.flux <- flux.result %>%
    mutate(HM.diagnose = "", LM.diagnose = "", best.flux = HM.flux,
           model = "HM", quality.check = "")

  # G factor ####
  # Ratio between HM/LM. Default is 2
  g.diagnostic <- paste("Overestimation of flux (G factor > ", g.limit, ")", sep = "")

  if (any(grepl("g.factor", criteria))) {
    best.flux <- best.flux %>%
      mutate(HM.diagnose = ifelse(g.fact > g.limit,
                                  g.diagnostic, HM.diagnose)) %>%
      mutate(best.flux = ifelse(HM.diagnose == g.diagnostic, LM.flux, best.flux),
             model = ifelse(HM.diagnose == g.diagnostic, "LM", model),
             quality.check = ifelse(HM.diagnose == g.diagnostic, g.diagnostic, quality.check))
  }

  # kappa max ####
  k.diagnostic <- "Overestimation of flux (kappa.max)"

  if (any(grepl("kappa", criteria))) {
    best.flux <- best.flux %>%
      mutate(HM.diagnose = ifelse(abs(HM.k) > abs(k.max),
                                  k.diagnostic, HM.diagnose)) %>%
      mutate(best.flux = ifelse(HM.diagnose == k.diagnostic, LM.flux, best.flux),
             model = ifelse(HM.diagnose == k.diagnostic, "LM", model),
             quality.check = ifelse(HM.diagnose == k.diagnostic, k.diagnostic, quality.check))
  }

  # p-value ####
  # Statistical limit of detectable flux. Default is p-value < 0.05
  p.diagnostic <- paste("No detectable flux (p-value > ", p.val, ")", sep = "")

  if (any(grepl("p-value", criteria))) {
    best.flux <- best.flux %>%
      mutate(LM.diagnose = ifelse(LM.p.val > p.val,
                                  p.diagnostic, LM.diagnose)) %>%
      mutate(best.flux = ifelse(LM.diagnose == p.diagnostic, 0, best.flux),
             model = ifelse(LM.diagnose == p.diagnostic, "No flux", model),
             quality.check = ifelse(LM.diagnose == p.diagnostic, p.diagnostic, quality.check))
  }

  # MDF ####
  # Minimal detectable flux. Calculated with function MDF().
  # Based on instrument precision.
  mdf.diagnostic = "No detectable flux (MDF)"
  mdf.quality = "Check plot (MDF)"

  if (any(grepl("MDF", criteria))) {
    best.flux <- best.flux %>%
      # HM
      mutate(HM.diagnose = ifelse(abs(HM.flux) < (f.min + abs(HM.se)),
                                  mdf.diagnostic, HM.diagnose)) %>%
      mutate(best.flux = ifelse(HM.diagnose == mdf.diagnostic, 0, best.flux),
             model = ifelse(HM.diagnose == mdf.diagnostic, "No flux", model),
             quality.check = ifelse(HM.diagnose == mdf.diagnostic, mdf.quality, quality.check)) %>%
      # LM
      mutate(LM.diagnose = ifelse(abs(LM.flux) < (f.min + abs(LM.se)),
                                  mdf.diagnostic, LM.diagnose)) %>%
      mutate(best.flux = ifelse(LM.diagnose == mdf.diagnostic, 0, best.flux),
             model = ifelse(LM.diagnose == mdf.diagnostic, "No flux", model),
             quality.check = ifelse(LM.diagnose == mdf.diagnostic, mdf.quality, quality.check))
  }

  # Quality check ####
  # The quality of the measurements needs to be verified graphically

  ## R2 ####
  r2.quality = paste("Check plot (R2 < ", r2, ")", sep = "")

  if (any(grepl("R2", criteria))) {
    best.flux <- best.flux %>%
      mutate(quality.check = ifelse(abs(LM.R2) < r2 | abs(HM.R2) < r2,
                                    r2.quality, quality.check))
  }

  ## Intercept ####
  # Limits must have a minimum and a maximum value
  int.quality <- "Intercept out of bounds (LM)"
  C0.quality <- "Intercept out of bounds (HM)"

  if (any(grepl("intercept", criteria)) & is.null(intercept.lim)) {
    warning("'intercept' was selected as a criteria, but 'intercept.lim' is NULL and has no default.",
            call. = F)
  }

  if (any(grepl("intercept", criteria)) & !is.null(intercept.lim)) {
    best.flux <- best.flux %>%
      mutate(quality.check = ifelse(between(LM.intercept, intercept.lim[1], intercept.lim[2]),
                                    quality.check, int.quality)) %>%
      mutate(quality.check = ifelse(between(HM.C0, intercept.lim[1], intercept.lim[2]),
                                    quality.check, C0.quality))
  }

  ## SE.rel ####
  # Standard Error Relative (%). Calculated with deltamethod().
  # Default is 5%. Check up plot if SE.rel is above threshold.

  SE.quality = paste("Check plot (SE.rel >", SE.rel, "%)")

  if (any(grepl("SE.rel", criteria))) {
    best.flux <- best.flux %>%
      mutate(quality.check = ifelse(abs(LM.se.rel) > SE.rel |
                                      abs(HM.se.rel) > SE.rel,
                                    SE.quality, quality.check))
  }

  ## RMSE ####
  # Should be based on the entire dataset

  return(best.flux)
}
