#' Automatic flux separation between diffusion and ebullition
#'
#' This function calculate diffusion and ebullition fluxes based on measurements
#' separation. First, if CH4 is available, the function uses \code{find_bubbles}
#' to check if it makes sense trying to separate fluxes. If \code{find_bubbles}
#' returns NULL, \code{flux_separator} automatically skips the flux separation
#' and calculates total fluxes as diffusion only.
#' If \code{find_bubbles} identifies probable bubbling events,
#' \code{flux_separator}
#' uses function \code{find_first_linear_chunk}
#' to identify which is the first linear chunk in the measurements. This will be used to calculate diffusion flux
#'  (\code{diffusion.flux}).
#'  Total flux (\code{total.flux}) is calculated as the total change of concentration
#' during the incubation.
#' Ebullition (\code{ebullition.flux}) is calculated as the difference between
#' \code{total.flux} and \code{diffusion.flux}.
#' If user specifies `force.separation = TRUE`, the separation is performed regardless
#' of what returned \code{find_bubbles}.
#'
#' @param dataframe a data.frame containing gas measurements (see \code{gastype}
#'                  below), water vapor measurements (see \code{H2O_col} below)
#'                  and the following columns: \code{UniqueID}, \code{Etime}, and
#'                  the precision of the instrument for each gas (see description below).
#' The precision of the instrument is needed to restrict kappa-max
#' (\code{\link[goFlux]{k.max}}) in the non-linear flux calculation
#' (\code{\link[goFlux]{HM.flux}}). Kappa-max is inversely proportional to
#' instrument precision. If the precision of your instrument is unknown, it is
#' better to use a low value (e.g. 1 ppm) to allow for more curvature, especially
#' for water vapor fluxes, or very long measurements, that are normally curved.
#' The default values given for instrument precision are the ones provided by
#' the manufacturer upon request, for the latest model of this instrument
#' available at the time of the creation of this function (11-2023).
#'
#' @param gastype character string; specifies which column should be used for the
#'                flux calculations. Must be one of the following: "CO2dry_ppm",
#'                "CH4dry_ppb", "COdry_ppb", "N2Odry_ppb", "NH3dry_ppb" or "H2O_ppm".
#' @param auxfile a data.frame containing auxiliary information needed with the
#'                  following columns: \code{UniqueID}, \code{start.time}, \code{obs.length},
#'                  \code{Vtot}, \code{Area}, \code{Pcham}, and \code{Tcham}.
#' @param criteria character vector; criteria used to assess the goodness of fit
#'                 of the LM or HM flux estimates. Must be at least one the
#'                 following: "MAE", "RMSE", "AICc", "SE", "g.factor", "kappa",
#'                 "MDF", "nb.obs", "p-value", or "intercept". The default is
#'                 all of them.
#' @param force.separation logical; if \code{fluxSeparation = TRUE}, the model proceeds with
#'                        automatic separation between diffusion and ebullition fluxes.
#' @param aqua.flux data.frame, output of the flux calculation over the entire
#' incubation time.
#'
#' @return Returns a data frame with a \code{UniqueID} per
#' measurement, and model outputs for the diffusion component: 11 columns for
#' the linear model results (linear flux estimate
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
#' kappa-max (\code{\link[goFlux]{k.max}}), the g factor (g.fact;
#' \code{\link[goFlux]{g.factor}}), the number of observations used
#' (\code{nb.obs}) and the true initial gas concentration (\code{C0}) and final
#' gas concentration (\code{Ct}).
#' When \code{fluxSeparation} was successfully performed, it also returns
#' information on flux separation, with flux value and
#' standard deviation estimated for total, diffusive, and ebullition fluxes, along with
#' the number of observations used for the calculatino of diffusion (\code{nb.obs}).
#'
#'
#' @examples
#' blabla
#'
#' @seealso See also the function \code{automaticflux} and
#' \code{clickflux} for more information about usage.
#'
#' @export
goAquaFlux <- function(dataframe, gastype, auxfile, criteria, force.separation){

  id = unique(dataframe$UniqueID)

  # Initializing variables
  aqua.flux <- NULL
  aqua.flux$total.flux <- aqua.flux$ebullition.flux <- aqua.flux$diffusive.flux <-
    aqua.flux$start_diffusion <- aqua.flux$obs.length_diffusion <-
    aqua.flux$total.flux.SE <- aqua.flux$diffusive.flux.SE <- aqua.flux$ebullition.flux.SE <- NA

  # if CH4 in dataframe, check bubbles
  if(any(grepl(paste0("\\<CH4dry_ppb\\>"), names(dataframe)))){
    bubbles <- find.bubbles(time = dataframe$Etime,
                            conc = dataframe$CH4dry_ppb, window.size = 10)
  } else {
    warning(paste0("For ",id, ", CH4 measurements are unavailable, `find_bubbles()` cannot look for potential bubbles.
                   `bubbles` was set to NULL"))
    bubbles = NULL
  }

  if (is.null(bubbles) & force.separation==FALSE){
    warning(paste0("For ",id, ", no bubbles were found. Total flux is attributed to diffusion only"))

    autoIDed <- autoID(inputfile = dataframe, auxfile = auxfile_corr, shoulder = 0)

    aqua.flux <- goFlux(autoIDed, gastype)

    best_aqua.flux <- best.flux(aqua.flux)

    # p <- flux.plot(flux.results = best_aqua.flux, dataframe = dataframe, gastype = gastype)
    # print(p)

    aqua.flux$start_diffusion <- auxfile_corr$start.time
    aqua.flux$obs.length_diffusion <- auxfile_corr$obs.length

    aqua.flux$total.flux <- aqua.flux$best.flux
    aqua.flux$ebullition.flux <- 0
    aqua.flux$diffusive.flux <- aqua.flux$best.flux

    SD_total.flux <- SD_diffusion.flux <- aqua.flux$LM.SE*sqrt(aqua.flux$nb.obs)
    SD_ebullition.flux <- NA

  } else {
    if (is.null(bubbles)){warning(paste0("For ",id, ", no bubbles were found but flux separation was forced with `force.separation = TRUE`.
                                         Consider setting force.separation to FALSE."))}

    # trim dataframe if bubbles are identified at the end of the incubation

    # MAKE CHANGES HERE



    # calculate total flux with 2-points method + uncertainty propagation axpressed as SE
    total.flux <- goAquaFlux.total(dataframe, gastype, auxfile, t.window = 30)

    # calculate diffusion using goFlux and best.flux on the observations available before any bubbling event
    diffusive.flux <- goAquaFlux.diffusive(dataframe, gastype, auxfile, bubbles, minimum_window = 30)

    # calculate ebullition by difference between the two terms: ebullition = total - diffusive
    # Errors are propagated to obtain ebullition flux SE
    ebullition.flux <- goAquaFlux.ebullition(total.flux, diffusive.flux)


    aqua.flux$total.flux <- total.flux$flux
    aqua.flux$ebullition.flux <- ebullition.flux$flux
    aqua.flux$diffusive.flux <- diffusive.flux$flux

    aqua.flux$total.flux.SE <- total.flux$SE
    aqua.flux$ebullition.flux.SE <- ebullition.flux$SE
    aqua.flux$diffusive.flux.SE <- diffusive.flux$SE
    aqua.flux$start_diffusion <- 0
    aqua.flux$obs.length_diffusion <- diffusive.flux$first_bubble_time

  }
  return(aqua.flux)
}

flux_separator.loop <-  function(x, list_of_dataframes, gastype, auxfile, criteria, force.separation, aqua.flux) {

  # function to apply in the loop. Adapt parameters to your needs.
  myseparated.flux <- flux_separator(dataframe = list_of_dataframes[[x]], gastype, auxfile, criteria, force.separation, aqua.flux)

  return(myseparated.flux)
}
