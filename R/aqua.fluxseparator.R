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
#' @param mybest.flux data.frame, output of the flux calculation over the entire
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
aqua.fluxseparator <- function(dataframe, gastype, auxfile, criteria, force.separation, mybest.flux){

  # dataframe <- mydata_all[mydata_all$UniqueID==unique(mydata_all$UniqueID)[2],]
  id = unique(dataframe$UniqueID)
  auxfile_corr <- auxfile[auxfile$UniqueID==id,]
  mybest.flux <- mybest.flux[mybest.flux$UniqueID == id,]

  # Initializing variables
  mybest.flux$total.flux <- mybest.flux$ebullition.flux <- mybest.flux$diffusion.flux <-
    mybest.flux$start_diffusion <- mybest.flux$obs.length_diffusion <-
    mybest.flux$total.flux.SD <- mybest.flux$diffusion.flux.SD <- mybest.flux$ebullition.flux.SD <- NA

  # if CH4 in dataframe, check bubbles
  if(any(grepl(paste0("\\<CH4dry_ppb\\>"), names(dataframe)))){
    bubbles <- find_bubbles(time = dataframe$Etime,
                            conc = dataframe$CH4dry_ppb, window.size = 10)
  } else {
    warning(paste0("For ",id, ", CH4 measurements are unavailable, `find_bubbles()` cannot look for potential bubbles.
                   `bubbles` was set to NULL"))
    bubbles = NULL
  }

  if (is.null(bubbles) & force.separation==FALSE){
    warning(paste0("For ",id, ", no bubbles were found. Total flux is attributed to diffusion only"))

    mybest.flux$start_diffusion <- auxfile_corr$start.time
    mybest.flux$obs.length_diffusion <- auxfile_corr$obs.length

    mybest.flux$total.flux <- mybest.flux$best.flux
    mybest.flux$ebullition.flux <- 0
    mybest.flux$diffusion.flux <- mybest.flux$best.flux

    SD_total.flux <- SD_diffusion.flux <- mybest.flux$LM.SE*sqrt(mybest.flux$nb.obs)
    SD_ebullition.flux <- NA

  } else {
    if (is.null(bubbles)){warning(paste0("For ",id, ", no bubbles were found but flux separation was forced with `force.separation = TRUE`.
                                         Consider setting force.separation to FALSE."))}

    dataframe$flag_bubbles <- FALSE
    for(b in seq_along(bubbles$start)){
      dataframe$flag_bubbles[seq(bubbles$start[b], bubbles$end[b])] <- TRUE
    }

    # Automatically identify the first linear chunk
    linear_chunk <- find_first_linear_chunk(dataframe = dataframe[!dataframe$flag_bubbles,], gastype = gastype, length.min = 20)

    # new start for dataframe
    dataframe <- dataframe[dataframe$POSIX.time>=linear_chunk$start.time,]
    dataframe$Etime <- as.numeric(dataframe$POSIX.time) - min(as.numeric(dataframe$POSIX.time))

    # computing density probability of first derivative
    d_df <- get_dCdt_density(dataframe, gastype)
    d <- d_df[[1]]
    mydf <- d_df[[2]]

    kstar = 0.5

    half_dens_max <- kstar*max(d$y)
    ind_over_half_dens_max <- which(d$y>half_dens_max)

    # we define lower and upper boundaries of the "main slope" as the slopes with
    # a density of probability higher than half of the maximum density
    lower_bound <- d$x[first(ind_over_half_dens_max)]
    upper_bound <- d$x[last(ind_over_half_dens_max)]

    avg_slope <- mean(d$x[ind_over_half_dens_max])
    sd_slope <- sd(d$x[ind_over_half_dens_max])

    delta_C <- last(mydf$concsmooth) - min(mydf$concsmooth)
    duration <- last(mydf$time)

    t.win <- 30
    C0 = min(mydf$concsmooth[mydf$time<t.win])
    Cf = max(mydf$concsmooth[mydf$time>max(mydf$time)-t.win])
    incubation_time = last(mydf$time)

    # for each incubation, extraCf data selected at previous step
    auxfile_corr$start.time <- linear_chunk$start.time
    auxfile_corr$obs.length <- linear_chunk$obs.length
    auxfile_corr$end.time <- auxfile_corr$start.time + auxfile_corr$obs.length


    drop <- c("Tcham","Pcham","Area","Vtot")
    dataframe_sel <- dplyr::select(dataframe, -which(names(dataframe) %in% drop))

    mydata_ow_corr <- my_obs.win(inputfile = dataframe_sel, auxfile = auxfile_corr, shoulder = 0)

    # Join mydata_ow with info on start end incubation
    mydiffusion_auto <- lapply(seq_along(mydata_ow_corr), join_auxfile_with_data.loop, flux.unique = mydata_ow_corr) %>%
      map_df(., ~as.data.frame(.x))

    # Calculate fluxes
    flux_diffusion <- goFlux(mydiffusion_auto, gastype)

    mybest.flux_diffusion <- best.flux(flux_diffusion, criteria)

    mybest.flux <- mybest.flux_diffusion
    # names(mybest.flux)[-1] <- paste0(names(mybest.flux)[-1],"_diffusion")

    # adding information of data used for diffusion model
    mybest.flux$start_diffusion <- linear_chunk$start.time
    mybest.flux$obs.length_diffusion <- linear_chunk$obs.length

    mybest.flux$total.flux <- (Cf-C0)/incubation_time*mybest.flux_diffusion$flux.term # nmol/m2/s
    mybest.flux$ebullition.flux <- mybest.flux$total.flux - mybest.flux_diffusion$best.flux # nmol/m2/s
    mybest.flux$diffusion.flux <- mybest.flux_diffusion$best.flux # nmol/m2/s

    # Error propagation (expressed as SD)
    SD_C0 <- sd(mydf$conc[mydf$time<t.win])
    SD_Cf <- sd(mydf$conc[mydf$time>max(mydf$time)-t.win])
    deltaconcs = Cf-C0
    SD_deltaconcs <- sqrt(SD_C0^2+SD_Cf^2)
    SD_total.flux <- abs(mybest.flux$total.flux) * SD_deltaconcs/deltaconcs
    if(mybest.flux_diffusion$model == "LM"){SE_diffusion.flux = mybest.flux_diffusion$LM.SE} else {SE_diffusion.flux = mybest.flux_diffusion$HM.SE}
    SD_diffusion.flux <- mybest.flux_diffusion$LM.SE*sqrt(mybest.flux_diffusion$nb.obs)
    SD_ebullition.flux <- sqrt(SD_diffusion.flux^2+SD_total.flux^2)

    # warnings
    if( mybest.flux$ebullition.flux < abs(mybest.flux$diffusion.flux+SD_diffusion.flux)){
      warning(paste0("for ",id, ", ebullition term is within range of uncertainty of diffusion."))
      mybest.flux$quality.check <- "ebullition too low to be trusted"
    }

    if( mybest.flux$ebullition.flux < 0){
      warning(paste0("for ",id, ", negative ebullition term. It was forced to 0."))
      mybest.flux$ebullition.flux <- 0
    }

    if( mybest.flux$diffusion.flux > mybest.flux$total.flux){
      warning(paste0("for ",id, ", diffusion term is larger than total flux estimated."))
      mybest.flux$quality.check <- "diffusion > total flux"
    }
  }

  mybest.flux$total.flux.SD <- SD_total.flux
  mybest.flux$diffusion.flux.SD <- SD_diffusion.flux
  mybest.flux$ebullition.flux.SD <- SD_ebullition.flux

  return(mybest.flux)
}

flux_separator.loop <-  function(x, list_of_dataframes, gastype, auxfile, criteria, force.separation, mybest.flux) {

  # function to apply in the loop. Adapt parameters to your needs.
  myseparated.flux <- flux_separator(dataframe = list_of_dataframes[[x]], gastype, auxfile, criteria, force.separation, mybest.flux)

  return(myseparated.flux)
}
