#' Plots for quality checking of GHG flux measurements
#'
#' Returns a list plots, drawn from flux restults (output from the functions
#' \code{\link[GoFluxYourself]{goFlux}} and \code{\link[GoFluxYourself]{best.flux}}).
#' The plots are customizable.
#'
#' @param flux.results a data.frame; output from the function
#'                     \code{\link[GoFluxYourself]{best.flux}}
#' @param dataframe a data.frame containing gas measurements (see \code{gastype}
#'                  below) and the following columns: \code{UniqueID}, \code{Etime}
#'                  and \code{flag} (same \code{dataframe} as used with the function
#'                  \code{\link[GoFluxYourself]{goFlux}}).
#' @param gastype character string; specifies which column was used for the
#'                flux calculations. Must be one of the following: "CO2dry_ppm",
#'                "CH4dry_ppb", "N2Odry_ppb" or "H2O_ppm".
#' @param shoulder numerical value; time before and after measurement in observation
#'                 window (seconds). Default is 30 seconds.
#' @param plot.legend character vector; specifies which parameters should be
#'                    displayed in a legend above each plot. "flux" is always
#'                    displayed. A maximum of 6 parameters can be displayed in
#'                    the legend (including "flux"). Chose up to five extra
#'                    parameters from the following: "MAE", "RMSE", "SErel",
#'                    "SE", "r2", "LM.p.val", "HM.k", "k.max", "k.ratio" and
#'                    "g.factor". Default is \code{plot.legend = c("MAE", "RMSE")}.
#' @param plot.display character vector; specifies which parameters should be
#'                     displayed on the plot. Chose from the following: "C0",
#'                     "Ci", "cham.close", "cham.open", "crop", "MDF", "nb.obs",
#'                     "flux.term" and "prec". Default is
#'                     \code{plot.display = c("cham.close", "cham.open")}.
#' @param quality.check logical; if \code{quality.check = TRUE}, the column
#'                      \code{quality.check} (output from the function
#'                      \code{\link[GoFluxYourself]{best.flux}}) is displayed
#'                      below the plot.
#' @param flux.unit character string; flux units to be displayed on the plots.
#'                  By default, the units are
#'                  \ifelse{html}{\out{µmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{µmol m^{-2}s^{-1}}{ASCII}}
#'                  (if initial concentration is ppm, e.g. CO2dry_ppm) and
#'                  \ifelse{html}{\out{nmol m<sup>-2</sup>s<sup>-1</sup>}}{\eqn{nmol m^{-2}s^{-1}}{ASCII}}
#'                  (if initial concentration is ppb, e.g. CH4dry_ppb). \cr
#'                  For example, one may want to use
#'                  \ifelse{html}{\out{nmol kg<sup>-1</sup>h<sup>-1</sup>}}{\eqn{µmol kg^{-1}h^{-1}}{ASCII}}
#'                  for incubated soil samples. In such a case, write
#'                  \code{flux.unit = "nmol~kg^-1*h^-1"}.
#' @param flux.term.unit character string; units for the flux term to be displayed
#'                       on the plots. By default, the units are
#'                       \ifelse{html}{\out{mol m<sup>-2</sup>}}{\eqn{mol m^{-2}}{ASCII}}:
#'                       \code{flux.term.unit = "mol~m^-2"}
#' @param best.model logical; if \code{best.model = TRUE}, display a star sign
#'                   next to the best model selected by the function
#'                   \code{\link[GoFluxYourself]{best.flux}}.
#' @param p.val.disp character string; how should the p-value be displayed in the
#'                   legend above the plot. Chose one of the following: "star",
#'                   "round", "value".
#'
#' @details
#' In \code{plot.legend}, one may chose to display up to 5 additional parameters
#' in a legend above the plots. Some parameters are displayed for both the linear
#' model (\code{\link[GoFluxYourself]{HM.flux}}) and the non-linear model
#' (\code{\link[GoFluxYourself]{HM.flux}}): Mean Absolute Error (\code{MAE}),
#' Root Mean Square Error (\code{RMSE}), Standard Error (\code{SE}), relative
#' Standard Error (\code{SErel}), and coefficient of determination (\code{r2}).
#' The p-value (\code{LM.p.val}) is displayed for the linear model only. The kappa
#' (\code{HM.k}), kappa-max (\code{\link[GoFluxYourself]{k.max}}), kappa ratio
#' (\code{k.ratio}) and g-factor (\code{\link[GoFluxYourself]{g.factor}}) are
#' displayed for the Hutchinson and Mosier model only. One may chose to display
#' no additional parameter with \code{plot.legend = NULL}.
#'
#' In \code{plot.display}, one may chose to display some parameters on the plot:
#' The initial and final gas concentrations (\code{C0} and \code{Ci}) for both
#' models, the number of observations (\code{nb.obs}) flagged, the Minimal
#' Detectable Flux (\code{\link[GoFluxYourself]{MDF}}), the flux term
#' (\code{\link[GoFluxYourself]{flux.term}}), the instrument precision (prec),
#' the chamber closure (\code{cham.close}) and opening (\code{cham.open})
#' (indicated with a green star), and the data points between chamber closure
#' and opening that have been removed (\code{crop}) (indicated in light red).
#' For manual chamber measurements, because there is no automatic chamber closure
#' and opening, no green stars can be displayed. In addition, \code{crop} is
#' only relevant if data points have been removed with the function
#' \code{crop.meas()} (this function is not available yet).
#' One may chose to display none of these parameters with \code{plot.display = NULL}.
#' The order in which \code{prec}, \code{flux.term}, \code{MDF} and \code{nb.obs}
#' are put in \code{plot.display = c()} decides the order in which they are
#' displayed at the bottom of the plot.
#'
#' In \code{flux.unit}, remember to multiply the flux results with an appropriate
#' factor to convert the results from a unit to another. If kilograms of soil
#' were used to calculate the fluxes (see the details section of the function
#' \code{\link[GoFluxYourself]{goFlux}}), the units would be
#' \ifelse{html}{\out{µmol kg<sup>-1</sup>h<sup>-1</sup>}}{\eqn{µmol kg^{-1}s^{-1}}{ASCII}}.
#' To convert the units to
#' \ifelse{html}{\out{µmol kg<sup>-1</sup>h<sup>-1</sup>}}{\eqn{µmol kg^{-1}h^{-1}}{ASCII}}
#' instead, one would need to multiply the flux results by 3600 to convert from
#' seconds to hours. To print non-ASCII characters use Unicode. For example, to
#' print the Greek letter "mu" (\eqn{µ}), use the Unicode \code{\\u03BC}:
#' \code{flux.unit = "\\u03BCmol~kg^-1*h^-1"}.
#'
#' In \code{p.val.disp}, if \code{p.val.disp = "star"}, the p-values will be
#' displayed as star symbols (asterisks) as follows: ***, ** or * for p-values
#' of p < 0.001, p < 0.01 and p < 0.05, respectively. If \code{p.val.disp = "round"},
#' the p-values are rounded to p < 0.001, p < 0.01 and p < 0.05. If
#' \code{p.val.disp = "value"}, the actual values are displayed, rounded to two
#' significant numbers.
#'
#' @return a list of plots, one per UniqueID, drawn from flux restults (output
#' from the functions #' \code{\link[GoFluxYourself]{goFlux}} and
#' \code{\link[GoFluxYourself]{best.flux}}).
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso See also the functions \code{\link[GoFluxYourself]{goFlux}},
#'          \code{\link[GoFluxYourself]{best.flux}} and
#'          \code{\link[GoFluxYourself]{flux2pdf}}
#'          for more information about usage.
#'
#' @seealso Look up the functions \code{\link[GoFluxYourself]{g.factor}},
#'          \code{\link[GoFluxYourself]{k.max}},
#'          \code{\link[GoFluxYourself]{MDF}},
#'          \code{\link[GoFluxYourself]{flux.term}},
#'          \code{\link[GoFluxYourself]{LM.flux}} and
#'          \code{\link[GoFluxYourself]{HM.flux}} for more information about
#'          these parameters.
#'
#' @examples
#' data(LGR_manID)
#' LGR_flux <- goFlux(LGR_manID, "CO2dry_ppm")
#' criteria <- c("MAE", "g.factor", "MDF", "SErel")
#' LGR_res <- best.flux(LGR_flux, criteria)
#' LGR_plots <- flux.plot(
#'   flux.results = LGR_res, dataframe = LGR_manID,
#'   gastype = "CO2dry_ppm", quality.check = TRUE,
#'   plot.legend = c("MAE", "RMSE", "k.ratio", "g.factor", "SErel"),
#'   plot.display = c("Ci", "C0", "MDF", "prec", "nb.obs", "flux.term"))
#'
#' @export
#'
flux.plot <- function(flux.results, dataframe, gastype, shoulder = 30,
                      plot.legend = c("MAE", "RMSE"),
                      plot.display = c("MDF", "prec"),
                      quality.check = TRUE, flux.unit = NULL,
                      flux.term.unit = NULL, best.model = TRUE,
                      p.val.disp = "round") {

  # Check arguments ####
  if(is.null(shoulder)) stop("'shoulder' is required") else{
    if(!is.numeric(shoulder)) stop("'shoulder' must be of class numeric") else{
      if(shoulder < 0) stop("'shoulder' cannot be a negative value")}}

  ## Check dataframe ####
  if(missing(dataframe)) stop("'dataframe' is required")
  if(!is.null(dataframe) & !is.data.frame(dataframe)){
    stop("'dataframe' must be of class data.frame")}

  ### gastype and match in dataframe ####
  if(missing(gastype)) stop("'gastype' is required")
  if(!is.null(gastype) & !is.character(gastype)) stop("'gastype' must be a character string")
  if(!any(grepl(paste("\\<", gastype, "\\>", sep = ""),
                c("CO2dry_ppm", "CH4dry_ppb", "N2Odry_ppb", "H2O_ppm")))){
    stop("'gastype' must be one of the following: 'CO2dry_ppm', 'CH4dry_ppb', 'N2Odry_ppb' or 'H2O_ppm'")}
  if(!any(grepl(paste("\\<", gastype, "\\>", sep = ""), names(dataframe)))){
    stop("'dataframe' must contain a column that matches 'gastype'")}
  if(any(grepl(paste("\\<", gastype, "\\>", sep = ""), names(dataframe))) &
     !is.numeric(dataframe[,gastype][[1]])){
    stop("The column that matches 'gastype' in 'dataframe' must be of class numeric")}

  ### Etime and flag ####
  if(!any(grepl("\\<Etime\\>", names(dataframe)))) stop("'dataframe' must contain 'Etime'")
  if(any(grepl("\\<Etime\\>", names(dataframe))) & !is.numeric(dataframe$Etime)){
    stop("'Etime' in 'dataframe' must be of class numeric (or integer)")}

  if(!any(grepl("\\<flag\\>", names(dataframe)))) stop("'dataframe' must contain 'flag'")
  if(any(grepl("\\<flag\\>", names(dataframe))) & !is.numeric(dataframe$flag)){
    stop("'flag' in 'dataframe' must be of class numeric (or integer)")}

  ### POSIX.time ####
  if(!any(grepl("\\<POSIX.time\\>", names(dataframe)))) stop("'dataframe' must contain 'POSIX.time'")
  if(any(grepl("\\<POSIX.time\\>", names(dataframe))) & !is.POSIXct(dataframe$POSIX.time)){
    stop("'POSIX.time' in 'dataframe' must be of class POSIXct")}

  ### UniqueID ####
  if(!any(grepl("\\<UniqueID\\>", names(dataframe)))){
    stop("'UniqueID' is required and was not found in 'dataframe'")}

  ## Check flux.results ####
  if(missing(flux.results)) stop("'flux.results' is required")
  if(!is.null(flux.results) & !is.data.frame(flux.results)){
    stop("'flux.results' must be of class data.frame")}

  ### UniqueID ####
  if(!any(grepl("\\<UniqueID\\>", names(flux.results)))){
    stop("'UniqueID' is required and was not found in 'flux.results'")}

  ### Is there any match between dataframe and flux.results?
  if(!any(unique(dataframe$UniqueID) %in% unique(flux.results$UniqueID))){
    stop("'UniqueID' in 'flux.results' has no match for 'UniqueID' in 'dataframe'")}

  ### LM results ####
  ### LM.flux
  if(!any(grepl("\\<LM.flux\\>", names(flux.results)))){
    stop("'LM.flux' required in 'flux.results'")
  } else if(!is.numeric(flux.results$LM.flux)){
    stop("'LM.flux' in 'flux.results' must be of class numeric")}
  ### LM.C0
  if(!any(grepl("\\<LM.C0\\>", names(flux.results)))){
    stop("'LM.C0' required in 'flux.results'")
  } else if(!is.numeric(flux.results$LM.C0)){
    stop("'LM.C0' in 'flux.results' must be of class numeric")}
  ### LM.slope
  if(!any(grepl("\\<LM.slope\\>", names(flux.results)))){
    stop("'LM.slope' required in 'flux.results'")
  } else if(!is.numeric(flux.results$LM.slope)){
    stop("'LM.slope' in 'flux.results' must be of class numeric")}

  ### HM results ####
  ### HM.flux
  if(!any(grepl("\\<HM.flux\\>", names(flux.results)))){
    stop("'HM.flux' required in 'flux.results'")
  } else if(!is.numeric(flux.results$HM.flux)){
    stop("'HM.flux' in 'flux.results' must be of class numeric")}
  ### HM.C0
  if(!any(grepl("\\<HM.C0\\>", names(flux.results)))){
    stop("'HM.C0' required in 'flux.results'")
  } else if(!is.numeric(flux.results$HM.C0)){
    stop("'HM.C0' in 'flux.results' must be of class numeric")}
  ### HM.Ci
  if(!any(grepl("\\<HM.Ci\\>", names(flux.results)))){
    stop("'HM.Ci' required in 'flux.results'")
  } else if(!is.numeric(flux.results$HM.Ci)){
    stop("'HM.Ci' in 'flux.results' must be of class numeric")}
  ### HM.k
  if(!any(grepl("\\<HM.k\\>", names(flux.results)))){
    stop("'HM.k' required in 'flux.results'")
  } else if(!is.numeric(flux.results$HM.k)){
    stop("'HM.k' in 'flux.results' must be of class numeric")}

  ## Check plot.legend ####
  plot.legend.all <- c("MAE", "RMSE", "SErel", "SE", "r2", "LM.p.val", "HM.k",
                       "k.max", "k.ratio", "g.factor", "best.model")
  if(!is.null(plot.legend)){
    if(!is.character(plot.legend)){
      stop("'plot.legend' must be of class character")
    } else if(length(plot.legend) > 5){
      stop("in 'plot.legend': A maximum of 5 additional parameters can be displayed above the plot.")
    } else if(!any(grepl(paste(paste("\\<", plot.legend.all, "\\>", sep = ""),
                               collapse = "|"), plot.legend))){
      stop("if 'plot.legend' is not NULL, it must contain at least one of the following: 'MAE', 'RMSE', 'SErel', 'SE', 'r2', 'LM.p.val', 'HM.k', 'k.max', 'k.ratio', 'g.factor', 'best.model'")
    }
    ### MAE ####
    if(any(grepl("\\<MAE\\>", plot.legend))){
      if(!any(grepl("\\<LM.MAE\\>", names(flux.results)))){
        stop("'MAE' selected in 'plot.legend', but 'LM.MAE' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$LM.MAE)){
        stop("'LM.MAE' in 'flux.results' must be of class numeric")}
      if(!any(grepl("\\<HM.MAE\\>", names(flux.results)))){
        stop("'MAE' selected in 'plot.legend', but 'HM.MAE' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$HM.MAE)){
        stop("'HM.MAE' in 'flux.results' must be of class numeric")}
    }
    ### RMSE ####
    if(any(grepl("\\<RMSE\\>", plot.legend))){
      if(!any(grepl("\\<LM.RMSE\\>", names(flux.results)))){
        stop("'RMSE' selected in 'plot.legend', but 'LM.RMSE' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$LM.RMSE)){
        stop("'LM.RMSE' in 'flux.results' must be of class numeric")}
      if(!any(grepl("\\<HM.RMSE\\>", names(flux.results)))){
        stop("'RMSE' selected in 'plot.legend', but 'HM.RMSE' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$HM.RMSE)){
        stop("'HM.RMSE' in 'flux.results' must be of class numeric")}
    }
    ### SErel ####
    if(any(grepl("\\<SErel\\>", plot.legend))){
      if(!any(grepl("\\<LM.se.rel\\>", names(flux.results)))){
        stop("'SErel' selected in 'plot.legend', but 'LM.se.rel' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$LM.se.rel)){
        stop("'LM.se.rel' in 'flux.results' must be of class numeric")}
      if(!any(grepl("\\<HM.se.rel\\>", names(flux.results)))){
        stop("'SErel' selected in 'plot.legend', but 'HM.se.rel' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$HM.se.rel)){
        stop("'HM.se.rel' in 'flux.results' must be of class numeric")}
    }
    ### SE ####
    if(any(grepl("\\<SE\\>", plot.legend))){
      if(!any(grepl("\\<LM.se\\>", names(flux.results)))){
        stop("'SE' selected in 'plot.legend', but 'LM.se' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$LM.se)){
        stop("'LM.se' in 'flux.results' must be of class numeric")}
      if(!any(grepl("\\<HM.se\\>", names(flux.results)))){
        stop("'SE' selected in 'plot.legend', but 'HM.se' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$HM.se)){
        stop("'HM.se' in 'flux.results' must be of class numeric")}
    }
    ### r2 ####
    if(any(grepl("\\<r2", plot.legend))){
      if(!any(grepl("\\<LM.r2\\>", names(flux.results)))){
        stop("'r2' selected in 'plot.legend', but 'LM.r2' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$LM.r2)){
        stop("'LM.r2' in 'flux.results' must be of class numeric")}
      if(!any(grepl("\\<HM.r2\\>", names(flux.results)))){
        stop("'r2' selected in 'plot.legend', but 'HM.r2' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$HM.r2)){
        stop("'HM.r2' in 'flux.results' must be of class numeric")}
    }
    ### LM.p.val ####
    if(any(grepl("\\<LM.p.val\\>", plot.legend))){
      if(!any(grepl("\\<LM.p.val\\>", names(flux.results)))){
        stop("'LM.p.val' selected in 'plot.legend', but missing in 'flux.results'")
      } else if(!is.numeric(flux.results$LM.p.val)){
        stop("'LM.p.val' in 'flux.results' must be of class numeric")}
    }
    ### HM.k ####
    if(any(grepl("\\<HM.k", plot.legend))){
      if(!any(grepl("\\<HM.k\\>", names(flux.results)))){
        stop("'HM.k' selected in 'plot.legend', but missing in 'flux.results'")
      } else if(!is.numeric(flux.results$HM.k)){
        stop("'HM.k' in 'flux.results' must be of class numeric")}
    }
    ### k.max ####
    if(any(grepl("\\<k.max", plot.legend))){
      if(!any(grepl("\\<k.max\\>", names(flux.results)))){
        stop("'k.max' selected in 'plot.legend', but missing in 'flux.results'")
      } else if(!is.numeric(flux.results$k.max)){
        stop("'k.max' in 'flux.results' must be of class numeric")}
    }
    ### k.ratio ####
    if(any(grepl("\\<k.ratio", plot.legend))){
      if(!any(grepl("\\<k.max\\>", names(flux.results)))){
        stop("'k.ratio' selected in 'plot.legend', but 'k.max' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$k.max)){
        stop("'k.max' in 'flux.results' must be of class numeric")}
      if(!any(grepl("\\<HM.k\\>", names(flux.results)))){
        stop("'k.ratio' selected in 'plot.legend', but 'HM.k' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$HM.k)){
        stop("'HM.k' in 'flux.results' must be of class numeric")}
    }
    ### g.factor ####
    if(any(grepl("\\<g.factor", plot.legend))){
      if(!any(grepl("\\<g.fact\\>", names(flux.results)))){
        stop("'g.factor' selected in 'plot.legend', but 'g.fact' missing in 'flux.results'")
      } else if(!is.numeric(flux.results$g.fact)){
        stop("'g.fact' in 'flux.results' must be of class numeric")}
    }
  }
  ## Check plot.display ####
  plot.display.all <- c("Ci", "C0", "cham.close", "cham.open", "nb.obs", "crop",
                        "prec", "flux.term", "MDF")
  if(!is.null(plot.display)){
    if(!is.character(plot.display)){
      stop("'plot.display' must be of class character")
    } else if(!any(grepl(paste(paste("\\<", plot.display.all, "\\>", sep = ""),
                               collapse = "|"), plot.display))){
      stop("if 'plot.display' is not NULL, it must contain at least one of the following: 'Ci', 'C0', 'cham.close', 'cham.open', 'nb.obs', 'crop', 'prec', 'flux.term', 'MDF'")
    }
    ### cham.close ####
    if(any(grepl("\\<cham.close\\>", plot.display))){
      if(!any(grepl("\\<cham.close\\>", names(dataframe)))){
        stop("'cham.close' selected in 'plot.display', but missing in 'dataframe'")
      } else if(!is.POSIXct(dataframe$cham.close)){
        stop("'cham.close' in 'dataframe' must be of class POSIXct")}
    }
    ### cham.open ####
    if(any(grepl("\\<cham.open\\>", plot.display))){
      if(!any(grepl("\\<cham.open\\>", names(dataframe)))){
        stop("'cham.open' selected in 'plot.display', but missing in 'dataframe'")
      } else if(!is.POSIXct(dataframe$cham.open)){
        stop("'cham.open' in 'dataframe' must be of class POSIXct")}
    }
    ### nb.obs ####
    if(any(grepl("\\<nb.obs\\>", plot.display))){
      if(!any(grepl("\\<nb.obs\\>", names(flux.results)))){
        stop("'nb.obs' selected in 'plot.display', but missing in 'flux.results'")
      } else if(!is.numeric(flux.results$nb.obs)){
        stop("'nb.obs' in 'flux.results' must be of class numeric")}
    }
    ### crop ####
    if(any(grepl("\\<crop\\>", plot.display))){
      if(nrow(dataframe %>% filter(flag == 2)) == 0){
        if(!any(grepl("\\<cham.open\\>", names(dataframe)))){
          stop("'crop' selected in 'plot.display', but 'cham.open' missing in 'dataframe'")
        } else if(!is.POSIXct(dataframe$cham.open)){
          stop("'cham.open' in 'dataframe' must be of class POSIXct")}
        if(!any(grepl("\\<cham.close\\>", names(dataframe)))){
          stop("'crop' selected in 'plot.display', but 'cham.close' missing in 'dataframe'")
        } else if(!is.POSIXct(dataframe$cham.close)){
          stop("'cham.close' in 'dataframe' must be of class POSIXct")}
      }
    }
    ### prec ####
    if(any(grepl("\\<prec\\>", plot.display))){
      if(!any(grepl("\\<prec\\>", names(flux.results)))){
        stop("'prec' selected in 'plot.display', but missing in 'flux.results'")
      } else if(!is.numeric(flux.results$prec)){
        stop("'prec' in 'flux.results' must be of class numeric")}
    }
    ### flux.term ####
    if(any(grepl("\\<flux.term\\>", plot.display))){
      if(!any(grepl("\\<flux.term\\>", names(flux.results)))){
        stop("'flux.term' selected in 'plot.display', but missing in 'flux.results'")
      } else if(!is.numeric(flux.results$flux.term)){
        stop("'flux.term' in 'flux.results' must be of class numeric")}
    }
    ### MDF ####
    if(any(grepl("\\<MDF\\>", plot.display))){
      if(!any(grepl("\\<MDF\\>", names(flux.results)))){
        stop("'MDF' selected in 'plot.display', but missing in 'flux.results'")
      } else if(!is.numeric(flux.results$MDF)){
        stop("'MDF' in 'flux.results' must be of class numeric")}
    }
  }
  ## Check quality.check ####
  if(quality.check != TRUE & quality.check != FALSE){
    stop("'quality.check' must be TRUE or FALSE")}
  if(isTRUE(quality.check)){
    if(!any(grepl("\\<quality.check\\>", names(flux.results)))){
      stop("'quality.check' is TRUE, but missing in 'flux.results'")
    } else if(!is.character(flux.results$quality.check)){
      stop("'quality.check' in 'flux.results' must be of class character")}
  }
  ## Check best.model ####
  if(best.model != TRUE & best.model != FALSE){
    stop("'best.model' must be TRUE or FALSE")}
  if(isTRUE(best.model)){
    if(!any(grepl("\\<model\\>", names(flux.results)))){
      stop("'best.model' selected in 'plot.legend', but 'model' missing in 'flux.results'")
    } else if(!is.character(flux.results$model)){
      stop("'model' in 'flux.results' must be of class character")}
  }
  ## flux.term and flux units ####
  if(!is.null(flux.unit)){
    if(!is.character(flux.unit)) stop("'flux.unit' must be of class character")}
  if(!is.null(flux.term.unit)){
    if(!is.character(flux.term.unit)) stop("'flux.term.unit' must be of class character")}

  ## p.val.disp ####
  if(is.null(p.val.disp)) {stop("'p.val.disp' cannot be NULL")
  } else if(!is.character(p.val.disp)){
    stop("'p.val.disp' must be of class character")} else {
      if(!any(grepl(paste("\\<", p.val.disp, "\\>", sep = ""), c("star", "round", "value")))){
        stop("'p.val.disp' must be one of the following: 'star', 'round' or 'value'")}
    }

  # Assign NULL to variables without binding ####
  UniqueID <- HM.Ci <- HM.C0 <- HM.k <- . <- flag <- start.Etime <-
    end.Etime <- Etime <- x <- y <- content <- color <- POSIX.time <-
    HM.C0.display <- LM.C0.display <- quality.check.display <-
    cham.open.display <- nb.obs.display <- prec.display <-
    flux.term.display <- cham.close.display <- MDF.display <-
    HM.Ci.display <- legend.flux <- legend.MAE <-
    legend.RMSE <- legend.se.rel <- legend.se <- legend.r2 <-
    legend.LM.p.val <- legend.g.factor <- legend.HM.k <- legend.k.max <-
    legend.k.ratio <- best.model.display <- GASTYPE <- NULL

  # Hutchinson and Mosier model
  HMmod <- function(Ci, C0, k, x){
    Ci + (C0 - Ci) * exp(-k * x)
  }

  # Translate p-values into star symbols or abbreviations
  p.val.star <- function(x) {
    ifelse(x < 0.001, "***",
           ifelse(x < 0.01, "**",
                  ifelse(x < 0.05, "*", "NS")))
  }
  p.val.round <- function(x) {
    ifelse(x < 0.001, "p < 0.001",
           ifelse(x < 0.01, "p < 0.01",
                  ifelse(x < 0.05, "p < 0.05", "NS")))
  }

  # FUNCTION STARTS ####

  # Define gas units
  if(gastype == "CO2dry_ppm") gas.unit <- "ppm"
  if(gastype == "CH4dry_ppb") gas.unit <- "ppb"
  if(gastype == "N2Odry_ppb") gas.unit <- "ppb"
  if(gastype == "H2O_ppm") gas.unit <- "ppm"

  # Define y axis legend on plots
  if(gastype == "CO2dry_ppm") ylab <- ylab(expression(bold(CO["2"]*" dry (ppm)")))
  if(gastype == "CH4dry_ppb") ylab <- ylab(expression(CH["4"]*" dry (ppb)"))
  if(gastype == "N2Odry_ppb") ylab <- ylab(expression(N["2"]*"O dry (ppb)"))
  if(gastype == "H2O_ppm") ylab <- ylab(expression(H["2"]*"O (ppm)"))

  # Define flux units
  if(!is.null(flux.unit)) flux.unit <- flux.unit else {
    if(gastype == "CO2dry_ppm") flux.unit <- "\u03BCmol~m^-2*s^-1"
    if(gastype == "CH4dry_ppb") flux.unit <- "nmol~m^-2*s^-1"
    if(gastype == "N2Odry_ppb") flux.unit <- "nmol~m^-2*s^-1"
    if(gastype == "H2O_ppm") flux.unit <- "\u03BCmol~m^-2*s^-1"
  }

  # Define flux.term units
  if(!is.null(flux.term.unit)) flux.term.unit <- flux.term.unit else {
    flux.term.unit <- "mol~m^-2"
  }

  # Create a list of dataframe (by UniqueID)
  data_split <- dataframe %>%
    right_join(flux.results, by = c("UniqueID")) %>% group_by(UniqueID) %>%
    # Correct Etime for NAs
    # mutate(start.Etime = POSIX.time[which(Etime == 0)[1]],
    #        Etime = as.numeric(POSIX.time - start.Etime, units = "secs")) %>%
    # Calculate HM_mod
    mutate(HM_mod = HMmod(HM.Ci, HM.C0, HM.k, Etime)) %>%
    #select(!c(start.Etime)) %>%
    group_split()

  # Remove non-measurements (flag == 0)
  data_corr <- lapply(seq_along(data_split), function(f) {
    data_split[[f]] %>% filter(flag == 1) })

  # Loop through list of data frames (by UniqueID)
  pboptions(char = "=")
  plot_list <- pblapply(seq_along(data_split), function(f) {

    # Plot limits
    if(any(grepl("\\<nb.obs\\>", data_split[[f]]$quality.check))){

      xmax <- max(na.omit(data_split[[f]]$Etime))
      xmin <- min(na.omit(data_split[[f]]$Etime))
      xdiff <- xmax - xmin

      ymax <- max(na.omit(data_split[[f]][, gastype]))
      ymin <- min(na.omit(data_split[[f]][, gastype]))
      ydiff <- ymax - ymin

    } else {

      xmax <- max(na.omit(data_corr[[f]]$Etime)) + shoulder
      xmin <- -shoulder
      xdiff <- xmax - xmin

      ymax <- max(na.omit(data_corr[[f]][, gastype]))
      ymin <- min(na.omit(data_corr[[f]][, gastype]))
      ydiff <- ymax - ymin

      if(any(grepl("\\<crop\\>", plot.display))){
        if(nrow(dataframe %>% filter(flag == 2)) == 0){
          cham.close <- unique(na.omit(data_corr[[f]]$cham.close))
          cham.open <- unique(na.omit(data_corr[[f]]$cham.open))
          flag2 <- which(between(data_split[[f]]$POSIX.time, cham.close, cham.open)) %>%
            setdiff(which(data_split[[f]]$flag == 1))
          data_split[[f]] <- data_split[[f]] %>%
            mutate(flag = if_else(row_number() %in% flag2, 2, flag))

          xmax <- data_split[[f]] %>% filter(flag != 0) %>%
            select(Etime) %>% max(na.omit(.)) + shoulder
          xmin <- data_split[[f]] %>% filter(flag != 0) %>%
            select(Etime) %>% min(na.omit(.)) - shoulder
          xdiff <- xmax - xmin

          ymax <- data_split[[f]] %>% filter(flag != 0) %>%
            select(all_of(gastype)) %>% max(na.omit(.))
          ymin <- data_split[[f]] %>% filter(flag != 0) %>%
            select(all_of(gastype)) %>% min(na.omit(.))
          ydiff <- ymax - ymin
        }
      }
    }
  ## plot.legend ####

  ### LM and HM flux are always in the legend
  LM.flux <- signif(unique(data_corr[[f]]$LM.flux), 2)
  HM.flux <- signif(unique(data_corr[[f]]$HM.flux), 2)

  legend.flux <- cbind.data.frame(
    content = c("Model", "lm", "HM", paste("'Flux units:'", "~", flux.unit),
                "Flux", LM.flux, HM.flux, ""))

  ### Legend length ####
  legend.length <- length(grep(paste(c(
    "\\<MAE\\>", "\\<RMSE\\>", "\\<SErel\\>", "\\<SE\\>", "\\<LM.p.val\\>",
    "\\<r2\\>", "\\<HM.k\\>", "\\<k.max\\>", "\\<k.ratio\\>", "\\<g.factor\\>",
    "\\<best.model\\>"),
    collapse = "|"), plot.legend)) +2

  ### Legend content ####
  if(!is.null(plot.legend)){

    ### MAE ####
    if(any(grepl("\\<MAE\\>", plot.legend))){
      LM.MAE <- signif(unique(data_corr[[f]]$LM.MAE), 3)
      HM.MAE <- signif(unique(data_corr[[f]]$HM.MAE), 3)
      legend.MAE <- cbind.data.frame(content = c("MAE", LM.MAE, HM.MAE, ""))
    }
    ### RMSE ####
    if(any(grepl("\\<RMSE\\>", plot.legend))){
      LM.RMSE <- signif(unique(data_corr[[f]]$LM.RMSE), 2)
      HM.RMSE <- signif(unique(data_corr[[f]]$HM.RMSE), 2)
      legend.RMSE <- cbind.data.frame(content = c("RMSE", LM.RMSE, HM.RMSE, ""))
    }
    ### SErel ####
    if(any(grepl("\\<SErel\\>", plot.legend))){
      LM.se.rel <- round(unique(data_corr[[f]]$LM.se.rel), 1)
      HM.se.rel <- round(unique(data_corr[[f]]$HM.se.rel), 1)
      legend.se.rel <- cbind.data.frame(content = c(
        "'SE rel.'", paste(LM.se.rel, "~'%'"), paste(HM.se.rel, "~'%'"), ""))
    }
    ### SE ####
    if(any(grepl("\\<SE\\>", plot.legend))){
      LM.se <- round(unique(data_corr[[f]]$LM.se), 3)
      HM.se <- round(unique(data_corr[[f]]$HM.se), 3)
      legend.se <- cbind.data.frame(content = c("SE", LM.se, HM.se, ""))
    }
    ### LM.p.val ####
    if(any(grepl("\\<LM.p.val\\>", plot.legend))){
      if(any(grepl("\\<round\\>", p.val.disp))){
        LM.p.val <- p.val.round(unique(data_corr[[f]]$LM.p.val))}
      if(any(grepl("\\<star\\>", p.val.disp))){
        LM.p.val <- p.val.star(unique(data_corr[[f]]$LM.p.val))}
      if(any(grepl("\\<value\\>", p.val.disp))){
        LM.p.val <- signif(unique(data_corr[[f]]$LM.p.val), 2)}
      legend.LM.p.val <- cbind.data.frame(content = c("'p-value'", LM.p.val, "", ""))
    }
    ### r2 ####
    if(any(grepl("\\<r2\\>", plot.legend))){
      LM.r2 <- round(unique(data_corr[[f]]$LM.r2), 3)
      HM.r2 <- round(unique(data_corr[[f]]$HM.r2), 3)
      legend.r2 <- cbind.data.frame(content = c("r^2", LM.r2, HM.r2, ""))
    }
    ### HM.k ####
    if(any(grepl("\\<HM.k\\>", plot.legend))){
      HM.k <- signif(unique(data_corr[[f]]$HM.k), 2)
      legend.HM.k <- cbind.data.frame(content = c("'kappa'", "", HM.k, ""))
    }
    ### k.max ####
    if(any(grepl("\\<k.max\\>", plot.legend))){
      k.max <- signif(unique(data_corr[[f]]$k.max), 2)
      legend.k.max <- cbind.data.frame(content = c("kappa~'max'", "", k.max, ""))
    }
    ### k.ratio ####
    if(any(grepl("\\<k.ratio\\>", plot.legend))){
      HM.k <- unique(data_corr[[f]]$HM.k)
      k.max <- unique(data_corr[[f]]$k.max)
      k.ratio <- round(HM.k/k.max*100, 0)
      legend.k.ratio <- cbind.data.frame(content = c("kappa~'ratio'", "",
                                                     paste(k.ratio, "~'%'"), ""))
    }
    ### g.factor ####
    if(any(grepl("\\<g.factor\\>", plot.legend))){
      g.factor <- round(unique(data_corr[[f]]$g.fact), 1)
      legend.g.factor <- cbind.data.frame(content = c("'g-factor'", "", g.factor, ""))
    }
  }
  ## Legends' positions
  seq.x <- seq.rep(0.93, -0.13, 4, legend.length)
  seq.y <- seq.rep(0.28, -0.07, legend.length, 4, rep.seq = T)

  ## Merge legend data frames
  mod.legend <- rbind(legend.flux, legend.MAE, legend.RMSE, legend.se.rel,
                      legend.se, legend.r2, legend.LM.p.val, legend.g.factor,
                      legend.HM.k, legend.k.max, legend.k.ratio) %>%
    cbind.data.frame(
    color = rep(c("black", "blue", "red", "black"), legend.length),
    x = xmax - xdiff*seq.x,
    y = ymax + ydiff*seq.y)

  ## best.model ####
  if(isTRUE(best.model)){
    model <- unique(data_corr[[f]]$model)
    if(model == "LM") best.model.plot <- data.frame(y = ymax + ydiff*0.207)
    if(model == "HM") best.model.plot <- data.frame(y = ymax + ydiff*0.137)
    best.model.plot <- best.model.plot %>% mutate(x = xmax - xdiff*0.95)
    best.model.display <- geom_star(data = best.model.plot, aes(
      x = x, y = y), color = "black", size = 1.5)
  }

  ## plot.display ####
  if(!is.null(plot.display)){
    ### C0 ####
    if(any(grepl("\\<C0\\>", plot.display))){
      # C0 values
      LM.C0 <- round(unique(data_corr[[f]]$LM.C0), 0)
      HM.C0 <- round(unique(data_corr[[f]]$HM.C0), 0)
      # Plot
      LM.C0.display <- annotate(
        "text", x = xmin+xdiff*0.2, y = (ymax+ymin)/2 + ydiff*0.12,
        label = paste("~~lm~C[0]", "~'='~", LM.C0, "~", gas.unit),
        colour = "blue", hjust = 0.5, parse = TRUE, size = 3.2)
      HM.C0.display <- annotate(
        "text", x = xmin+xdiff*0.2, y = (ymax+ymin)/2 + ydiff*0.05,
        label = paste("HM~C[0]", "~'='~", HM.C0, "~", gas.unit),
        colour = "red", hjust = 0.5, parse = TRUE, size = 3.2)
    }
    ### Ci ####
    if(any(grepl("\\<Ci\\>", plot.display))){
      # Ci values
      HM.Ci <- round(unique(data_corr[[f]]$HM.Ci), 0)
      # Plot
      HM.Ci.display <- annotate(
        "text", x = xmin+xdiff*0.2, y = (ymax+ymin)/2 - ydiff*0.02,
        label = paste("~~HM~C[i]", "~'='~", HM.Ci, "~", gas.unit),
        colour = "red", hjust = 0.5, parse = TRUE, size = 3.2)
    }
    ### cham.close ####
    if(any(grepl("\\<cham.close\\>", plot.display))){
      # value
      cham.close <- data_split[[f]] %>%
        filter(POSIX.time == cham.close) %>%
        rename(GASTYPE = all_of(gastype))
      # plot
      cham.close.display <- geom_star(data = cham.close, aes(
        x = Etime, y = GASTYPE), fill = "#008000", size = 3)
    }
    ### cham.open ####
    if(any(grepl("\\<cham.open\\>", plot.display))){
      # value
      cham.open <- data_split[[f]] %>%
        filter(POSIX.time == cham.open) %>%
        rename(GASTYPE = all_of(gastype))
      # plot
      cham.open.display <- geom_star(data = cham.open, aes(
        x = Etime, y = GASTYPE), fill = "#008000", size = 3)
    }
    ### NEW PLOT LIMITS with nb.obs, MDF, flux term and prec ####
    if(any(grepl(paste(c("\\<MDF\\>", "\\<nb.obs\\>", "\\<flux.term\\>", "\\<prec\\>"),
                       collapse = "|"), plot.display))){
      display.length <- length(grep(paste(c(
        "\\<MDF\\>", "\\<nb.obs\\>", "\\<flux.term\\>", "\\<prec\\>"),
        collapse = "|"), plot.display))
      if(display.length > 2) ymin <- ymin - ydiff*min(seq.y)*1.8
      if(display.length <= 2) ymin <- ymin - ydiff*min(seq.y)*0.9
    }
    ### nb.obs ####
    if(any(grepl("\\<nb.obs\\>", plot.display))){
      # position
      nb.obs.ord <- which(grep(paste(c("\\<MDF\\>", "\\<nb.obs\\>",
                                       "\\<flux.term\\>", "\\<prec\\>"), collapse = "|"),
                               plot.display, value = T) == "nb.obs")
      if(nb.obs.ord == 1 | nb.obs.ord == 3) nb.obs.x <- 3
      if(nb.obs.ord == 2 | nb.obs.ord == 4) nb.obs.x <- 6
      if(nb.obs.ord <= 2) nb.obs.y <- -0.4
      if(nb.obs.ord > 2) nb.obs.y <- 1.6
      # value
      nb.obs <- round(unique(data_corr[[f]]$nb.obs), 0)
      # nb.obs.display
      nb.obs.display <- annotate(
        "text", x = seq(xmin, xmax, length.out=9)[nb.obs.x], colour = "black",
        y = ymin - ydiff*min(seq.y)*nb.obs.y/2, hjust = 0,
        label = paste(nb.obs, "~'data points'"), parse = TRUE, size = 3.2)
    }
    ### MDF ####
    if(any(grepl("\\<MDF\\>", plot.display))){
      # position
      MDF.ord <- which(grep(paste(c("\\<MDF\\>", "\\<nb.obs\\>",
                                    "\\<flux.term\\>", "\\<prec\\>"), collapse = "|"),
                            plot.display, value = T) == "MDF")
      if(MDF.ord == 1 | MDF.ord == 3) MDF.x <- 3
      if(MDF.ord == 2 | MDF.ord == 4) MDF.x <- 6
      if(MDF.ord <= 2) MDF.y <- -0.4
      if(MDF.ord > 2) MDF.y <- 1.6
      # value
      MDF <- signif(unique(data_corr[[f]]$MDF), 2)
      # MDF.display
      MDF.display <- annotate(
        "text", x = seq(xmin, xmax, length.out=9)[MDF.x], colour = "black",
        y = ymin - ydiff*min(seq.y)*MDF.y/2, hjust = 0,
        label = paste("'MDF ='~", MDF, "~", flux.unit), parse = TRUE, size = 3.2)
    }
    ### flux.term ####
    if(any(grepl("\\<flux.term\\>", plot.display))){
      # position
      flux.term.ord <- which(grep(paste(c("\\<MDF\\>", "\\<nb.obs\\>",
                                          "\\<flux.term\\>", "\\<prec\\>"), collapse = "|"),
                                  plot.display, value = T) == "flux.term")
      if(flux.term.ord == 1 | flux.term.ord == 3) flux.term.x <- 3
      if(flux.term.ord == 2 | flux.term.ord == 4) flux.term.x <- 6
      if(flux.term.ord <= 2) flux.term.y <- -0.4
      if(flux.term.ord > 2) flux.term.y <- 1.6
      # value
      flux.term <- round(unique(data_corr[[f]]$flux.term), 1)
      # flux.term.display
      flux.term.display <- annotate(
        "text", x = seq(xmin, xmax, length.out=9)[flux.term.x], colour = "black",
        y = ymin - ydiff*min(seq.y)*flux.term.y/2, hjust = 0, parse = TRUE,
        label = paste("'flux.term ='~", flux.term, "~", flux.term.unit), size = 3.2)
    }
    ### prec ####
    if(any(grepl("\\<prec\\>", plot.display))){
      # position
      prec.ord <- which(grep(paste(c("\\<MDF\\>", "\\<nb.obs\\>",
                                     "\\<flux.term\\>", "\\<prec\\>"), collapse = "|"),
                             plot.display, value = T) == "prec")
      if(prec.ord == 1 | prec.ord == 3) prec.x <- 3
      if(prec.ord == 2 | prec.ord == 4) prec.x <- 6
      if(prec.ord <= 2) prec.y <- -0.4
      if(prec.ord > 2) prec.y <- 1.6
      # value
      prec <- unique(data_corr[[f]]$prec)
      # prec.display
      prec.display <- annotate(
        "text", x = seq(xmin, xmax, length.out=9)[prec.x], colour = "black",
        y = ymin - ydiff*min(seq.y)*prec.y/2, hjust = 0,
        label = paste("'prec ='~", prec, "~", gas.unit), parse = TRUE, size = 3.2)
    }
  }

    ## Extract quality check ####
    if(quality.check == TRUE){
      # NEW PLOT LIMITS
      ymin <- ymin - ydiff*min(seq.y)*1.8
      # value
      quality <- unique(data_split[[f]]$quality.check)
      # quality.check.display
      quality.check.display <- annotate(
        "text", x = seq(xmin, xmax, length.out=9)[2], colour = "black",
        y = ymin - ydiff*min(seq.y)*0.9, hjust = 0, parse = TRUE, size = 3.2,
        label = paste("'Quality check:'~", paste("'", quality, "'")))
    }

    # Content of plot
    Etime <- data_split[[f]]$Etime
    gas_meas <- Reduce("c", data_split[[f]][, gastype])
    flag <- data_split[[f]]$flag
    plot_data <- cbind.data.frame(gas_meas, Etime, flag)

    LM.slope <- unique(data_corr[[f]]$LM.slope)
    LM.C0 <- unique(data_corr[[f]]$LM.C0)
    UniqueID <- unique(data_corr[[f]]$UniqueID)
    HM_mod <- data_split[[f]]$HM_mod

    # Draw plot ####
    plot <- ggplot(plot_data, aes(x = Etime)) +
      geom_point(aes(y = gas_meas, col = as.factor(flag))) +
      scale_color_manual(values = c("darkgrey", "black", "pink"), guide = "none") +

      # Linear model
      geom_abline(slope = LM.slope, intercept = LM.C0,
                  linewidth = 1, col = "blue") +

      # Hutchinson and Mosier
      geom_line(aes(y = HM_mod), linewidth = 1, col = "red") +

      # Add a legend with info on the two models
      new_scale_color() +
      geom_text(data = mod.legend, parse = T, size = 3.5,
                aes(x = x, y = y, label = content, hjust = 0, color = color)) +
      scale_color_manual(values = mod.legend$color, guide = "none") +

      # plot.display
      HM.C0.display + LM.C0.display +
      HM.Ci.display +
      cham.close.display + cham.open.display +
      nb.obs.display +
      MDF.display +
      flux.term.display +
      prec.display +
      quality.check.display +
      best.model.display +

      # Make the plot pretty
      xlab("Time (sec)") + ylab +
      scale_x_continuous(breaks = seq(-60, max(Etime), 30),
                         minor_breaks = seq(-60, max(Etime)+60, 10)) +
      coord_cartesian(xlim = c(xmin + xdiff*0.05, xmax - xdiff*0.05),
                      ylim = c(ymin - ydiff*0.05, ymax + ydiff*max(seq.y))) +
      theme_bw() +
      theme(axis.title.x = element_text(size = 10, face = "bold"),
            axis.title.y = element_text(size = 10, face = "bold"))

    return(plot)
  })
}
