






goAquaFlux.diffusive <- function(dataframe,
                                 gastype,
                                 auxfile,
                                 bubbles = NULL,
                                 minimum_window = 30) {

  dataframe <- dataframe[!duplicated(dataframe$POSIX.time), ]

  # --- Determine diffusive window
  if (is.null(bubbles) || nrow(bubbles) == 0) {

    # No bubbling detected
    df_diff <- dataframe
    first_bubble_time <- NA

  } else {

    first_bubble_time <- bubbles$start[1]
    df_diff <- dataframe[dataframe$Etime < first_bubble_time, ]

  }

  n_used <- nrow(df_diff)

  # --- Check minimum data requirement
  if (n_used < minimum_window) {
    return(list(
      flux = NA,
      SE = NA,
      n_used = n_used,
      first_bubble_time = first_bubble_time,
      message = "Insufficient diffusive observations"
    ))
  }

  # --- calling goFlux to compute diffusive flux
  if(!is.na(first_bubble_time)){auxfile$obs.length <- first_bubble_time}
  autoIDed <- autoID(inputfile = df_diff, auxfile = auxfile, shoulder = 0)

  aquaFlux.diff <- goFlux(autoIDed, gastype)

  best.flux.diff <- best.flux(aquaFlux.diff)
  best.flux.diff$SE_best_model <- ifelse(best.flux.diff$model =="LM", best.flux.diff$LM.SE, best.flux.diff$HM.SE)



  return(list(
    flux = best.flux.diff$best.flux,
    SE = best.flux.diff$SE_best_model,
    n_used = n_used,
    first_bubble_time = first_bubble_time
  ))
}
