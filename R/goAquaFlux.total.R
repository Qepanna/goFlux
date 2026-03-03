



flux_conversion <- function(V_L, P_kPa, A_cm2, T_C, H2O_mol) {
  (V_L * P_kPa * (1 - H2O_mol)) /
    (8.314 * (A_cm2 / 10000) * (T_C + 273.15))
}

goAquaFlux.total <- function(dataframe,
                             gastype,
                             auxfile,
                             bubbles = NULL,
                             t.window = 30,
                             minimum_window = 10) {

  # --- Build time vector
  time0 <- dataframe$POSIX.time[1]

  df <- data.frame(
    time = as.numeric(dataframe$POSIX.time - time0),
    conc = dataframe[[gastype]]
  )

  df <- df[!duplicated(df$time), ]

  T_total <- max(df$time)

  # ----------------------------
  # Determine final stable window
  # ----------------------------

  if (is.null(bubbles) || nrow(bubbles) == 0) {

    # No bubbling → use end window
    end_limit <- T_total

  } else {

    last_bubble_start <- bubbles$start[nrow(bubbles)]

    # If last bubble occurs near the end,
    # define Cf before that bubble
    end_limit <- last_bubble_start

  }

  # Define final window
  idxf <- df$time >= (end_limit - t.window) & df$time < end_limit

  if (sum(idxf) < minimum_window) {
    return(list(
      flux = NA,
      SE = NA,
      message = "No stable final window available",
      incubation_time = T_total
    ))
  }

  # ----------------------------
  # Initial window (always at start)
  # ----------------------------

  idx0 <- df$time <= t.window

  if (sum(idx0) < minimum_window) {
    return(list(
      flux = NA,
      SE = NA,
      message = "No stable initial window available",
      incubation_time = T_total
    ))
  }

  # Compute means
  C0_vals <- df$conc[idx0]
  Cf_vals <- df$conc[idxf]

  C0 <- mean(C0_vals, na.rm = TRUE)
  Cf <- mean(Cf_vals, na.rm = TRUE)

  # Variances
  s0 <- var(C0_vals, na.rm = TRUE)
  sf <- var(Cf_vals, na.rm = TRUE)

  n0 <- sum(idx0)
  nf <- sum(idxf)

  # ----------------------------
  # Conversion term
  # ----------------------------

  H2O_mol <- mean(dataframe$H2O_ppm[1:30], na.rm = TRUE) / 1e6

  K <- flux_conversion(
    V_L = auxfile$Vtot,
    P_kPa = auxfile$Pcham,
    A_cm2 = auxfile$Area,
    T_C = auxfile$Tcham,
    H2O_mol = H2O_mol
  )

  # ----------------------------
  # Flux and SE
  # ----------------------------

  effective_time <- end_limit  # time span used

  flux <- (Cf - C0) / effective_time * K

  flux_se <- (K / effective_time) *
    sqrt((s0 / n0) + (sf / nf))

  return(list(
    flux = flux,
    SE = flux_se,
    C0 = C0,
    Cf = Cf,
    incubation_time = effective_time
  ))
}
