



flux_conversion <- function(V_L, P_kPa, A_cm2, T_C, H2O_mol) {
  (V_L * P_kPa * (1 - H2O_mol)) /
    (8.314 * (A_cm2 / 10000) * (T_C + 273.15))
}

goAquaFlux.total <- function(dataframe,
                             gastype,
                             auxfile,
                             t.window = 30) {

  # Build working dataframe
  time0 <- dataframe$POSIX.time[1]

  mydf <- data.frame(
    time = as.numeric(dataframe$POSIX.time - time0),
    conc = dataframe[[gastype]]
  )

  mydf <- mydf[!duplicated(mydf$time), ]

  T_total <- max(mydf$time)

  # Define windows
  idx0 <- mydf$time <= t.window
  idxf <- mydf$time >= (T_total - t.window)

  C0_vals <- mydf$conc[idx0]
  Cf_vals <- mydf$conc[idxf]

  # Means
  C0 <- mean(C0_vals, na.rm = TRUE)
  Cf <- mean(Cf_vals, na.rm = TRUE)

  # Variances
  s0 <- var(C0_vals, na.rm = TRUE)
  sf <- var(Cf_vals, na.rm = TRUE)

  n0 <- sum(idx0)
  nf <- sum(idxf)

  # Conversion term
  H2O_mol <- mean(dataframe$H2O_ppm[1:30], na.rm = TRUE) / 1e6

  K <- flux_conversion(
    V_L = auxfile$Vtot,
    P_kPa = auxfile$Pcham,
    A_cm2 = auxfile$Area,
    T_C = auxfile$Tcham,
    H2O_mol = H2O_mol
  )

  # Flux
  flux <- (Cf - C0) / T_total * K

  # Standard Error (analytical propagation)
  flux_se <- (K / T_total) *
    sqrt( (s0 / n0) + (sf / nf) )

  return(list(
    flux = flux,
    SE = flux_se,
    C0 = C0,
    Cf = Cf,
    incubation_time = T_total
  ))
}
