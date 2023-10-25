#' Plots for quality checking of GHG flux measurements
#'
#' Description
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
#'
#' @return a list of plots, one per UniqueID
#'
#' @include GoFluxYourself-package.R
#'
#' @seealso See also the function \code{\link[GoFluxYourself]{goFlux}},
#'          \code{\link[GoFluxYourself]{best.flux}} and
#'          \code{\link[GoFluxYourself]{flux2pdf}}
#'          for more information about usage.
#'
#' @examples
#' data(example_LGR_manID)
#' example_LGR_flux <- goFlux(example_LGR_manID, "CO2dry_ppm")
#' criteria <- c("MAE", "g.factor", "kappa", "MDF", "SE.rel")
#' example_LGR_res <- best.flux(example_LGR_flux, criteria)
#' example_LGR_plots <- flux.plot(example_LGR_res, example_LGR_manID, "CO2dry_ppm")
#'
#' @export
#'
flux.plot <- function(flux.results, dataframe, gastype, shoulder = 30) {

  # Assign NULL to variables without binding
  UniqueID <- HM.Ci <- HM.C0 <- HM.k <- . <- flag <- start.Etime <-
    end.Etime <- Etime <- x <- y <- content <- color <- POSIX.time <- NULL

  # Hutchinson and Mosier model
  HMmod <- function(Ci, C0, k, x){
    Ci + (C0 - Ci) * exp(-k * x)
  }

  # Translate p-values into star symbols
  p.val.star <- function(x) {
    ifelse(x < 0.001, "***",
           ifelse(x < 0.01, "**",
                  ifelse(x < 0.05, "*", "NS")))
  }

  # Define y axis legend on plots
  ylab <- ifelse(gastype == "CO2dry_ppm", "CO2 dry (ppm)",
                 ifelse(gastype == "CH4dry_ppb", "CH4 dry (ppb)",
                        ifelse(gastype == "N2Odry_ppb", "N2O dry (ppb)",
                               ifelse(grepl("H2O_ppm", gastype), "H2O (ppm)", NA))))

  # Create a list of dataframe (by UniqueID)
  data_split <- dataframe %>%
    right_join(flux.results, by = c("UniqueID")) %>% group_by(UniqueID) %>%
    # Correct Etime for NAs
    mutate(start.Etime = POSIX.time[which(Etime == 0)[1]],
           Etime = as.numeric(POSIX.time - start.Etime, units = "secs"),
           # Calculate HM_mod
           HM_mod = HMmod(HM.Ci, HM.C0, HM.k, Etime)) %>%
    select(!c(start.Etime)) %>%
    group_split()

  # Remove non-measurements (flag == 0)
  data_corr <- lapply(seq_along(data_split), function(f) {
    data_split[[f]] %>% filter(flag == 1) })

  # Loop through list of data frames (by UniqueID)
  pboptions(char = "=")
  plot_list <- pblapply(seq_along(data_split), function(f) {

    # Output from best.flux
    if(any(grepl("quality.check", names(data_split[[f]])))){
      quality.check <- unique(data_split[[f]]$quality.check)}
    if(any(grepl("model", names(data_split[[f]])))){
      model <- unique(data_split[[f]]$model)}
    if(any(grepl("best.flux", names(data_split[[f]])))){
      best.flux <- unique(data_split[[f]]$best.flux)}
    if(any(grepl("LM.diagnose", names(data_split[[f]])))){
      LM.diagnose <- unique(data_split[[f]]$LM.diagnose)}
    if(any(grepl("HM.diagnose", names(data_split[[f]])))){
      HM.diagnose <- unique(data_split[[f]]$HM.diagnose)}
    if(any(grepl("nb.obs", names(data_split[[f]])))){
      nb.obs <- unique(data_split[[f]]$nb.obs)}

    # Plot limits
    obs.length <- max(na.omit(data_corr[[f]]$Etime))
    xmax <- max(na.omit(data_split[[f]]$Etime)) %>%
      ifelse(. > obs.length + shoulder, obs.length + shoulder, .)
    xmin <- min(na.omit(data_split[[f]]$Etime)) %>%
      ifelse(. < -shoulder, -shoulder, .)
    xdiff <- xmax - xmin
    seq.x <- seq.rep(0.87, -0.12, 3, 6)

    if(any(grepl("nb.obs", quality.check))){
      ymax <- max(na.omit(data_split[[f]][, gastype]))
      ymin <- min(na.omit(data_split[[f]][, gastype]))
      ydiff <- ymax - ymin
    } else {
      ymax <- max(na.omit(data_corr[[f]][, gastype]))
      ymin <- min(na.omit(data_corr[[f]][, gastype]))
      ydiff <- ymax - ymin
    }
    seq.y <- seq.rep(0.21, -0.07, 6, 3, rep.seq = T)

    # Content of legend on plot
    LM.flux <- round(unique(data_corr[[f]]$LM.flux), 2)
    HM.flux <- round(unique(data_corr[[f]]$HM.flux), 2)
    LM.se.rel <- round(unique(data_corr[[f]]$LM.se.rel), 2)
    HM.se.rel <- round(unique(data_corr[[f]]$HM.se.rel), 2)
    LM.r2 <- round(unique(data_corr[[f]]$LM.r2), 3)
    HM.r2 <- round(unique(data_corr[[f]]$HM.r2), 3)
    LM.RMSE <- round(unique(data_corr[[f]]$LM.RMSE), 3)
    HM.RMSE <- round(unique(data_corr[[f]]$HM.RMSE), 3)
    LM.p.val <- p.val.star(unique(data_corr[[f]]$LM.p.val))
    HM.k <- unique(data_corr[[f]]$HM.k)
    kappa.ratio <- round((HM.k / unique(data_corr[[f]]$k.max) * 100), 1)

    mod.legend <- cbind.data.frame(color = rep(c("black", "blue", "red"), 6)) %>%
      mutate(x = xmax - xdiff*seq.x,
             y = ymax + ydiff*seq.y,
             content = c("Model", "lm", "HM",
                         "Flux", LM.flux, HM.flux,
                         "SE (%)", LM.se.rel, HM.se.rel,
                         "r2", LM.r2, HM.r2,
                         "RMSE", LM.RMSE, HM.RMSE,
                         "p-val / kappa (%)", LM.p.val, kappa.ratio))

    # Content of plot
    Etime <- data_split[[f]]$Etime
    gas_meas <- Reduce("c", data_split[[f]][, gastype])
    flag <- data_split[[f]]$flag
    plot_data <- cbind.data.frame(gas_meas, Etime, flag)

    LM.slope <- unique(data_corr[[f]]$LM.slope)
    LM.C0 <- unique(data_corr[[f]]$LM.C0)
    UniqueID <- unique(data_corr[[f]]$UniqueID)
    HM_mod <- data_split[[f]]$HM_mod

    # Draw plot
    plot <- ggplot(plot_data, aes(x = Etime)) +
      geom_point(aes(y = gas_meas, col = as.factor(flag))) +
      scale_color_manual(values = c("darkgrey", "black"), guide = "none") +

      # Linear model
      geom_abline(slope = LM.slope, intercept = LM.C0,
                  linewidth = 1, col = "blue") +

      ## Hutchinson and Mosier
      geom_line(aes(y = HM_mod), linewidth = 1, col = "red") +

      # Add a legend with info on the two models
      new_scale_color() +
      geom_text(data = mod.legend, aes(x = x, y = y, label = content,
                                       hjust = 0, color = color)) +
      scale_color_manual(values = mod.legend$color, guide = "none") +

      # Make the plot pretty
      xlab("Time (sec)") + ylab(ylab) +
      scale_x_continuous(breaks = seq(-60, max(Etime), 30),
                         minor_breaks = seq(-60, max(Etime)+60, 10)) +
      coord_cartesian(xlim = c(xmin + xdiff*0.05, xmax - xdiff*0.05),
                      ylim = c(ymin - ydiff*0.02, ymax + ydiff*max(seq.y))) +
      theme_bw() +
      theme(axis.title.x = element_text(size = 10, face = "bold"),
            axis.title.y = element_text(size = 10, face = "bold"))

    return(plot)
  })
}
