# =============================================================================
# Demo: aquatic GHG flux partitioning (diffusion + ebullition) with goAquaFlux
# =============================================================================

# Clear workspace and console
rm(list = ls())     # clear workspace
cat("\014")         ## FIX: form-feed to clear the console (was "/014", a literal)

library(dplyr)
library(egg)
library(goFlux)
# library(devtools)
library(zoo)          # find.bubbles() rolling statistics
library(pbapply)
library(ggnewscale)
library(lubridate)
library(tidytable)

# --- Source the aquatic extension from the repo root --------------------------
repo_root <- dirname((rstudioapi::getSourceEditorContext()$path))
setwd(repo_root)

fs <- c("find.bubbles.R",
        "goAquaFlux.diffusive.R",
        "goAquaFlux.ebullition.R",
        "goAquaFlux.total.R",
        "goAquaFlux.R",
        "flux.plot.aqua.R",
        "MDF.R",
        "flux.term.R")
for (f in fs) source(f)

# --- Load data ----------------------------------------------------------------
setwd("C:/Projects/myGit/aquaGHG/")
mydata_all <- NULL
fs <- list.files(path = "data/", pattern = ".RData", full.names = TRUE)
for (f in fs[c(1, 3)]) {
  load(file = f)
  mydata$Etime <- as.numeric(mydata$Etime)
  mydata_all <- rbind(mydata_all, mydata)
  rm(mydata)
}

# --- Load auxfile -------------------------------------------------------------
auxfile <- read.csv("data/myauxfile.csv")
auxfile$start.time <- as.POSIXct(auxfile$start.time, tz = "UTC",
                                 format = "%d/%m/%Y %H:%M:%S")
auxfile <- auxfile[auxfile$UniqueID %in% unique(mydata_all$UniqueID), ]
auxfile$end.time <- auxfile$start.time + auxfile$obs.length

# --- Automatic selection of measurement windows -------------------------------
IDed <- autoID(inputfile = mydata_all, auxfile = auxfile,
               shoulder = 60, deadband = 0, crop.end = 0)

# --- Run the aquatic flux partitioning ----------------------------------------
gastype <- "CH4dry_ppb"   # or "CO2dry_ppm"

fluxres <- goAquaFlux(dataframe = IDed,
                      gastype = gastype,
                      use_bubble_detection = TRUE)

## NEW: the bubble detector is now tunable from goAquaFlux(). For strong
## diffusion, the "diff" method (rolling variance of increments) is more robust:
# fluxres <- goAquaFlux(IDed, gastype = gastype,
#                       bubble.method = "diff",
#                       bubble.args = list(k = 5, min_magnitude = 10))

## NEW: return_df = FALSE returns the raw per-incubation list for power users.
# fluxres_raw <- goAquaFlux(IDed, gastype = gastype, return_df = FALSE)

# --- Plots --------------------------------------------------------------------
p <- flux.plot.aqua(
  flux.results.ls = fluxres,
  dataframe = IDed,
  gastype = gastype,
  plot.display = c("diffusive.window", "ebullition.events"),
  conversion.factor = 1)

print(p)
