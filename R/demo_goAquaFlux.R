

# Demo


# clearing workspace and console
rm(list = ls()) # clear workspace
cat("/014") # clear console


library(dplyr)
library(egg)
library(goFlux)
# library(devtools)
library(zoo)
library(pbapply)
library(ggnewscale)
library(lubridate)
library(tidytable)


#Import functions of repo
repo_root <- dirname((rstudioapi::getSourceEditorContext()$path))
setwd(repo_root)

fs <- c("find.bubbles.R",
        "goAquaFlux.diffusive.R",
        "goAquaFlux.ebullition.R",
        "goAquaFlux.R",
        "flux.plot.aqua.R",
        "MDF.R" ,
        "flux.term.R" ,
        "goAquaFlux.total.R")
# fs <- list.files(path = repo_root, pattern = ".R")
for (f in fs){  # fs[-which(fs=="demo_goAquaFlux.R")]){
  source(f)
}


# Loading data
setwd("C:/Projects/myGit/aquaGHG/")
mydata_all <- NULL
fs <- list.files(path = "data/",pattern = ".RData", full.names = T)
for(f in fs[c(1,3)]){
  load(file = f)
  # print(range(mydata$POSIX.time))
  # print(as.numeric(max(mydata$POSIX.time)) - as.numeric(min(mydata$POSIX.time)))
  mydata$Etime <- as.numeric(mydata$Etime)
  mydata_all <- rbind(mydata_all, mydata)
  rm(mydata)
}

# Loading auxfile table
auxfile = read.csv("data/myauxfile.csv")
auxfile$start.time <- as.POSIXct(auxfile$start.time, tz = 'UTC', format="%d/%m/%Y %H:%M:%S")
auxfile <- auxfile[which(auxfile$UniqueID %in% unique(mydata_all$UniqueID)),]
# auxfile$start.time <- max(min(mydata_all$POSIX.time), auxfile$start.time)
auxfile$end.time <- auxfile$start.time + auxfile$obs.length
# auxfile$end.time <- min(max(mydata_all$POSIX.time), auxfile$end.time)



# Automatic selection of data
IDed <- autoID(inputfile = mydata_all, auxfile = auxfile, shoulder = 60, deadband = 0, crop.end = 0)


# # Define a window of observation for each measurement using the function obs.win
# # By default, the gastype used to identify measurements is CO2dry_ppm
# ow <- obs.win(inputfile = mydata_all,
#               auxfile = auxfile[which(auxfile$UniqueID %in% unique(mydata_all$UniqueID)),])
#
# # Click on a scatter plot to identify start and end times using click.peak2
# # Tip: create a folder to store results
# IDed <- click.peak2(ow.list = ow)



# dataframe = IDed
# gastype = "CH4dry_ppb"
# H2O_col = "H2O_ppm"
# prec = NULL
# criteria = c("MAE", "RMSE", "AICc", "SE", "g.factor","kappa", "MDF", "nb.obs", "intercept", "p-value")
# Area <- offset <- Vtot <- Vcham <- Pcham <- Tcham <- NULL
# use_bubble_detection = TRUE
# bubble.window.size = 30
# bubble_gas = "CH4dry_ppb"
# ebullition.final_window_min = 30
# ebullition.window_C0Cf = 10
# diffusion.minimum_window = 30


# CH4_flux <- best.flux(goFlux(IDed, gastype = "CH4dry_ppb"))


# CH4dry_ppb  CO2dry_ppm
gastype = "CH4dry_ppb"

fluxres <- goAquaFlux(dataframe = IDed, gastype = gastype, use_bubble_detection = T)



# Create plots
flux.plot.aqua(
  flux.results = fluxres,
  dataframe = IDed,
  gastype = gastype,
  plot.display = c("diffusive.window", "ebullition.events"),
  conversion.factor = 1)



