

# Demo


# clearing workspace and console
rm(list = ls()) # clear workspace
cat("/014") # clear console


library(dplyr)
library(egg)
library(goFlux)
library(devtools)
library(zoo)
library(pbapply)
library(ggnewscale)


#Import functions of repo
repo_root <- dirname((rstudioapi::getSourceEditorContext()$path))
setwd(repo_root)
source("find.bubbles.R")
source("goAquaFlux.R")
source("goAquaFlux.total.R")
source("goAquaFlux.diffusive.R")
source("goAquaFlux.ebullition.R")
source("flux.term.R")
source("MDF.R")


# Loading data
setwd("C:/Projects/myGit/aquaGHG/")
mydata_all <- NULL
fs <- list.files(path = "data/",pattern = ".RData", full.names = T)
for(f in fs){
  load(file = f)
  mydata$Etime <- as.numeric(mydata$Etime)
  mydata_all <- rbind(mydata_all, mydata)
  rm(mydata)
}

# Loading auxfile table
auxfile = read.csv("data/myauxfile.csv")
auxfile$start.time <- as.POSIXct(auxfile$start.time, tz = 'UTC', format="%d/%m/%Y %H:%M")
#
# k = 3
# load(file = fs[k])
# mydata$Etime <- as.numeric(mydata$Etime)
# mydata$flag <- 1
#
# ggplot()+
#   geom_rect(data = bubbl, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), col = 'red', alpha=0.2)+
#   geom_point(data = mydata, aes(Etime, CH4dry_ppb))+
#   theme_article()


# Define a window of observation for each measurement using the function obs.win
# By default, the gastype used to identify measurements is CO2dry_ppm
ow <- obs.win(inputfile = mydata_all, auxfile = auxfile)

# Click on a scatter plot to identify start and end times using click.peak2
# Tip: create a folder to store results
IDed <- click.peak2(ow.list = ow)

# Automatic selection of data
# IDed <- autoID(inputfile = mydata_all, auxfile = auxfile, shoulder = 0)

CO2_flux <- goFlux(IDed, "CO2dry_ppm")
CO2_flux_aqua <- goAquaFlux(IDed, gastype = "CO2dry_ppm", bubble_gas = "CH4dry_ppb")

CH4_flux <- best.flux(goFlux(IDed, "CH4dry_ppb"))
CH4_flux_aqua <- goAquaFlux(dataframe = IDed, gastype = "CH4dry_ppb")


