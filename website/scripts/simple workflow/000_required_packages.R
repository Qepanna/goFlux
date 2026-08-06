#==============================================================================#
#                                                                              #
#  goFlux workflow - Required packages                                         #
#                                                                              #
#  This scripts is used to install and load required packages                  #
#  used with the goFlux package.                                               #
#                                                                              #
#==============================================================================#

# Install goFlux from GitHub ---------------------------------------------------

# To load the package goFlux from GitHub, you require remotes or devtools
if (!require("remotes", quietly = TRUE)) install.packages("remotes")

# Before re-installing the package, you must first detach it
try(detach("package:goFlux", unload = TRUE), silent = TRUE)

# Then the package can be re-installed (or installed for the first time)
remotes::install_github("Qepanna/goFlux")

# Load package
library(goFlux)

# Packages installed from CRAN -------------------------------------------------

if (!require("openxlsx", quietly = TRUE)) install.packages("openxlsx")
library(openxlsx)

if (!require("purrr", quietly = TRUE)) install.packages("purrr")
library(purrr)

if (!require("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)

if (!require("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)
