## a290
## Carina Rauen Firkowski
## April 20, 2023
##
## This script is used to define constants and load necessary functions in a
## reproducible way across scripts.



# Workspace -------------------------------------------------------------------

# Load packages
library(terra)
library(sf)
library(tidyverse)
library(ggpubr)
library(rsyncrosim)
library(smoothr)



# Directories ------------------------------------------------------------------

# Core directories
rootDir <- "."
dataDir          <- file.path(rootDir, "Data")
intermediatesDir <- file.path(rootDir, "Intermediates")
outputDir <- file.path(rootDir, "Outputs")
libraryDir <- file.path(rootDir, "Libraries")

# Composite directories
tabularDataDir <- file.path(dataDir, "Tabular")
spatialDataDir <- file.path(dataDir, "Spatial")



# Parameters -------------------------------------------------------------------

# Target CRS
targetCRS <- "EPSG:3162"


