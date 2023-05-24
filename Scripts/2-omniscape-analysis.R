## a295
## Carina Rauen Firkowski 
## April 20, 2023
##
## This script runs an Omniscape connectivity analysis based on the current 
## landscape conditions at the EcoPark System for three focal species. 
## 
## - BLBR  Northern short-tailed shrew (Blarina brevicauda)
## - ODVI  White-tailed deer (Odocoileus virginianus)
## - EMBL  Blanding’s turtle (Emydoidea blandingii)
##
## For each focal species, the analysis requires three inputs, available at the
## analysis area level, and derived from the 2020 connectivity study:
## - resistance layer, representing resistance to movement based on current LULC
## - source layer, representing patches of suitable habitat
## - radius, representing the dispersal distance
##
## NOTE: code commented out is needed when running script for the first time.



# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# # Resistance layer
# resistanceRasterBLBR <- rast(file.path(spatialDataDir, "Resistance",
#                                        "BLBR_Resistance_20km.tif"))
# resistanceRasterEMBL <- rast(file.path(spatialDataDir, "Resistance",
#                                        "EMBL_Resistance_20km.tif"))
# resistanceRasterODVI <- rast(file.path(spatialDataDir, "Resistance",
#                                        "ODVI_Resistance_20km.tif"))
# 
# # Source layer
# patchesBLBR <- rast(file.path(spatialDataDir, "Habitat patch",
#                               "BLBR_HabitatPatchID_20km.tif"))
# patchesEMBL <- rast(file.path(spatialDataDir, "Habitat patch",
#                               "EMBL_HabitatPatchID_20km.tif"))
# patchesODVI <- rast(file.path(spatialDataDir, "Habitat patch",
#                               "ODVI_HabitatPatchID_20km.tif"))



# Spatial processing -----------------------------------------------------------

# # Spatial parameters
# targetRes <- res(resistanceRasterBLBR)
# targetExt <- ext(resistanceRasterBLBR)
# targetCRS <- crs(resistanceRasterBLBR)
# 
# # Template raster
# templateRaster <- rast()
# crs(templateRaster) <- targetCRS
# ext(templateRaster) <- targetExt
# res(templateRaster) <- targetRes
# 
# # Re-sample source layer based on template raster
# patchesTargetBLBR <- patchesBLBR %>% 
#   resample(y = templateRaster, method = "mode")
# patchesTargetEMBL <- patchesEMBL %>% 
#   resample(y = templateRaster, method = "mode")
# patchesTargetODVI <- patchesODVI %>% 
#   resample(y = templateRaster, method = "mode")
# 
# # Set all patches to 1 to represent sources
# sourceRasterBLBR <- which.lyr(!is.na(patchesTargetBLBR))
# sourceRasterEMBL <- which.lyr(!is.na(patchesTargetEMBL))
# sourceRasterODVI <- which.lyr(!is.na(patchesTargetODVI))
# 
# # Save raster files with NAflag supported by Omniscape
# # Resistance layers
# writeRaster(x = resistanceRasterBLBR,
#             filename = file.path(intermediatesDir, "Omniscape",
#                                  "resistanceBLBR.tif"),
#             datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
# writeRaster(x = resistanceRasterEMBL,
#             filename = file.path(intermediatesDir, "Omniscape",
#                                  "resistanceEMBL.tif"),
#             datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
# writeRaster(x = resistanceRasterODVI,
#             filename = file.path(intermediatesDir, "Omniscape",
#                                  "resistanceODVI.tif"),
#             datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
# # Source layers
# writeRaster(x = sourceRasterBLBR,
#             filename = file.path(intermediatesDir, "Omniscape",
#                                  "sourcesBLBR.tif"),
#             datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
# writeRaster(x = sourceRasterEMBL,
#             filename = file.path(intermediatesDir, "Omniscape",
#                                  "sourcesEMBL.tif"),
#             datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
# writeRaster(x = sourceRasterODVI,
#             filename = file.path(intermediatesDir, "Omniscape",
#                                  "sourcesODVI.tif"),
#             datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)

# Paths to files for Omniscape
# Resistance layers
resistanceBLBR <- normalizePath(file.path(intermediatesDir, "Omniscape", 
                                          "resistanceBLBR.tif"))
resistanceEMBL <- normalizePath(file.path(intermediatesDir, "Omniscape",
                                          "resistanceEMBL.tif"))
resistanceODVI <- normalizePath(file.path(intermediatesDir, "Omniscape",
                                          "resistanceODVI.tif"))
# Source layers
sourcesBLBR <- normalizePath(file.path(intermediatesDir, "Omniscape", 
                                       "sourcesBLBR.tif"))
sourcesEMBL <- normalizePath(file.path(intermediatesDir, "Omniscape",
                                       "sourcesEMBL.tif"))
sourcesODVI <- normalizePath(file.path(intermediatesDir, "Omniscape",
                                       "sourcesODVI.tif"))


# Parameters -------------------------------------------------------------------

# # Reclassification table for resistance
# resistanceReclass <- matrix(c(1, 1,
#                               2, 2,
#                               4, 4,
#                               6, 6,
#                               8, 8,
#                               16, 16,
#                               32, 32), ncol = 2, byrow = TRUE)
# # Save
# write.table(resistanceReclass, 
#             file = file.path(intermediatesDir,
#                              "Omniscape",
#                              "reclass_resistance.txt"),
#             row.names = FALSE, col.names = FALSE)

# Reclassification path
resistanceReclassPath <- normalizePath(file.path(intermediatesDir, "Omniscape",
                                                 "reclass_resistance.txt"))

# Radius - species' median dispersal distance divided by spatial resolution
# BLBR = 500 m
radiusBLBR <- 500 / 15
# EMBL = 2,000 m
radiusEMBL <- 2000 / 15
# ODVI = 20,000 m
radiusODVI <- 20000 / 15



# omniscape Library ------------------------------------------------------------

# Open a new SyncroSim session
omniscapeSession <- session()

# Create a new omniscape Library
omniscapeLibrary <- ssimLibrary(
  name = "a295-omniscape-current-conditions.ssim",
  session = omniscapeSession, overwrite = TRUE,
  package = "omniscape")
# Open default Project
omniscapeProject <- rsyncrosim::project(ssimObject = omniscapeLibrary, 
                                        project = "Definitions")

# Set path to Julia executable 
juliaConfiguration <- data.frame(
  julia_path = 
    "C:\\Users\\CarinaFirkowski\\AppData\\Local\\Programs\\Julia-1.8.2\\bin\\julia.exe")
saveDatasheet(ssimObject = omniscapeLibrary, 
              data = juliaConfiguration, 
              name = "omniscape_juliaConfiguration")

# Enable "Use Conda"
condaDatasheet <- datasheet(omniscapeLibrary, name = "core_Options")
condaDatasheet$UseConda <- TRUE
saveDatasheet(ssimObject = omniscapeLibrary, 
              data = condaDatasheet, 
              name = "core_Options")



# Omniscape analysis -----------------------------------------------------------

# Target species
species <- c("BLBR", "EMBL", "ODVI")

for(spp in species){
  
  # Create Scenario
  newScenario <- scenario(
    ssimObject = omniscapeProject,
    scenario = paste0("currentConditions", spp))
  
  # Input Datasheet ------------------------------------
  
  # Open
  requiredInput <- datasheet(newScenario, name = "omniscape_Required")
  
  # Resistance
  if(spp == "BLBR") resistancePath <- resistanceBLBR
  if(spp == "EMBL") resistancePath <- resistanceEMBL
  if(spp == "ODVI") resistancePath <- resistanceODVI
  
  # Radius
  if(spp == "BLBR") radiusValue <- radiusBLBR
  if(spp == "EMBL") radiusValue <- radiusEMBL
  if(spp == "ODVI") radiusValue <- radiusODVI
  
  # Source
  if(spp == "BLBR") sourcePath <- sourcesBLBR
  if(spp == "EMBL") sourcePath <- sourcesEMBL
  if(spp == "ODVI") sourcePath <- sourcesODVI
  
  # Edit
  inputRow <- data.frame(resistance_file = resistancePath, 
                         radius = round(radiusValue),
                         source_file = sourcePath)
  requiredInput <- addRow(requiredInput, inputRow)
  
  # Save
  saveDatasheet(ssimObject = newScenario, 
                data = requiredInput, 
                name = "omniscape_Required")
  
  
  
  # General Options Datasheet --------------------------
  
  # Open
  generalOptions <- datasheet(newScenario, 
                              name = "omniscape_GeneralOptions")
  # Edit
  generalOptions <- data.frame(block_size = round(radiusValue/10),
                               source_from_resistance = FALSE,
                               resistance_is_conductance = FALSE,
                               r_cutoff = 9999,
                               buffer = 0,
                               source_threshold = 0,
                               calc_normalized_current = TRUE,
                               calc_flow_potential = TRUE,
                               allow_different_projections = FALSE,
                               connect_four_neighbors_only = FALSE,
                               solver = "cg+amg")
  # Save
  saveDatasheet(ssimObject = newScenario, 
                data = generalOptions, 
                name = "omniscape_GeneralOptions")
  
  
  
  # Resistance Options Datasheet -----------------------
  
  # Open
  resistanceOptions <- datasheet(newScenario, 
                                 name = "omniscape_ResistanceOptions")
  # Edit
  resistanceOptions <- data.frame(reclassify_resistance = TRUE,
                                  reclass_table = resistanceReclassPath,
                                  write_reclassified_resistance = FALSE)
  # Save
  saveDatasheet(ssimObject = newScenario, 
                data = resistanceOptions, 
                name = "omniscape_ResistanceOptions")
  
  
  
  # Multiprocessing ------------------------------------
  
  # Open
  multiprocessing <- datasheet(newScenario, name = "core_Multiprocessing")
  
  # Edit
  multiprocessing$EnableMultiprocessing <- TRUE
  multiprocessing$MaximumJobs <- 10
  
  # Save
  saveDatasheet(ssimObject = newScenario, 
                data = multiprocessing, 
                name = "core_Multiprocessing")
  
  
  
  # Run ------------------------------------------------
  
  myResults <- run(newScenario)
  
}




