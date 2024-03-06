## a295
## Carina Rauen Firkowski 
## June 1, 2023
##
## This script pre-processes the inputs required to run Omniscape.



# Load constants
source("Scripts/0-constants.R")



# Define inputs parameters -----------------------------------------------------

# Previous connectivity assessment results (2020)
#dataSource <- spatialDataDir
# OR
# Refined inputs from current project (2023)
dataSource <- outputDir

# Define file name
#if(dataSource == spatialDataDir) extentName <- "20km"
if(dataSource == outputDir) extentName <- "AnalysisArea"

# Define source strength method
# Fixed = all sources have the same value (1)
sourceMethod <- "Fixed"
# OR
# Variable = source strength differs based on habitat suitability
#sourceMethod <- "Variable"



# Open data layers -------------------------------------------------------------

# Target species
speciesList <- c("BLBR", "EMBL", "ODVI")

for(species in speciesList){
  
  # Resistance layer
  resistanceRaster <- rast(file.path(dataSource, "Resistance",
                                     paste0(species, "_ResistanceCrossings_", 
                                            extentName, ".tif")))
  
  # Source layer
  if(sourceMethod == "Fixed"){
    # Fixed source strength 
    patchesRaster <- rast(file.path(dataSource, "Habitat patch",
                                    paste0(species, "_HabitatPatchID_",
                                           extentName, ".tif")))
  }
  if(sourceMethod == "Variable"){
    # Variable source strength
    patchesRaster <- rast(file.path(dataSource, "Habitat suitability",
                                    paste0(species, "_HabitatSuitability_", 
                                           extentName, ".tif")))
  }
  
  
  
  # Spatial processing ---------------------------------------------------------
  
  # Spatial parameters
  targetRes <- res(resistanceRaster)
  targetExt <- ext(resistanceRaster)
  targetCRS <- crs(resistanceRaster)
  
  # Template raster
  templateRaster <- rast()
  crs(templateRaster) <- targetCRS
  ext(templateRaster) <- targetExt
  res(templateRaster) <- targetRes
  
  # Re-sample source layer based on template raster
  patchesTarget <- patchesRaster %>% 
    resample(y = templateRaster, method = "mode")
  
  # Set all patches to 1 to represent sources
  sourceRaster <- which.lyr(!is.na(patchesTarget))

  # Save raster files with NAflag supported by Omniscape
  # Resistance layers
  writeRaster(x = resistanceRaster,
              filename = file.path(intermediatesDir, "Omniscape",
                                   paste0(species, "_ResistanceCrossings_", 
                                          sourceMethod, ".tif")),
              datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
  
  # Source layers
  writeRaster(x = sourceRaster,
              filename = file.path(intermediatesDir, "Omniscape",
                                   paste0(species, "_Sources_", 
                                          sourceMethod, ".tif")),
              datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)

}



# # Parameters -------------------------------------------------------------------
# 
# # Reclassification table for resistance
# resistanceReclass <- matrix(c(1, 1,
#                               2, 2,
#                               4, 4,
#                               6, 6,
#                               8, 8,
#                               16, 16,
#                               32, 32,
#                               100, 100), ncol = 2, byrow = TRUE)
# # Save
# write.table(resistanceReclass, 
#             file = file.path(intermediatesDir,
#                              "Omniscape",
#                              "reclass_resistance.txt"),
#             row.names = FALSE, col.names = FALSE)


