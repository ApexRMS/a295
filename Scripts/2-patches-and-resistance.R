## a295
## Carina Rauen Firkowski 
## June 01, 2023
##
## Originally written by Bronwyn Rayfield, Chloe Debeyser and Caroline Tucker on
## 09/2020 for the Royal Botanical Gardens Wildlife Corridor Mapping project.
##
## This script creates the habitat suitability and resistance raster layers,
## based on:
##   - the LULC data layer created in the "1-LULC" script 
##   - the crosswalk tables between land cover classes and species-specific:
##        - resistance values
##        - habitat suitability



# Load constants
source("Scripts/0-constants.R")



# Parameters -------------------------------------------------------------------

# Settings
options(stringsAsFactors = FALSE, SHAPE_RESTORE_SHX = TRUE, 
        useFancyQuotes = FALSE, digits = 10)

# Species
specieslist <- c("BLBR", "ODVI", "EMBL")

# Buffer width (in km)
analysisAreaBuffer <- 20

# Suitability threshold
suitabilityThreshold <- 60



# Load tabular data ------------------------------------------------------------

# Tabular data
crosswalkHabSuit <- read_csv(file.path(tabularDataDir, "Crosswalk", 
                                       "FocalSpeciesHabitatSuitabilityCrosswalk.csv"))
crosswalkResist <- read_csv(file.path(tabularDataDir, "Crosswalk",
                                      "FocalSpeciesResistanceCrosswalk.csv"))
minPatchSize <- read_csv(file.path(tabularDataDir, "Crosswalk",
                                   "FocalSpeciesMinPatchSize.csv"))



# Load spatial data ------------------------------------------------------------

# Analysis area
analysisArea <- st_read(dsn = file.path(intermediatesDir),
                        layer = "analysisArea")

# Focal area
focalArea <- st_read(dsn = file.path(intermediatesDir),
                     layer = "focalArea")

# LULC
LULC <- rast(file.path(outputDir, "LULC", "LULC_ELC_AnalysisArea_new.tif")) %>%
  terra::project(targetCRS)

# Parks
currentParks <- st_read(dsn = file.path(spatialDataDir, "EcoPark lands"), 
                        layer = "CurrentEcoParkLands") %>%
  st_transform(targetCRS)

# Specific land
hendryMulti <- currentParks[which(currentParks$OBJECTID == 24), ]
hendryMulti$DestinationID <- 100
hendry <- hendryMulti %>%
  st_cast("POLYGON") %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea) %>%
  rasterize(LULC, field = "DestinationID") %>%
  app(function(x){ ifelse(x == 100, 100, NA)})


# Create habitat suitability layer ---------------------------------------------

# Loop over species to generate habitat suitability & resistance rasters
for(species in specieslist){
  
  # Reclassify LULC based on habitat suitability values
  suitabilityRaster <- LULC %>%
    classify(crosswalkHabSuit[, c("LULC_ID", species)])
  
  # Assign the maximum value to the specific EcoPark System land
  if(species == "EMBL"){
    suitabilityRaster <- max(suitabilityRaster, hendry, na.rm = TRUE)
  }
  
  
  # Create habitat patches -----------------------
  
  # Species-specific minimum patch size 
  patchSizeThreshold <- minPatchSize$MinPatchSizeHa[
    minPatchSize$Species == species]
  
  # Select only patches than meet the minimum suitability threshold
  habitatRaster <- suitabilityRaster >= suitabilityThreshold
  
  # Convert from hectares to m
  conversionFromHa <- res(habitatRaster)[1] * res(habitatRaster)[2] * (1/10000)
  
  # Combine neighbouring patches of like suitability
  habitatClump <- patches(habitatRaster, directions = 8,
                          zeroAsNA = TRUE, allowGaps = FALSE)
  
  # Calculate patch size
  habitatClumpID <- data.frame(freq(habitatClump))
  
  # Identify patches with frequency smaller than minimum habitat patch size (ha)
  habitatClumpID <- habitatClumpID[habitatClumpID$count < 
                                     patchSizeThreshold / conversionFromHa,]
  
  # Remove patches that do not meet the criteria
  habitatRaster[habitatClump %in% habitatClumpID$value] <- 0
  
  # Unique ID for all patches > min. size & suitability threshold
  habitatRasterCont <- patches(habitatRaster, directions = 8, 
                               zeroAsNA = TRUE, allowGaps = FALSE) 
  
  # Create a binary raster of suitable habitat
  binaryHabitatRaster <- app(habitatRaster, fun = function(x){
    ifelse(x > 0, 1, x)
    })
  
  
  # Create resistance layer ----------------------
  
  # Reclassify
  resistanceRasterReclass <- LULC %>%
    classify(crosswalkResist[, c("LULC_ID", species)])
  
  # Overlay habitat patches
  resistanceRaster <- lapp(c(resistanceRasterReclass, habitatRaster), 
                              fun = function(x, y){
                                return(ifelse(y == 1, 1, x))
                                })
  
  
  # Crop resistance to focal area ----------------
  
  resistanceFocal <- resistanceRaster %>%
    crop(ext(focalArea), snap = "out") %>%
    mask(focalArea)
  
  # Save outputs ---------------------------------

  writeRaster(habitatRaster, 
              file.path(outputDir, "Habitat patch",
                       paste0(species, "_HabitatPatches_AnalysisArea.tif")), 
             overwrite = TRUE)
  writeRaster(habitatRasterCont, 
             file.path(outputDir, "Habitat patch",
                       paste0(species, "_HabitatPatchID_AnalysisArea.tif")), 
             overwrite = TRUE)
  writeRaster(resistanceRaster, 
             file.path(outputDir, "Resistance",
                       paste0(species, "_Resistance_AnalysisArea.tif")), 
             overwrite = TRUE)
  writeRaster(resistanceFocal, 
            file.path(outputDir, "Resistance",
                      paste0(species, "_Resistance_FocalArea.tif")), 
            overwrite = TRUE)
  
} # End loop


