## a295
## Carina Rauen Firkowski 
## January 22, 2023
##
## This script generated the Omniscape inputs for the whole program evaluation
## library, which includes the following scenario:
##   - Current landscape
##   - "Natural areas" within the minimum convex polygon around the Eco Park
##     System become urbanized


# Load constants
source("Scripts/0-constants.R")



# Load tabular data ------------------------------------------------------------

crosswalkLULC <- read_csv(file.path(
  tabularDataDir, "Crosswalk", "LULC.csv"))
crosswalkHabSuit <- read_csv(file.path(
  tabularDataDir, "Crosswalk", "FocalSpeciesHabitatSuitabilityCrosswalk.csv"))
crosswalkResist <- read_csv(file.path(
  tabularDataDir, "Crosswalk", "FocalSpeciesResistanceCrosswalk.csv"))
minPatchSize <- read_csv(file.path(
  tabularDataDir, "Crosswalk", "FocalSpeciesMinPatchSize.csv"))



# Load spatial data ------------------------------------------------------------

# Analysis area
analysisArea <- st_read(dsn = file.path(intermediatesDir),
                        layer = "analysisArea")
# Minimum convex polygon
minCovexPolygon <- st_read(dsn = file.path(intermediatesDir),
                           layer = "minConvexPolygon")

# LULC
LULC <- rast(file.path(outputDir, "LULC", 
                       "LULC_ELC_AnalysisArea_29Jan2024.tif")) %>%
  terra::project(targetCRS)


# Resistance layer
resistanceBLBR <- rast(file.path(outputDir, "Resistance", 
                                 "BLBR_Resistance_AnalysisArea.tif"))

# Parks
currentParks <- st_read(dsn = file.path(spatialDataDir, "EcoPark lands"), 
                        layer = "CurrentEcoParkLands") %>%
  st_transform(targetCRS)



# Pre-process spatial data -----------------------------------------------------

# Eco Park System --------------------------------

hendryMulti <- currentParks[which(currentParks$OBJECTID == 24), ]
hendryMulti$DestinationID <- 100
hendry <- hendryMulti %>%
  st_cast("POLYGON") %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea) %>%
  rasterize(LULC, field = "DestinationID") %>%
  app(function(x){ ifelse(x == 100, 100, NA)})



# Parameters -------------------------------------------------------------------

# Focal species
specieslist <- c("BLBR", "ODVI", "EMBL")

# Suitability threshold
suitabilityThreshold <- 60

# Spatial parameters
# Reference raster
referenceRaster <- rast(normalizePath(file.path(intermediatesDir, "Omniscape", 
                                                paste0("BLBR", "_Resistance_",
                                                       "Fixed", ".tif"))))
targetRes <- res(referenceRaster)
targetExt <- ext(referenceRaster)
targetCRS <- crs(referenceRaster)
# Template raster
templateRaster <- rast()
crs(templateRaster) <- targetCRS
ext(templateRaster) <- targetExt
res(templateRaster) <- targetRes

# Function to generate Omniscape inputs
generate_omniscape_inputs <- function(restoredLULC, scenarioName, impactArea){
  
  # Loop over species to generate Omniscape inputs
  for(species in specieslist){
    
    # Create source layer ------------------------
    
    # Reclassify LULC based on habitat suitability values
    suitabilityRaster <- restoredLULC %>%
      terra::project(targetCRS) %>%
      classify(crosswalkHabSuit[, c("LULC_ID", species)])
    
    # Assign the maximum value to the specific EcoPark System land
    if(species == "EMBL"){
      suitabilityRaster <- max(suitabilityRaster, hendry, na.rm = TRUE)
    }
    
    # Species-specific minimum patch size 
    patchSizeThreshold <- minPatchSize$MinPatchSizeHa[
      minPatchSize$Species == species]
    
    # Select only patches than meet the minimum suitability threshold
    habitatRaster <- suitabilityRaster >= suitabilityThreshold
    
    # Convert from hectares to m
    conversionFromHa <- res(habitatRaster)[1] * res(habitatRaster)[2] * (1/10000)
    
    # Combine neighboring patches of like suitability
    habitatClump <- patches(habitatRaster, directions = 8,
                            zeroAsNA = TRUE, allowGaps = FALSE)
    
    # Calculate patch size
    habitatClumpID <- data.frame(freq(habitatClump))
    
    # Identify patches with frequency smaller than minimum habitat patch size (ha)
    habitatClumpID <- habitatClumpID[habitatClumpID$count < 
                                       patchSizeThreshold / conversionFromHa,]
    
    # Remove patches that do not meet the criteria
    habitatRaster[habitatClump %in% habitatClumpID$value] <- 0
    
    # Create a binary raster of suitable habitat
    binaryHabitatRaster <- app(habitatRaster, fun = function(x){
      ifelse(x > 0, 1, x)
    })
    
    # Re-sample source layer based on template raster
    sourceRaster <- binaryHabitatRaster %>% 
      resample(y = templateRaster, method = "mode") %>%
      terra::project(targetCRS)
    
    # Set all non-source patches to NA
    sourceRaster[sourceRaster != 1] <- NA
    
    
    # Create resistance layer --------------------
    
    # Reclassify
    resistanceRasterReclass <- restoredLULC %>%
      terra::project(targetCRS) %>%
      classify(crosswalkResist[, c("LULC_ID", species)])
    
    # Add lowest resistance to habitat patches
    resistanceRaster <- lapp(c(resistanceRasterReclass, habitatRaster), 
                             fun = function(x, y){
                               return(ifelse(y == 1, 1, x))
                             })
    
    
    # Call culverts & bridges update if necessary ------------------------------
    
    if(scenarioName == "ProgramEvaluation_Baseline"){
      source("Scripts/8-update-resistance-function.R")
      finalResistance <- update_resistance(
        resistanceRaster = resistanceRaster, species = species)
    } else {
      finalResistance <- resistanceRaster
    }
    
    # Save outputs -------------------------------
    
    # Save raster files with NAflag supported by Omniscape
    # Resistance layer
    writeRaster(x = finalResistance,
                filename = file.path(
                  intermediatesDir, "Omniscape", "Program evaluation",
                  paste0(species, "_Resistance_", scenarioName, ".tif")),
                datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
    
    # Source layer
    writeRaster(x = sourceRaster,
                filename = file.path(
                  intermediatesDir, "Omniscape", "Program evaluation",
                  paste0(species, "_Source_", scenarioName, ".tif")),
                datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
    
  } # End loop
  
  return("Omniscape inputs saved to file")
}



# Program evaluation inputs ----------------------------------------------------


# Mask LULC to EcoPark system lands
minConvexLULC <- LULC %>% 
  mask(minCovexPolygon)

# Mask LULC to EcoPark system lands
# ecoparksLULC <- LULC %>% 
#   mask(currentParks)

# Set LULC to "Built-Up Area - Impervious" for all natural land cover classes
naturalToUrbanized <- minConvexLULC 
naturalToUrbanized[!is.na(minConvexLULC) 
                 & minConvexLULC != 170 
                 & minConvexLULC != 214
                 & minConvexLULC != 213
                 & minConvexLULC != 212
                 & minConvexLULC != 211
                 & minConvexLULC != 210
                 & minConvexLULC != 206
                 & minConvexLULC != 205
                 & minConvexLULC != 204
                 & minConvexLULC != 203
                 & minConvexLULC != 202
                 & minConvexLULC != 194
                 & minConvexLULC != 193
                 & minConvexLULC != 192
                 & minConvexLULC != 191] <- 203 

# Merge LULC and urbanized EcoPark System
alternativeLULC <- merge(naturalToUrbanized, LULC)

# Save raster
writeRaster(x = alternativeLULC,
            filename = file.path(outputDir, "LULC", 
                                 "LULC_ELC_NaturalToUrbanized.tif"),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)

# Generate required inputs for Omniscape at the case-study extent
# Baseline
generate_omniscape_inputs(LULC, 
                          scenarioName = "ProgramEvaluation_Baseline",
                          impactArea = analysisArea)
# Alternative
generate_omniscape_inputs(alternativeLULC, 
                          scenarioName = "ProgramEvaluation_Alternative",
                          impactArea = analysisArea)


