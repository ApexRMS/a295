## a295
## Carina Rauen Firkowski 
## October 05, 2023
##
## This script generated the Omniscape inputs for two connectivity impact 
## assessment case-studies:
##   - Highway 6 overpass
##   - Pleasant View restoration
## Each case-study has three scenarios. For the Highway 6 overpass, scenarios 
## consider the conversion of LULC cover classes to Deciduous Forest at varying
## degrees:
##   - Scenario 1: Ambitious, 90 m overpass and conversion of 92 km^2
##   - Scenario 2: Intermediate, 90 m overpass and conversion of 1.5 km^2
##   - Scenario 3: Minimum, 60 m overpass and conversion of 140 m^2
## In all scenarios, changes to both the resistance and source layers apply.
## For the Pleasant View restoration, scenarios are based on the strategy
## mapping results given current connectivity in the area.
##   - Scenario 1: Reduction in the resistance of "Tilled" areas. Only changes 
##                 to the resistance layer apply.
##   - Scenario 2: Conversion of "Tilled" areas into "Deciduous Forest". Changes
##                 to both the resistance and source layers apply.



# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Analysis area
analysisArea <- st_read(dsn = file.path(intermediatesDir),
                        layer = "analysisArea")
# Focal area
focalArea <- st_read(dsn = file.path(intermediatesDir),
                     layer = "focalArea")

# Pleasant View restoration area
restorationArea <- st_read(dsn = file.path(intermediatesDir),
                        layer = "restorationFocalArea")

# Impact assessment area
impactAreaHW6 <- st_read(dsn = file.path(intermediatesDir),
                         layer = "HW6analysisExtent")
impactAreaPV <- st_read(dsn = file.path(intermediatesDir),
                        layer = "impactAssessmentBuffer")

# LULC
LULC <- rast(file.path(outputDir, "LULC", "LULC_ELC_AnalysisArea_new.tif")) %>%
  terra::project(targetCRS)
# LULC for the Highway 6 overpass case-study
hw6ambitiousLULC <- rast(file.path(intermediatesDir, "LULC", 
                                   "LULC_ELC_AnalysisArea-HW6-90mOverpassAmbitious.tif")) %>%
  terra::project(targetCRS)
hw6intermediateLULC <- rast(file.path(intermediatesDir, "LULC", 
                                   "LULC_ELC_AnalysisArea-HW6-90mOverpass.tif")) %>%
  terra::project(targetCRS)
hw6minimumLULC <- rast(file.path(intermediatesDir, "LULC", 
                                   "LULC_ELC_AnalysisArea-HW6-60mOverpassNoRestoration.tif")) %>%
  terra::project(targetCRS)

# Resistance layer
resistanceBLBR <- rast(file.path(outputDir, "Resistance", 
                                 "BLBR_Resistance_AnalysisArea.tif"))
resistanceEMBL <- rast(file.path(outputDir, "Resistance", 
                                 "EMBL_Resistance_AnalysisArea.tif"))
resistanceODVI <- rast(file.path(outputDir, "Resistance", 
                                 "ODVI_Resistance_AnalysisArea.tif"))
# Source layer
sourceBLBR <- rast(file.path(intermediatesDir, "Omniscape", 
                             "BLBR_Sources_Fixed.tif"))
sourceEMBL <- rast(file.path(intermediatesDir, "Omniscape",
                             "EMBL_Sources_Fixed.tif"))
sourceODVI <- rast(file.path(intermediatesDir, "Omniscape", 
                             "ODVI_Sources_Fixed.tif"))

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



# Load tabular data ------------------------------------------------------------

crosswalkHabSuit <- read_csv(file.path(tabularDataDir, "Crosswalk", 
                             "FocalSpeciesHabitatSuitabilityCrosswalk.csv"))
crosswalkResist <- read_csv(file.path(tabularDataDir, "Crosswalk",
                            "FocalSpeciesResistanceCrosswalk.csv"))
minPatchSize <- read_csv(file.path(tabularDataDir, "Crosswalk",
                         "FocalSpeciesMinPatchSize.csv"))


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
    
    # Mask and crop to impact assessment area ----
    
    resistanceRasterArea <- resistanceRaster %>%
      crop(impactArea) %>%
      mask(impactArea)
    sourceRasterArea <- sourceRaster %>%
      crop(impactArea) %>%
      mask(impactArea)
    
    # Save outputs -------------------------------
    
    # Save raster files with NAflag supported by Omniscape
    # Resistance layer
    writeRaster(x = resistanceRasterArea,
                filename = file.path(
                  intermediatesDir, "Omniscape", "Case-studies", "11-Jan-2024",
                  paste0(species, "_Resistance_", scenarioName, ".tif")),
                datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
    
    # Source layer
    writeRaster(x = sourceRasterArea,
                filename = file.path(
                  intermediatesDir, "Omniscape", "Case-studies", "11-Jan-2024",
                  paste0(species, "_Source_", scenarioName, ".tif")),
                datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
    
  } # End loop
  
  return("Omniscape inputs saved to file")
}



# The Highway 6 overpass case-study --------------------------------------------

generate_omniscape_inputs(LULC, scenarioName = "HW6_Baseline", impactArea = impactAreaHW6)
generate_omniscape_inputs(hw6ambitiousLULC, scenarioName = "HW6_Ambitious", impactArea = impactAreaHW6)
generate_omniscape_inputs(hw6intermediateLULC, scenarioName = "HW6_Intermediate", impactArea = impactAreaHW6)
generate_omniscape_inputs(hw6minimumLULC, scenarioName = "HW6_Minimum", impactArea = impactAreaHW6)



# The Pleasant View case-studies -----------------------------------------------

# Mask resistance, LULC and strategy maps to restoration area
restoreBLBR <- resistanceBLBR %>% 
  crop(restorationArea) %>% mask(restorationArea)
restoreEMBL <- resistanceEMBL %>% 
  crop(restorationArea) %>% mask(restorationArea)
restoreODVI <- resistanceODVI %>% 
  crop(restorationArea) %>% mask(restorationArea)
restoreLULC <- LULC %>%
  crop(restorationArea) %>%
  mask(restorationArea)



# Restoration case-study -------------------------

# Reclassify "Tilled" to "Deciduous Forest"
deciduousLULC <- restoreLULC
deciduousLULC[deciduousLULC == 193] <- 93
restoredDeciduousLULC <- merge(deciduousLULC, LULC)

# Save raster
# Analysis extent
writeRaster(x = restoredDeciduousLULC,
            filename = file.path(outputDir, "LULC", 
                                 "LULC_ELC_RestoreToDeciduous.tif"),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)


# Generate required inputs for Omniscape at the case-study extent
# Baseline
generate_omniscape_inputs(LULC, 
                          scenarioName = "RestorationBaseline",
                          impactArea = restorationArea)
# Alternative
generate_omniscape_inputs(restoredDeciduousLULC, 
                          scenarioName = "RestoretoDeciduous",
                          impactArea = restorationArea)



# Incentives case-study --------------------------

# Set all non "Tilled" cells to NA
tilledLULC <- restoreLULC
tilledLULC[tilledLULC != 193] <- NA

# Mask resistance map to restoration area
resistanceHalfBLBR <- mask(restoreBLBR, tilledLULC)
resistanceHalfEMBL <- mask(restoreEMBL, tilledLULC)
resistanceHalfODVI <- mask(restoreODVI, tilledLULC)

# Reduce resistance in "Tilled" cells by half 
resistanceHalfBLBR <- resistanceHalfBLBR/2
resistanceHalfEMBL <- resistanceHalfEMBL/2
resistanceHalfODVI <- resistanceHalfODVI/2

# Merge resistance layer back to analysis area extent
restoredBLBR <- merge(resistanceHalfBLBR, resistanceBLBR)
restoredEMBL <- merge(resistanceHalfEMBL, resistanceEMBL)
restoredODVI <- merge(resistanceHalfODVI, resistanceODVI)

# Crop to case-study extent
incentivesBLBR <- restoredBLBR %>% 
  crop(restorationArea) %>% mask(restorationArea)
incentivesEMBL <- restoredEMBL %>% 
  crop(restorationArea) %>% mask(restorationArea)
incentivesODVI <- restoredODVI %>% 
  crop(restorationArea) %>% mask(restorationArea)

# Crop sources to case-study extent
sourceIncentivesBLBR <- sourceBLBR %>% 
  crop(restorationArea) %>% mask(restorationArea)
sourceIncentivesEMBL <- sourceEMBL %>% 
  crop(restorationArea) %>% mask(restorationArea)
sourceIncentivesODVI <- sourceODVI %>% 
  crop(restorationArea) %>% mask(restorationArea)

# Save to file
# Case-study extent
# Resistance
writeRaster(x = incentivesBLBR,
            filename = file.path(
              intermediatesDir, "Omniscape", "Case-studies",
              paste0("BLBR", "_Resistance_", "Incentives", ".tif")),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
writeRaster(x = incentivesEMBL,
            filename = file.path(
              intermediatesDir, "Omniscape", "Case-studies",
              paste0("EMBL", "_Resistance_", "Incentives", ".tif")),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
writeRaster(x = incentivesODVI,
            filename = file.path(
              intermediatesDir, "Omniscape", "Case-studies",
              paste0("ODVI", "_Resistance_", "Incentives", ".tif")),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
# Sources
writeRaster(x = sourceIncentivesBLBR,
            filename = file.path(
              intermediatesDir, "Omniscape", "Case-studies",
              paste0("BLBR", "_Sources_", "Incentives", ".tif")),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
writeRaster(x = sourceIncentivesEMBL,
            filename = file.path(
              intermediatesDir, "Omniscape", "Case-studies",
              paste0("EMBL", "_Sources_", "Incentives", ".tif")),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
writeRaster(x = sourceIncentivesODVI,
            filename = file.path(
              intermediatesDir, "Omniscape", "Case-studies",
              paste0("ODVI", "_Sources_", "Incentives", ".tif")),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
# Analysis extent
writeRaster(x = restoredBLBR,
            filename = file.path(
              intermediatesDir, "Omniscape", "Case-studies",
              paste0("BLBR", "_Resistance_", "Incentives", "_Analysis", ".tif")),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
writeRaster(x = restoredEMBL,
            filename = file.path(
              intermediatesDir, "Omniscape", "Case-studies",
              paste0("EMBL", "_Resistance_", "Incentives", "_Analysis", ".tif")),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)
writeRaster(x = restoredODVI,
            filename = file.path(
              intermediatesDir, "Omniscape", "Case-studies",
              paste0("ODVI", "_Resistance_", "Incentives", "_Analysis", ".tif")),
            datatype = "INT2S", NAflag = 9999L, overwrite = TRUE)


