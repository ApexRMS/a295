## a295
## Carina Rauen Firkowski 
## June 01, 2023
##
## Originally written by Bronwyn Rayfield, Chloe Debeyser and Caroline Tucker on
## 08/2020 for the Royal Botanical Gardens Wildlife Corridor Mapping project.
##
## This script processes land use and land cover layers to create a LULC map at 
## the analysis area extent. LULC maps are also created for visualization at the
## analysis and focal area extents. Tabular summaries of the frequency of each 
## LULC classes are also created for both extents. 



# Load constants
source("Scripts/0-constants.R")



# Load tabular data ------------------------------------------------------------

# Study area
studyAreaPoints <- read_csv(file.path(tabularDataDir, "Study Area",
                                      "Vertices 20200610 DAG.csv"))

# LULC crosswalk
crosswalk <- read_csv(file.path(tabularDataDir, "Crosswalk",
                                "LULC.csv"))
# ELC crosswalk
ELCcrosswalkHalton <- read_csv(file.path(tabularDataDir, "Crosswalk",
                                         "ELC-Halton.csv"))
ELCcrosswalkHamilton <- read_csv(file.path(tabularDataDir, "Crosswalk",
                                          "ELC-Hamilton.csv"))

                            
# Load spatial data ------------------------------------------------------------

# Analysis area
analysisArea <- st_read(dsn = file.path(intermediatesDir),
                        layer = "analysisArea")

# Focal area
focalArea <- st_read(dsn = file.path(intermediatesDir),
                     layer = "focalArea")


# LULC data --------------------------------------

# SOLRIS
SOLRISRaw <- rast(file.path(spatialDataDir, "Raw", 
                            "SOLRIS", 
                            "SOLRIS_Version_3_0_LAMBERT.tif"))

# Provincial lands
OLCDBRaw <- rast(file.path(spatialDataDir, "Raw",
                           "Provincial Land Cover 1996 - 28 Class Grid",
                           "plc1996_28.tif"))

# Roads
ORNRaw <- st_read(dsn = file.path(spatialDataDir, "Raw",
                                  "Ontario_Road_Network__ORN__Segment_With_Address-shp"), 
                  layer = "Ontario_Road_Network__ORN__Segment_With_Address")

# Water
OHNRaw <- st_read(dsn = file.path(spatialDataDir, "Raw",
                                  "Ontario_Hydro_Network__OHN__-_Waterbody-shp"),
                  layer = "Ontario_Hydro_Network__OHN__-_Waterbody")

# Urban areas
urbanRaw <- st_read(dsn = file.path(spatialDataDir, "Raw", 
                                    "Built-Up_Area-shp"), 
                    layer = "Built-Up_Area")

# Railway
railRaw <- st_read(dsn = file.path(spatialDataDir, "Raw",
                                   "ORWNTRK", "LIO-2019-09-30"),
                   layer = "ORWN_TRACK")

# EcoPark System partner lands
currentParks <- st_read(dsn = file.path(spatialDataDir, "EcoPark lands"),
                        layer = "CurrentEcoParkLands")
# Berry's tract
berryII <- currentParks[which(currentParks$Name_1 == "Berry Tract II"),]
# Hopkins tract
hopkinsMulti <- currentParks[which(currentParks$Name_1 == "Hopkins Track"),]
hopkins <- st_cast(hopkinsMulti, "POLYGON")

# Culvert and bridges
culverts <- st_read(dsn = file.path(spatialDataDir, 
                                    "Culverts & Bridges", "Update"),
                    layer = "C2E_Ecocorridor_BridgesCulverts")



# ELC --------------------------------------------

# Halton region
elcHaltonRaw <- st_read(dsn = file.path(spatialDataDir, "Raw", "ELC"),
                        layer = "Ecological_Land_Classification__ELC_") %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)

# RGB
# Borers Falls
elcBorersRaw <- st_read(
  dsn = file.path(spatialDataDir, "Raw", "ELC", "RGB",
                  "Borers_Falls_Ecological_Land_Classification"),
  layer = "Ecological_Land_Classification") %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)
# Burlington Heights
elcBurligntonRaw <- st_read(
  dsn = file.path(spatialDataDir, "Raw", "ELC", "RGB",
                  "Burlington_Heights_Ecological_Land_Classification"),
  layer = "Ecological_Land_Classification") %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)
# Clappison Grindstone
elcClappisonRaw <- st_read(
  dsn = file.path(spatialDataDir, "Raw", "ELC", "RGB",
                  "Clappison_Grindstone_Ecological_Land_Classification"),
  layer = "Ecological_Land_Classification") %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)
# Cootes Paradise
elcCootesRaw <- st_read(
  dsn = file.path(spatialDataDir, "Raw", "ELC", "RGB",
                  "Cootes_Paradise_Ecological_Land_Classification"),
  layer = "Ecological_Land_Classification") %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)
# Waterdown Sassafras
elcSassafrasRaw <- st_read(
  dsn = file.path(spatialDataDir, "Raw", "ELC", "RGB",
                  "Waterdown_Sassafras_Ecological_Land_Classification"),
  layer = "Ecological_Land_Classification") %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)

# Hamilton
elcHamiltonRaw <- st_read(
  dsn = file.path(spatialDataDir, "Raw", "ELC", "Hamilton"),
  layer = "elc") %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)



# Crop LULC data to analysis area & standardize layers -------------------------

# Buffer around roads to ensure alignment with SOLRIS
roadbuffer <- 12 


# SOLRIS -----------------------------------------

# Crop data to analysis area
SOLRISanalysis <- SOLRISRaw %>%
  crop(ext(analysisArea), snap = "out") %>%
  mask(analysisArea)

# Reclassify values resistance scores
SOLRISreclass <- SOLRISanalysis %>%
  classify(crosswalk[which(crosswalk$Source_Name == "SOLRIS"), 
                       c("Source_ID", "Destination_ID")])


# Provincial lands -------------------------------

# Align with SOLRIS and buffer SOLRIS extent
OLCDBext <- extend(ext(SOLRISanalysis), 1000)

# Crop data to analysis area
OLCDBanalysis <- OLCDBRaw %>% 
  crop(OLCDBext, snap = "out") %>% 
  terra::project(crs(SOLRISanalysis), method = "near") %>%
  resample(SOLRISanalysis, method = "near") %>%
  mask(analysisArea)

# Reclassify
OLCDBreclass <- OLCDBanalysis %>%
  classify(crosswalk[which(crosswalk$Source_Name == "OLCDB"), 
  		                     c("Source_ID", "Destination_ID")])

# Roads ------------------------------------------

# Crop data to analysis area
ORNanalysis <- ORNRaw %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea) %>%
  st_buffer(roadbuffer)

# Road classes
ORNmajor <- ORNanalysis[ORNanalysis$ROAD_CLASS %in% c("Freeway", 
                                                      "Expressway / Highway", 
                                                      "Ramp", 
                                                      "Rapid Transit"),]
ORNintrm <- ORNanalysis[ORNanalysis$ROAD_CLASS %in% c("Arterial", 
                                                      "Collector"),]
ORNminor <- ORNanalysis[ORNanalysis$ROAD_CLASS %in% c("Local / Strata", 
                                                      "Local / Street", 
                                                      "Service", 
                                                      "Alleyway / Laneway", 
                                                      "<NA>", 
                                                      "Local / Unknown"),]
# Reclassify & rasterize
# All
ORNreclass <- ORNanalysis %>%
  left_join(crosswalk[which(crosswalk$Source_Name == "ORN"), 
                      c("Destination_ID", "Source_Label")], 
            by = c("ROAD_CLASS" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID", touches = TRUE)
# Major
ORNreclassMajor <- ORNmajor %>%
  left_join(crosswalk[which(crosswalk$Source_Name == "ORN"), 
                      c("Destination_ID", "Source_Label")], 
            by = c("ROAD_CLASS" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID", touches = TRUE)
# Intermediate
ORNreclassIntrm <- ORNintrm %>%
  left_join(crosswalk[which(crosswalk$Source_Name == "ORN"), 
                      c("Destination_ID", "Source_Label")], 
            by = c("ROAD_CLASS" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID", touches = TRUE)
# Minor
ORNreclassMinor <- ORNminor %>%
  left_join(crosswalk[which(crosswalk$Source_Name == "ORN"), 
                      c("Destination_ID", "Source_Label")], 
            by = c("ROAD_CLASS" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID", touches = TRUE)


# Railway ----------------------------------------

# Crop data to analysis area
railAnalysis <- railRaw %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea) %>%
  mutate(TYPE = "Railway")

# Reclassify & rasterize
railReclass <- railAnalysis %>%
  left_join(crosswalk[which(crosswalk$Source_Name == "ORWNTRK"), 
                      c("Destination_ID", "Source_Label")], 
            by = c("TYPE" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID") 		


# Water ------------------------------------------

# Crop to buffered study area
OHNanalysis <- OHNRaw %>%
  st_transform(crs(analysisArea)) %>%
  mutate(AREA = as.numeric(st_area(.))) %>%
  filter(AREA >= 100) %>%
  st_intersection(analysisArea) 

# Reclassify
OHNreclass <- OHNanalysis %>%
  left_join(crosswalk[which(crosswalk$Source_Name == "OHN"), 
                      c("Destination_ID", "Source_Label")], 
            by = c("WATERBODY_" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID")


# Urban Areas ------------------------------------

# Crop data to analysis area
urbanAnalysis <- urbanRaw %>%
  st_transform(crs(analysisArea)) %>%
  st_buffer(0) %>%
  st_intersection(analysisArea)

# Reclassify
urbanReclass <- urbanAnalysis %>%
  left_join(crosswalk[which(crosswalk$Source_Name == "Built-up Areas"), 
  		                c("Destination_ID", "Source_Label")], 
  		      by = c("COMMUNIT_1" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID")


# Specific land plots ---------------------------

# Crop data to analysis area
berryAnalysis <- berryII %>%
	st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)
hopkinsAnalysis <- hopkins %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)

# Add attribute for "Pasture and Abandoned fields"
berryAnalysis$DestinationID <- 194
hopkinsAnalysis$DestinationID <- 194		

# Rasterize
berryReclass <- rasterize(berryAnalysis, SOLRISanalysis, 
                          field = "DestinationID")
hopkinsReclass <- rasterize(hopkinsAnalysis, SOLRISanalysis,
                            field = "DestinationID")



# Clean ELC data ---------------------------------------------------------------

# Function to transform and crop to analysis area parameters
set_analysisArea <- function(x){
  x %>%
    st_transform(., crs(analysisArea)) %>%
    st_intersection(., analysisArea)
}


# Halton region ----------------------------------

# Set analysis area
elcHaltonAnalysis <- set_analysisArea(elcHaltonRaw)

# Reclassify & rasterize
elcHaltonReclass <- elcHaltonAnalysis %>%
  left_join(ELCcrosswalkHalton[which(ELCcrosswalkHalton$Source_Name == "ELC"), 
                               c("Destination_ID", "Source_Label")], 
            by = c("Series_Des" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID")


# Borers Falls -----------------------------------

# Set analysis area
elcBorersAnalysis <- set_analysisArea(elcBorersRaw)

# Reclassify & rasterize
elcBorersReclass <- elcBorersAnalysis %>%
  left_join(ELCcrosswalkHamilton[which(ELCcrosswalkHamilton$Data_Source == "borers"), 
                                 c("Destination_ID", "Source_Label")], 
            by = c("elc_mod" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID")


# Burlington Heights -----------------------------

# Set analysis area
elcBurligntonAnalysis <- set_analysisArea(elcBurligntonRaw)

# Reclassify & rasterize
elcBurligntonReclass <- elcBurligntonAnalysis %>%
  left_join(ELCcrosswalkHamilton[
    which(ELCcrosswalkHamilton$Data_Source == "burlington"), 
    c("Destination_ID", "Source_Label")], 
    by = c("Series_Des" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID")


# Clappison Grindstone ---------------------------

# Set analysis area
elcClappisonAnalysis <- set_analysisArea(elcClappisonRaw)

# Reclassify & rasterize
elcClappisonReclass <- elcClappisonAnalysis %>%
  left_join(ELCcrosswalkHamilton[which(
    ELCcrosswalkHamilton$Data_Source == "clappison"), 
    c("Destination_ID", "Source_Label")], 
    by = c("ELC1" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID")


# Cootes Paradise --------------------------------

# Set analysis area
elcCootesAnalysis <- set_analysisArea(elcCootesRaw)

# Reclassify & rasterize
elcCootesReclass <- elcCootesAnalysis %>%
  left_join(ELCcrosswalkHamilton[which(
    ELCcrosswalkHamilton$Data_Source == "cootes"), 
    c("Destination_ID", "Source_Label")], 
    by = c("elc_mod" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID")


# Waterdown Sassafras ----------------------------

# Set analysis area
elcSassafrasAnalysis <- set_analysisArea(elcSassafrasRaw)

# Reclassify & rasterize
elcSassafrasReclass <- elcSassafrasAnalysis %>%
  left_join(ELCcrosswalkHamilton[which(
    ELCcrosswalkHamilton$Data_Source == "sassafras"), 
    c("Destination_ID", "Source_Label")], 
    by = c("ELC1" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID")


# Hamilton region --------------------------------

# Set analysis area
elcHamiltonAnalysis <- set_analysisArea(elcHamiltonRaw)

# Reclassify & rasterize
elcHamiltonReclass <- elcHamiltonAnalysis %>%
  left_join(ELCcrosswalkHamilton[which(
    ELCcrosswalkHamilton$Data_Source == "hamilton"), 
    c("Destination_ID", "Source_Label")], 
    by = c("NEW_VEG_CO" = "Source_Label")) %>%
  rasterize(SOLRISanalysis, field = "Destination_ID")

# All ELC layers ----------------------------------

ELCreclass <- merge(elcBorersReclass, elcBurligntonReclass, 
                    elcClappisonReclass, elcCootesReclass, elcSassafrasReclass, 
                    elcHamiltonReclass, elcHaltonReclass)



# Combine LULC layers ----------------------------------------------------------

# Remove undifferentiated cells from SOLRIS to receive value from OLCDB
SOLRISreclass[SOLRISreclass == 250] <- NA

# Merge raster layers
# NOTE: urban areas have priority, followed by ORN major roads, ORN intermediate
#       roads, ORN minor roads, OHN, specific land plots, SOLRIS, and OLCDB.
LULC <- merge(ORNreclassMajor, urbanReclass, railReclass, 
              ORNreclassIntrm, ORNreclassMinor, OHNreclass, 
              berryReclass, hopkinsReclass, SOLRISreclass, OLCDBreclass)

# LULC for visualization
LULCviz <- merge(railReclass, ORNreclassMajor, ORNreclassIntrm, ORNreclassMinor,
                 OHNreclass, berryReclass, hopkinsReclass, 
                 SOLRISreclass, OLCDBreclass)

# LULC with ELC data
LULC_ELC <- merge(ORNreclassMajor, urbanReclass, railReclass,
                  ORNreclassIntrm, ORNreclassMinor, OHNreclass,
                  berryReclass, hopkinsReclass, 
                  elcBorersReclass, elcBurligntonReclass, 
                  elcClappisonReclass, elcCootesReclass, elcSassafrasReclass, 
                  elcHamiltonReclass, elcHaltonReclass, 
                  SOLRISreclass, OLCDBreclass)

# LULC for visualization
LULC_ELCviz <- merge(railReclass, 
                     ORNreclassMajor, ORNreclassIntrm, ORNreclassMinor,
                     OHNreclass, berryReclass, hopkinsReclass,
                     elcBorersReclass, elcBurligntonReclass, 
                     elcClappisonReclass, elcCootesReclass, elcSassafrasReclass, 
                     elcHamiltonReclass, elcHaltonReclass, 
                     SOLRISreclass, OLCDBreclass)

# Crop data to focal area
LULCvizFocal <- LULCviz %>%
  crop(ext(focalArea), snap = "out") %>%
  mask(focalArea)
LULC_ELCvizFocal <- LULC_ELCviz %>%
  crop(ext(focalArea), snap = "out") %>%
  mask(focalArea)

# Calculate percentage of each class in LULC maps
# Analysis area
freqLULCvizAnalysis <- as_tibble(freq(LULCviz)) %>%
  filter(!is.na(value)) %>% 
  mutate(percent = count/sum(count)*100) %>% 
  arrange(desc(percent))
freqLULC_ELCvizAnalysis <- as_tibble(freq(LULC_ELCviz)) %>%
  filter(!is.na(value)) %>% 
  mutate(percent = count/sum(count)*100) %>% 
  arrange(desc(percent))
# Focal area
freqLULCvizFocal <- as_tibble(freq(LULCvizFocal)) %>%
  filter(!is.na(value)) %>% 
  mutate(percent = count/sum(count)*100) %>% 
  arrange(desc(percent))
freqLULC_ELCvizFocal <- as_tibble(freq(LULC_ELCvizFocal)) %>%
  filter(!is.na(value)) %>% 
  mutate(percent = count/sum(count)*100) %>% 
  arrange(desc(percent))




# Save outputs -----------------------------------------------------------------

# ELC -------------------------------------------

writeRaster(ELCreclass, 
            file.path(outputDir, "ELC", "ELC_AnalysisArea_new.tif"), 
            overwrite=TRUE)

# LULC ------------------------------------------

# Frequency 
write_csv(freqLULCvizAnalysis, 
          file.path(outputDir, "LULC", 
                    "LULC_Visualization_AnalysisArea_Freq.csv"))
write_csv(freqLULCvizFocal, 
          file.path(outputDir, "LULC",
                    "LULC_Visualization_FocalArea_Freq.csv"))
write_csv(freqLULC_ELCvizAnalysis, 
          file.path(outputDir, "LULC", 
                    "LULC_ELC_Visualization_AnalysisArea_Freq.csv"))
write_csv(freqLULC_ELCvizFocal, 
          file.path(outputDir, "LULC",
                    "LULC_ELC_Visualization_FocalArea_Freq.csv"))

# Analysis area
writeRaster(LULC, 
            file.path(outputDir, "LULC", "LULC_AnalysisArea.tif"), 
            overwrite = TRUE)
writeRaster(LULCviz, 
            file.path(outputDir, "LULC", "LULC_Visualization_AnalysisArea.tif"), 
            overwrite=TRUE)
writeRaster(LULC_ELC, 
            file.path(outputDir, "LULC", "LULC_ELC_AnalysisArea.tif"), 
            overwrite = TRUE)
writeRaster(LULC_ELCviz, 
            file.path(outputDir, "LULC", "LULC_ELC_Visualization_AnalysisArea.tif"), 
            overwrite=TRUE)

# Focal area
writeRaster(LULCvizFocal, 
            file.path(outputDir, "LULC", "LULC_Visualization_FocalArea.tif"), 
            overwrite=TRUE)
writeRaster(LULC_ELCvizFocal, 
            file.path(outputDir, "LULC", "LULC_ELC_Visualization_FocalArea.tif"), 
            overwrite=TRUE)


