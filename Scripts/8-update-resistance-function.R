## a295
## Carina Rauen Firkowski 
## November 20, 2023
##
## This script updates the resistance raster layers with the location of viable 
## culverts and bridges for species.



# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Analysis area
analysisArea <- st_read(dsn = file.path(intermediatesDir),
                        layer = "analysisArea")

# Focal area
focalArea <- st_read(dsn = file.path(intermediatesDir),
                     layer = "focalArea")

# Culvert and bridges
crossingsRaw <- st_read(dsn = file.path(spatialDataDir, 
                                        "Culverts & Bridges", "Update",
                                        "C2E_EcoCorridors_10242023"),
                        layer = "C2E_EcoCorridors_10242023")

# Template raster
templateRaster <- rast(file.path(outputDir, "Resistance", 
                                 "BLBR_Resistance_AnalysisArea.tif"))

# Crop to analysis area
crossingsAnalysis <- crossingsRaw %>%
  st_transform(crs(analysisArea)) %>%
  st_intersection(analysisArea)



# Parameters -------------------------------------------------------------------

# Buffer culverts by constant (value in meters)
bufferSize <- 30

# Suitable culverts & bridges receive a resistance value of 10
culvertResistancevalue <- 1
bridgeResistancevalue <- 1



# Calculate openness ratio (OR) for culverts -----------------------------------

# Function 
# NOTE: input are in cm and output units are in meters
# Calculation for circular culverts
ORcirc <- function(length, width){ 
  (pi * (0.5 * width/100)^2) / (length/100)
}
# Calculation for box culverts
ORbox <- function(length, width, height){ 
  ((height/100) * (width/100)) / (length/100)
}

# Subset culvert points
culverts <- crossingsAnalysis %>%
  filter(StructureT == "Culvert")

# Calculate OR 
culverts$OR <- culverts %>%
  apply(., 1, function(x){
    ifelse(x$StructureS != "Box", 
           ORcirc(length = x$Barrel_Len, 
                  width = min(x$Diameter_W, x$Diameter_1, na.rm = TRUE)),
           ORbox(length = x$Barrel_Len, 
                 width = min(x$Diameter_W, x$Diameter_1, na.rm = TRUE), 
                 height = min(x$Height_S1, x$Height_S2, na.Rm = TRUE)))
  })



# Pre-process bridges data -----------------------------------------------------

# Subset bridge points
bridges <- crossingsAnalysis %>%
  filter(StructureT == "Bridge")

# Bridges receive resistance value of 1
bridges <- bridges %>% 
  mutate(Resistance = bridgeResistancevalue)

# Add buffer
bridgeBuffer <- bridges %>%
  st_buffer(., bufferSize)

# Rasterize
bridgeRaster <- bridgeBuffer %>% 
  rasterize(templateRaster, field = "Resistance")



# Update resistance per species for current connectivity -----------------------

update_resistance <- function(resistanceRaster, species, 
                              culvertData = culverts,
                              bridgeRasterData = bridgeRaster){
  
  # Set minimum culvert size
  if(species == "BLBR"){
    minCulvertSize <- 0.05   # m
    minHeightWidth <- 30     # cm
    maxLength <- NA          # cm
  }  
  if(species == "EMBL"){
    minCulvertSize <- 0.1
    minHeightWidth <- 50
    maxLength <- 2500
  }  
  if(species == "ODVI"){
    minCulvertSize <- 0.6
    minHeightWidth <- 200
    maxLength <- 9000
  }
  
  # Filter culverts ------------------------------
  
  # Keep culverts > minCulvertSize
  culvertsOR <- culvertData %>% 
    #drop_na(OR) %>%
    filter(OR > minCulvertSize) %>%
    filter(!is.infinite(OR)) 
  
  # Keep culverts with height and width > minHeightWidth
  # Box culverts
  culvertsBox <- culvertsOR %>%
    filter(StructureS == "Box")
  culvertsBoxHW <- culvertsBox %>%
    filter(Diameter_W >= minHeightWidth) %>%
    filter(Diameter_1 >= minHeightWidth) %>%
    filter(Height_S1 >= minHeightWidth) %>%
    filter(Height_S2 >= minHeightWidth)
  # Circular culverts
  culvertsCir <- culvertsOR %>%
    filter(StructureS != "Box")
  culvertsCirHW <- culvertsCir %>%
    filter(Diameter_W >= minHeightWidth) %>%
    filter(Diameter_1 >= minHeightWidth)
  
  # Merge box and circular culverts
  culvertsSpp <- rbind(culvertsBoxHW, culvertsCirHW)
  
  # Keep culverts with maximum length < maxLength
  if(!is.na(maxLength)){
    culvertsSpp <- culvertsSpp %>%
      filter(Barrel_Len <= maxLength)
  }
  
  # Add resistance value to filtered culverts
  culvertsSpp <- culvertsSpp %>%
    mutate(Resistance = culvertResistancevalue)
  
  # Keep culverts with light at end
  #culvertsSpp <- culvertsSpp %>%
  #  filter(Light >= "At end")
  
  
  
  if(dim(culvertsSpp)[1] == 0){
    
    # Combine raster layers ------------------------
    
    resistanceStack <- c(resistanceRaster, bridgeRasterData)
    combinedRaster  <- min(resistanceStack, na.rm = TRUE)
  }
  
  # For species with suitable culverts
  if(dim(culvertsSpp)[1] != 0){
    
    # Convert culverts to raster files -------------
    
    # Create a buffer around culvert and bridge points
    culvertBuffer <- culvertsSpp %>%
      st_buffer(., bufferSize)
    
    # Rasterize
    culvertRaster <- culvertBuffer %>% 
      rasterize(templateRaster, field = "Resistance")
    
    
    # Combine raster layers ------------------------
    
    resistanceStack <- c(resistanceRaster, culvertRaster, bridgeRasterData)
    combinedRaster  <- min(resistanceStack, na.rm = TRUE)
    
  } 
  
  return(combinedRaster)
  
}



