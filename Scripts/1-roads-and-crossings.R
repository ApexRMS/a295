## a295
## Carina Rauen Firkowski 
## May 30, 2023
##
## This script reclassifies the road classes.



# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Target CRS
targetCRS <- "EPSG:3162"

# Focal area
analysisArea <- st_read(dsn = file.path(intermediatesDir),
                     layer = "analysisArea")

# Roads
roads <- st_read(dsn = file.path(spatialDataDir, "Roads"),
                                 layer = "ORN_20km") %>%
  st_transform(targetCRS)

# Culverts
culverts <- st_read(dsn = file.path(spatialDataDir, "Culverts", "Raw"),
                    layer = "C2E_Culverts") %>%
  st_transform(targetCRS)



# Classify roads ---------------------------------------------------------------

# Reclass
roadsReclass <- roads[,"ROAD_CLASS"] %>%
  mutate(ROAD_CLASS  = case_when(
    ROAD_CLASS == "Arterial" ~ "Arterial & Collector",
    ROAD_CLASS == "Collector" ~ "Arterial & Collector",
    ROAD_CLASS == "Expressway / Highway" ~ "Major",
    ROAD_CLASS == "Freeway" ~ "Major",
    ROAD_CLASS == "Ramp" ~ "Major",
    ROAD_CLASS == "Rapid Transit" ~ "Major",
    ROAD_CLASS == "Service" ~ "Major",
    ROAD_CLASS == "Alleyway / Laneway" ~ "Minor",
    ROAD_CLASS == "Local / Strata" ~ "Minor",
    ROAD_CLASS == "Local / Street" ~ "Minor",
    ROAD_CLASS == "Local / Unkown" ~ "Minor",
    ROAD_CLASS == "Resource / Recreation" ~ "Minor"))

# Save
st_write(obj = roadsReclass,
         dsn = file.path(intermediatesDir),
         layer = "roadsReclass",
         overwrite = TRUE,
         driver = "ESRI Shapefile")



# Calculate suitable culverts --------------------------------------------------

# Clip to focal study area
culvertsStudy <- st_intersection(culverts, analysisArea) 

# Minimum culvert size
minCulvertSize <- 0.05

# Functions to convert units to meters
ORcirc <- function(length, width){
  (pi * (0.5 * width / 1000)^2) / (length)}
ORbox <- function(length, width, height){
  ((height / 1000) * (width / 1000)) / (length)}

# Calculate opening radius for each culvert 
culvertsStudy$OR <- culvertsStudy %>%
  apply(., 1, function(x){
          ifelse(x$CULVERTSHA != "Box", 
                 ORcirc(length = (x$BARREL_LEN), 
                        width = (min(x$DIAMETER_W, 
                                     x$DIAMETER_1, 
                                     na.rm = TRUE))),
                 ORbox(length = (x$BARREL_LEN), 
                       width = (min(x$DIAMETER_W, x$DIAMETER_1, na.rm=TRUE)), 
                       height = min(x$HEIGHT_S1, x$HEIGHT_S2, na.rm=TRUE)))
        })

# Filter curlverts with opening radius < minCulvertSize
suitableCulverts <- culvertsStudy %>% 
  filter(., OR > minCulvertSize)

# Save
st_write(obj = suitableCulverts,
         file.path(intermediatesDir, "suitableCulverts.shp"),
         delete_layer = TRUE,
         driver = "ESRI Shapefile")



