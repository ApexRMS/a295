## a295
## Carina Rauen Firkowski 
## May 30, 2023
##
## This script reclassifies the road classes. The same reclassification is used
## in the "1-LULC.R" script.



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



