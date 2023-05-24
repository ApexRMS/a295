## a295
## Carina Rauen Firkowski 
## April 21, 2023
##
## This script creates the study area raster from a series of points. It 
## produces three outputs:
## - a minimum convex polygon around the partner agency land
## - the focal area: minimum convex polygon + 2 km buffer
## - analysis area: minimum convex polygon + 20 km buffer



# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Study area
studyAreaPoints <- read_csv(file.path(tabularDataDir, "Study area",
                                      "Vertices 20200610 DAG.csv"))

# Target CRS
targetCRS <- "EPSG:3162"
# Buffer width (in km)
focalAreaBuffer <- 2
analysisAreaBuffer <- 20



# Raster layers ----------------------------------------------------------------

# Create minimum convex polygon from points
pointToPolygon <- studyAreaPoints %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
# Project to target CRS
minConvexPolygon <- st_transform(pointToPolygon, crs(targetCRS))
# Save
st_write(obj = minConvexPolygon,
         dsn = file.path(intermediatesDir),
         layer = "minConvexPolygon",
         overwrite = TRUE,
         driver = "ESRI Shapefile")

# Create focal area with buffer around minimum convex polygon
focalArea <- st_buffer(minConvexPolygon, (focalAreaBuffer * 1000))
# Save
st_write(obj = focalArea,
         dsn = file.path(intermediatesDir),
         layer = "focalArea",
         overwrite = TRUE,
         driver = "ESRI Shapefile")

# Create focal area with buffer around minimum convex polygon
analysisArea <- st_buffer(minConvexPolygon, (analysisAreaBuffer * 1000))
# Save
st_write(obj = analysisArea,
         dsn = file.path(intermediatesDir),
         layer = "analysisArea",
         overwrite = TRUE,
         driver = "ESRI Shapefile")


