## a295
## Carina Rauen Firkowski 
## June 12, 2023
##
## This script pre-processes the inputs required to run Linkage Mapper.



# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Analysis area
analysisArea <- st_read(dsn = file.path(intermediatesDir),
                        layer = "analysisArea")

# Focal area
focalArea <- st_read(dsn = file.path(intermediatesDir),
                     layer = "focalArea")

# EcoPark System partner lands
securedLand <- st_read(dsn = file.path(spatialDataDir, "EcoPark lands"),
                       layer = "CurrentEcoParkLands") %>%
  st_transform(targetCRS)

# Niagara Escarpment
niagaraEscarp <- st_read(dsn = file.path(spatialDataDir, "Niagara Escarpment",
                                         "LIO-2023-01-20"),
                         layer = "NE_PLAN_DESIGNATION") %>%
  st_transform(targetCRS)

resistanceRaster <- rast(file.path(outputDir, "Resistance",
                                   paste0("BLBR", "_Resistance_FocalArea.tif")))

# OECMs
OECMs <- st_read(dsn = file.path(intermediatesDir),
                 layer = "OECMs") %>%
  st_transform(targetCRS)



# Spatial parameters ----------------------------------------------------------

# Reference
targetRes <- res(resistanceRaster)
targetExt <- ext(resistanceRaster)

# Template raster
templateRaster <- rast()
crs(templateRaster) <- targetCRS
ext(templateRaster) <- targetExt
res(templateRaster) <- targetRes



# Sources ----------------------------------------------------------------------

# EcoPark properties -----------------------------

# Calculate buffer around and union polygons
securedLandBuffer <- securedLand %>%
  st_buffer(dist = 3*15) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  fill_holes(threshold = Inf)

# Revert boundaries to exclude buffer
securedLandSourcesRaw <- securedLandBuffer %>%
  st_buffer(dist = -3*15) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Remove small patches
securedLandSourcesRaw$Area <- as.numeric(st_area(securedLandSourcesRaw))
securedLandSources <- securedLandSourcesRaw %>%
  filter(Area >= 40000)

# Add source ID
securedLandSources$Name <- 1L:dim(securedLandSources)[1]


# OECMs ------------------------------------------

# Calculate buffer around and union polygons
OECMsBuffer <- OECMs %>%
  st_intersection(analysisArea) %>%
  st_buffer(dist = 3*15) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  fill_holes(threshold = Inf)

# Revert boundaries to exclude buffer
OECMsSourcesRaw <- OECMsBuffer %>%
  st_buffer(dist = -3*15) %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

# Remove small patches
OECMsSourcesRaw$Area <- as.numeric(st_area(OECMsSourcesRaw))
OECMsSources <- OECMsSourcesRaw %>%
  filter(Area >= 1000000)

# Add source ID
OECMsSources$Name <- 1L:dim(OECMsSources)[1]


# C2E --------------------------------------------

# Cootes Paradise Marsh
cootes <- securedLand %>%
  filter(Name_1 == "Cootes Paradise" &
           AreaType == "Garden") %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  mutate(SourceName = 1L)


# Niagara Escarpment
fullEscarpment <- niagaraEscarp %>%
  st_intersection(focalArea) %>%
  filter(DESIG == "Escarpment Natural Area" |
           DESIG == "Escarpment Protection Area") %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  mutate(SourceName = 2L)
focalEscarpment <- fullEscarpment[3,]

# Combine sources
C2Esources <- rbind(focalEscarpment, cootes)



# Write to file ----------------------------------------------------------------

st_write(obj = securedLandSources[,"Name"],
         dsn = file.path(outputDir, "Linkage Mapper"),
         layer = "securedLandSources",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")

st_write(obj = OECMsSources[,"Name"],
         dsn = file.path(outputDir, "Linkage Mapper"),
         layer = "OECMsSources",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")

st_write(obj = C2Esources,
         dsn = file.path(outputDir, "Linkage Mapper"),
         layer = "C2Esources",
         factorsAsCharacter = FALSE,
         overwrite = TRUE,
         driver = "ESRI Shapefile")



