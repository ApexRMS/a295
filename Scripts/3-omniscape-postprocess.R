## a295
## Carina Rauen Firkowski
## April 20, 2023
##
## This script post-processes Omniscape outputs by: 
##   - classifying the normalized current into movement classes
##   - masking the output layers to the focal area
##   - creating a tabular summary of area and proportion of the focal area
##     covered by each movement class



# Load constants
source("Scripts/0-constants.R")



# Load spatial data ------------------------------------------------------------

# Raw normalized current
normCurrentBLBR <- rast(file.path(outputDir, "Omniscape",
                        "BLBR - normalized current.tif"))

normCurrentEMBL <- rast(file.path(outputDir, "Omniscape",
                        "EMBL - normalized current.tif"))

normCurrentODVI <- rast(file.path(outputDir, "Omniscape",
                        "ODVI - normalized current.tif"))

# Focal area
focalArea <- st_read(dsn = file.path(intermediatesDir),
                     layer = "focalArea")



# Classify raw normalized current into movement types --------------------------

# Reclassification table
# NOTE: values follow Cameron et al. 2022 Conservation Science and Practice
reclassTable <- matrix(c(-Inf, 0.7,  1,   # Impeded
                         0.7,  1.29, 2,   # Diffuse
                         1.29, 1.7,  3,   # Intensified
                         1.7, +Inf,  4),  # Channelized
                         ncol = 3, byrow = TRUE)

# Reclass
movementTypesBLBR <- classify(normCurrentBLBR, reclassTable)
movementTypesEMBL <- classify(normCurrentEMBL, reclassTable)
movementTypesODVI <- classify(normCurrentODVI, reclassTable)



# Mask data layers from the analysis to the focal area -------------------------

# Raw normalized current
normCurrentBLBR_masked <- mask(normCurrentBLBR, focalArea)
normCurrentEMBL_masked <- mask(normCurrentEMBL, focalArea)
normCurrentODVI_masked <- mask(normCurrentODVI, focalArea)

# Movement types
movementTypesBLBR_masked <- mask(movementTypesBLBR, focalArea)
movementTypesEMBL_masked <- mask(movementTypesEMBL, focalArea)
movementTypesODVI_masked <- mask(movementTypesODVI, focalArea)



# Tabular summary --------------------------------------------------------------

# Calculate relative proportion of movement class per species
tabularBLBR <- movementTypesBLBR_masked %>%
    freq() %>%
    mutate(freq = count / sum(count),
           spp = "BLBR")
tabularEMBL <- movementTypesEMBL_masked %>%
    freq() %>%
    mutate(freq = count / sum(count),
           spp = "EMBL")
tabularODVI <- movementTypesODVI_masked %>%
    freq() %>%
    mutate(freq = count / sum(count),
           spp = "ODVI")

# Combine results
tabularSummary <- rbind(tabularBLBR[,-1], tabularEMBL[,-1], tabularODVI[,-1]) |>
    mutate(value = case_when(value == 1 ~ "Impeded",
                             value == 2 ~ "Diffuse",
                             value == 3 ~ "Intensified",
                             value == 4 ~ "Channelized")) %>%
    rename(class = value, area = count, proportion = freq)



# Save files -------------------------------------------------------------------

# Raw normalized current
# Focal area
writeRaster(normCurrentBLBR_masked,
            filename = file.path(outputDir, "Omniscape", 
                                 "BLBR - normalized current - focal area.tif"),
             datatype = "FLT4S", NAflag = -9999L, overwrite = TRUE)
writeRaster(normCurrentEMBL_masked,
            filename = file.path(outputDir, "Omniscape", 
                                 "EMBL - normalized current - focal area.tif"),
             datatype = "FLT4S", NAflag = -9999L, overwrite = TRUE)
writeRaster(normCurrentODVI_masked,
            filename = file.path(outputDir, "Omniscape", 
                                 "ODVI - normalized current - focal area.tif"),
             datatype = "FLT4S", NAflag = -9999L, overwrite = TRUE)

# Movement types
# Analysis area
writeRaster(movementTypesBLBR,
            filename = file.path(outputDir, "Omniscape", 
                                 "BLBR - movement types.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(movementTypesEMBL,
            filename = file.path(outputDir, "Omniscape", 
                                 "EMBL - movement types.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(movementTypesODVI,
            filename = file.path(outputDir, "Omniscape", 
                                 "ODVI - movement types.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
# Focal area
writeRaster(movementTypesBLBR_masked,
            filename = file.path(outputDir, "Omniscape", 
                                 "BLBR - movement types - focal area.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(movementTypesEMBL_masked,
            filename = file.path(outputDir, "Omniscape", 
                                 "EMBL - movement types - focal area.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(movementTypesODVI_masked,
            filename = file.path(outputDir, "Omniscape", 
                                 "ODVI - movement types - focal area.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)

# Tabular summary
write.csv(tabularSummary, file = file.path(outputDir, "Omniscape", 
                                           "omniscape-tabular-summary.csv"))
