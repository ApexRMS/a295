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
normCurrentBLBR <- rast(file.path(outputDir, "Omniscape", "07-Dec-2023",
                        "normalized_cum_currmap_BLBR.tif"))

normCurrentEMBL <- rast(file.path(outputDir, "Omniscape", "07-Dec-2023",
                        "normalized_cum_currmap_EMBL.tif"))

normCurrentODVI <- rast(file.path(outputDir, "Omniscape", "07-Dec-2023",
                        "normalized_cum_currmap_ODVI.tif"))

# Focal area
focalArea <- st_read(dsn = file.path(intermediatesDir),
                     layer = "focalArea")

# EcoPark System partner lands
securedLand <- st_read(dsn = file.path(spatialDataDir, "EcoPark lands"),
                       layer = "CurrentEcoParkLands") %>%
    st_transform(targetCRS)



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



# Identify strategies ----------------------------------------------------------

# Spatial parameters
targetRes <- res(movementTypesBLBR)
targetExt <- ext(movementTypesBLBR)
targetCRS <- crs(focalArea)

# Template raster
templateRaster <- rast()
crs(templateRaster) <- targetCRS
ext(templateRaster) <- targetExt
res(templateRaster) <- targetRes

# Create raster of EcoPark System lands
landRaster <- rasterize(securedLand, templateRaster)

# Classify secured lands as 10 and non-secured lands as 0
landRaster <- classify(landRaster, matrix(c(1, 10, NA, 0), 
                                          ncol = 2, byrow = TRUE))

# Combine movement types and secured land layers
preStrategiesBLBR <- landRaster + movementTypesBLBR
preStrategiesEMBL <- landRaster + movementTypesEMBL
preStrategiesODVI <- landRaster + movementTypesODVI

# Reclassification table
# Note: Strategies were modified from Cameron et al. 2022 Conservation Science 
#       and Practice
reclassStrategies <- matrix(c(1, 0,   # Impeded = No strategy
                              2, 1,   # Diffuse + Non-secured = Strategy 1
                              3, 1,   # Intensified + Non-secured = Strategy 1
                              4, 2,   # Channelized + Non-secured = Strategy 2 
                              11, 0,  # Impeded = No strategy
                              12, 3,  # Diffuse + Secured = Strategy 3
                              13, 3,  # Intensified + Secured = Strategy 4
                              14, 4), # Channelized + Secured = Strategy 4
                            ncol = 2, byrow = TRUE)

# Reclassify
strategiesBLBR <- classify(preStrategiesBLBR, reclassStrategies)
strategiesEMBL <- classify(preStrategiesEMBL, reclassStrategies)
strategiesODVI <- classify(preStrategiesODVI, reclassStrategies)


# Strategy representation across species ---------

# Reclass table for strategies
reclass_0 <- matrix(c(0, 1,
                      1, 0,
                      2, 0,
                      3, 0,
                      4, 0), ncol = 2, byrow = TRUE)
reclass_1 <- matrix(c(1, 1,
                      2, 0,
                      3, 0,
                      4, 0), ncol = 2, byrow = TRUE)
reclass_2 <- matrix(c(1, 0,
                      2, 1,
                      3, 0,
                      4, 0), ncol = 2, byrow = TRUE)
reclass_3 <- matrix(c(1, 0,
                      2, 0,
                      3, 1,
                      4, 0), ncol = 2, byrow = TRUE)
reclass_4 <- matrix(c(1, 0,
                      2, 0,
                      3, 0,
                      4, 1), ncol = 2, byrow = TRUE)

# Create binary maps of strategies for each species (12 maps)
BLBR_0 <- classify(strategiesBLBR, reclass_0)
BLBR_1 <- classify(strategiesBLBR, reclass_1)
BLBR_2 <- classify(strategiesBLBR, reclass_2)
BLBR_3 <- classify(strategiesBLBR, reclass_3)
BLBR_4 <- classify(strategiesBLBR, reclass_4)
EMBL_0 <- classify(strategiesEMBL, reclass_0)
EMBL_1 <- classify(strategiesEMBL, reclass_1)
EMBL_2 <- classify(strategiesEMBL, reclass_2)
EMBL_3 <- classify(strategiesEMBL, reclass_3)
EMBL_4 <- classify(strategiesEMBL, reclass_4)
ODVI_0 <- classify(strategiesODVI, reclass_0)
ODVI_1 <- classify(strategiesODVI, reclass_1)
ODVI_2 <- classify(strategiesODVI, reclass_2)
ODVI_3 <- classify(strategiesODVI, reclass_3)
ODVI_4 <- classify(strategiesODVI, reclass_4)

# Combine each strategy across species (4 maps)
strategy0 <- BLBR_0 + EMBL_0 + ODVI_0
strategy1 <- BLBR_1 + EMBL_1 + ODVI_1
strategy2 <- BLBR_2 + EMBL_2 + ODVI_2
strategy3 <- BLBR_3 + EMBL_3 + ODVI_3
strategy4 <- BLBR_4 + EMBL_4 + ODVI_4



# Maximum consensus of strategies across species --

# NOTE: Based on consensus at maximum consensus at the movement strategy level,
#       which is then reclassified into a consensus strategy map 

# # Reclass table for movement types
# reclass_impeded <- matrix(c(1, 1,
#                             2, 0,
#                             3, 0,
#                             4, 0), ncol = 2, byrow = TRUE)
# reclass_diffuse <- matrix(c(1, 0,
#                             2, 1,
#                             3, 0,
#                             4, 0), ncol = 2, byrow = TRUE)
# reclass_intensified <- matrix(c(1, 0,
#                             2, 0,
#                             3, 1,
#                             4, 0), ncol = 2, byrow = TRUE)
# reclass_channelized <- matrix(c(1, 0,
#                                 2, 0,
#                                 3, 0,
#                                 4, 1), ncol = 2, byrow = TRUE)
# 
# # Create binary maps for each movement types per species (9 maps)
# BLBR_impeded <- classify(movementTypesBLBR, reclass_impeded)
# BLBR_diffuse <- classify(movementTypesBLBR, reclass_diffuse)
# BLBR_intensified <- classify(movementTypesBLBR, reclass_intensified)
# BLBR_channelized <- classify(movementTypesBLBR, reclass_channelized)
# EMBL_impeded <- classify(movementTypesEMBL, reclass_impeded)
# EMBL_diffuse <- classify(movementTypesEMBL, reclass_diffuse)
# EMBL_intensified <- classify(movementTypesEMBL, reclass_intensified)
# EMBL_channelized <- classify(movementTypesEMBL, reclass_channelized)
# ODVI_impeded <- classify(movementTypesODVI, reclass_impeded)
# ODVI_diffuse <- classify(movementTypesODVI, reclass_diffuse)
# ODVI_intensified <- classify(movementTypesODVI, reclass_intensified)
# ODVI_channelized <- classify(movementTypesODVI, reclass_channelized)
# 
# # Combine each movement type across species
# spp_impeded <- BLBR_impeded + EMBL_impeded + ODVI_impeded
# spp_diffuse <- BLBR_diffuse + EMBL_diffuse + ODVI_diffuse
# spp_intensified <- BLBR_intensified + EMBL_intensified + ODVI_intensified
# spp_channelized <- BLBR_channelized + EMBL_channelized + ODVI_channelized
# 
# # Reclass consensus (> 3) back to movement type
# reclass_impeded1 <- matrix(c(0, 0,
#                              1, 0,
#                              2, 0,
#                              3, 1), ncol = 2, byrow = TRUE)
# reclass_diffuse2 <- matrix(c(0, 0,
#                              1, 0,
#                              2, 0,
#                              3, 2), ncol = 2, byrow = TRUE)
# reclass_intensified3 <- matrix(c(0, 0,
#                                  1, 0,
#                                  2, 0,
#                                  3, 3), ncol = 2, byrow = TRUE)
# reclass_channelized4 <- matrix(c(0, 0,
#                                  1, 0,
#                                  2, 0,
#                                  3, 4), ncol = 2, byrow = TRUE)
# 
# # Filter to maximum consensus across species (3)
# impededConsensus <- classify(spp_impeded, reclass_impeded1)
# diffuseConsensus <- classify(spp_diffuse, reclass_diffuse2)
# intensifiedConsensus <- classify(spp_intensified, reclass_intensified3)
# channelizedConsensus <- classify(spp_channelized, reclass_channelized4)
# 
# # Combine consensus movement types
# movementConsensus <- impededConsensus + diffuseConsensus + 
#   intensifiedConsensus + channelizedConsensus
# 
# # Combine consensus movement types and secured land layers
# preConsensusStrategies <- landRaster + movementConsensus
# 
# # Reclassification table
# reclassStrategies2 <- matrix(c(1, 11,  # Impeded = No strategy
#                                2, 1,   # Diffuse + Non-secured = Strategy 1
#                                3, 1,   # Intensified + Non-secured = Strategy 1
#                                4, 2,   # Channelized + Non-secured = Strategy 2 
#                                10, 0,  # No consensus
#                                11, 11, # Impeded = No strategy
#                                12, 3,  # Diffuse + Secured = Strategy 3
#                                13, 3,  # Intensifies + Secured = Strategy 4
#                                14, 4), # Channelized + Secured = Strategy 4
#                              ncol = 2, byrow = TRUE)
# 
# # Reclassify
# consensusStrategies <- classify(preConsensusStrategies, reclassStrategies2)



# Liberal consensus map --------------------------

# Get consensus cells for each strategy
strategy0_Consensus <- strategy0
strategy0_Consensus[strategy0_Consensus != 3] <- 0
strategy0_Consensus[strategy0_Consensus == 3] <- 1
strategy1_Consensus <- strategy1
strategy1_Consensus[strategy1_Consensus != 3] <- 0
strategy1_Consensus[strategy1_Consensus == 3] <- 2
strategy2_Consensus <- strategy2
strategy2_Consensus[strategy2_Consensus != 3] <- 0
strategy2_Consensus[strategy2_Consensus == 3] <- 3
strategy3_Consensus <- strategy3
strategy3_Consensus[strategy3_Consensus != 3] <- 0
strategy3_Consensus[strategy3_Consensus == 3] <- 4
strategy4_Consensus <- strategy4
strategy4_Consensus[strategy4_Consensus != 3] <- 0
strategy4_Consensus[strategy4_Consensus == 3] <- 5

# Combine strategies
overallConsensus <- strategy0_Consensus + strategy1_Consensus +
  strategy2_Consensus + strategy3_Consensus + strategy4_Consensus

# Mask data layers from the analysis to the focal area -------------------------

# Raw normalized current
normCurrentBLBR_masked <- mask(normCurrentBLBR, focalArea)
normCurrentEMBL_masked <- mask(normCurrentEMBL, focalArea)
normCurrentODVI_masked <- mask(normCurrentODVI, focalArea)

# Movement types
movementTypesBLBR_masked <- mask(movementTypesBLBR, focalArea)
movementTypesEMBL_masked <- mask(movementTypesEMBL, focalArea)
movementTypesODVI_masked <- mask(movementTypesODVI, focalArea)

# Strategies
strategiesBLBR_masked <- mask(strategiesBLBR, focalArea)
strategiesEMBL_masked <- mask(strategiesEMBL, focalArea)
strategiesODVI_masked <- mask(strategiesODVI, focalArea)

# Summary of strategies across species
strategy0_masked <- mask(strategy0, focalArea)
strategy1_masked <- mask(strategy1, focalArea)
strategy2_masked <- mask(strategy2, focalArea)
strategy3_masked <- mask(strategy3, focalArea)
strategy4_masked <- mask(strategy4, focalArea)

# liberal consensus
overallConsensus_masked <- mask(overallConsensus, focalArea)

# # Consensus mapping
# movementConsensus_masked <- mask(movementConsensus, focalArea)
# consensusStrategies_masked <- mask(consensusStrategies, focalArea)



# Tabular summary --------------------------------------------------------------

# Movement types ---------------------------------

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
    rename(class = value, 
           area = count,
           proportion = freq)

# Area in km^2
tabularSummary$area <- (tabularSummary$area*targetRes[1])/1000000


# Strategies per species -------------------------

# Calculate relative proportion of movement class per species
strategies_tabularBLBR <- strategiesBLBR_masked %>%
  freq() %>%
  mutate(freq = count / sum(count),
         spp = "BLBR")
strategies_tabularEMBL <- strategiesEMBL_masked %>%
  freq() %>%
  mutate(freq = count / sum(count),
         spp = "EMBL")
strategies_tabularODVI <- strategiesODVI_masked %>%
  freq() %>%
  mutate(freq = count / sum(count),
         spp = "ODVI")

# Combine results
strategies_tabularSummary <- rbind(strategies_tabularBLBR[,-1], 
                                   strategies_tabularEMBL[,-1], 
                                   strategies_tabularODVI[,-1]) %>%
  mutate(value = case_when(value == 0 ~ "No strategy",
                           value == 1 ~ "Strategy 1",
                           value == 2 ~ "Strategy 2",
                           value == 3 ~ "Strategy 3",
                           value == 4 ~ "Strategy 4")) %>%
  rename(class = value, 
         area = count,
         proportion = freq)

# Area in km^2
strategies_tabularSummary$area <- (strategies_tabularSummary$area*
                                     targetRes[1])/1000000


# Liberal consensus ------------------------------

# Calculate relative proportion of movement class per species
liberalConsensus_summary <- overallConsensus_masked %>%
  freq() %>%
  mutate(freq = count / sum(count))

# Area in km^2
liberalConsensus_summary$area <- (liberalConsensus_summary$count*
                                     targetRes[1])/1000000


# # Consensus --------------------------------------
# 
# # Calculate relative proportion of movement class per species
# strategies_0 <- strategy0_masked %>%
#   freq() %>%
#   mutate(freq = count / sum(count),
#          strategy = "Impeded")
# strategies_1 <- strategy1_masked %>%
#   freq() %>%
#   mutate(freq = count / sum(count),
#          strategy = "1")
# strategies_2 <- strategy2_masked %>%
#   freq() %>%
#   mutate(freq = count / sum(count),
#          strategy = "2")
# strategies_3 <- strategy3_masked %>%
#   freq() %>%
#   mutate(freq = count / sum(count),
#          strategy = "3")
# strategies_4 <- strategy4_masked %>%
#   freq() %>%
#   mutate(freq = count / sum(count),
#          strategy = "4")
# 
# strategies_all <- consensusStrategies_masked %>%
#   freq() %>%
#   mutate(freq = count / sum(count))



# Save files -------------------------------------------------------------------

# Raw normalized current
# Focal area
writeRaster(normCurrentBLBR_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023",
                                 "BLBR_NormalizedCurrent_FocalArea.tif"),
             datatype = "FLT4S", NAflag = -9999L, overwrite = TRUE)
writeRaster(normCurrentEMBL_masked,
            filename = file.path(outputDir, "Omniscape",  "07-Dec-2023",
                                 "EMBL_NormalizedCurrent_FocalArea.tif"),
             datatype = "FLT4S", NAflag = -9999L, overwrite = TRUE)
writeRaster(normCurrentODVI_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "ODVI_NormalizedCurrent_FocalArea.tif"),
             datatype = "FLT4S", NAflag = -9999L, overwrite = TRUE)

# Movement types
# Analysis area
writeRaster(movementTypesBLBR,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "BLBR_MovementTypes_AnalysisArea.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(movementTypesEMBL,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "EMBL_MovementTypes_AnalysisArea.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(movementTypesODVI,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "ODVI_MovementTypes_AnalysisArea.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
# Focal area
writeRaster(movementTypesBLBR_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "BLBR_MovementTypes_FocalArea.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(movementTypesEMBL_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "EMBL_MovementTypes_FocalArea.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(movementTypesODVI_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "ODVI_MovementTypes_FocalArea.tif"),
             datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)

# Strategies
# Analysis area
writeRaster(strategiesBLBR,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "BLBR_Strategies_AnalysisArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(strategiesEMBL,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "EMBL_Strategies_AnalysisArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(strategiesODVI,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "ODVI_Strategies_AnalysisArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
# Focal area
writeRaster(strategiesBLBR_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "BLBR_Strategies_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(strategiesEMBL_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "EMBL_Strategies_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(strategiesODVI_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "ODVI_Strategies_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)

# Summary of strategies across species
writeRaster(strategy0_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "SppSummary_Impeded_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(strategy1_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "SppSummary_PlanningIncentives_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(strategy2_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "SppSummary_AcquisitionRestoration_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(strategy3_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "SppSummary_ManagePermeability_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(strategy4_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "SppSummary_LocalizedInterventions_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)

# Summary of strategies across species
writeRaster(overallConsensus_masked,
            filename = file.path(outputDir, "Omniscape", "07-Dec-2023", 
                                 "SppConsensus_Liberal_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)

# Consensus mapping
writeRaster(movementConsensus_masked,
            filename = file.path(outputDir, "Omniscape", "20-Nov-2023", 
                                 "Consensus_ConnectivityCategories_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)
writeRaster(consensusStrategies_masked,
            filename = file.path(outputDir, "Omniscape", "20-Nov-2023", 
                                 "Consensus_Strategies_FocalArea.tif"),
            datatype = "INT2S", NAflag = -9999L, overwrite = TRUE)

# Tabular summary
write.csv(tabularSummary, file = file.path(outputDir, "Omniscape", "20-Nov-2023", 
                                           "omniscape-tabular-summary.csv"))
write.csv(strategies_tabularSummary, file = file.path(outputDir, "Omniscape", 
                                                      "20-Nov-2023", 
                                           "strategies-tabular-summary.csv"))
