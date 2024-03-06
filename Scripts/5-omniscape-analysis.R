## a295
## Carina Rauen Firkowski 
## April 20, 2023
##
## This script runs an Omniscape connectivity analysis based on the current 
## landscape conditions at the EcoPark System for three focal species. 
## 
## - BLBR  Northern short-tailed shrew (Blarina brevicauda)
## - ODVI  White-tailed deer (Odocoileus virginianus)
## - EMBL  Blanding’s turtle (Emydoidea blandingii)
##
## For each focal species, the analysis requires three inputs, available at the
## analysis area level:
## - resistance layer, representing resistance to movement based on current LULC
## - source layer, representing patches of suitable habitat
## - radius, representing the dispersal distance
##
## This script calls on file paths, defined in the "omniscape-inputs.R" script.
##
## NOTE: code commented out is needed when running script for the first time.



# Load constants
source("Scripts/0-constants.R")



# Parameters -------------------------------------------------------------------

# Define source strength method
# Fixed = all sources have the same value (1)
sourceMethod <- "Fixed"
# Variable = source strength differs based on habitat suitability
#sourceMethod <- "Variable"

# # Path to resistance reclassification table
# resistanceReclassPath <- normalizePath(file.path(intermediatesDir, 
#                                                  "Omniscape",
#                                                  "reclass_resistance.txt"))



# omniscape Library ------------------------------------------------------------

# Open a new SyncroSim session
omniscapeSession <- session()

# Create a new omniscape Library
omniscapeLibrary <- ssimLibrary(
  name = "a295-omniscape-current-conditions-07Dec2023.ssim",
  session = omniscapeSession, overwrite = TRUE,
  package = "omniscape")
# Open default Project
omniscapeProject <- rsyncrosim::project(ssimObject = omniscapeLibrary, 
                                        project = "Definitions")

# Set path to Julia executable 
juliaConfiguration <- data.frame(
  julia_path = 
    "C:\\Users\\CarinaFirkowski\\AppData\\Local\\Programs\\Julia-1.9.3\\bin\\julia.exe")
saveDatasheet(ssimObject = omniscapeLibrary, 
              data = juliaConfiguration, 
              name = "omniscape_juliaConfiguration")

# Enable "Use Conda"
condaDatasheet <- datasheet(omniscapeLibrary, name = "core_Options")
condaDatasheet$UseConda <- TRUE
saveDatasheet(ssimObject = omniscapeLibrary, 
              data = condaDatasheet, 
              name = "core_Options")



# Omniscape analysis -----------------------------------------------------------

# Target species
species <- c("BLBR", "EMBL", "ODVI")

for(spp in species){
  
  # Input parameters -----------------------------------
  
  # Resistance
  resistancePath <- normalizePath(file.path(intermediatesDir, "Omniscape", 
                                            paste0(spp, "_ResistanceCrossings_", 
                                                   sourceMethod, ".tif")))
  
  # Sources
  sourcePath <- normalizePath(file.path(intermediatesDir, "Omniscape", 
                                        paste0(spp, "_Sources_", 
                                               sourceMethod, ".tif")))
  
  # Radius
  if(spp == "BLBR") radiusValue <- 500 / 15    # 500 m
  if(spp == "EMBL") radiusValue <- 2000 / 15   # 2,000 m
  if(spp == "ODVI") radiusValue <- 20000 / 15  # 20,000 m
  
  
  # Scenario setup --------------------------------------
  
  newScenario <- scenario(
    ssimObject = omniscapeProject,
    scenario = paste0("currentConditions", spp))
  
  
  # Input Datasheet ---------------------
  
  # Open
  requiredInput <- datasheet(newScenario, name = "omniscape_Required")
  
  # Edit
  inputRow <- data.frame(resistance_file = resistancePath, 
                         radius = round(radiusValue),
                         source_file = sourcePath)
  requiredInput <- addRow(requiredInput, inputRow)
  
  # Save
  saveDatasheet(ssimObject = newScenario, 
                data = requiredInput, 
                name = "omniscape_Required")
  
  
  # General Options Datasheet -----------
  
  # Open
  generalOptions <- datasheet(newScenario, 
                              name = "omniscape_GeneralOptions")
  # Edit
  generalOptions <- data.frame(block_size = round(radiusValue/10),
                               source_from_resistance = FALSE,
                               resistance_is_conductance = FALSE,
                               r_cutoff = 9999,
                               buffer = 0,
                               source_threshold = 0,
                               calc_normalized_current = TRUE,
                               calc_flow_potential = TRUE,
                               allow_different_projections = FALSE,
                               connect_four_neighbors_only = FALSE,
                               solver = "cg+amg")
  # Save
  saveDatasheet(ssimObject = newScenario, 
                data = generalOptions, 
                name = "omniscape_GeneralOptions")
  
  
  # Resistance Options Datasheet --------
  
  # Open
  resistanceOptions <- datasheet(newScenario, 
                                 name = "omniscape_ResistanceOptions")
  # Edit
  resistanceOptions <- data.frame(reclassify_resistance = FALSE,
                                  write_reclassified_resistance = FALSE)
  # Save
  saveDatasheet(ssimObject = newScenario, 
                data = resistanceOptions, 
                name = "omniscape_ResistanceOptions")
  
  
  # Multiprocessing ---------------------
  
  # Open
  multiprocessing <- datasheet(newScenario, name = "core_Multiprocessing")
  
  # Edit
  multiprocessing$EnableMultiprocessing <- TRUE
  multiprocessing$MaximumJobs <- 10
  
  # Save
  saveDatasheet(ssimObject = newScenario, 
                data = multiprocessing, 
                name = "core_Multiprocessing")
  
  
  # Run ------------------------------------------------
  
  #myResults <- run(newScenario)
  
}




