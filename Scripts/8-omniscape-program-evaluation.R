## a295
## Carina Rauen Firkowski 
## January 29, 2024
##
## This script runs the Omniscape Impact tool on a set of two scenarios:
##    - Current LULC conditions
##    - A worst-case condition where all the natural land cover classes within
##      the EcoPark System minimum convex polygon get converted to "Built-up
##      Area - Impervious"
## The analyses are run for three focal species: 
##    - BLBR  Northern short-tailed shrew (Blarina brevicauda)
##    - ODVI  White-tailed deer (Odocoileus virginianus)
##    - EMBL  Blanding’s turtle (Emydoidea blandingii)



# Load constants
source("Scripts/0-constants.R")



# Omniscape analysis -----------------------------------------------------------

# Target species
speciesList <- c("BLBR", "EMBL", "ODVI")

# Scenario
scenarioNameList <- c("ProgramEvaluation_Baseline", "ProgramEvaluation_Alternative")

# Open a new SyncroSim session
omniscapeSession <- session()

for(spp in speciesList){
  
  # Create a new omniscape Library
  omniscapeLibrary <- ssimLibrary(
    name = paste0("a295-program-evaluation-", spp, ".ssim"),
    session = omniscapeSession, overwrite = TRUE,
    package = "omniscape")
  
  # Enable add-on package
  enableAddon(omniscapeLibrary, name = "omniscapeImpact")
  
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
  
  # Enable Conda
  condaDatasheet <- datasheet(omniscapeLibrary, name = "core_Options")
  condaDatasheet$UseConda <- TRUE
  saveDatasheet(ssimObject = omniscapeLibrary, 
                data = condaDatasheet, 
                name = "core_Options")
  
  # Parameterise connectivity categories
  connectivityCategories <- read.csv(file.path(
    libraryDir, "Connectivity Categories.csv"))
  saveDatasheet(ssimObject = omniscapeProject, 
                data = connectivityCategories, 
                name = "omniscape_movementTypes")
  
  
  # Setup scenarios ------------------------------------------------------------
  
  for(scenarioName in scenarioNameList){
  
    # Input parameters ---------------------------
    
    # Resistance
    resistancePath <- normalizePath(
      file.path(
        intermediatesDir, "Omniscape", "Program evaluation",
        paste0(spp, "_Resistance_", scenarioName, ".tif")))
    # Sources
    sourcePath <- normalizePath(
      file.path(
        intermediatesDir, "Omniscape", "Program evaluation",
        paste0(spp, "_Source_", scenarioName, ".tif")))

    # Radius
    if(spp == "BLBR") radiusValue <- 500 / 15    # 500 m
    if(spp == "EMBL") radiusValue <- 2000 / 15   # 2,000 m
    if(spp == "ODVI") radiusValue <- 20000 / 15  # 20,000 m


    # Scenario setup --------------------------------------
    
    newScenario <- scenario(
      ssimObject = omniscapeProject,
      scenario = scenarioName)
    
    
    # Pipeline Datasheet ------------------
    
    if(scenarioName == "ProgramEvaluation_Baseline"){
      scenarioPipeline <- read.csv(file.path(
        libraryDir, "Pipeline.csv"))
      saveDatasheet(ssimObject = newScenario, 
                    data = scenarioPipeline, 
                    name = "core_Pipeline")  
    }
    
    if(scenarioName == "ProgramEvaluation_Alternative"){
      pipelineAlternative <- read.csv(file.path(
        libraryDir, "Pipeline-Alternative.csv"))
      saveDatasheet(ssimObject = newScenario, 
                    data = pipelineAlternative, 
                    name = "core_Pipeline")  
    }
    
    
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
    
    
    # Connectivity category thresholds ----
    
    categoryThreshold <- read.csv(file.path(
      libraryDir, "Category Thresholds.csv"))
    saveDatasheet(ssimObject = newScenario, 
                  data = categoryThreshold, 
                  name = "omniscape_reclassificationThresholds")
    
    
    # Add-on Datasheet --------------------
    
    if(scenarioName == "ProgramEvaluation_Alternative"){
      
      # List all scenarios
      scenarioList <- scenario(omniscapeProject)
      # Get baseline scenario ID
      baselineID <- scenarioList$ScenarioID[
        scenarioList$Name == "ProgramEvaluation_Baseline" & 
          scenarioList$IsResult == "No"]
      # Get current alternative scenario ID
      alternativeID <- scenarioId(newScenario)
      
      # Prepare datasheet
      addonDatasheet <- data.frame(Baseline = baselineID,
                                   Alternative = alternativeID)
      
      # Save
      saveDatasheet(ssimObject = newScenario, 
                    data = addonDatasheet, 
                    name = "omniscapeImpact_differenceScenarios")
       
    }
    
    
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
}




