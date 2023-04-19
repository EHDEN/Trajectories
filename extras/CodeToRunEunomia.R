library(Trajectories)
library(DatabaseConnector)

# ##################################################
library(Trajectories)
library(DatabaseConnector)

# ##################################################
# SETTING UP THE PARAMETER VALUES
# ##################################################

if(!require(drat)){
  install.packages("drat")
  library(drat)
}

drat::addRepo("OHDSI")
if(!require(Eunomia)){
  install.packages("Eunomia")
  library(Eunomia)
}

connectionDetails <- getEunomiaConnectionDetails()
connection <- DatabaseConnector::connect(connectionDetails)

# on.exit(DatabaseConnector::disconnect(connection)) # Close db connection on error or exit

Eunomia::createCohorts(connectionDetails)

# Setting database parameters:
library(stringi)
mainOutputFolder <- file.path("~", "temp")
dir.create(mainOutputFolder, recursive = TRUE, showWarnings = FALSE) # Create directory if it does not exist
trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema = "temp_schema",
                                                               prefixForResultTableNames = "",
                                                               cdmDatabaseSchema = 'main',
                                                               vocabDatabaseSchema = 'main',
                                                               resultsSchema = 'main',
                                                               sqlRole = F,
                                                               inputFolder=system.file("extdata", "fulldb", package = "Trajectories"), # Full path to input folder that contains SQL file for cohort definition and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "T2D", package = "Trajectories")
                                                               mainOutputFolder=mainOutputFolder, # Subfolders to this will be created automatically
                                                               databaseHumanReadableName='Eunomia')


# ##################################################
# RUN DISCOVERY ANALYSIS
# ##################################################

Trajectories::discover(connection,
                       trajectoryLocalArgs,
                       createCohort=F,
                       validationSetSize=0,
                       createEventPairsTable=T,
                       runDiscoveryAnalysis=F,
                       createFilteredFullgraphs=T,
#                       createGraphsForSelectedEvents = F, # Complains as if it is unused argument
                       selfValidate=F,
                       cleanup=T,
                       beta=F) # for testing new things

# ##################################################
# DISCONNECT FROM DATABASE
# ##################################################

DatabaseConnector::disconnect(connection)
