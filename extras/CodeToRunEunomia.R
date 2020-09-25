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
Eunomia::createCohorts(connectionDetails)

# Setting database parameters:
library(stringi)
trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema = "temp_schema",
                                                               prefixForResultTableNames = "",
                                                               cdmDatabaseSchema = 'main',
                                                               vocabDatabaseSchema = 'main',
                                                               resultsSchema = 'main',
                                                               sqlRole = F,
                                                               cohortTableSchema='main',
                                                               cohortTable='cohort',
                                                               cohortId=5,
                                                               inputFolder=system.file("extdata", "fulldb", package = "Trajectories"), # Full path to input folder that contains SQL file for cohort definition and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "T2D", package = "Trajectories")
                                                               mainOutputFolder='/Users/sulevr/temp', #Subfolders to this will be created automatically
                                                               databaseHumanReadableName='Eunomia')



trajectoryAnalysisArgs<-Trajectories::TrajectoryAnalysisArgsFromInputFolder(trajectoryLocalArgs)


connection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit




#Create output folder for this analysis
outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

# Store used analysis arguments to JSON file
Trajectories::TrajectoryAnalysisArgsToJson(trajectoryAnalysisArgs, file.path(outputFolder,"trajectoryAnalysisArgs_used.json"))

# Create new cohort table
#Trajectories::createCohortTable(connection=connection,
#                                trajectoryAnalysisArgs=trajectoryAnalysisArgs,
#                                trajectoryLocalArgs=trajectoryLocalArgs)

# Fill cohort table with example cohort data
Trajectories::fillCohortTable(connection=connection,
                              trajectoryAnalysisArgs,
                              trajectoryLocalArgs)

# Create database tables of all event pairs (patient level data + summary statistics)
Trajectories::createEventPairsTable(connection=connection,
                                    trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                    trajectoryLocalArgs=trajectoryLocalArgs)


# Detect statistically significant directional event pairs and write the results to eventPairResultsFilename
Trajectories::runEventPairAnalysis(connection=connection,
                                   trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                   trajectoryLocalArgs=trajectoryLocalArgs)

#creates graph from eventPairResultsFilename, also alignes the a
Trajectories::createIgraph(connection=connection,
                           trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                           trajectoryLocalArgs=trajectoryLocalArgs,
                           eventName=NA) #we use NA here to draw graphs for top5 events



# Cleanup database after analysis
Trajectories::dbCleanup(connection=connection,
                        trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                        trajectoryLocalArgs=trajectoryLocalArgs)
