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
                                                               cohortId=1,
                                                               mainOutputFolder='/Users/Kaust/temp',
                                                               cohortSqlFile='example_cohort_RA.sql')


# Setting analysis parameters.
# NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code...
# (same "drug" event may occur several times which is not allowed)

trajectoryAnalysisArgs <- Trajectories::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                     maximumDaysBetweenEvents = 3650,
                                                                     minPatientsPerEventPair = 100,
                                                                     addConditions=T,
                                                                     addObservations=T,
                                                                     addProcedures=T,
                                                                     addDrugExposures=F,
                                                                     addDrugEras=T,
                                                                     addBirths=T,
                                                                     addDeaths=T,
                                                                     daysBeforeIndexDate=Inf,
                                                                     packageName='Trajectories',
                                                                     cohortName="Rheumatoid arthritis")



# ##################################################
# End of setting parameters. The actual code follows.


#create subfolder for the results if not exists already
subFolder=make.names(trajectoryAnalysisArgs$cohortName)
outputFolder <- file.path(trajectoryLocalArgs$mainOutputFolder, subFolder)
if (!dir.exists(outputFolder)){
  dir.create(outputFolder)
}

# ##################################################
# Connect to database
# ##################################################

connection <- DatabaseConnector::connect(connectionDetails)

on.exit(DatabaseConnector::disconnect(connection))

# Create new cohort table

Trajectories::createCohortTable(connection=connection,
                                trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                trajectoryLocalArgs=trajectoryLocalArgs)
# Fill cohort table with example cohort data
Trajectories::fillCohortTable(connection=connection,
                              trajectoryAnalysisArgs,
                              trajectoryLocalArgs)

# Create database tables of all event pairs (patient level data + summary statistics)
Trajectories::createEventPairsTable(connection=connection,
                                    trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                    trajectoryLocalArgs=trajectoryLocalArgs,
                                    eventParametersFilename = paste0(outputFolder,'/event_parameters.txt'))


# Detect statistically significant directional event pairs and write the results to eventPairResultsFilename
Trajectories::runEventPairAnalysis(connection=connection,
                                   trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                   trajectoryLocalArgs=trajectoryLocalArgs,
                                   eventPairResultsFilename = paste0(outputFolder,'/event_pairs.tsv'),
                                   eventPairResultsStatsFilename = paste0(outputFolder,'/event_pair_stats.txt')
)

#creates graph from eventPairResultsFilename, also alignes the a
Trajectories::createIgraph(connection=connection,
                           trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                           trajectoryLocalArgs=trajectoryLocalArgs,
                           eventPairResultsFilename = paste0(outputFolder,'/event_pairs.tsv'),
                           outputFolder=outputFolder, #without trailing slash
                           eventName=NA) #we use NA here to draw graphs for top5 events


# Drop created cohort table
Trajectories::dropCohortTable(connection=connection,
                              trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                              trajectoryLocalArgs=trajectoryLocalArgs)

# Cleanup database after analysis
Trajectories::dbCleanup(connection=connection,
                        trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                        trajectoryLocalArgs=trajectoryLocalArgs)

