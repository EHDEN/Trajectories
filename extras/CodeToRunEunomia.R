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
                                                               inputFolder=system.file("extdata", "fulldb", package = "Trajectories"), # Full path to input folder that contains SQL file for cohort definition and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "T2D", package = "Trajectories")
                                                               mainOutputFolder='/Users/sulevr/temp', #Subfolders to this will be created automatically
                                                               databaseHumanReadableName='Eunomia')



trajectoryAnalysisArgs<-Trajectories::TrajectoryAnalysisArgsFromInputFolder(trajectoryLocalArgs)
trajectoryAnalysisArgs$minPatientsPerEventPair=0.05

connection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit

# Setting analysis parameters. Two options here:
# a) either to to load them automatically from inputFolder via:

#Comment the following line in if you are running the package in validation mode. Comment it out if you are running the package in exploratory mode.
#trajectoryLocalArgs$inputFolder<-'/here/your/path/to/validation_setup'

trajectoryAnalysisArgs<-Trajectories::TrajectoryAnalysisArgsFromInputFolder(trajectoryLocalArgs)

# and note that you can still make changes to the parameters after loading it from file like this:
# trajectoryAnalysisArgs$minPatientsPerEventPair=1000

# b) or set them manually:
#trajectoryAnalysisArgs <- Trajectories::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
#                                                                     maximumDaysBetweenEvents = 3650,
#                                                                     minPatientsPerEventPair = 100,
#                                                                     addConditions=T,
#                                                                     addObservations=T,
#                                                                     addProcedures=T,
#                                                                     addDrugExposures=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
#                                                                     addDrugEras=T, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
#                                                                     addBirths=T,
#                                                                     addDeaths=T,
#                                                                     daysBeforeIndexDate=Inf,
#                                                                     packageName='Trajectories',
#                                                                     cohortName="Rheumatoid.arthritis")

#trajectoryAnalysisArgs$addDrugEras=F

# ##################################################
# End of setting parameters. The actual code follows.

# ##################################################
# Connect to database
# ##################################################

connection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit

##############################################################################################################

# BUILD A COHORT BASED ON COHORT DEFINITION SQL AND THEN DIVIDE IT TO 2 SETS: DISCOVERY & VALIDATION SET

##############################################################################################################

# Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
Trajectories::createAndFillCohortTable(connection=connection,
                                       trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                       trajectoryLocalArgs=trajectoryLocalArgs)

# Assign 50% of the event-periods from the cohort to validation set (discovery set=data in cohort table where cohort_id=1; validation set=data in cohort table where cohort_id=2)
Trajectories::createValidationSet(connection=connection,
                                  trajectoryAnalysisArgs,
                                  trajectoryLocalArgs,
                                  size=0.5)

##############################################################################################################

# RUN DISCOVERY ANALYSIS

##############################################################################################################

#check that the mode is now "DISCOVERY"
stopifnot(Trajectories::IsValidationMode(trajectoryAnalysisArgs,verbose=T)==F)

# Create database tables of all event pairs (patient level data + summary statistics). Uses cohort_id depending on the running mode of the package
Trajectories::createEventPairsTable(connection=connection,
                                    trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                    trajectoryLocalArgs=trajectoryLocalArgs)


# Detect statistically significant directional event pairs and write the results to eventPairResultsFilename. Also creates validation setup.
Trajectories::runDiscoveryAnalysis(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs,
                                   forceRecalculation=F)

# Draw unfiltered graphs of the discovery results (not limited to specific concept_id-s)
Trajectories::createFilteredFullgraphs(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs)

##############################################################################################################

# VALIDATE THE RESULTS OF DISCOVERY ANALYSIS (RUN VALIDATION ANALYSIS)

##############################################################################################################

# Load setup from output-folder-of-discovary-analysis/validation_setup folder
trajectoryLocalArgs$inputFolder=file.path(Trajectories::GetOutputFolder(trajectoryLocalArgs=trajectoryLocalArgs, trajectoryAnalysisArgs=trajectoryAnalysisArgs, createIfMissing = F),"validation_setup")
trajectoryAnalysisArgs<-Trajectories::TrajectoryAnalysisArgsFromInputFolder(trajectoryLocalArgs)
#check that the mode is now "VALIDATION"
stopifnot(Trajectories::IsValidationMode(trajectoryAnalysisArgs,verbose=T)==T)

# Create database tables of all event pairs (patient level data + summary statistics). Uses cohort_id depending on the running mode of the package (this time, takes cohort_id=2)
Trajectories::createEventPairsTable(connection=connection,
                                    trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                    trajectoryLocalArgs=trajectoryLocalArgs)


# Validate statistically significant directional event pairs and write the results to eventPairResultsFilename
Trajectories::runValidationAnalysis(connection,
                                    trajectoryAnalysisArgs,
                                    trajectoryLocalArgs,
                                    forceRecalculation=F)

# Draw unfiltered graphs of the validated results (not limited to specific concept_id-s)
Trajectories::createFilteredFullgraphs(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs)


# Draw plots for 5 most prevalent events (uses database connection and result tables in the database for trajectory alignments)
Trajectories::PlotTrajectoriesGraphForEvents(connection,
                                             trajectoryAnalysisArgs,
                                             trajectoryLocalArgs,
                                             eventIds=NA,
                                             limitOfNodes=100,
                                             skipOutputTables = F)

# Draw plots for specific events (uses database connection and result tables in the database for trajectory alignments)
Trajectories::PlotTrajectoriesGraphForEvents(connection,
                                             trajectoryAnalysisArgs,
                                             trajectoryLocalArgs,
                                             eventIds=trajectoryAnalysisArgs$eventIdsForGraphs,
                                             skipOutputTables = T)



########### CLEANUP: DROP ANALYSIS TABLES IF THERE IS NO NEED FOR THESE RESULTS ANYMORE ###########

# Cleanup database after analysis
Trajectories::dbCleanup(connection=connection,
                        trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                        trajectoryLocalArgs=trajectoryLocalArgs)

DatabaseConnector::disconnect(connection)

