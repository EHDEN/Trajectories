#Load required libraries
library(Trajectories)
library(DatabaseConnector)

# ##################################################
# SETTING UP THE PARAMETER VALUES
# ##################################################

# Change the values of the following parameters according to your database setup


connectionDetails = createConnectionDetails(dbms = 'postgresql',#  e.g. oracle, postgresql, redshift. See for all options in DatabaseConnector::createConnectionDetails()
                                            user = Sys.getenv('DB_USERNAME'), #Currently takes the value form .Renviron file in the package folder
                                            password = Sys.getenv('DB_PASSWORD'), #Currently takes the value form .Renviron file in the package folder
                                            connectionString = "jdbc:postgresql://10.6.6.29:5432/hwisc_epi"
                                            )


# Setting database parameters - CHANGE ACCORDING TO YOUR DATABASE:
# library(stringi)
trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema = "temp_schema",

                                                            prefixForResultTableNames = "sr_", # Alternatively, you could use this to randomly generate the prefix (requires library(stringi) to be loaded): paste0(  if(!is.null(attr(connectionDetails,'user'))) substr(USER,1,2), stri_rand_strings(1, 2, pattern = "[A-Za-z]"), sep="_")
                                                             cdmDatabaseSchema = 'ohdsi_cdm',
                                                              vocabDatabaseSchema = 'ohdsi_vocab',
                                                               resultsSchema = 'ohdsi_temp',
                                                              sqlRole = F, # You may always use 'F'. Setting specific role might be useful in PostgreSQL when you want to create tables by using specific role so that the others also see the results. However, then you must ensure that this role has permissions to read from all necessary schemas and also create tables to resultsSchema
                                                               cohortTableSchema= 'ohdsi_temp',
                                                               cohortTable='cohort',
                                                               cohortId=1,
                                                               mainOutputFolder='/Users/sulevr/temp',
                                                               databaseHumanReadableName='RITA MAITT', #Use something short. It will be added to the titles of the graph.

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
  print(paste0('Created folder for the results: ',outputFolder))
} else {
  print(paste0('The results will go to this folder (exists already): ',outputFolder))
}

# ##################################################
# Connect to database
# ##################################################

connection <- DatabaseConnector::connect(connectionDetails)

on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit

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

