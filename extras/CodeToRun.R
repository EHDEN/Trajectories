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
                                            connectionString = "jdbc:postgresql://localhost:63333/maitt"
                                            )


# Setting local system & database parameters - CHANGE ACCORDING TO YOUR SYSTEM & DATABASE:
trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema = "temp_schema",
                                                                prefixForResultTableNames = "sr_", # Alternatively, you could use this to randomly generate the prefix (requires library(stringi) to be loaded): paste0(  if(!is.null(attr(connectionDetails,'user'))) substr(USER,1,2), stri_rand_strings(1, 2, pattern = "[A-Za-z]"), sep="_")
                                                                cdmDatabaseSchema = 'ohdsi_cdm',
                                                                vocabDatabaseSchema = 'ohdsi_vocab',
                                                                resultsSchema = 'ohdsi_temp',
                                                                sqlRole = F, # You may always use 'F'. Setting specific role might be useful in PostgreSQL when you want to create tables by using specific role so that the others also see the results. However, then you must ensure that this role has permissions to read from all necessary schemas and also create tables to resultsSchema
                                                                cohortTableSchema= 'ohdsi_temp',
                                                                cohortTable='cohort',
                                                                cohortId=1,
                                                                inputFolder=system.file("extdata", "RA", package = "Trajectories"), # Full path to input folder that contains SQL file for cohort definition and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "T2D", package = "Trajectories")
                                                                mainOutputFolder='/Users/sulevr/temp', #Subfolders to this will be created automatically
                                                                databaseHumanReadableName='RITA MAITT') #Use something short. It will be added to the titles of the graph.


# Setting analysis parameters. Two options here:
# a) either to to load them automatically from inputFolder via:
     trajectoryAnalysisArgs<-Trajectories::TrajectoryAnalysisArgsFromInputFolder(trajectoryLocalArgs)
     # and note that you can still make changes to the parameters after loading it from file like this:
     # trajectoryAnalysisArgs$minPatientsPerEventPair=1000
# b) or set them manually:
#trajectoryAnalysisArgs <- Trajectories::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
#                                                                     maximumDaysBetweenEvents = 3650,
#                                                                     minPatientsPerEventPair = 1000,
#                                                                     addConditions=T,
#                                                                     addObservations=T,
#                                                                     addProcedures=T,
#                                                                     addDrugExposures=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
#                                                                     addDrugEras=T, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
#                                                                     addBirths=T,
#                                                                     addDeaths=T,
#                                                                     daysBeforeIndexDate=Inf,
#                                                                     packageName='Trajectories',
#                                                                     cohortName="Type 2 diabetes")

# ##################################################
# End of setting parameters. The actual code follows.




# ##################################################
# Connect to database
# ##################################################

connection <- DatabaseConnector::connect(connectionDetails)

on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit



#Create output folder for this analysis
outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

# Store used analysis arguments to JSON file
Trajectories::TrajectoryAnalysisArgsToJson(trajectoryAnalysisArgs, file.path(outputFolder,"trajectoryAnalysisArgs_used.json"))

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


# Drop created cohort table
Trajectories::dropCohortTable(connection=connection,
                              trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                              trajectoryLocalArgs=trajectoryLocalArgs)

# Cleanup database after analysis
Trajectories::dbCleanup(connection=connection,
                        trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                        trajectoryLocalArgs=trajectoryLocalArgs)

