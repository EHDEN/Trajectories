#Load required libraries
library(Trajectories)
library(DatabaseConnector)

# ##################################################
# SETTING UP THE PARAMETER VALUES, CONNECT TO DATABASE
# ##################################################

# Change the values of the following parameters according to your database setup
connectionDetails = createConnectionDetails(dbms = 'postgresql',#  e.g. oracle, postgresql, redshift. See for all options in DatabaseConnector::createConnectionDetails()
                                            user = Sys.getenv('DB_USERNAME'), #Currently takes the value form .Renviron file in the package folder
                                            password = Sys.getenv('DB_PASSWORD'), #Currently takes the value form .Renviron file in the package folder
                                            connectionString = "jdbc:postgresql://localhost:63333/maitt"
)
connection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit



# Setting local system & database parameters - CHANGE ACCORDING TO YOUR SYSTEM & DATABASE:
trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema = "temp_schema",
                                                               prefixForResultTableNames = "sr_", # Alternatively, you could use this to randomly generate the prefix (requires library(stringi) to be loaded): paste0(  if(!is.null(attr(connectionDetails,'user'))) substr(USER,1,2), stri_rand_strings(1, 2, pattern = "[A-Za-z]"), sep="_")
                                                               cdmDatabaseSchema = 'ohdsi_cdm_next',
                                                               vocabDatabaseSchema = 'ohdsi_cdm_next',
                                                               resultsSchema = 'ohdsi_temp',
                                                               sqlRole = F, # You may always use 'F'. Setting specific role might be useful in PostgreSQL when you want to create tables by using specific role so that the others also see the results. However, then you must ensure that this role has permissions to read from all necessary schemas and also create tables to resultsSchema
                                                               inputFolder=system.file("extdata", "T2D", package = "Trajectories"), # Full path to input folder that contains SQL file for cohort definition and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "T2D", package = "Trajectories")
                                                               mainOutputFolder='/Users/sulevr/temp', #Subfolders will be created automatically
                                                               databaseHumanReadableName='RITA') #Use something short. This will be used as a folder name an it will be added to the titles of the graph.



# ##################################################
# RUN DISCOVERY ANALYSIS
# ##################################################

Trajectories::discover(connection,
                       trajectoryLocalArgs,
                       createCohort=F,
                       validationSetSize=0, #set to 0 if you are you going to validate the results in another databaase anyways
                       createEventPairsTable=F,
                       runDiscoveryAnalysis=T,
                       forceRecalculationOfAnalysis=F, #used only if runDiscoveryAnalysis=T
                       createFilteredFullgraphs=T,
                       createGraphsForSelectedEvents=T,
                       selfValidate=F, #set to F if you are you going to validate the results in another databaase anyways
                       cleanup=F)

# ##################################################
# DISCONNECT FROM DATABASE
# ##################################################

DatabaseConnector::disconnect(connection)

