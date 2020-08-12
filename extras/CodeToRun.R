# ##################################################
# SETTING UP THE PARAMETER VALUES
# ##################################################

# Change the values of the following parameters according to your database setup
dbms='postgresql' # e.g. oracle, postgresql, redshift. See for all options in DatabaseConnector::createConnectionDetails()
connectionString = "jdbc:postgresql://10.6.6.29:5432/hwisc_epi" # See for examples in DatabaseConnector::createConnectionDetails()
cdmDatabaseSchema = 'ohdsi' # schema containing source data
vocabDatabaseSchema = 'ohdsi' # schema containing concepts library
resultsSchema = 'ohdsi_dev' #s chema the user has writing access to (used ti write new tables into)
oracleTempSchema = "temp_schema" # In case you are using oracle, schema for temporary tables need to be specified. A schema where temp tables can be created in Oracle. Otherwise leave it as it is (is not used)
USER = Sys.getenv('DB_USERNAME') #Currently takes the value form .Renviron file in the package folder
PASS = Sys.getenv('DB_PASSWORD') #Currently takes the value form .Renviron file in the package folder
sqlRole = F  # Role to use in SQL for writing tables in 'resultsSchema'. It should also have access to 'cdmDatabaseSchema' and 'vocabDatabaseSchema'. Set to FALSE (or F) if setting to a specific role is not needed. In Estonian data is has to be hwisc_epi_ohdsi_dev_create



# Change the output folder path. This is the folder where the final results are produced.
# Use full path and do NOT add trailing slash!
# The folder must already exist.
mainOutputFolder='/Users/sulevr/temp'

# The following parameters are used in the calculations.
# You can change them, but you can also leave them as they are.
minimumDaysBetweenEvents = 1 # The smallest number of days between 2 events of the patient that can be considered as event pair. Usually we have used 1.
# TODO should investigate what happens if minimumDaysBetweenEvents=0, 1, -1.... This number cannot be negative (breaks SQL)! Seems that 0 does not make sense as in this case we cannot check direction. So, the minimum value should be 1, I guess.
maximumDaysBetweenEvents = 3650  # The maximum number of days between 2 events of the patient that can be considered as event pair. Ususally we have not really limited it so we have used 3650 (10 years)
minPatientsPerEventPair = 200 # Minimum number of people having event1 -> event2 progression to be included in analysis. Can be used for limiting analysis to frequent event pairs only. However, it does not throw less frequent diagnosis pairs out of the (control group) data and therefore, does not affect the statistical significance.
addConditions=T # TRUE/FALSE parameter to indicate whether events from Condition_occurrence table should be included in the analysis
addObservations=T # TRUE/FALSE parameter to indicate whether events from Condition_occurrence table should be included in the analysis
addProcedures=T # TRUE/FALSE parameter to indicate whether events from Procedure_occurrence table should be included in the analysis
# NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
addDrugExposures=F # TRUE/FALSE parameter to indicate whether events from Drug_exposure table should be included in the analysis
addDrugEras=T# TRUE/FALSE parameter to indicate whether events from Drug_era table should be included in the analysis. NB! use either addDrugEras=T or addDrugExposures=T (not both) as it leads to analysis duplication...
addBirths=T # TRUE/FALSE parameter to indicate whether births events should be included in the analysis.
addDeaths=T # TRUE/FALSE parameter to indicate whether events from Death table should be included in the analysis.
daysBeforeIndexDate=Inf # 0 or any positive number that indicates for how many days before index date of the cohort the events are included in the analysis. In case one wants to include all events before index date, use value Inf
cohortSqlFile='example_cohort_RA.sql'
cohortName="Rheumatoid arthritis" # Reader-friendly short description of the cohort. Used in graph titles and file names (can contain spaces)


# The following parameters are just for customisation, mainly used during the development process of the package.
# You can leave them as they are.
library(stringi)
prefixForResultTableNames = paste0(substr(USER,1,2),stri_rand_strings(1, 2, pattern = "[A-Za-z]"),"_") # To avoid any collision with output table names (when someone runs the same analysis in parallel) we use a prefix for all table names that consists of 2 letters from username and 2 random characters
packageName='Trajectories' #do not edit, this is required by SqlRender::loadRenderTranslateSql

#Cohort table specifications (currently in development. Leave them as they are)
cohortTableSchema=resultsSchema #currently in development. Leave it as it is.
cohortTable=paste0(prefixForResultTableNames,'cohort') #currently in development. Leave it as it is.
cohortId=1

# ##################################################
# End of setting parameters. The actual code follows.


#create subfolder for the results if not exists already
subFolder=make.names(cohortName)
outputFolder <- file.path(mainOutputFolder, subFolder)
if (!dir.exists(outputFolder)){
  dir.create(outputFolder)
}



# ##################################################
# Load the main package
# ##################################################
library(Trajectories)


# ##################################################
# Connect to database
# ##################################################
library(DatabaseConnector)
connectionDetails = createConnectionDetails(dbms = dbms,
                                            user = USER,
                                            password = PASS,
                                            connectionString = connectionString)
connection <- DatabaseConnector::connect(connectionDetails)

on.exit(DatabaseConnector::disconnect(connection))


# Create new cohort table
Trajectories::createCohortTable(packageName=packageName,
                                       connection=connection,
                                       dbms = dbms,
                                       sqlRole = sqlRole,
                                       cohortTableSchema = cohortTableSchema,
                                       cohortTable = cohortTable)
# Fill cohort table with example cohort data
Trajectories::fillCohortTable(
  cohortSqlFile=cohortSqlFile,
  #cohortSqlFile='example_cohort_atrial_arrythmia.sql',
  #cohortSqlFile='example_cohort_atopic_dermatitis.sql',
  #cohortSqlFile='example_cohort_RA.sql',
  #cohortSqlFile='example_cohort_pregnantwomen.sql',
  #cohortSqlFile='example_cohort_asthma_without_COPD.sql',
  packageName=packageName,
  connection=connection,
  cdmDatabaseSchema=cdmDatabaseSchema,
  vocabDatabaseSchema=vocabDatabaseSchema,
  cohortTableSchema=cohortTableSchema,
  cohortTable = cohortTable,
  oracleTempSchema = oracleTempSchema,
  target_cohort_id = cohortId)

# Create database tables of all event pairs (patient level data + summary statistics)
Trajectories::createEventPairsTable(packageName=packageName,
                                           connection=connection,
                                           dbms = dbms,
                                           oracleTempSchema = NULL,
                                           sqlRole = sqlRole,
                                           resultsSchema =   resultsSchema,
                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                           vocabDatabaseSchema = vocabDatabaseSchema,
                                           addConditions=addConditions,
                                           addObservations=addObservations,
                                           addProcedures=addProcedures,
                                           addDrugExposures=addDrugExposures,
                                           addDrugEras=addDrugEras,
                                           addBirths=addBirths,
                                           addDeaths=addDeaths,
                                           minimumDaysBetweenEvents = minimumDaysBetweenEvents,
                                           maximumDaysBetweenEvents = maximumDaysBetweenEvents,
                                           minPatientsPerEventPair = minPatientsPerEventPair,
                                           daysBeforeIndexDate=daysBeforeIndexDate,
                                           prefixForResultTableNames = prefixForResultTableNames,
                                           cohortTableSchema=cohortTableSchema,
                                           cohortTable=cohortTable,
                                           cohortId=cohortId,
                                           eventParametersFilename = paste0(outputFolder,'/event_parameters.txt'))


# Detect statistically significant directional event pairs and write the results to eventPairResultsFilename
Trajectories::runEventPairAnalysis(
  packageName=packageName,
  connection=connection,
  dbms = dbms,
  oracleTempSchema = NULL,
  sqlRole = sqlRole,
  resultsSchema =   resultsSchema,
  prefixForResultTableNames = prefixForResultTableNames,
  eventPairResultsFilename = paste0(outputFolder,'/event_pairs.tsv'),
  eventPairResultsStatsFilename = paste0(outputFolder,'/event_pair_stats.txt')
)

#creates graph from eventPairResultsFilename, also alignes the a
Trajectories::createIgraph(packageName=packageName,
                                  connection=connection,
                                  sqlRole = sqlRole,
                                  resultsSchema =   resultsSchema,
                                  prefixForResultTableNames = prefixForResultTableNames,
                                  eventPairResultsFilename = paste0(outputFolder,'/event_pairs.tsv'),
                       outputFolder=outputFolder, #without trailing slash
                       cohortName=cohortName,
                       eventName=NA) #we use NA here to draw graphs for top5 events





# Drop created cohort table
Trajectories::dropCohortTable(packageName=packageName,
                                     connection=connection,
                                     dbms = dbms,
                                     sqlRole = sqlRole,
                                     cohortTableSchema = cohortTableSchema,
                                     cohortTable = cohortTable)

# Cleanup database after analysis
Trajectories::dbCleanup(packageName=packageName,
                               connection=connection,
                               dbms = dbms,
                               oracleTempSchema = NULL,
                               sqlRole = sqlRole,
                               resultsSchema =   resultsSchema,
                               prefixForResultTableNames = prefixForResultTableNames)


