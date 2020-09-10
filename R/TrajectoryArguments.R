
#' An object to hold analysis-specific data
#'
#' @param minimumDaysBetweenEvents The smallest number of days between 2 events of the patient that can be considered as event pair. Usually we have used 1.
#' @param maximumDaysBetweenEvents The maximum number of days between 2 events of the patient that can be considered as event pair. Ususally we have not really limited it so we have used 3650 (10 years)
#' @param minPatientsPerEventPair Minimum number of people having event1 -> event2 progression to be included in analysis. Can be used for limiting analysis to frequent event pairs only. However, it does not throw less frequent diagnosis pairs out of the (control group) data and therefore, does not affect the statistical significance.
#' @param addConditions TRUE/FALSE parameter to indicate whether events from Condition_occurrence table should be included in the analysis
#' @param addObservations TRUE/FALSE parameter to indicate whether events from Condition_occurrence table should be included in the analysis
#' @param addProcedures TRUE/FALSE parameter to indicate whether events from Procedure_occurrence table should be included in the analysis
#' @param addDrugExposures TRUE/FALSE parameter to indicate whether events from Drug_exposure table should be included in the analysis
#' @param addDrugEras TRUE/FALSE parameter to indicate whether events from Drug_era table should be included in the analysis. NB! use either addDrugEras=T or addDrugExposures=T (not both) as it leads to analysis duplication...
#' @param addBirths TRUE/FALSE parameter to indicate whether births events should be included in the analysis.
#' @param addDeaths TRUE/FALSE parameter to indicate whether events from Death table should be included in the analysis.
#' @param daysBeforeIndexDate 0 or any positive number that indicates for how many days before index date of the cohort the events are included in the analysis. In case one wants to include all events before index date, use value Inf
#' @param packageName Do not use/edit, this is required by SqlRender::loadRenderTranslateSql
#' @param cohortName Reader-friendly short description of the cohort. Used in graph titles and file names (can contain spaces)
#'
#' @return
#' @export
#'
#' @examples
createTrajectoryAnalysisArgs <- function(minimumDaysBetweenEvents,
                                         maximumDaysBetweenEvents,
                                         minPatientsPerEventPair,
                                         addConditions,
                                         addObservations,
                                         addProcedures,
                                         addDrugExposures,
                                         addDrugEras,
                                         addBirths,
                                         addDeaths,
                                         daysBeforeIndexDate,
                                         packageName = 'Trajectories',
                                         cohortName) {


  value <- list(minimumDaysBetweenEvents=minimumDaysBetweenEvents,maximumDaysBetweenEvents=maximumDaysBetweenEvents, minPatientsPerEventPair=minPatientsPerEventPair,
                addConditions=addConditions,addObservations=addObservations,addProcedures=addProcedures,addDrugExposures=addDrugExposures,
                addDrugEras=addDrugEras,addBirths=addBirths,addDeaths=addDeaths,
                daysBeforeIndexDate=daysBeforeIndexDate,packageName=packageName,cohortName=cohortName)
  class(value) <- 'TrajectoryAnalysisArgs'
  return(value)

}

#' An object to hold local database-specific parameters
#'
#' @param cdmDatabaseSchema Schema containing source data in OMOP CDM format
#' @param vocabDatabaseSchema Schema containing OMOP vocabulary
#' @param resultsSchema Schema the user has writing access to (used to write analysis tables into)
#' @param oracleTempSchema In case you are using oracle, schema for temporary tables need to be specified. A schema where temp tables can be created in Oracle. Otherwise leave it as it is (is not used)
#' @param sqlRole Role to use in SQL for writing tables in 'resultsSchema'. It should also have access to 'cdmDatabaseSchema' and 'vocabDatabaseSchema'. Set to FALSE (or F) if setting to a specific role is not needed. It should be safe to use F if you have no idea of what the SQL roles mean.
#' @param prefixForResultTableNames Table prefix that is used for all output tables to avoid any collision with existing table names. An empty string is also allowed.
#' @param cohortTableSchema Schema where cohort table is located
#' @param cohortTable Name of the cohort table in cohortTableSchema
#' @param cohortId ID of the cohort (in cohortTable) that will be used for the analysis
#' @param mainOutputFolder The output folder path. This is the folder where the final results are produced into. Use full path and do NOT add trailing slash! The folder must already exist. Default value is the default working directory.
#' @param databaseHumanReadableName In the future, it will be added to the titles of the graph to indicate what data is this. Use something short. Currently this parameter is not used.
#' @param cohortSqlFile Name of the built-in cohort file to be used when creating cohorts. Currently, you can use 'example_cohort_RA.sql', for instance
#'
#' @return
#' @export
#'
#' @examples
createTrajectoryLocalArgs <- function(cdmDatabaseSchema,
                                      vocabDatabaseSchema,
                                      resultsSchema,
                                      oracleTempSchema,
                                      sqlRole=F,
                                      prefixForResultTableNames='',
                                      cohortTableSchema,
                                      cohortTable,
                                      cohortId,
                                      mainOutputFolder=getwd(),
                                      databaseHumanReadableName='My database',
                                      cohortSqlFile) {

  value <- list(cdmDatabaseSchema=cdmDatabaseSchema,vocabDatabaseSchema=vocabDatabaseSchema,
                resultsSchema=resultsSchema,oracleTempSchema=oracleTempSchema,sqlRole=sqlRole,
                prefixForResultTableNames=prefixForResultTableNames, cohortTableSchema=cohortTableSchema,
                cohortTable=cohortTable,cohortId=cohortId,
                mainOutputFolder=mainOutputFolder, databaseHumanReadableName=databaseHumanReadableName, cohortSqlFile=cohortSqlFile)
  class(value) <- 'TrajectoryLocalArgs'
  return(value)

}
