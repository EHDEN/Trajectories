
#' Title
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
#' @param packageName do not edit, this is required by SqlRender::loadRenderTranslateSql
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
  class(value) <- 'TrajectoryArguments'
  return(value)

}

#' Title
#'
#' @param cdmDatabaseSchema schema containing source data
#' @param vocabDatabaseSchema schema containing concepts library
#' @param resultsSchema schema the user has writing access to (used ti write new tables into)
#' @param oracleTempSchema In case you are using oracle, schema for temporary tables need to be specified. A schema where temp tables can be created in Oracle. Otherwise leave it as it is (is not used)
#' @param sqlRole Role to use in SQL for writing tables in 'resultsSchema'. It should also have access to 'cdmDatabaseSchema' and 'vocabDatabaseSchema'. Set to FALSE (or F) if setting to a specific role is not needed. In Estonian data is has to be hwisc_epi_ohdsi_dev_create
#' @param prefixForResultTableNames To avoid any collision with output table names (when someone runs the same analysis in parallel) we use a prefix for all table names that consists of 2 letters from username and 2 random characters. In case there is no username given (e.g Eunomia package, the prefix is simply 2 random characters)
#' Cohort table specifications (currently in development. Leave them as they are)
#' @param cohortTableSchema
#' @param cohortTable
#' @param cohortId
#' @param mainOutputFolder Change the output folder path. This is the folder where the final results are produced.Use full path and do NOT add trailing slash! The folder must already exist.
#' @param cohortSqlFile
#'
#' @return
#' @export
#'
#' @examples
createTrajectoryLocalArgs <- function(cdmDatabaseSchema,
                                              vocabDatabaseSchema,
                                              resultsSchema,
                                              oracleTempSchema,
                                              sqlRole,
                                              prefixForResultTableNames,
                                              cohortTableSchema,
                                              cohortTable,
                                              cohortId,
                                              mainOutputFolder,
                                              cohortSqlFile) {

  value <- list(cdmDatabaseSchema=cdmDatabaseSchema,vocabDatabaseSchema=vocabDatabaseSchema,
                resultsSchema=resultsSchema,oracleTempSchema=oracleTempSchema,sqlRole=sqlRole,
                prefixForResultTableNames=prefixForResultTableNames, cohortTableSchema=cohortTableSchema,
                cohortTable=cohortTable,cohortId=cohortId,
                mainOutputFolder=mainOutputFolder, cohortSqlFile=cohortSqlFile)
  class(value) <- 'TrajectoryArguments'
  return(value)

}




