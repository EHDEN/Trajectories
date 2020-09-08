library(SqlRender)
#' Creates event pairs table and populates it with the data
#'
#' @param packageName Must always have value 'Trajectories'. The value is needed by SqlRender to find the SQL scripts from the right path.
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param oracleTempSchema A schema that can be used to create temp tables in when using Oracle. Used by SqlRender
#' @param sqlRole Database role that is used when creating tables to 'resultsSchema'. It should also have access to 'cdmDatabaseSchema' and 'vocabDatabaseSchema'. Set to F a specific role is not needed.
#' @param resultsSchema Database schema where the temporary analysis tables are created. They are temporary in a sense that they are deleted in the end of the analysis (the tables are not created as CREATE TEMPORARY TABLE...)
#' @param cdmDatabaseSchema Database schema where the actual OMOP-formatted data is stored and taken into analysis
#' @param vocabDatabaseSchema Database schema that contains 'concept' table of OMOP vocabulary
#' @param addConditions TRUE/FALSE parameter to indicate whether events from Condition_occurrence table should be included in the analysis
#' @param addObservations TRUE/FALSE parameter to indicate whether events from Observation table should be included in the analysis
#' @param addProcedures TRUE/FALSE parameter to indicate whether events from Procedure_occurrence table should be included in the analysis
#' @param addDrugExposures TRUE/FALSE parameter to indicate whether events from Drug_exposure table should be included in the analysis
#' @param addDrugEras TRUE/FALSE parameter to indicate whether events from Drug_era table should be included in the analysis
#' @param addBirths TRUE/FALSE parameter to indicate whether births events should be included in the analysis.
#' @param addDeaths TRUE/FALSE parameter to indicate whether events from Death table should be included in the analysis.
#' @param minimumDaysBetweenEvents The minimum number of days between 2 events of the patient that can be considered as event pair
#' @param maximumDaysBetweenEvents The maximum number of days between 2 events of the patient that can be considered as event pair
#' @param minPatientsPerEventPair Minimum number of people having event1 -> event2 progression to be included in analysis. Can be used for limiting analysis to frequent event pairs only. However, it does not throw less frequent diagnosis pairs out of the (control group) data and therefore, does not affect the statistical significance.
#' @param prefixForResultTableNames This is the prefix that is used for all table names in analysis process. The aim is to avoid any collision with output table names (when someone runs the same analysis in parallel). Default value is ''.
#' @param cohortTableSchema
#' @param cohortTable
#' @param cohortId Cohort ID in cohortTable. Default value is 1.
#' @param eventParametersFilename Filename where the parameters of creating event pairs are written (so that one could later see what what where the exact parameters). Default value is 'event_parameters.txt'. If set to FALSE, no output file is created.
#' @param daysBeforeIndexDate 0 or any positive number that indicates for how many days before index date of the cohort the events are included in the analysis. In case one wants to include all events before index date, use value Inf
#'
#' @return
#' @export
#'
#' @examples
createEventPairsTable<-function(connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs,
                                eventParametersFilename
                               ) {
  print(paste0("Create database tables for all event pairs (patient level data + summary statistics) to '",trajectoryLocalArgs$resultsSchema,"' schema..."))
  print(paste("(Hint: The lower the 'minPatientsPerEventPair' parameter, the more time the analysis takes)"))

  msg=c(paste('PARAMETER VALUES THAT WERE USED TO CREATE EVENT PAIR TABLES:'),
        paste('============================================================'),
        paste(format(Sys.time(), '%d %B %Y %H:%M')),
        paste(''),
        paste('dbms:',connection@dbms),
        paste(trajectoryAnalysisArgs),
        paste('prefixForResultTableNames for created tables:',trajectoryLocalArgs$prefixForResultTableNames)
  )
  print(msg)

  #sanitud check of the input parameters

  #if(daysBeforeIndexDate<0) stop(paste0('ERROR: parameter daysBeforeIndexDate=',daysBeforeIndexDate,' but negative values are not allowed.'))

  #if(addDrugEras==T & addDrugExposures==T) stop('ERROR: Both addDrugEras and addDrugExposures parameters are set to TRUE, but these should not be both TRUE at the same time. Set at least one of them to FALSE.')


  #if(eventParametersFilename!=F) {
  #  print(paste0('Writing event pair creation parameters to ',eventParametersFilename,'...'))

  #  fileConn<-file(eventParametersFilename)
  #  writeLines(msg, fileConn)
  #  close(fileConn)

   # print('... done.')
  #}


  print(paste0("Running SQL..."))

  #Set SQL role of the database session
  Trajectories::setRole(connection,trajectoryLocalArgs$sqlRole)

  RenderedSql = SqlRender::loadRenderTranslateSql(sqlFilename='1CohortCC.sql',
                                                  packageName=trajectoryAnalysisArgs$packageName,
                                                  dbms = connection@dbms,
                                                  oracleTempSchema = NULL,
                                                  resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                  cdmDatabaseSchema = trajectoryLocalArgs$cdmDatabaseSchema,
                                                  vocabDatabaseSchema = trajectoryLocalArgs$vocabDatabaseSchema,
                                                  minimumDaysBetweenEvents = trajectoryAnalysisArgs$minimumDaysBetweenEvents,
                                                  maximumDaysBetweenEvents = trajectoryAnalysisArgs$maximumDaysBetweenEvents,
                                                  minPatientsPerEventPair = trajectoryAnalysisArgs$minPatientsPerEventPair,
                                                  daysBeforeIndexDate = trajectoryAnalysisArgs$daysBeforeIndexDate,
                                                  prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                  cohortTableSchema = trajectoryLocalArgs$cohortTableSchema,
                                                  cohortTable = trajectoryLocalArgs$cohortTable,
                                                  cohortId = trajectoryLocalArgs$cohortId,
                                                  addConditions = trajectoryAnalysisArgs$addConditions,
                                                  addObservations = trajectoryAnalysisArgs$addObservations,
                                                  addProcedures = trajectoryAnalysisArgs$addProcedures,
                                                  addDrugExposures = trajectoryAnalysisArgs$addDrugExposures,
                                                  addDrugEras = trajectoryAnalysisArgs$addDrugEras,
                                                  addBirths = trajectoryAnalysisArgs$addBirths,
                                                  addDeaths = trajectoryAnalysisArgs$addDeaths
  )


  DatabaseConnector::executeSql(connection, RenderedSql)


  # Get all (frequent) event pairs from the database
  RenderedSql <- SqlRender::loadRenderTranslateSql("2GetPairs.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=connection@dbms,
                                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                   prefix = trajectoryLocalArgs$prefixForResultTableNames
  )
  dpairs = DatabaseConnector::querySql(connection, RenderedSql)
  print(paste0('There are ',nrow(dpairs),' event pairs that are going to be analyzed.'))

  print('TASK COMPLETED: Creating event pairs data completed successfully.')
}
