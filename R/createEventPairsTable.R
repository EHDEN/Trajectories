library(SqlRender)
#' Creates event pairs table and populates it with the data
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @inheritParams GetOutputFolder
#' @return
#' @export
#'
#' @examples
createEventPairsTable<-function(connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs
                               ) {
  print(paste0("Create database tables + data for all event pairs to '",trajectoryLocalArgs$resultsSchema,"' schema..."))


  #In case trajectoryAnalysisArgs$minPatientsPerEventPair < 1, the actual value means "prevalence", not absolute number.
  #Therefore, we need to calculate the absolute number from this
  if(trajectoryAnalysisArgs$minPatientsPerEventPair<1) {
    cohortCount<-getCohortSize(connection, trajectoryLocalArgs)
    minPatientsPerEventPair=round(cohortCount*trajectoryAnalysisArgs$minPatientsPerEventPair)
    print(paste0('Parameter value of minPatientsPerEventPair=',trajectoryAnalysisArgs$minPatientsPerEventPair,' is less than 1. ',
                  'Therefore, it is handled as prevalence instead of an absolute number. ',
                  'The absolute number is calculated based on cohort size (n=',cohortCount,') as follows: ',
                  'minPatientsPerEventPair = ',cohortCount,' x ',trajectoryAnalysisArgs$minPatientsPerEventPair,' = ',minPatientsPerEventPair)
          )
  } else {
    minPatientsPerEventPair=trajectoryAnalysisArgs$minPatientsPerEventPair
  }


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
                                                  minPatientsPerEventPair = minPatientsPerEventPair,
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
