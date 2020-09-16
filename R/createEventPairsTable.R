library(SqlRender)
#' Creates event pairs table and populates it with the data
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @return
#' @export
#'
#' @examples
createEventPairsTable<-function(connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs
                               ) {
  print(paste0("Create database tables for all event pairs (patient level data + summary statistics) to '",trajectoryLocalArgs$resultsSchema,"' schema..."))

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
