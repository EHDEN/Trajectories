library(SqlRender)
#' Creates event pairs table and populates it with the data
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param eventParametersFilename Filename where the parameters of creating event pairs are written (so that one could later see what what where the exact parameters). Default value is 'event_parameters.txt'. If set to FALSE, no output file is created.
#'
#' @return
#' @export
#'
#' @examples
createEventPairsTable<-function(connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs,
                                eventParametersFilename='event_parameters.txt'
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

  fileConn<-file("creaate_table_as_CohortCC.txt")
  writeLines(RenderedSql, fileConn)
  close(fileConn)

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
