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
  logger::log_info(paste0("Create database tables + data for all event pairs to '",trajectoryLocalArgs$resultsSchema,"' schema..."))


  #In case trajectoryAnalysisArgs$minPatientsPerEventPair < 1, the actual value means "prevalence", not absolute number.
  #Therefore, we need to calculate the absolute number from this
  if(trajectoryAnalysisArgs$minPatientsPerEventPair<1) {
    cohortCount<-getCohortSize(connection, trajectoryLocalArgs)
    minPatientsPerEventPair=round(cohortCount*trajectoryAnalysisArgs$minPatientsPerEventPair)
    logger::log_info(paste0('Parameter value of minPatientsPerEventPair=',trajectoryAnalysisArgs$minPatientsPerEventPair,' is less than 1. ',
                  'Therefore, it is handled as prevalence instead of an absolute number. ',
                  'The absolute number is calculated based on cohort size (n=',cohortCount,') as follows: ',
                  'minPatientsPerEventPair = ',cohortCount,' x ',trajectoryAnalysisArgs$minPatientsPerEventPair,' = ',minPatientsPerEventPair)
          )
  } else {
    minPatientsPerEventPair=trajectoryAnalysisArgs$minPatientsPerEventPair
  }

  # Check if there is data in person.birth_datetime if addBirths=T
  if(trajectoryAnalysisArgs$addBirths==T){
    Trajectories::addBirthsChecker(connection=connection,
                                   trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                   trajectoryLocalArgs=trajectoryLocalArgs)
  }
  logger::log_info(paste0("Running SQL..."))

  #Set SQL role of the database session
  Trajectories::setRole(connection,trajectoryLocalArgs$sqlRole)

  RenderedSql = Trajectories::loadRenderTranslateSql(sqlFilename='1CohortCC.sql',
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
                                                  addConditions = ifelse(trajectoryAnalysisArgs$addConditions==T,1,0),
                                                  addObservations = ifelse(trajectoryAnalysisArgs$addObservations==T,1,0),
                                                  addProcedures = ifelse(trajectoryAnalysisArgs$addProcedures==T,1,0),
                                                  addDrugExposures = ifelse(trajectoryAnalysisArgs$addDrugExposures==T,1,0),
                                                  addDrugEras = ifelse(trajectoryAnalysisArgs$addDrugEras==T,1,0),
                                                  addBirths = ifelse(trajectoryAnalysisArgs$addBirths==T,1,0),
                                                  addDeaths = ifelse(trajectoryAnalysisArgs$addDeaths==T,1,0)
  )


  DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = TRUE, reportOverallTime = TRUE)

  #During the execution of 1CohortCC.sql, the SQL scripts some debug information to table @resultsSchema.@prefiXdebug. Lets write this to log also.
  sql<-"SELECT ENTRY FROM @resultsSchema.@prefiXdebug ORDER BY timestamp;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))
  logger::log_info('Debug info from database operations:')
  for(row in res$ENTRY) {
    #print(row)
    logger::log_info(row)
  }


  # Get all (frequent) event pairs from the database
  RenderedSql <- Trajectories::loadRenderTranslateSql("2GetPairs.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=connection@dbms,
                                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                   prefix = trajectoryLocalArgs$prefixForResultTableNames
  )
  dpairs = DatabaseConnector::querySql(connection, RenderedSql)
  logger::log_info(paste0('There are ',nrow(dpairs),' event pairs that are going to be analyzed.'))

  logger::log_info('TASK COMPLETED: Creating event pairs data completed successfully.')
}
