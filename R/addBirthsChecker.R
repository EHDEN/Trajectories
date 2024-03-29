#' Checks whether person.birth_datetime is recorded in the data if addBirths is set to TRUE
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#'
#' @return
#'
#' @examples
addBirthsChecker <- function(connection,
                             trajectoryAnalysisArgs,
                             trajectoryLocalArgs) {
  ParallelLogger::logInfo('Parameter addBirths is set to TRUE. Check whether person.birth_datetime is actually recorded in the data...')
  births_sql <- SqlRender::loadRenderTranslateSql("addBirthsChecker.sql",
                                                packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                dbms=connection@dbms,
                                                cdmDatabaseSchema =  trajectoryLocalArgs$cdmDatabaseSchema
                                                )
  birthDateTimeCount = DatabaseConnector::querySql(connection, births_sql)
  if(birthDateTimeCount$BIRTHCOUNTS<1) {
    ParallelLogger::logWarn("The column person.birth_datetime is empty in the database (cannot use it), therefore setting addBirths to FALSE")
    trajectoryAnalysisArgs$addBirths=F
  } else {
    ParallelLogger::logInfo('...OK')
  }
}
