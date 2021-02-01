library(SqlRender)
#' Checks whether person.birth_datetime is recorded in the data if addBirths is set to TRUE
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#'
#' @return
#' @export
#'
#' @examples
addBirthsChecker <- function(connection,
                             trajectoryAnalysisArgs,
                             trajectoryLocalArgs) {
  logger::log_info('Parameter addBirths is set to TRUE. Check whether person.birth_datetime is actually recorded in the data...')
  births_sql <- SqlRender::loadRenderTranslateSql("addBirthsChecker.sql",
                                                packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                dbms=connection@dbms,
                                                cdmDatabaseSchema =  trajectoryLocalArgs$cdmDatabaseSchema
                                                )
  birthDateTimeCount = DatabaseConnector::querySql(connection, births_sql)
  if(birthDateTimeCount$BIRTHCOUNTS<1) {
    logger::log_warn("The column person.birth_datetime is empty, setting addBirths to FALSE")
    trajectoryAnalysisArgs$addBirths=F
  } else {
    logger::log_info('...OK')
  }
}
