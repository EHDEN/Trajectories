library(SqlRender)

#' Removes all database tables that were created during the analysis
#'
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#'
#' @return
#' @export
#'
#' @examples
dbCleanup<-function(connection,
                    trajectoryAnalysisArgs,
                    trajectoryLocalArgs) {

  log_info(paste0("Cleanup: dropping all analysis tables with prefix '",trajectoryLocalArgs$prefixForResultTableNames,"' from database..."))

  #Set SQL role of the database session
  Trajectories::setRole(connection,trajectoryLocalArgs$sqlRole)

  RenderedSql <- SqlRender::loadRenderTranslateSql("12TableDropper.sql",
                                                   packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                   dbms=connection@dbms,
                                                   resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                   prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection, RenderedSql)
  log_info('...done.')
}
