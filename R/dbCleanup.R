library(SqlRender)

#' Removes all database tables that were created during the analysis
#'
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#'
#' @return
#' @export
#'
#' @examples
dbCleanup<-function(connection,
                    trajectoryAnalysisArgs,
                    trajectoryLocalArgs) {

  print(paste0("Cleanup: dropping all analysis tables with prefix '",trajectoryLocalArgs$prefixForResultTableNames,"' from database..."))

  #Set SQL role of the database session
  Trajectories::setRole(connection,trajectoryLocalArgs$sqlRole)

  RenderedSql <- SqlRender::loadRenderTranslateSql("12TableDropper.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=connection@dbms,
                                                   resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                   prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection, RenderedSql)
  print('...done.')
}
