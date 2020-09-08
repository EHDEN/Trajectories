library(SqlRender)

#' Removes all database tables that were created during the analysis
#'
#' @param packageName Must always have value 'Trajectories'. The value is needed by SqlRender to find the SQL scripts from the right path.
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param dbms The target SQL dialect that is used by SqlRender
#' @param oracleTempSchema A schema that can be used to create temp tables in when using Oracle. Used by SqlRender
#' @param sqlRole Database role that was used when creating/dropping tables in resultsSchema. Set to F if a specicif role is not needed
#' @param resultsSchema Database schema where the temporary analysis tables were created.
#' @param prefixForResultTableNames This is the prefix that was used for all table names in analysis process. Default value is ''. This function is going to delete tables starting with that prefix.
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
