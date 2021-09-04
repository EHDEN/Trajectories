#' Function to create empty cohort table. Script is based on  https://ohdsi.github.io/TheBookOfOhdsi/SqlAndR.html#implementing-the-study-using-sql-and-r (21th May 2020)
#'
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#'
#' @return
#'
#' @examples
createCohortTable<-function(connection,
                            trajectoryAnalysisArgs,
                            trajectoryLocalArgs) {

  ParallelLogger::logInfo('Creating cohort table ',trajectoryLocalArgs$resultsSchema,'.',trajectoryLocalArgs$prefixForResultTableNames,'cohort ...')

  #Set SQL role of the database session
  Trajectories:::setRole(connection,trajectoryLocalArgs$sqlRole)

  RenderedSql <- Trajectories:::loadRenderTranslateSql('createCohortTable.sql',
                                                      packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                      dbms=connection@dbms,
                                                      resultsSchema=trajectoryLocalArgs$resultsSchema,
                                                      prefiX =  trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection, RenderedSql)

  ParallelLogger::logInfo('...done.')
}
