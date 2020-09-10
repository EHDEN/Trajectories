library(SqlRender)
library(DatabaseConnector)


#' Function to drop cohort table.
#'
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#'
#' @return
#' @export
#'
#' @examples
dropCohortTable<-function(connection,
                          trajectoryAnalysisArgs,
                          trajectoryLocalArgs) {

  print(paste0('Dropping cohort table <',trajectoryLocalArgs$cohortTable,'> from <',trajectoryLocalArgs$cohortTableSchema,'> schema...'))

  #Set SQL role of the database session
  Trajectories::setRole(connection,trajectoryLocalArgs$sqlRole)

  RenderedSql <- SqlRender::loadRenderTranslateSql('dropCohortTable.sql',
                                                 packageName=trajectoryAnalysisArgs$packageName,
                                                 dbms=connection@dbms,
                                                 cohortTableSchema=trajectoryLocalArgs$cohortTableSchema,
                                                 cohortTable=trajectoryLocalArgs$cohortTable
  )
  DatabaseConnector::executeSql(connection, RenderedSql)

  print('...done.')
}
