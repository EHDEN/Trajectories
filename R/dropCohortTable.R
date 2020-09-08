library(SqlRender)
library(DatabaseConnector)


#' Function to drop cohort table.
#'
#' @param packageName
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param dbms
#' @param sqlRole SQL role that is used to drop cohort table in cohortTableSchema. Set to FALSE if a specific role is not needed
#' @param cohortTableSchema
#' @param cohortTable
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
