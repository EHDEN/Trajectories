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
dropCohortTable<-function(packageName,
                            connection,
                             dbms,
                             sqlRole=F,
                             cohortTableSchema = cohortTableSchema,
                             cohortTable = "my_cohorts") {

  print(paste0('Dropping cohort table <',cohortTable,'> from <',cohortTableSchema,'> schema...'))

  #Set SQL role of the database session
  Trajectories::setRole(connection,sqlRole)

  RenderedSql <- SqlRender::loadRenderTranslateSql('dropCohortTable.sql',
                                                 packageName=packageName,
                                                 dbms=dbms,
                                                 cohortTableSchema=cohortTableSchema,
                                                 cohortTable=cohortTable
  )
  DatabaseConnector::executeSql(connection, RenderedSql)

  print('...done.')
}
