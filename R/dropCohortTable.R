library(SqlRender)
library(DatabaseConnector)


#' Function to drop cohort table.
#'
#' @param packageName
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param dbms
#' @param sqlRole SQL role that is used to drop cohort table in cohortTableSchema. Set to empty string ('') if setting a specific role is not needed.
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
                             sqlRole='',
                             cohortTableSchema = cohortTableSchema,
                             cohortTable = "my_cohorts") {

  print(paste0('Dropping cohort table <',cohortTable,'> from <',cohortTableSchema,'> schema...'))

  # Make sure that no-one uses F as sqlRole
  if(sqlRole==F) sqlRole="";

  RenderedSql <- SqlRender::loadRenderTranslateSql('dropCohortTable.sql',
                                                 packageName=packageName,
                                                 dbms=dbms,
                                                 sqlRole=sqlRole,
                                                 cohortTableSchema=cohortTableSchema,
                                                 cohortTable=cohortTable
  )
  DatabaseConnector::executeSql(connection, RenderedSql)

  print('...done.')
}
