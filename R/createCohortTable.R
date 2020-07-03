library(SqlRender)
library(DatabaseConnector)


#' Function to create empty cohort table. Script is based on  https://ohdsi.github.io/TheBookOfOhdsi/SqlAndR.html#implementing-the-study-using-sql-and-r (21th May 2020)
#'
#' @param packageName
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param dbms
#' @param sqlRole SQL role that is used to create cohort table in cohortTableSchema. Set to FALSE if a specific role is not needed
#' @param cohortTableSchema
#' @param cohortTable
#'
#' @return
#' @export
#'
#' @examples
createCohortTable<-function(packageName,
                            connection,
                             dbms,
                             sqlRole=F,
                             cohortTableSchema = cohortTableSchema,
                             cohortTable = "my_cohorts") {

  print(paste0('Creating cohort table <',cohortTable,'> to <',cohortTableSchema,'> schema...'))

  #Set SQL role of the database session
  Trajectories::setRole(connection,sqlRole)

  RenderedSql <- SqlRender::loadRenderTranslateSql('createCohortTable.sql',
                                                 packageName=packageName,
                                                 dbms=dbms,
                                                 cohortTableSchema=cohortTableSchema,
                                                 cohortTable=cohortTable
  )
  DatabaseConnector::executeSql(connection, RenderedSql)

  print('...done.')
}
