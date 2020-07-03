library(SqlRender)
library(DatabaseConnector)


#' Function to fill the cohort table.
#'
#' @param cohortSqlFile SQL file in the package directory/inst/sql/sql_server). If set to F, uses built-in (example) cohort definition (example_cohort_RA.sql).
#' @param packageName
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param cdm_database_schema
#' @param vocabDatabaseSchema
#' @param cohortTableSchema
#' @param cohortTable
#' @param oracleTempSchema
#' @param target_cohort_id ID that is given to the created cohort. Default is 1.
#'
#' @return
#' @export
#'
#' @examples
fillCohortTable<-function(cohortSqlFile,
                            packageName,
                            connection,
                            cdmDatabaseSchema,
                            vocabDatabaseSchema,
                            cohortTableSchema,
                            cohortTable = "my_cohorts",
                            oracleTempSchema,
                            target_cohort_id = 1) {

  print(paste0('Filling cohort table <',cohortTable,'> in <',cohortTableSchema,'> schema...'))
  if(cohortSqlFile==F) {
    cohortSqlFile='example_cohort_RA.sql'
    print(paste0('As cohortSqlFile==F, using built-in (example) cohort definition from packageDirectory/inst/sql/sql_server/',cohortSqlFile))
  } else {
    print(paste0('Using packageDirectory/inst/sql/sql_server/',cohortSqlFile,' as cohort definition'))
  }

    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = cohortSqlFile,
                                             packageName = packageName,
                                             dbms = attr(connection, "dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema = vocabDatabaseSchema,
                                             target_database_schema = cohortTableSchema,
                                             target_cohort_table = cohortTable,
                                             target_cohort_id = target_cohort_id)
    DatabaseConnector::executeSql(connection, sql)

    print('...done.')

    #check how many records are there in the cohort table
    RenderedSql <- SqlRender::render("SELECT COUNT(*) FROM @target_database_schema.@target_cohort_table WHERE cohort_definition_id = @target_cohort_id;",
                                     #packageName = packageName,
                                     #dbms = attr(connection, "dbms"),
                                     target_database_schema = cohortTableSchema,
                                     target_cohort_table = cohortTable,
                                     target_cohort_id = target_cohort_id)
    result = DatabaseConnector::querySql(connection, RenderedSql)
    print(paste0('There are ',result$COUNT,' rows in this cohort (id=',target_cohort_id,') in the cohort table.'))

}
