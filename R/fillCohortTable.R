library(SqlRender)
library(DatabaseConnector)

#####

#' Function to fill the cohort table.
#'
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#'
#' @return
#' @export
#'
#' @examples
fillCohortTable<-function(connection,
                          trajectoryAnalysisArgs,
                          trajectoryLocalArgs) {

  print(paste0('Filling cohort table <',trajectoryLocalArgs$cohortTable,'> in <',trajectoryLocalArgs$cohortTableSchema,'> schema...'))
  if(trajectoryLocalArgs$cohortSqlFile==F) {
    cohortSqlFile='example_cohort_RA.sql'
    print(paste0('As cohortSqlFile==F, using built-in (example) cohort definition from packageDirectory/inst/sql/sql_server/',cohortSqlFile))
  } else {
    cohortSqlFile=trajectoryLocalArgs$cohortSqlFile
    print(paste0('Using packageDirectory/inst/sql/sql_server/',cohortSqlFile,' as cohort definition'))
  }

    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = trajectoryLocalArgs$cohortSqlFile,
                                             packageName = trajectoryAnalysisArgs$packageName,
                                             dbms = attr(connection, "dbms"),
                                             oracleTempSchema = trajectoryLocalArgs$oracleTempSchema,
                                             cdm_database_schema = trajectoryLocalArgs$cdmDatabaseSchema,
                                             vocabulary_database_schema = trajectoryLocalArgs$vocabDatabaseSchema,
                                             target_database_schema = trajectoryLocalArgs$cohortTableSchema,
                                             target_cohort_table = trajectoryLocalArgs$cohortTable,
                                             target_cohort_id = trajectoryLocalArgs$cohortId)

    DatabaseConnector::executeSql(connection, sql)

    print('...done.')

    #check how many records are there in the cohort table
    RenderedSql <- SqlRender::render("SELECT COUNT(*) FROM @target_database_schema.@target_cohort_table WHERE cohort_definition_id = @target_cohort_id;",
                                     #packageName = trajectoryAnalysisArgs$packageName,
                                     #dbms = attr(connection, "dbms"),
                                     target_database_schema = trajectoryLocalArgs$cohortTableSchema,
                                     target_cohort_table = trajectoryLocalArgs$cohortTable,
                                     target_cohort_id = trajectoryLocalArgs$cohortId)
    result = DatabaseConnector::querySql(connection, RenderedSql)
    print(paste0('There are ',result$COUNT,' rows in this cohort (id=',trajectoryLocalArgs$cohortId,') in the cohort table.'))

}
