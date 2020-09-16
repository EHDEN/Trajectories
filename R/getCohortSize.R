#' Gets cohort size (number of rows in cohort table of that particluar cohort ID)
#'
#' @param connection Database connection object
#' @inheritParams GetOutputFolder
#'
#' @return
#' @export
#'
#' @examples
getCohortSize<-function(connection, trajectoryLocalArgs) {


  #replace parameter values in SQL
  sql <- SqlRender::render("SELECT COUNT(*) AS CCC FROM @target_database_schema.@target_cohort_table WHERE cohort_definition_id = @target_cohort_id;",
                                   target_database_schema = trajectoryLocalArgs$cohortTableSchema,
                                   target_cohort_table = trajectoryLocalArgs$cohortTable,
                                   target_cohort_id = trajectoryLocalArgs$cohortId)

  #translate SQL into right dialect
  sql <- SqlRender::translate(sql = sql,
                              targetDialect=attr(connection, "dbms"),
                              oracleTempSchema = trajectoryLocalArgs$oracleTempSchema)

  result = DatabaseConnector::querySql(connection, sql)

  return (result$CCC)
}

