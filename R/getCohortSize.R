
#' Gets cohort size (number of rows in cohort table of that particluar cohort ID)
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @inheritParams GetOutputFolder
#'
#' @return
#' @export
#'
#' @examples
getCohortSize<-function(connection,
                        trajectoryAnalysisArgs,
                        trajectoryLocalArgs) {


  #replace parameter values in SQL
  sql <- SqlRender::render("SELECT COUNT(*) AS CCC FROM @resultsSchema.@prefiXcohort WHERE cohort_definition_id = @cohortId;",
                           resultsSchema = trajectoryLocalArgs$resultsSchema,
                           prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                           cohortId=ifelse(Trajectories::IsValidationMode(trajectoryAnalysisArgs),2,1))

  #translate SQL into right dialect
  sql <- SqlRender::translate(sql = sql,
                              targetDialect=attr(connection, "dbms"),
                              oracleTempSchema = trajectoryLocalArgs$oracleTempSchema)

  result = DatabaseConnector::querySql(connection, sql)

  return (result$CCC)
}
