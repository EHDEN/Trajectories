
#' Gets cohort size (number of rows in cohort table of that particluar cohort ID)
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @inheritParams GetOutputFolder
#'
#' @return
#'
#' @examples
getCohortSize<-function(connection,
                        trajectoryAnalysisArgs,
                        trajectoryLocalArgs) {


  #replace parameter values in SQL
  sql <- SqlRender::render("SELECT COUNT(*) AS TOTAL FROM @resultsSchema.@prefiXtraj_base_cohort WHERE IS_VALIDATION_SET = @validationMode;",
                           resultsSchema = trajectoryLocalArgs$resultsSchema,
                           prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                           validationMode=ifelse(Trajectories:::IsValidationMode(trajectoryAnalysisArgs),1,0))

  #translate SQL into right dialect
  sql <- SqlRender::translate(sql = sql,
                              targetDialect=attr(connection, "dbms"),
                              oracleTempSchema = trajectoryLocalArgs$oracleTempSchema)

  result = DatabaseConnector::querySql(connection, sql)

  return (result$TOTAL)
}
