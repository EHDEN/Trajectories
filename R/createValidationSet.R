#' Randomly assigns event pairs from the cohort to validation set by using the given ratio
#'
#' @param size Ratio for assigning event pairs from the cohort to validation set.
#' @inheritParams createTrajectoryAnalysisArgs
#' @inheritParams createTrajectoryLocalArgs
#' @inheritParams createEventPairsTable
#'
#' @return
#'
#' @examples
createValidationSet<-function(
                                connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs,
                                size=0.5
                               ) {

  ParallelLogger::logInfo("Assigning ",round(100*size),"% of events from the cohort to validation set...")
  RenderedSql = Trajectories:::loadRenderTranslateSql(sqlFilename='createValidationSet.sql',
                                                     packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                     dbms = connection@dbms,
                                                     oracleTempSchema = NULL,
                                                     resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                     prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                     size=size
  )
  DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = TRUE, reportOverallTime = TRUE)
  ParallelLogger::logInfo("...done.")

  #replace parameter values in SQL
  sql <- SqlRender::render("SELECT cohort_definition_id,count(*) AS COUNT FROM @resultsSchema.@prefiXcohort group by cohort_definition_id;",
                           resultsSchema = trajectoryLocalArgs$resultsSchema,
                           prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  #translate SQL into right dialect
  sql <- SqlRender::translate(sql = sql,
                              targetDialect=attr(connection, "dbms"),
                              oracleTempSchema = trajectoryLocalArgs$oracleTempSchema)
  # run query
  result = DatabaseConnector::querySql(connection, sql)
  num.d<-ifelse(length(result[result$COHORT_DEFINITION_ID==1,'COUNT'])==0,0,result[result$COHORT_DEFINITION_ID==1,'COUNT'])
  num.v<-ifelse(length(result[result$COHORT_DEFINITION_ID==2,'COUNT'])==0,0,result[result$COHORT_DEFINITION_ID==2,'COUNT'])
  num.t<-sum(result$COUNT)
  ParallelLogger::logInfo(num.t,' event-periods of the whole cohort divided into ',num.d,' periods in DISCOVERY set and ',num.v,' in VALIDATION set')

}
