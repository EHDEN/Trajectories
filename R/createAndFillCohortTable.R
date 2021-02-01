#####

#' Creates and fills cohort table that is going to be used by the Trajectories package
#'
#' First, it creates the cohort table. Note that this not the standard cohort table that is used in OMOP CDM schema, it is specifically created for this package amd later deleted.
#' Second, it loads cohort definition SQL file from the input path (given in trajectoryAnalysisArgs) and executes it in the database so the cohort table will  be filled in with event-periods that satisfy the cohort requirements.
#' The cohort_id of the built cohort is 1.
#'
#' @inheritParams GetOutputFolder
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#'
#' @return
#' @export
#'
#' @examples
createAndFillCohortTable<-function(connection,
                          trajectoryAnalysisArgs,
                          trajectoryLocalArgs) {

  createCohortTable(connection,
                    trajectoryAnalysisArgs,
                    trajectoryLocalArgs)

  fillCohortTable(connection,
                    trajectoryAnalysisArgs,
                    trajectoryLocalArgs)

}
