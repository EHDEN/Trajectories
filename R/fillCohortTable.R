library(SqlRender)
library(DatabaseConnector)

#####

#' Title
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#'
#' @return
#' @export
#'
#' @examples
fillCohortTable<-function(connection,
                          trajectoryAnalysisArgs,
                          trajectoryLocalArgs) {

    f<-file.path(trajectoryLocalArgs$inputFolder,'cohort.sql')
    logger::log_info(paste0('Filling cohort table <',trajectoryLocalArgs$cohortTable,'> in <',trajectoryLocalArgs$cohortTableSchema,'> schema based on cohort definition in <',f,'>...'))

    if (!dir.exists(trajectoryLocalArgs$inputFolder)) stop(paste0("ERROR in fillCohortTable(): trajectoryLocalArgs$inputFolder '",inputFolder,"' does not exist."))
    if (!file.exists(f)) stop(paste0("ERROR in fillCohortTable(): there is no 'cohort.sql' file in inputFolder '",trajectoryLocalArgs$inputFolder,"'."))

    #Read in SQL
    sql <- readChar(f, file.info(f)$size)

    # Store it also to output folder (for later audits)
    outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
    file.copy(from=f, to=file.path(outputFolder,'cohort_used.sql'))

    #replace parameter values in SQL
    sql <- SqlRender::render(sql = sql,
                             cdm_database_schema = trajectoryLocalArgs$cdmDatabaseSchema,
                             vocabulary_database_schema = trajectoryLocalArgs$vocabDatabaseSchema,
                             target_database_schema = trajectoryLocalArgs$cohortTableSchema,
                             target_cohort_table = trajectoryLocalArgs$cohortTable,
                             target_cohort_id = trajectoryLocalArgs$cohortId,
                             warnOnMissingParameters=F)

    #translate into right dialect
    sql <- SqlRender::translate(sql = sql,
                                targetDialect=attr(connection, "dbms"),
                                oracleTempSchema = trajectoryLocalArgs$oracleTempSchema)
    #execute translated SQL
    DatabaseConnector::executeSql(connection, sql)


    logger::log_info('...done filling cohort table.')

    #check how many records are there in the cohort table
    count<-getCohortSize(connection, trajectoryLocalArgs)
    logger::log_info(paste0('There are ',count,' rows in this cohort (id=',trajectoryLocalArgs$cohortId,') in the cohort table.'))

}
