#####

#' Function to fill the cohort table.
#'
#' @inheritParams GetOutputFolder
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#'
#' @return
#'
#' @examples
fillCohortTable<-function(connection,
                          trajectoryAnalysisArgs,
                          trajectoryLocalArgs) {

    f<-file.path(trajectoryLocalArgs$inputFolder,'cohort.sql')
    cohortTableName<-paste0(trajectoryLocalArgs$prefixForResultTableNames,'cohort')
    ParallelLogger::logInfo('Filling cohort table <',trajectoryLocalArgs$resultsSchema,'.',cohortTableName,'> based on cohort definition in file <',f,'>...')

    if (!dir.exists(trajectoryLocalArgs$inputFolder)) stop(paste0("ERROR in fillCohortTable(): trajectoryLocalArgs$inputFolder '",inputFolder,"' does not exist."))
    if (!file.exists(f)) stop(paste0("ERROR in fillCohortTable(): there is no 'cohort.sql' file in inputFolder '",trajectoryLocalArgs$inputFolder,"'."))

    #Read in SQL
    sql <- readChar(f, file.info(f)$size)

    # Store it also to output folder (for later audits)
    outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
    file.copy(from=f, to=file.path(outputFolder,'cohort_used.sql'))

    #replace parameter values in SQL
    sql <- SqlRender::render(sql = sql,
                             cdm_database_schema = trajectoryLocalArgs$cdmDatabaseSchema,
                             vocabulary_database_schema = trajectoryLocalArgs$vocabDatabaseSchema,
                             target_database_schema = trajectoryLocalArgs$resultsSchema,
                             target_cohort_table = cohortTableName,
                             target_cohort_id = ifelse(Trajectories:::IsValidationMode(trajectoryAnalysisArgs),2,1),
                             warnOnMissingParameters=F)

    #translate into right dialect
    sql <- SqlRender::translate(sql = sql,
                                targetDialect=attr(connection, "dbms"))
    #execute translated SQL
    DatabaseConnector::executeSql(connection, sql)


    ParallelLogger::logInfo('...done filling cohort table.')

    #check how many records are there in the cohort table
    count<-Trajectories:::getCohortSize(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs)
    ParallelLogger::logInfo('There are ',count,' rows in this cohort in the cohort table.')

}
