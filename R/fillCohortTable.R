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
    logger::log_info(paste0('Filling cohort table <{cohortTableName}> in <',trajectoryLocalArgs$resultsSchema,'> schema based on cohort definition in <',f,'>...'))

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
                             target_database_schema = trajectoryLocalArgs$resultsSchema,
                             target_cohort_table = cohortTableName,
                             target_cohort_id = ifelse(Trajectories::IsValidationMode(trajectoryAnalysisArgs),2,1),
                             warnOnMissingParameters=F)

    #translate into right dialect
    sql <- SqlRender::translate(sql = sql,
                                targetDialect=attr(connection, "dbms"),
                                oracleTempSchema = trajectoryLocalArgs$oracleTempSchema)
    #execute translated SQL
    DatabaseConnector::executeSql(connection, sql)


    logger::log_info('...done filling cohort table.')

    #check how many records are there in the cohort table
    count<-Trajectories::getCohortSize(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs)
    logger::log_info(paste0('There are ',count,' rows in this cohort in the cohort table.'))

}
