#' Creates event pairs table and populates it with the data
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @inheritParams GetOutputFolder
#' @return
#'
#' @examples
createEventPairsTableBrunak<-function(connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs
) {

  COHORT_ID=ifelse(Trajectories:::IsValidationMode(trajectoryAnalysisArgs)==TRUE,2,1) #cohort_id=1 when running in DISCOVERY mode, cohort_id=2 when running in VALIDATION mode,

  ParallelLogger::logInfo("Create database tables + data for all event pairs from cohort ID=",COHORT_ID," to '",trajectoryLocalArgs$resultsSchema,"' schema...")


  #In case trajectoryAnalysisArgs$minPatientsPerEventPair < 1, the actual value means "prevalence", not absolute number.
  #Therefore, we need to calculate the absolute number from this
  if(trajectoryAnalysisArgs$minPatientsPerEventPair<1) {
    cohortCount<-getCohortSize(connection, trajectoryAnalysisArgs, trajectoryLocalArgs)
    minPatientsPerEventPair=round(cohortCount*trajectoryAnalysisArgs$minPatientsPerEventPair)
    if(minPatientsPerEventPair==0) minPatientsPerEventPair=1
    ParallelLogger::logInfo('Parameter value of minPatientsPerEventPair=',trajectoryAnalysisArgs$minPatientsPerEventPair,' is less than 1. ',
                            'Therefore, it is handled as prevalence instead of an absolute number. ',
                            'The absolute number is calculated based on cohort size (n=',cohortCount,') as follows: ',
                            'minPatientsPerEventPair = ',cohortCount,' x ',trajectoryAnalysisArgs$minPatientsPerEventPair,' = ',minPatientsPerEventPair)
  } else {
    minPatientsPerEventPair=trajectoryAnalysisArgs$minPatientsPerEventPair
  }

  # Check if there is data in person.birth_datetime if addBirths=T
  if(trajectoryAnalysisArgs$addBirths==T){
    Trajectories:::addBirthsChecker(connection=connection,
                                    trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                    trajectoryLocalArgs=trajectoryLocalArgs)
  }



  # Store used analysis arguments to JSON file
  Trajectories:::TrajectoryAnalysisArgsToJson(trajectoryAnalysisArgs, file.path(Trajectories:::GetOutputFolder(trajectoryLocalArgs=trajectoryLocalArgs,trajectoryAnalysisArgs=trajectoryAnalysisArgs,createIfMissing=F),'logs',"trajectoryAnalysisArgs_used.json"))


  #Set SQL role of the database session
  Trajectories:::setRole(connection,trajectoryLocalArgs$sqlRole)

  # Create everything up to E1E2_model table
  ParallelLogger::logInfo("Creating event pairs in data...")
  RenderedSql = Trajectories:::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part1-brunak.sql',
                                                      packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                      dbms = connection@dbms,
                                                      oracleTempSchema = NULL,
                                                      resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                      cdmDatabaseSchema = trajectoryLocalArgs$cdmDatabaseSchema,
                                                      vocabDatabaseSchema = trajectoryLocalArgs$vocabDatabaseSchema,
                                                      minimumDaysBetweenEvents = trajectoryAnalysisArgs$minimumDaysBetweenEvents,
                                                      maximumDaysBetweenEvents = trajectoryAnalysisArgs$maximumDaysBetweenEvents,
                                                      daysBeforeIndexDate = trajectoryAnalysisArgs$daysBeforeIndexDate,
                                                      prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                      cohortTableSchema = trajectoryLocalArgs$resultsSchema,
                                                      cohortTable = paste0(trajectoryLocalArgs$prefixForResultTableNames,'cohort'),
                                                      cohortId = COHORT_ID,
                                                      addConditions = ifelse(trajectoryAnalysisArgs$addConditions==T,1,0),
                                                      addObservations = ifelse(trajectoryAnalysisArgs$addObservations==T,1,0),
                                                      addProcedures = ifelse(trajectoryAnalysisArgs$addProcedures==T,1,0),
                                                      addDrugExposures = ifelse(trajectoryAnalysisArgs$addDrugExposures==T,1,0),
                                                      addDrugEras = ifelse(trajectoryAnalysisArgs$addDrugEras==T,1,0),
                                                      addBirths = ifelse(trajectoryAnalysisArgs$addBirths==T,1,0),
                                                      addDeaths = ifelse(trajectoryAnalysisArgs$addDeaths==T,1,0)
  )
  DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = TRUE, reportOverallTime = TRUE)
  ParallelLogger::logInfo("...done.")

  # Create and fill E1E2_model_input table
  # There are two options here - either to build it from the actual data OR (in validation mode) build it from given event-pairs
  #Check whether to run the method in validation mode
  isValidationMode=Trajectories:::IsValidationMode(trajectoryAnalysisArgs)
  if(isValidationMode) {

    f=file.path(trajectoryLocalArgs$inputFolder,'event_pairs_for_validation.tsv')
    ParallelLogger::logInfo("Data to E1E2_MODEL is taken from file '",f,"'...")

    #2Can't use simply insertTable here because in Eunomia package it does not solve schema name correctly. That's why this is commented out and we manually create SQL here :(
    #insertTable(connection, tablename, edges, tempTable=F, progressBar=T)

    e = read.csv2(file = f, sep = '\t', header = TRUE, as.is=T)
    e$RR_IN_PREVIOUS_STUDY<-as.numeric(e$RR_IN_PREVIOUS_STUDY)

    if(any(is.na(e$RR_IN_PREVIOUS_STUDY))) ParallelLogger::logError("Package is run in validation mode, but RR_IN_PREVIOUS_STUDY in file 'event_pairs_for_validation.tsv' is empty for at least one pair. This is not allowed as this is used for power calculations (please remove such rows).")

    if(nrow(e)==0) ParallelLogger::logError("Package is run in validation mode, but file 'event_pairs_for_validation.tsv' is empty")

    #Put data to table E1E2_model_input
    Trajectories:::insertTable(connection = connection,
                               databaseSchema=trajectoryLocalArgs$resultsSchema,
                               tableName = paste0(trajectoryLocalArgs$prefixForResultTableNames,'E1E2_model_input'),
                               data = e %>%
                                 dplyr::select(E1_CONCEPT_ID, E2_CONCEPT_ID, E1_NAME, E1_DOMAIN, E2_NAME, E2_DOMAIN, RR_IN_PREVIOUS_STUDY),
                               dropTableIfExists=T,
                               progressBar = T)
    ParallelLogger::logInfo("...done.")

  } else {
    ParallelLogger::logInfo("Running package in DISCOVERY mode (not validating someone's results)")
    RenderedSql = Trajectories:::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part2a-brunak.sql',
                                                        packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                        dbms = connection@dbms,
                                                        oracleTempSchema = NULL,
                                                        resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                        vocabDatabaseSchema = trajectoryLocalArgs$vocabDatabaseSchema,
                                                        minPatientsPerEventPair=minPatientsPerEventPair,
                                                        prefiX = trajectoryLocalArgs$prefixForResultTableNames
    )
    DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = TRUE, reportOverallTime = TRUE)

  }



  # Create E1E2_model table and all that is inherited from that
  ParallelLogger::logInfo("Creating statistics for events pairs that are going to be analyzed...")
  RenderedSql = Trajectories:::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part3-brunak.sql',
                                                      packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                      dbms = connection@dbms,
                                                      oracleTempSchema = NULL,
                                                      resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                      prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = TRUE, reportOverallTime = TRUE)
  ParallelLogger::logInfo("...done.")

  #During the execution of previous scripts, SQL writes some debug information to table @resultsSchema.@prefiXdebug. Lets write this to log also.
  sql<-"SELECT ENTRY FROM @resultsSchema.@prefiXdebug ORDER BY timestamp;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))
  ParallelLogger::logInfo('Debug info from database operations:')
  for(row in res$ENTRY) {
    #print(row)
    ParallelLogger::logInfo(row)
  }


  # Get all (frequent) event pairs from the database
  RenderedSql <- Trajectories:::loadRenderTranslateSql("GetNumPairs.sql",
                                                       packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                       dbms=connection@dbms,
                                                       resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                       prefix = trajectoryLocalArgs$prefixForResultTableNames
  )
  dpairs = DatabaseConnector::querySql(connection, RenderedSql)
  ParallelLogger::logInfo('There are ',dpairs$TOTAL,' event pairs that are going to be analyzed.')

  ParallelLogger::logInfo('TASK COMPLETED: Creating event pairs data completed successfully.')
}
