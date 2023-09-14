#' Creates event pairs table and populates it with the data
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @inheritParams GetOutputFolder
#' @return
#'
#' @examples
createEventPairsTable<-function(connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs
                               ) {

  # IS_VALIDATION_SET=0 when running in DISCOVERY mode
  # IS_VALIDATION_SET=1 when running in VALIDATION mode
  IS_VALIDATION_SET=ifelse(Trajectories:::IsValidationMode(trajectoryAnalysisArgs)==TRUE,1,0)

  ParallelLogger::logInfo("Create database tables + data for all event periods having IS_VALIDATION_SET=",
                          IS_VALIDATION_SET, " to '",trajectoryLocalArgs$resultsSchema,"' schema...")


  # In case trajectoryAnalysisArgs$minPatientsPerEventPair < 1,
  # the actual value means "prevalence", not absolute number.
  # Therefore, we need to calculate the absolute number from this
  minPatientsPerEventPair<-Trajectories:::getMinPatientsPerEventPair(connection,
                                                                     trajectoryAnalysisArgs,
                                                                     trajectoryLocalArgs)

  # Store used analysis arguments to JSON file
  Trajectories:::TrajectoryAnalysisArgsToJson(trajectoryAnalysisArgs,
                                              file.path(Trajectories:::GetOutputFolder(trajectoryLocalArgs=trajectoryLocalArgs,
                                                                                       trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                                                                       createIfMissing=F),
                                                        'logs',
                                                        "trajectoryAnalysisArgs_used.json")
                                              )

  # Create everything up to E1E2_model table
  ParallelLogger::logInfo("Creating event pairs in data...")
  RenderedSql = Trajectories:::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part1.sql',
                                                      packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                      dbms = connection@dbms,
                                                      cdmDatabaseSchema = trajectoryLocalArgs$cdmDatabaseSchema,
                                                      resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                      minimumDaysBetweenEvents = trajectoryAnalysisArgs$minimumDaysBetweenEvents,
                                                      maximumDaysBetweenEvents = trajectoryAnalysisArgs$maximumDaysBetweenEvents,
                                                      prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection,
                                sql=RenderedSql,
                                profile=F,
                                progressBar = TRUE,
                                reportOverallTime = TRUE)

  ParallelLogger::logInfo("...done.")

  # Create and fill E1E2_model_input table
  # There are two options here:
  # 1. either to build it from the actual data OR
  # 2. (in validation mode) build it from given event-pairs
  # Check whether to run the method in validation mode
  isValidationMode=Trajectories:::IsValidationMode(trajectoryAnalysisArgs)
  if(isValidationMode) {

    f=file.path(trajectoryLocalArgs$inputFolder,'event_pairs_for_validation.tsv')
    ParallelLogger::logInfo("Data to E1E2_MODEL is taken from file '",f,"'...")

    # Can't use simply insertTable here
    # because in Eunomia package it does not solve schema name correctly.
    # That's why this is commented out and we manually create SQL here :(
    # insertTable(connection, tablename, edges, tempTable=F, progressBar=T)
    e = read.csv2(file = f, sep = '\t', header = TRUE, as.is=T)
    e$RR_IN_PREVIOUS_STUDY<-as.numeric(e$RR_IN_PREVIOUS_STUDY)

    # convert Inf values to 999
    e <- e %>% mutate(RR_IN_PREVIOUS_STUDY=ifelse(RR_IN_PREVIOUS_STUDY==Inf,999,RR_IN_PREVIOUS_STUDY))

    if(any(is.na(e$RR_IN_PREVIOUS_STUDY))) ParallelLogger::logError("Package is run in validation mode, but RR_IN_PREVIOUS_STUDY in file 'event_pairs_for_validation.tsv' is empty for at least one pair. This is not allowed (please remove such rows).")
    if(!is.numeric(e$RR_IN_PREVIOUS_STUDY)) ParallelLogger::logError("Package is run in validation mode, but RR_IN_PREVIOUS_STUDY column in file 'event_pairs_for_validation.tsv' is not numeric. This is not allowed (please remove non-numeric rows).")

    if(nrow(e)==0) ParallelLogger::logError("Package is run in validation mode, but file 'event_pairs_for_validation.tsv' is empty")

    # Put data to table E1E2_model_input
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
    RenderedSql = Trajectories:::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part2a.sql',
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
  RenderedSql = Trajectories:::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part3.sql',
                                                     packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                     dbms = connection@dbms,
                                                     oracleTempSchema = NULL,
                                                     resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                     prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = TRUE, reportOverallTime = TRUE)
  ParallelLogger::logInfo("...done.")

  # During the execution of previous scripts,
  # SQL writes some debug information to table @resultsSchema.@prefiXdebug.
  # Let's write this to log also:
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
  ParallelLogger::logInfo('There are ', dpairs$TOTAL, ' event pairs that are going to be analyzed.')

  ParallelLogger::logInfo('TASK COMPLETED: Creating event pairs data completed successfully.')
}

getMinPatientsPerEventPair<-function(connection,
                                     trajectoryAnalysisArgs,
                                     trajectoryLocalArgs) {
  # In case trajectoryAnalysisArgs$minPatientsPerEventPair < 1,
  # the actual value means "prevalence", not absolute number.
  # Therefore, we need to calculate the absolute number from this
  if(trajectoryAnalysisArgs$minPatientsPerEventPair<1) {
    cohortCount<-Trajectories:::getCohortSize(connection, trajectoryAnalysisArgs, trajectoryLocalArgs)
    minPatientsPerEventPair=round(cohortCount*trajectoryAnalysisArgs$minPatientsPerEventPair)
    if(minPatientsPerEventPair==0) minPatientsPerEventPair=1
    ParallelLogger::logInfo('Parameter value of minPatientsPerEventPair=',trajectoryAnalysisArgs$minPatientsPerEventPair,' is less than 1. ',
                            'Therefore, it is handled as prevalence instead of an absolute number. ',
                            'The absolute number is calculated based on cohort size (n=',cohortCount,') as follows: ',
                            'minPatientsPerEventPair = ',cohortCount,' x ',trajectoryAnalysisArgs$minPatientsPerEventPair,' = ',minPatientsPerEventPair)
  } else {
    minPatientsPerEventPair=trajectoryAnalysisArgs$minPatientsPerEventPair
  }
  return(minPatientsPerEventPair)
}
