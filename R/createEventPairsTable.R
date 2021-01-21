library(SqlRender)
#' Creates event pairs table and populates it with the data
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @inheritParams GetOutputFolder
#' @return
#' @export
#'
#' @examples
createEventPairsTable<-function(connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs
                               ) {
  logger::log_info(paste0("Create database tables + data for all event pairs to '",trajectoryLocalArgs$resultsSchema,"' schema..."))


  #In case trajectoryAnalysisArgs$minPatientsPerEventPair < 1, the actual value means "prevalence", not absolute number.
  #Therefore, we need to calculate the absolute number from this
  if(trajectoryAnalysisArgs$minPatientsPerEventPair<1) {
    cohortCount<-getCohortSize(connection, trajectoryLocalArgs)
    minPatientsPerEventPair=round(cohortCount*trajectoryAnalysisArgs$minPatientsPerEventPair)
    if(minPatientsPerEventPair==0) minPatientsPerEventPair=1
    logger::log_info(paste0('Parameter value of minPatientsPerEventPair=',trajectoryAnalysisArgs$minPatientsPerEventPair,' is less than 1. ',
                  'Therefore, it is handled as prevalence instead of an absolute number. ',
                  'The absolute number is calculated based on cohort size (n=',cohortCount,') as follows: ',
                  'minPatientsPerEventPair = ',cohortCount,' x ',trajectoryAnalysisArgs$minPatientsPerEventPair,' = ',minPatientsPerEventPair)
          )
  } else {
    minPatientsPerEventPair=trajectoryAnalysisArgs$minPatientsPerEventPair
  }

  # Check if there is data in person.birth_datetime if addBirths=T
  if(trajectoryAnalysisArgs$addBirths==T){
    Trajectories::addBirthsChecker(connection=connection,
                                   trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                   trajectoryLocalArgs=trajectoryLocalArgs)
  }
  logger::log_info(paste0("Running SQL..."))

  #Set SQL role of the database session
  Trajectories::setRole(connection,trajectoryLocalArgs$sqlRole)

  # Create everything up to E1E2_model table
  logger::log_info("Creating event pairs in data...")
  RenderedSql = Trajectories::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part1.sql',
                                                  packageName=trajectoryAnalysisArgs$packageName,
                                                  dbms = connection@dbms,
                                                  oracleTempSchema = NULL,
                                                  resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                  cdmDatabaseSchema = trajectoryLocalArgs$cdmDatabaseSchema,
                                                  minimumDaysBetweenEvents = trajectoryAnalysisArgs$minimumDaysBetweenEvents,
                                                  maximumDaysBetweenEvents = trajectoryAnalysisArgs$maximumDaysBetweenEvents,
                                                  daysBeforeIndexDate = trajectoryAnalysisArgs$daysBeforeIndexDate,
                                                  prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                  cohortTableSchema = trajectoryLocalArgs$cohortTableSchema,
                                                  cohortTable = trajectoryLocalArgs$cohortTable,
                                                  cohortId = trajectoryLocalArgs$cohortId,
                                                  addConditions = ifelse(trajectoryAnalysisArgs$addConditions==T,1,0),
                                                  addObservations = ifelse(trajectoryAnalysisArgs$addObservations==T,1,0),
                                                  addProcedures = ifelse(trajectoryAnalysisArgs$addProcedures==T,1,0),
                                                  addDrugExposures = ifelse(trajectoryAnalysisArgs$addDrugExposures==T,1,0),
                                                  addDrugEras = ifelse(trajectoryAnalysisArgs$addDrugEras==T,1,0),
                                                  addBirths = ifelse(trajectoryAnalysisArgs$addBirths==T,1,0),
                                                  addDeaths = ifelse(trajectoryAnalysisArgs$addDeaths==T,1,0)
  )
  DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = TRUE, reportOverallTime = TRUE)
  logger::log_info("...done.")

  # Create and fill E1E2_model_input table
  # There are two options here - either to build it from the actual data OR (in validation mode) build it from given event-pairs
  #Check whether to run the method in validation mode
  isValidationMode=Trajectories::IsValidationMode(trajectoryLocalArgs, verbose=T)
  if(isValidationMode) {

    f=file.path(trajectoryLocalArgs$inputFolder,'event_pairs_for_validation.tsv')
    logger::log_info("Data to E1E2_MODEL is taken from file '{f}'...")

    #2Can't use simply insertTable here because in Eunomia package it does not solve schema name correctly. That's why this is commented out and we manually create SQL here :(
    #insertTable(connection, tablename, edges, tempTable=F, progressBar=T)

    e = read.csv2(file = f, sep = '\t', header = TRUE, as.is=T)
    e$RR_IN_PREVIOUS_STUDY<-as.numeric(e$RR_IN_PREVIOUS_STUDY)

    if(nrow(e)==0) logger::log_error("Package is run in validation mode, but file 'event_pairs_for_validation.tsv' is empty")

    #Create empty table manually
    RenderedSql <- Trajectories::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part2b.sql',
                                                        packageName=trajectoryAnalysisArgs$packageName,
                                                        dbms = connection@dbms,
                                                        oracleTempSchema = NULL,
                                                        resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                        prefiX = trajectoryLocalArgs$prefixForResultTableNames
    )
    DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = TRUE, reportOverallTime = TRUE)

    #Fill with data
    tablename<-paste0(trajectoryLocalArgs$resultsSchema,'.',trajectoryLocalArgs$prefixForResultTableNames,'E1E2_MODEL_INPUT')
    logger::log_info("Filling {tablename} with data from 'event_pairs_for_validation.tsv'...")
    e$sql <- paste0("INSERT INTO ",
                   tablename,
                   " (E1_CONCEPT_ID,E2_CONCEPT_ID,E1_NAME,E1_DOMAIN,E2_NAME,E2_DOMAIN,RR_IN_PREVIOUS_STUDY) VALUES (",
                   e$E1_CONCEPT_ID,
                   ",",
                   e$E2_CONCEPT_ID,
                   ",'",
                   e$E1_NAME,
                   "','",
                   e$E1_DOMAIN,
                   "','",
                   e$E2_NAME,
                   "','",
                   e$E2_DOMAIN,
                   "',",
                   ifelse(is.na(e$RR_IN_PREVIOUS_STUDY),'NULL',e$RR_IN_PREVIOUS_STUDY),
                   ");")
    e <- e %>% arrange(-RR_IN_PREVIOUS_STUDY)
    for(insertSql in e$sql) {
      RenderedSql <- SqlRender::translate(insertSql,targetDialect=attr(connection, "dbms"))
      #print(RenderedSql)
      DatabaseConnector::executeSql(connection, RenderedSql)
    }
    logger::log_info("...done.")

  } else {
    logger::log_info("Running package in DISCOVERY mode (not validating someone's results)")
    RenderedSql = Trajectories::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part2a.sql',
                                                       packageName=trajectoryAnalysisArgs$packageName,
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
  logger::log_info("Creating statistics for events pairs that are going to be analyzed...")
  RenderedSql = Trajectories::loadRenderTranslateSql(sqlFilename='createEventPairsTable-part3.sql',
                                                     packageName=trajectoryAnalysisArgs$packageName,
                                                     dbms = connection@dbms,
                                                     oracleTempSchema = NULL,
                                                     resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                     prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = TRUE, reportOverallTime = TRUE)
  logger::log_info("...done.")

  #During the execution of previous scripts, SQL writes some debug information to table @resultsSchema.@prefiXdebug. Lets write this to log also.
  sql<-"SELECT ENTRY FROM @resultsSchema.@prefiXdebug ORDER BY timestamp;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))
  logger::log_info('Debug info from database operations:')
  for(row in res$ENTRY) {
    #print(row)
    logger::log_info(row)
  }


  # Get all (frequent) event pairs from the database
  RenderedSql <- Trajectories::loadRenderTranslateSql("GetNumPairs.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=connection@dbms,
                                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                   prefix = trajectoryLocalArgs$prefixForResultTableNames
  )
  dpairs = DatabaseConnector::querySql(connection, RenderedSql)
  logger::log_info(paste0('There are ',dpairs$TOTAL,' event pairs that are going to be analyzed.'))

  logger::log_info('TASK COMPLETED: Creating event pairs data completed successfully.')
}
