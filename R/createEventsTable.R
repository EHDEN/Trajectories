#' Function to create empty cohort table. Script is based on  https://ohdsi.github.io/TheBookOfOhdsi/SqlAndR.html#implementing-the-study-using-sql-and-r (21th May 2020)
#'
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#'
#' @return
#'
#' @examples
createEventsTable<-function(connection,
                            trajectoryAnalysisArgs,
                            trajectoryLocalArgs,
                            baseCohort=list('0' = 'All persons'),
                            targetCohorts=list(
                              "701" = "PAP test 30-65",
                              "700" = "Kolposkoopia",
                              "672" = "HPV test",
                              "699" = "Observation of ASCUS 30-59",
                              "696" = "Observation of LSIL 30-59",
                              '692' = 'Observation of ASC-H 30-59',
                              '690' = 'Observation of HSIL 30-59',
                              '654' = 'Observation of NILM 30-59'
                            )) {  # used only if createCohort=T (validation set created while building cohort))


  ParallelLogger::logInfo('Creating events table ',trajectoryLocalArgs$resultsSchema,'.',trajectoryLocalArgs$prefixForResultTableNames,'events...')

  # ##################################################
  # 0. If cohorts have not been built yet to cdm.cohort table, this is the place where it should be done.
  # ##################################################

  # TODO. As a result, the cohort id-name map is returned
  # Use CohortGenerator package for this


  # ##################################################
  # 1. Create and fill traj_cohort_def table (cohort names)
  # ##################################################
  basecohort <- tibble(
    COHORT_ID = as.numeric(names(baseCohort)[1]),
    COHORT_NAME = unlist(baseCohort)[1],
    IS_BASE_COHORT = 1
  )

  cohorts <- tibble(
    COHORT_ID = as.numeric(names(targetCohorts)),
    COHORT_NAME = unlist(targetCohorts),
    IS_BASE_COHORT = 0
  )

  all_cohort_defs <- bind_rows(basecohort, cohorts)

  Trajectories:::insertTable(connection = connection,
                             databaseSchema=trajectoryLocalArgs$resultsSchema,
                             tableName = paste0(trajectoryLocalArgs$prefixForResultTableNames,'traj_cohort_def'),
                             data = all_cohort_defs,
                             dropTableIfExists=T,
                             progressBar = T)

  #res=querySql(connection, paste0('SELECT * FROM ',trajectoryLocalArgs$resultsSchema,'.',paste0(trajectoryLocalArgs$prefixForResultTableNames,'traj_cohort_def')))
  #ParallelLogger::logInfo(nrow(res),' cohorts added to traj_cohort_def')


  # ##################################################
  # 3a. Create and fill traj_base_cohort table
  # ##################################################

  RenderedSql <- Trajectories:::loadRenderTranslateSql('dropTable.sql',
                                                       packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                       dbms=connection@dbms,
                                                       schema=trajectoryLocalArgs$resultsSchema,
                                                       prefiX =  trajectoryLocalArgs$prefixForResultTableNames,
                                                       table='traj_base_cohort'
  )
  DatabaseConnector::executeSql(connection, RenderedSql)

  if(basecohort[[1,'COHORT_ID']]==0) {
    ParallelLogger::logInfo('Creating base cohort of all people in the database...')
    # special case. If base cohort id=0, then we artificially create the cohort - all people with full observation period
    RenderedSql <- Trajectories:::loadRenderTranslateSql('createTableBaseCohortFromAllPersons.sql',
                                                         packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                         dbms=connection@dbms,
                                                         resultsSchema=trajectoryLocalArgs$resultsSchema,
                                                         cdmDatabaseSchema=trajectoryLocalArgs$cdmDatabaseSchema,
                                                         prefiX =  trajectoryLocalArgs$prefixForResultTableNames
    )
  } else {
    RenderedSql <- Trajectories:::loadRenderTranslateSql('createTableBaseCohort.sql',
                                                         packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                         dbms=connection@dbms,
                                                         resultsSchema=trajectoryLocalArgs$resultsSchema,
                                                         cdmDatabaseSchema=trajectoryLocalArgs$cdmDatabaseSchema,
                                                         cohortDatabaseSchema=trajectoryLocalArgs$cohortDatabaseSchema,
                                                         cohortTableName=trajectoryLocalArgs$cohortTableName,
                                                         cohortId=basecohort[[1,'COHORT_ID']],
                                                         prefiX =  trajectoryLocalArgs$prefixForResultTableNames
    )
  }
  #print(RenderedSql)
  DatabaseConnector::executeSql(connection, RenderedSql)

  res_base=querySql(connection, paste0('SELECT COUNT(*) AS TOTAL FROM ',trajectoryLocalArgs$resultsSchema,'.',paste0(trajectoryLocalArgs$prefixForResultTableNames,'traj_base_cohort')))
  ParallelLogger::logInfo(res_base$TOTAL,' eventperiods added to traj_base_cohort')
  if(res_base$TOTAL==0) {
    ParallelLogger::logError('Something is not right: there are no people in the base cohort (cohort id=',basecohort[[1,'COHORT_ID']],'). Have you built the cohort in Atlas already?')
    stop()
  }





  # ##################################################
  # 2. Create empty traj_events table (for patient-level data)
  # ##################################################

  RenderedSql <- Trajectories:::loadRenderTranslateSql('dropTable.sql',
                                                       packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                       dbms=connection@dbms,
                                                       schema=trajectoryLocalArgs$resultsSchema,
                                                       prefiX =  trajectoryLocalArgs$prefixForResultTableNames,
                                                       table='events'
  )
  DatabaseConnector::executeSql(connection, RenderedSql)

  #querySql(connection, paste0('SELECT * FROM main.traj_cohort;'))

  # ##################################################
  # 3. Fill traj_cohort table with selected cohorts (expects that all cohorts have been built already)
  # ##################################################

  RenderedSql <- Trajectories:::loadRenderTranslateSql('addEvents.sql',
                                                       packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                       dbms=connection@dbms,
                                                       resultsSchema=trajectoryLocalArgs$resultsSchema,
                                                       cohortDatabaseSchema=trajectoryLocalArgs$cohortDatabaseSchema,
                                                       cohortTableName=trajectoryLocalArgs$cohortTableName,
                                                       prefiX =  trajectoryLocalArgs$prefixForResultTableNames,
                                                       baseCohortId=basecohort[[1,'COHORT_ID']],
                                                       daysBeforeIndexDate=trajectoryAnalysisArgs$daysBeforeIndexDate
  )
  DatabaseConnector::executeSql(connection, RenderedSql)



  #

  # ##################################################
  # 4. Get cohort counts in the event table
  # ##################################################

  #replace parameter values in SQL
  sql <- SqlRender::render("SELECT e.cohort_id, COUNT(*) AS NUM_OCCURRENCES, COUNT(distinct e.person_id) AS NUM_PERSONS FROM @resultsSchema.@prefiXevents e
                            LEFT JOIN @resultsSchema.@prefiXtraj_base_cohort b ON e.eventperiod_id=b.eventperiod_id
                           GROUP BY cohort_id;",
                           resultsSchema = trajectoryLocalArgs$resultsSchema,
                           prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  #translate SQL into right dialect
  sql <- SqlRender::translate(sql = sql,
                              targetDialect=attr(connection, "dbms"))
  # run query
  result = DatabaseConnector::querySql(connection, sql)

  print('Cohort counts in the EVENTS table after preprocessing: ')
  print(cohorts %>% left_join(result))


  # Total count
  sql <- SqlRender::render("SELECT COUNT(*) TOTAL FROM @resultsSchema.@prefiXevents;",
                           resultsSchema = trajectoryLocalArgs$resultsSchema,
                           prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  #translate SQL into right dialect
  sql <- SqlRender::translate(sql = sql,
                              targetDialect=attr(connection, "dbms"))
  # run query
  result = DatabaseConnector::querySql(connection, sql)
  ParallelLogger::logInfo('...done. There are ',res_base,' event periods in base cohort table and ',result$TOTAL,' rows in events table.')
}


