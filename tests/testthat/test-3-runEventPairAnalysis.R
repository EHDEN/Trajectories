testthat::context("Testing runEventPairAnalysis")
library(Trajectories)
library(DatabaseConnector)
library(stringr)
library(SqlRender)
library(Eunomia)

#querySql(connection, paste0('SELECT COUNT(*) FROM PERSON;'))
#querySql(connection, paste0('SELECT COUNT(*) FROM OBSERVATION_PERIOD;'))
#querySql(connection, paste0('SELECT * FROM OBSERVATION_PERIOD;'))
#querySql(connection, paste0('SELECT COUNT(*) FROM CONDITION_OCCURRENCE;'))
#querySql(connection, paste0('SELECT * FROM CONDITION_OCCURRENCE LIMIT 30;'))
#querySql(connection, paste0('SELECT * FROM E1E2_MODEL;'))


testthat::test_that("No significant event pairs in random data", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=100) #in analysis, use 100 patients
  limitToConcepts(connection)
  #setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  addRandomEvents(connection,n_per_person_range=c(0,10)) # Add up to 10 random events per each patient



  # Setting database parameters:
  #library(stringi)


  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                       maximumDaysBetweenEvents = 365*120,
                                                                       minPatientsPerEventPair = 5,
                                                                       addConditions=T,
                                                                       addObservations=F,
                                                                       addProcedures=F,
                                                                       addDrugExposures=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addDrugEras=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addBirths=F,
                                                                       addDeaths=F,
                                                                       daysBeforeIndexDate=Inf,
                                                                       cohortName="test",
                                                                       RRrangeToSkip=c(0,1))


  #Create output folder for this analysis
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)
  print(paste0('Test that output folder ',outputFolder,' exists: ',dir.exists(outputFolder)))
  testthat::expect_equal(dir.exists(outputFolder), TRUE)

  # Set up logger
  Trajectories:::InitLogger(logfile = file.path(outputFolder,'log.txt'), threshold = 'INFO')

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories:::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories:::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)

  #querySql(connection, glue::glue('SELECT COUNT(*) AS TOTAL FROM {trajectoryLocalArgs$resultsSchema}.{trajectoryLocalArgs$prefixForResultTableNames}E1E2_model'))

  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs)



  #print(paste0('Test that output folder ',outputFolder,' exists: ',dir.exists(outputFolder)))
  #testthat::expect_equal(dir.exists(outputFolder), TRUE)
  #print('Files in directory:')
  #list.files(outputFolder)

  #test that output file was created, but it has header row only
  eventPairResultsFilename = file.path(outputFolder,'tables','event_pairs_tested.tsv')
  print(paste0('Test that ',eventPairResultsFilename,' exists...'))
  testthat::expect_equal(file.exists(eventPairResultsFilename), TRUE)
  directional_event_pairs<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_directional.tsv')
  testthat::expect_equal(nrow(directional_event_pairs),0)

})

testthat::test_that("Test ability to detect a synthetic event pair in data", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=100) #in analysis, use 100 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848),n=20,days_to_skip_from_obs_period_start=365) # Add asthma->pneumonia pair for 20 patients
  addRandomEvents(connection,n_per_person_range=c(0,10),exclude_person_ids=person_ids,exclude_concept_ids=c(317009,255848)) # Add up to 10 random events per each patient, except events 317009 and 255848
  all_person_ids<-getPatientIds(connection)
  non_case_person_ids<-setdiff(all_person_ids,person_ids)
  non_case_person_ids_sample<-sample(non_case_person_ids,size=floor(length(non_case_person_ids)/2))
  addRandomEvents(connection,n_per_person_range=c(0,1), include_concept_ids=c(255848), include_person_ids=non_case_person_ids_sample)

  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                       maximumDaysBetweenEvents = 365*120,
                                                                       minPatientsPerEventPair = 10,
                                                                       addConditions=T,
                                                                       addObservations=F,
                                                                       addProcedures=F,
                                                                       addDrugExposures=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addDrugEras=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addBirths=F,
                                                                       addDeaths=F,
                                                                       daysBeforeIndexDate=Inf,
                                                                       cohortName="test")


  #Create output folder for this analysis
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories:::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories:::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)

  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs)

  #test that event pair 317009->255848 is among tested pairs


  #test that there is at least 1 event pairs tested
  tested_event_pairs<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_gt(nrow(tested_event_pairs),0)
  #test that event pair 317009->255848 is among them
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_equal(row$E1_COUNT_IN_EVENTS, 20)
  testthat::expect_gte(row$E2_COUNT_IN_EVENTS, 20)
  testthat::expect_equal(row$E1_COUNT_IN_PAIRS, 20)
  testthat::expect_equal(row$E1_BEFORE_E2_COUNT_IN_EVENTS, 20)

  #test that event pair 317009->255848 is found significant and the counts are correct
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs)
  testthat::expect_equal(row$E1_COUNT_IN_PAIRS, 20)
  testthat::expect_equal(row$E1_BEFORE_E2_COUNT_IN_EVENTS, 20)

  testthat::expect_gt(as.numeric(row$RR_PVALUE), 0) # P-value of RR is calculated
  testthat::expect_gt(as.numeric(row$DIRECTIONAL_PVALUE), 0) # P-value of direction is calculated

  #test that the names are correct
  testthat::expect_equal(row$E1_NAME, 'Asthma')
  testthat::expect_equal(row$E1_DOMAIN, 'Condition')
  testthat::expect_equal(row$E2_NAME, 'Pneumonia')
  testthat::expect_equal(row$E2_DOMAIN, 'Condition')


})


testthat::test_that("Test ability to detect a synthetic event pair in data with 0 counts of these events outside the pair (same as previous test, but no random events)", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=100) #in analysis, use 100 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848),n=20,days_to_skip_from_obs_period_start=365) # Add asthma->pneumonia pair for 20 patients
  addRandomEvents(connection,n_per_person_range=c(0,10),exclude_person_ids=person_ids,exclude_concept_ids=c(317009,255848)) # Add up to 10 random events per each patient, except events 317009 and 255848
  #all_person_ids<-getPatientIds(connection)
  #non_case_person_ids<-setdiff(all_person_ids,person_ids)
  #non_case_person_ids_sample<-sample(non_case_person_ids,size=floor(length(non_case_person_ids)/2))
  #addRandomEvents(connection,n_per_person_range=c(0,1), include_concept_ids=c(255848), include_person_ids=non_case_person_ids_sample)

  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                        maximumDaysBetweenEvents = 365*120,
                                                                        minPatientsPerEventPair = 10,
                                                                        addConditions=T,
                                                                        addObservations=F,
                                                                        addProcedures=F,
                                                                        addDrugExposures=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                        addDrugEras=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                        addBirths=F,
                                                                        addDeaths=F,
                                                                        daysBeforeIndexDate=Inf,
                                                                        cohortName="test")


  #Create output folder for this analysis
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories:::createAndFillCohortTable(connection=connection,
                                          trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                          trajectoryLocalArgs=trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories:::createEventPairsTable(connection=connection,
                                       trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                       trajectoryLocalArgs=trajectoryLocalArgs)

  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)

  #test that event pair 317009->255848 is among tested pairs


  #test that there is at least 1 event pairs tested
  tested_event_pairs<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_gt(nrow(tested_event_pairs),0)
  #test that event pair 317009->255848 is among them
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_equal(row$E1_COUNT_IN_EVENTS, 20)
  testthat::expect_equal(row$E2_COUNT_IN_EVENTS, 20)
  testthat::expect_equal(row$E1_COUNT_IN_PAIRS, 20)
  testthat::expect_equal(row$E1_BEFORE_E2_COUNT_IN_EVENTS, 20)

  #test that event pair 317009->255848 is found significant and the counts are correct
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs)
  testthat::expect_equal(row$E1_COUNT_IN_PAIRS, 20)
  testthat::expect_equal(row$E1_BEFORE_E2_COUNT_IN_EVENTS, 20)

  testthat::expect_gt(as.numeric(row$RR_PVALUE), 0) # P-value of RR is calculated
  testthat::expect_gt(as.numeric(row$DIRECTIONAL_PVALUE), 0) # P-value of direction is calculated

  #test that the names are correct
  testthat::expect_equal(row$E1_NAME, 'Asthma')
  testthat::expect_equal(row$E1_DOMAIN, 'Condition')
  testthat::expect_equal(row$E2_NAME, 'Pneumonia')
  testthat::expect_equal(row$E2_DOMAIN, 'Condition')


})


testthat::test_that("Test that error in matchit() function does not cause the analysis to stop", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=100) #in analysis, use 100 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848),n=20,days_to_skip_from_obs_period_start=365) # Add asthma->pneumonia pair for 10 patients
  #leave only females to cases (remove males) (8532=F)
  executeSql(connection, paste0('DELETE FROM CONDITION_OCCURRENCE WHERE PERSON_ID IN (
                              SELECT PERSON_ID FROM PERSON WHERE gender_concept_id!=8532
  );'))
  #querySql(connection, paste0('SELECT p.gender_concept_id,COUNT(*) FROM CONDITION_OCCURRENCE c LEFT JOIN PERSON p ON c.person_id=p.person_id GROUP BY p.gender_concept_id;'))
  all_person_ids<-getPatientIds(connection)
  non_case_person_ids<-setdiff(all_person_ids,person_ids)
  non_case_person_ids_sample<-sample(non_case_person_ids,size=50)
  addRandomEvents(connection,n_per_person_range=c(0,1), include_concept_ids=c(255848), include_person_ids=non_case_person_ids_sample)
  #remove all females from non-cases. That should lead to error in matching (no gender match)
  executeSql(connection, paste0('DELETE FROM CONDITION_OCCURRENCE WHERE PERSON_ID IN (
                              SELECT PERSON_ID FROM PERSON WHERE gender_concept_id=8532
                              AND PERSON_ID IN (',paste0(non_case_person_ids_sample,collapse=","),')
  );'))
  #querySql(connection, paste0('SELECT p.gender_concept_id,COUNT(*) FROM CONDITION_OCCURRENCE c LEFT JOIN PERSON p ON c.person_id=p.person_id GROUP BY p.gender_concept_id;'))



  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                        maximumDaysBetweenEvents = 365*120,
                                                                        minPatientsPerEventPair = 2,
                                                                        addConditions=T,
                                                                        addObservations=F,
                                                                        addProcedures=F,
                                                                        addDrugExposures=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                        addDrugEras=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                        addBirths=F,
                                                                        addDeaths=F,
                                                                        daysBeforeIndexDate=Inf,
                                                                        cohortName="test")


  #Create output folder for this analysis
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories:::createAndFillCohortTable(connection=connection,
                                          trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                          trajectoryLocalArgs=trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories:::createEventPairsTable(connection=connection,
                                       trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                       trajectoryLocalArgs=trajectoryLocalArgs)

  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)

  #No further test, just confirm that the previous command did not produce any error


})


testthat::test_that("Test ability to detect a synthetic event pair association (not directional) in data", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=250) #in analysis, use 50 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848),n=50,days_to_skip_from_obs_period_start=365) # Add asthma->pneumonia pair for 30 patients
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(255848,317009),n=50,days_to_skip_from_obs_period_start=365,excludePatientIds=person_ids) # Add pneumonia->asthma pair for (different) 30 patients
  addRandomEvents(connection,n_per_person_range=c(0,10),exclude_concept_ids=c(317009,255848)) # Add up to 10 random events per each patient, except events 317009 and 255848


  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                       maximumDaysBetweenEvents = 365*120,
                                                                       minPatientsPerEventPair = 40,
                                                                       addConditions=T,
                                                                       addObservations=F,
                                                                       addProcedures=F,
                                                                       addDrugExposures=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addDrugEras=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addBirths=F,
                                                                       addDeaths=F,
                                                                       daysBeforeIndexDate=Inf,
                                                                       cohortName="test")


  #Create output folder for this analysis
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories:::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories:::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)

  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs)

  #test that event pair 317009->255848 is among tested pairs


  #test that there is at least 2 event pairs tested
  tested_event_pairs<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_gte(nrow(tested_event_pairs),2)
  #test that event pair 317009->255848 is among them
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_equal(row$E1_COUNT_IN_EVENTS, 100)
  testthat::expect_equal(row$E2_COUNT_IN_EVENTS, 100)
  testthat::expect_equal(row$E1_BEFORE_E2_COUNT_IN_EVENTS, 50)
  testthat::expect_gt(as.numeric(row$RR_PVALUE), 0) # P-value of RR is calculated
  testthat::expect_equal((is.na(row[1,]$DIRECTIONAL_SIGNIFICANT) | row[1,]$DIRECTIONAL_SIGNIFICANT==''),TRUE) # Order Not significant

  #test that event pair (opposite) 255848->317009 is among them
  row<-getEventPairFromEventPairsTable(event1_concept_id=255848,event2_concept_id=317009,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_equal(row$E1_COUNT_IN_EVENTS, 100)
  testthat::expect_equal(row$E2_COUNT_IN_EVENTS, 100)
  testthat::expect_equal(row$E1_BEFORE_E2_COUNT_IN_EVENTS, 50)
  testthat::expect_gt(as.numeric(row$RR_PVALUE), 0) # P-value of RR is calculated
  testthat::expect_equal((is.na(row[1,]$DIRECTIONAL_SIGNIFICANT) | row[1,]$DIRECTIONAL_SIGNIFICANT==''),TRUE) # Order Not significant

  #test that event pair 317009->255848 is not represented in results file as the direction is not significant
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_directional.tsv')
  testthat::expect_equal(nrow(row),0)

  #test that event pair 255848->317009 is not represented in results file as the direction is not significant
  row<-getEventPairFromEventPairsTable(event1_concept_id=255848,event2_concept_id=317009,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_directional.tsv')
  testthat::expect_equal(nrow(row),0)


})


testthat::test_that("Test ability to detect a longer trajectory (consisting of 2 pairs)", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=400) #in analysis, use 300 patients.
  limitToConcepts(connection)
  #setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2011-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848,4299128),n=100,days_to_skip_from_obs_period_start=365) # Add asthma->pneumonia pair for 20 patients
  addRandomEvents(connection,n_per_person_range=c(0,2),exclude_concept_ids=c(317009,255848,4299128)) # Add up to 10 random events per each patient, except events 317009 and 255848

  all_person_ids<-getPatientIds(connection)
  non_case_person_ids<-setdiff(all_person_ids,person_ids)
  non_case_person_ids_sample<-sample(non_case_person_ids,size=floor(length(non_case_person_ids)/2))
  addRandomEvents(connection,n_per_person_range=c(0,1),include_concept_ids=c(317009,255848,4299128), include_person_ids = non_case_person_ids_sample) # Add up to 10 random events per each patient, except events 317009 and 255848


  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                       maximumDaysBetweenEvents = 365*120,
                                                                       minPatientsPerEventPair = 19,
                                                                       addConditions=T,
                                                                       addObservations=F,
                                                                       addProcedures=F,
                                                                       addDrugExposures=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addDrugEras=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addBirths=F,
                                                                       addDeaths=F,
                                                                       daysBeforeIndexDate=Inf,
                                                                       cohortName="test")


  #Create output folder for this analysis
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  # Set up logger (when debugging only)
  #Trajectories:::InitLogger(logfile = file.path(outputFolder,'log.txt'), threshold = 'DEBUG')

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories:::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories:::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)
  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs)

  #test that event pair 317009->255848 is among tested pairs


  #test that there is at least 2 event pairs tested
  tested_event_pairs<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_gte(nrow(tested_event_pairs),2)
  #test that event pair 317009->255848 is among them
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_gte(row$E1_COUNT_IN_PAIRS, 100)
  testthat::expect_gte(row$E2_COUNT_IN_PAIRS, 100)
  testthat::expect_equal(row$E1_BEFORE_E2_COUNT_IN_EVENTS, 100)

  #test that event pair 317009->4299128 is among them
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=4299128,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_gte(row$E1_COUNT_IN_PAIRS, 100)
  testthat::expect_gte(row$E2_COUNT_IN_PAIRS, 100)
  testthat::expect_equal(row$E1_BEFORE_E2_COUNT_IN_EVENTS, 100)

  #test that event pair 255848->4299128 is among them
  row<-getEventPairFromEventPairsTable(event1_concept_id=255848,event2_concept_id=4299128,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_gte(row$E1_COUNT_IN_PAIRS, 100)
  testthat::expect_gte(row$E2_COUNT_IN_PAIRS, 100)
  testthat::expect_equal(row$E1_BEFORE_E2_COUNT_IN_EVENTS, 100)

  #test that event pair 317009->255848 is found directionally significant and the counts are correct
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs)
  testthat::expect_gt(as.numeric(row$RR_PVALUE), 0) # P-value of associaction is calculated
  testthat::expect_gt(as.numeric(row$DIRECTIONAL_PVALUE), 0) # P-value of direction is calculated
  testthat::expect_equal(row$DIRECTIONAL_SIGNIFICANT, '*') # P-value of direction is calculated


  #test that event pair 255848->4299128 is found directionally significant and the counts are correct
  row<-getEventPairFromEventPairsTable(event1_concept_id=255848,event2_concept_id=4299128,trajectoryLocalArgs,trajectoryAnalysisArgs)
  testthat::expect_gt(as.numeric(row$RR_PVALUE), 0) # P-value of associaction is calculated
  testthat::expect_gt(as.numeric(row$DIRECTIONAL_PVALUE), 0) # P-value of direction is calculated
  testthat::expect_equal(row$DIRECTIONAL_SIGNIFICANT, '*') # P-value of direction is calculated

  #test that event pair 317009->4299128 is found directionally significant and the counts are correct
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=4299128,trajectoryLocalArgs,trajectoryAnalysisArgs)
  testthat::expect_gt(as.numeric(row$RR_PVALUE), 0) # P-value of associaction is calculated
  testthat::expect_gt(as.numeric(row$DIRECTIONAL_PVALUE), 0) # P-value of direction is calculated
  testthat::expect_equal(row$DIRECTIONAL_SIGNIFICANT, '*') # P-value of direction is calculated

})



testthat::test_that("Test that forceRecalculation=F does not cause any error", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=100) #in analysis, use 100 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848),n=20,days_to_skip_from_obs_period_start=365) # Add asthma->pneumonia pair for 20 patients
  addRandomEvents(connection,n_per_person_range=c(0,10),exclude_concept_ids=c(317009,255848)) # Add up to 10 random events per each patient, except events 317009 and 255848


  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                       maximumDaysBetweenEvents = 365*120,
                                                                       minPatientsPerEventPair = 10,
                                                                       addConditions=T,
                                                                       addObservations=F,
                                                                       addProcedures=F,
                                                                       addDrugExposures=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addDrugEras=F, # NB! DO NOT USE BOTH addDrugEras=T and addDrugExposures=T (not both) as it leads to analysis duplication and breaks some code... (same "drug" event may occur several times which is not allowed)
                                                                       addBirths=F,
                                                                       addDeaths=F,
                                                                       daysBeforeIndexDate=Inf,
                                                                       cohortName="test")


  #Create output folder for this analysis
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories:::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories:::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)

  #no previous results
  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs,
                                     forceRecalculation = F)
  tested_event_pairs<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  #at least 1 pair significant
  num.pairs.significant=nrow(tested_event_pairs)
  testthat::expect_gte(num.pairs.significant,1)

  #create new results
  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs,
                                     forceRecalculation = T)
  tested_event_pairs<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_equal(nrow(tested_event_pairs),num.pairs.significant)

  #overwrite results
  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs,
                                     forceRecalculation = F)
  tested_event_pairs<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  testthat::expect_equal(nrow(tested_event_pairs),num.pairs.significant)

})
