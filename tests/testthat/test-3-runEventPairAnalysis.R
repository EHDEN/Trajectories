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

#creates synthecitc trajectory A->B where A always leads to B. Also adds B occurrences so that RR holds.
create_synthetic_trajectory<-function(num_total_patients=100,num_traj=10,num_random_events=c(0,10),traj=c(317009,255848),rr=2) {
  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=num_total_patients)
  limitToConcepts(connection)
  #setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  all_person_ids<-getPatientIds(connection)

  # Add up to x random events per each patient, except events in the trajectory
  addRandomEvents(connection,n_per_person_range=num_random_events,exclude_person_ids=c(),exclude_concept_ids=traj)

  if(length(traj)==2) {
    # The number of total A-s (can't be smaller than num_trajs)
    num_As=num_traj*2 #the probability that B is followed after A is 50% (currently hardcoded)
    ParallelLogger::logInfo(paste0("Number of '",traj[1],"' patients before duplication: ",num_As))
    num_As_without_preceding_B=num_As-num_traj
    prob_B_with_prior_A=num_traj/num_As
    ParallelLogger::logInfo(paste0('prob_B_with_prior_A=',prob_B_with_prior_A))
    # Add trajectory A->B for x patients
    person_ids<-addConditionEventTrajectory(connection,event_concept_ids=traj,n=num_traj,days_to_skip_from_obs_period_start=0)
    # For num_As_without_preceding_B patients, add only the first event (so that the risk of getting the second event after the first is 50%)
    non_case_person_ids<-setdiff(all_person_ids,person_ids)
    num_patients_without_second_event=num_As_without_preceding_B
    non_case_person_ids_sample<-sample(non_case_person_ids,size=num_patients_without_second_event)
    addRandomEvents(connection,n_per_person_range=c(1,1), include_concept_ids=c(traj[1]), include_person_ids=non_case_person_ids_sample)
    person_ids<-c(person_ids,non_case_person_ids_sample)
    # Duplicate data (persons and events) - for control group (ideal match)
    person_id_map<-duplicatePersonsAndTheirConditions(connection)
    # In duplicated part (control group), replace all A-s with random events
    persons_with_A <- querySql(connection, paste0('SELECT DISTINCT person_id FROM CONDITION_OCCURRENCE WHERE condition_concept_id=',traj[1],';'))
    controls_with_A <- intersect(persons_with_A$PERSON_ID,person_id_map$PERSON_ID_NEW)
    for(control_with_A in controls_with_A) {
      replaceEventWithRandomEventForPeson(connection,person_id=control_with_A,concept_id_to_replace=traj[1],exclude_concept_ids=traj,include_concept_ids=c())
    }
    # Calculate required prob_B_without_prior_A from RR
    #rr=2
    prob_B_without_prior_A=prob_B_with_prior_A/rr
    ParallelLogger::logInfo(paste0('prob_B_without_prior_A=',prob_B_without_prior_A))
    # Calculate, how many B-s can we have in duplicated part to satisfy expected RR criterion
    max_num_Bs_in_duplicated_part <- round(prob_B_without_prior_A*num_As)
    ParallelLogger::logInfo(paste0('max_num_Bs_in_duplicated_part=',max_num_Bs_in_duplicated_part))
    # In duplicated part (control group), replace B-s one-by-one with random events until number of B-s is smaller than max_num_Bs_in_duplicated_part
    num_Bs_in_duplicated_part=num_traj
    for(control_with_A in controls_with_A) {
      if(num_Bs_in_duplicated_part<=max_num_Bs_in_duplicated_part) break;
      if(num_Bs_in_duplicated_part<0) {
        ParallelLogger::logWarn(paste0('Something is not right - need to decrease ',traj[2],' events in control group to satisfy RR=',rr,' requirement but there are no ',traj[2],' events in the control group anyore'))
        break;
      }
      replaceEventWithRandomEventForPeson(connection,person_id=control_with_A,concept_id_to_replace=traj[2],exclude_concept_ids=traj,include_concept_ids=c())
      num_Bs_in_duplicated_part = num_Bs_in_duplicated_part - 1
    }
    ParallelLogger::logInfo(paste0('num_Bs_in_duplicated_part after eliminating extra B-s=',num_Bs_in_duplicated_part))


    #tests
    res1=querySql(connection,paste0('SELECT COUNT(DISTINCT person_id) AS total FROM CONDITION_OCCURRENCE WHERE condition_concept_id=',traj[1]))
    res2=querySql(connection,paste0('SELECT COUNT(DISTINCT person_id) AS total FROM CONDITION_OCCURRENCE WHERE condition_concept_id=',traj[2]))
    ParallelLogger::logInfo(paste0("Number of '",traj[1],"' events in data: ",res1$TOTAL))
    ParallelLogger::logInfo(paste0("Number of '",traj[2],"' events in data: ",res2$TOTAL))
  }



  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                        maximumDaysBetweenEvents = 365*120,
                                                                        minPatientsPerEventPair = ifelse(num_traj>=2,floor(num_traj/2),1), #use num_traj/2
                                                                        addConditions=T,
                                                                        addObservations=F,
                                                                        addProcedures=F,
                                                                        addDrugExposures=F,
                                                                        addDrugEras=F,
                                                                        addBirths=F,
                                                                        addDeaths=F,
                                                                        daysBeforeIndexDate=Inf,
                                                                        cohortName="test")

  return(list(connection=connection,trajectoryAnalysisArgs=trajectoryAnalysisArgs,trajectoryLocalArgs=trajectoryLocalArgs))
}


#creates synthecitc trajectory A->B where A always leads to B. Also adds B occurrences so that RR holds.
create_synthetic_trajectory_old2<-function(num_total_patients=100,num_traj=10,num_random_events=c(0,10),traj=c(317009,255848),rr=2) {
  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=num_total_patients)
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  all_person_ids<-getPatientIds(connection)

  # Add trajectory for x patients
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=traj,n=num_traj,days_to_skip_from_obs_period_start=0)
  non_case_person_ids<-setdiff(all_person_ids,person_ids)
  # For x/2 patients, add only the first event (so that the risk of getting the second event after the first is 50%)
  num_patients_without_second_event=num_traj
  non_case_person_ids_sample<-sample(non_case_person_ids,size=num_patients_without_second_event)
  addRandomEvents(connection,n_per_person_range=c(1,1), include_concept_ids=c(traj[1]), include_person_ids=non_case_person_ids_sample)
  person_ids<-c(person_ids,non_case_person_ids_sample)
  # Add up to x random events per each patient, except events in the trajectory
  addRandomEvents(connection,n_per_person_range=num_random_events,exclude_person_ids=c(),exclude_concept_ids=traj)
  # Add 1 second trajectory event for patients who do not have the trajectory
  # These are added so that the event prevalence among these patients corresponds to the expected RR value
  prob_a_b=num_traj/(num_traj+num_patients_without_second_event)
  non_case_person_ids<-setdiff(all_person_ids,person_ids)
  prob_b_without_prior_a<-prob_a_b/rr
  for(event in traj[2:length(traj)]) {
    non_case_person_ids_sample<-sample(non_case_person_ids,size=floor(prob_b_without_prior_a*length(non_case_person_ids)))
    addRandomEvents(connection,n_per_person_range=c(1,1), include_concept_ids=c(event), include_person_ids=non_case_person_ids_sample)
  }

  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                        maximumDaysBetweenEvents = 365*120,
                                                                        minPatientsPerEventPair = ifelse(num_traj>=10,floor(num_traj/2),1), #use num_traj/2
                                                                        addConditions=T,
                                                                        addObservations=F,
                                                                        addProcedures=F,
                                                                        addDrugExposures=F,
                                                                        addDrugEras=F,
                                                                        addBirths=F,
                                                                        addDeaths=F,
                                                                        daysBeforeIndexDate=Inf,
                                                                        cohortName="test")

  return(list(connection=connection,trajectoryAnalysisArgs=trajectoryAnalysisArgs,trajectoryLocalArgs=trajectoryLocalArgs))
}

create_synthetic_trajectory_old<-function(num_total_patients=100,num_traj=10,num_random_events=c(0,10),traj=c(317009,255848)) {
  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=num_total_patients)
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  # Add trajectory for x patients
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=traj,n=num_traj,days_to_skip_from_obs_period_start=0)
  # Add up to x random events per each patient, except events in the trajectory
  addRandomEvents(connection,n_per_person_range=num_random_events,exclude_person_ids=c(),exclude_concept_ids=traj)
  # Add 1 random trajectory event for patients who do not have the trajectory
  # These are added so that the event prevalence among these patients is not more than 0.1 x num_traj
  all_person_ids<-getPatientIds(connection)
  non_case_person_ids<-setdiff(all_person_ids,person_ids)
  for(event in traj) {
    non_case_person_ids_sample<-
      non_case_person_ids_sample<-sample(non_case_person_ids,size=floor(1/length(traj)))
    addRandomEvents(connection,n_per_person_range=c(1,1), include_concept_ids=c(event), include_person_ids=non_case_person_ids_sample)
  }

  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                        maximumDaysBetweenEvents = 365*120,
                                                                        minPatientsPerEventPair = ifelse(num_traj>=10,floor(num_traj/2),1), #use num_traj/2
                                                                        addConditions=T,
                                                                        addObservations=F,
                                                                        addProcedures=F,
                                                                        addDrugExposures=F,
                                                                        addDrugEras=F,
                                                                        addBirths=F,
                                                                        addDeaths=F,
                                                                        daysBeforeIndexDate=Inf,
                                                                        cohortName="test")

  return(connection)
}

run_analysis_for_synthetic_trajectory<-function(num_total_patients=100,num_traj=10,num_random_events=c(0,10),traj=c(317009,255848),rr=2) {

  r=create_synthetic_trajectory(num_total_patients=num_total_patients,num_traj=num_traj,num_random_events=num_random_events,traj=traj,rr=rr)

  connection=r$connection
  trajectoryAnalysisArgs=r$trajectoryAnalysisArgs
  trajectoryLocalArgs=r$trajectoryLocalArgs


  #Create output folder for this analysis
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

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

  Trajectories:::runDiscoveryAnalysis(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)
  return(list(connection=connection,trajectoryAnalysisArgs=trajectoryAnalysisArgs,trajectoryLocalArgs=trajectoryLocalArgs))
}


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



num_total_patients=1000
num_traj=100
num_random_events=c(0,20)
rr=2

is_trajectory_detected<-function(num_total_patients=num_total_patients,num_traj=num_traj,num_random_events=num_random_events,rr=rr) {

  traj=c(317009,255848)
  r=run_analysis_for_synthetic_trajectory(num_total_patients=num_total_patients,num_traj=num_traj,num_random_events=num_random_events,traj=traj,rr=rr)
  trajectoryAnalysisArgs=r$trajectoryAnalysisArgs
  trajectoryLocalArgs=r$trajectoryLocalArgs

  #get number of directional pairs
  num_directional_pairs<-nrow(getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_directional.tsv'))

  #is our trajectory among these?
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')


  if(nrow(row)!=1) return(list(is_detected=F,detected_rr=row$RR,num_directional_pairs=num_directional_pairs));
  if(is.na(row$DIRECTIONAL_SIGNIFICANT)) return(list(is_detected=F,detected_rr=row$RR,num_directional_pairs=num_directional_pairs));
  if(row$DIRECTIONAL_SIGNIFICANT=='*') return(list(is_detected=T,detected_rr=row$RR,num_directional_pairs=num_directional_pairs));
  return(list(is_detected=F,detected_rr=row$RR,num_directional_pairs=num_directional_pairs));
}

is_trajectory_detected_from_no_trajectory<-function(num_total_patients=num_total_patients,num_traj=num_traj,num_random_events=num_random_events,rr=rr) {

  traj=c()
  r=run_analysis_for_synthetic_trajectory(num_total_patients=num_total_patients,num_traj=num_traj,num_random_events=num_random_events,traj=traj,rr=rr)
  trajectoryAnalysisArgs=r$trajectoryAnalysisArgs
  trajectoryLocalArgs=r$trajectoryLocalArgs

  #get number of directional pairs
  num_directional_pairs<-nrow(getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_directional.tsv'))

  return(list(is_detected=F,detected_rr=NA,num_directional_pairs=num_directional_pairs));
}


library(ggplot2)
res<-data.frame()
for(num_total_patients in c(1000)) {
  for(rr in c(5,2,1.5,1.2,1)) {
    for(num_traj in c(100,50,20,10)) {

        #for(num_random_events in c(1,6,12,17,30)) { #1=general prevalence is 2%, 6=10%,12 =20%, 17=30%, 30=50% (if the number of different random events is 30) }
        for(num_random_events in c(1,6,12,17,30)) { #1=general prevalence is 2%, 6=10%,12 =20%, 17=30%, 30=50% (if the number of different random events is 30) }
          if(num_traj>0) {
            r<-is_trajectory_detected(num_total_patients=num_total_patients,num_traj=num_traj,num_random_events=c(0,num_random_events),rr=rr)
          } else if (rr=1) {
            r<-is_trajectory_detected_from_no_trajectory(num_total_patients=num_total_patients,num_traj=num_traj,num_random_events=c(0,num_random_events),rr=rr)
          }

          d<-r$is_detected

          res<-rbind(res,data.frame(num_total_patients=num_total_patients,num_event_pair_occurrences=num_traj,num_random_events=num_random_events,test_rr=rr,detected_rr=r$detected_rr,num_different_directional_pairs_detected=r$num_directional_pairs,was_added_pair_detected=d))
          save(res,file='test2.Robj')

          if(d==F) break;
        }
    }

  }
  #add no trajectory test
  for(num_random_events in c(1,6,12,17,30)) { #1=general prevalence is 2%, 6=10%,12 =20%, 17=30%, 30=50% (if the number of different random events is 30) }'
    r<-is_trajectory_detected_from_no_trajectory(num_total_patients=num_total_patients,num_traj=num_traj,num_random_events=c(0,num_random_events),rr=1)
    d<-r$is_detected

    res<-rbind(res,data.frame(num_total_patients=num_total_patients,num_traj=num_traj,num_random_events=num_random_events,test_rr=1,detected_rr=r$detected_rr,num_directional_pairs=num_directional_pairs,is_detected=d))

  }
}

#res_rr_2<-res

res <- res %>% select(-prevalence_random_events) %>% select(-prevalence)

res$prevalence<-round(100*res$num_traj/res$num_total_patients)
res <- res %>%
        mutate(prevalence_random_events=case_when(
          num_random_events == 1 ~ 2,
          num_random_events == 6 ~ 10,
          num_random_events == 12 ~ 20,
          num_random_events == 17 ~ 30,
          num_random_events == 30 ~ 50,
          TRUE ~ num_random_events
        ))
library(ggplot2)
for(rr in c(5,2,1.5,1.2,1)) {
  ggplot(res %>% filter(test_rr==rr),
         aes(x=prevalence,y=prevalence_random_events,color=is_detected)) + geom_point() +
    xlab('Prevalence of the trajectory in the cohort, %') +
    ylab('Avegrage prevalence of random events, %') +
    ggtitle(paste0('Test results for RR=',rr)) +
    theme_bw()
  ggsave(filename=paste0('rr',rr,'.pdf'))
}
rr=4
ggplot(res %>% filter(test_rr==rr),
       aes(x=prevalence,y=prevalence_random_events,color=is_detected)) + geom_point() +
  xlab('Prevalence of the trajectory in the cohort, %') +
  ylab('Avegrage prevalence of random events, %') +
  ggtitle(paste0('Test results for RR=',rr)) +
  theme_bw()

head(res)
save(res,file="res.R")
openxlsx::write.xlsx(res,'testresults.xlsx')
load('test.Robj')
load('rr2.R')




