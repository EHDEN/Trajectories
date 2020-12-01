context("Testing runEventPairAnalysis")
library(Trajectories)
library(DatabaseConnector)
library(stringr)
library(SqlRender)

#querySql(connection, paste0('SELECT COUNT(*) FROM PERSON;'))
#querySql(connection, paste0('SELECT COUNT(*) FROM OBSERVATION_PERIOD;'))
#querySql(connection, paste0('SELECT * FROM OBSERVATION_PERIOD;'))
#querySql(connection, paste0('SELECT COUNT(*) FROM CONDITION_OCCURRENCE;'))
#querySql(connection, paste0('SELECT * FROM CONDITION_OCCURRENCE LIMIT 30;'))
#querySql(connection, paste0('SELECT * FROM E1E2_MODEL;'))


test_that("No significant event pairs in random data", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=100) #in analysis, use 1000 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  #person_ids<-addConditionEventPair(connection,event1_concept_id=317009,event2_concept_id=255848,n=20) # Add asthma->pneumonia pair for 20 patients
  addRandomEvents(connection,n_per_person_range=c(0,10)) # Add up to 10 random events per each patient



  # Setting database parameters:
  #library(stringi)


  trajectoryAnalysisArgs <- Trajectories::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
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
                                                                       packageName='Trajectories',
                                                                       cohortName="test")


  #Create output folder for this analysis
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)
  print(paste0('Test that output folder ',outputFolder,' exists: ',dir.exists(outputFolder)))
  expect_equal(dir.exists(outputFolder), TRUE)

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # Fill cohort table with example cohort data
  Trajectories::fillCohortTable(connection=connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)

  Trajectories::runEventPairAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs)


  #print(paste0('Test that output folder ',outputFolder,' exists: ',dir.exists(outputFolder)))
  #expect_equal(dir.exists(outputFolder), TRUE)
  #print('Files in directory:')
  #list.files(outputFolder)

  #test that no output file (event_pairs.csv) was created
  eventPairResultsFilename = file.path(outputFolder,'event_pairs.tsv')
  print(paste0('Test that ',eventPairResultsFilename,' exists...'))
  expect_equal(file.exists(eventPairResultsFilename), FALSE)

})

test_that("Test ability to detect a synthetic event pair in data", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=100) #in analysis, use 1000 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  person_ids<-addConditionEventPair(connection,event1_concept_id=317009,event2_concept_id=255848,n=20) # Add asthma->pneumonia pair for 20 patients
  addRandomEvents(connection,n_per_person_range=c(0,10),exclude_concept_ids=c(317009,255848)) # Add up to 10 random events per each patient, except events 317009 and 255848


  trajectoryAnalysisArgs <- Trajectories::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
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
                                                                       packageName='Trajectories',
                                                                       cohortName="test")


  #Create output folder for this analysis
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # Fill cohort table with example cohort data
  Trajectories::fillCohortTable(connection=connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)
  Trajectories::runEventPairAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs)

  #test that event pair 317009->255848 is among tested pairs


  #test that there is at least 1 event pairs tested
  tested_event_pairs<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  expect_gt(nrow(tested_event_pairs),0)
  #test that event pair 317009->255848 is among them
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs_tested.tsv')
  expect_equal(row$E1_COUNT, 20)
  expect_equal(row$E2_COUNT, 20)
  expect_equal(row$E1_E2_EVENTPERIOD_COUNT, 20)
  expect_equal(row$EVENTPERIOD_COUNT_E1_OCCURS_FIRST, 20)

  #test that event pair 317009->255848 is found significant and the counts are correct
  row<-getEventPairFromEventPairsTable(event1_concept_id=317009,event2_concept_id=255848,trajectoryLocalArgs,trajectoryAnalysisArgs)
  expect_equal(row$E1_COUNT, 20)
  expect_equal(row$E2_COUNT, 20)
  expect_equal(row$E1_E2_EVENTPERIOD_COUNT, 20)
  expect_equal(row$EVENTPERIOD_COUNT_E1_OCCURS_FIRST, 20)

  #test that the names are correct
  expect_equal(row$E1_NAME, 'Asthma')
  expect_equal(row$E1_DOMAIN, 'Condition')
  expect_equal(row$E2_NAME, 'Pneumonia')
  expect_equal(row$E2_DOMAIN, 'Condition')


})

