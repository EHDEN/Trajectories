context("Testing runEventPairAnalysis")
library(Trajectories)
library(DatabaseConnector)
library(stringr)
library(SqlRender)


testthat::test_that("Test alignments to graph", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=1000) #in analysis, use 500 patients. Big enough number is needed for having sufficient amount of events on same disharge_date
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2011-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848,4299128),n=500) # Add Asthma->Pneumonia->Third degree burn trajectory for 100 patients
  addRandomEvents(connection,n_per_person_range=c(0,2), exclude_concept_ids=c(317009,255848,4299128)) # Add up to 10 random events per each patient, except events 317009, 255848, 4299128
  #in order to calc RR, some 255848 and 4299128 events need to be added to some (20% fraction) non-trajectory patients also (add max 1 to each patient to avoid trajectory occurrence)
  all_person_ids<-getPatientIds(connection)
  nontraj_person_ids<-setdiff(all_person_ids,person_ids)
  nontraj_person_ids_sample<-sample(nontraj_person_ids,size=floor(length(nontraj_person_ids)/2))
  addRandomEvents(connection,n_per_person_range=c(0,1), include_concept_ids=c(317009,255848,4299128), include_person_ids=nontraj_person_ids_sample)



  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
                                                                       maximumDaysBetweenEvents = 365*120,
                                                                       minPatientsPerEventPair = 499,
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

  # Set up logger
  Trajectories:::InitLogger(logfile = file.path(outputFolder,'log.txt'), threshold = 'INFO' )

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)
  removeTrajectoryFile(trajectoryLocalArgs,trajectoryAnalysisArgs)

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



  Trajectories:::countTrajectories(connection,
                      trajectoryAnalysisArgs,
                      trajectoryLocalArgs)

  row<-getTrajectoryFromTrajectoryFile(trajectoryLocalArgs,trajectoryAnalysisArgs,trajectory_concept_ids=c(317009,255848,4299128))
  #Test that the trajectory comes out from the trajectory file
  testthat::expect_equal(nrow(row),1)
  #Test that the trajectory count is 500
  testthat::expect_equal(row$trajectory.count,500)
  testthat::expect_equal(row$length,3)

})


testthat::test_that("Test that if A->B sometimes happen on the same day, it does not break counting of the trajectories", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=1000) #in analysis, use 500 patients. Big enough number is needed for having sufficient amount of events on same disharge_date
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2010-01-01')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848),n=100) # Add Asthma->Pneumonia trajectory for 100 patients on a same day
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-01-01')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848),n=500) # Add Asthma->Pneumonia trajectory for 10 patients on different days
  addRandomEvents(connection,n_per_person_range=c(0,2), exclude_concept_ids=c(317009,255848,4299128)) # Add up to 10 random events per each patient, except events 317009, 255848, 4299128
  #in order to calc RR, some 255848 and 4299128 events need to be added to some (20% fraction) non-trajectory patients also (add max 1 to each patient to avoid trajectory occurrence)
  all_person_ids<-getPatientIds(connection)
  nontraj_person_ids<-setdiff(all_person_ids,person_ids)
  nontraj_person_ids_sample<-sample(nontraj_person_ids,size=floor(length(nontraj_person_ids)/2))
  addRandomEvents(connection,n_per_person_range=c(0,1), include_concept_ids=c(317009,255848,4299128), include_person_ids=nontraj_person_ids_sample)



  trajectoryAnalysisArgs <- Trajectories:::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 0,
                                                                        maximumDaysBetweenEvents = 365*120,
                                                                        minPatientsPerEventPair = 499,
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

  # Set up logger
  Trajectories:::InitLogger(logfile = file.path(outputFolder,'log.txt'), threshold = 'INFO' )

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)
  removeTrajectoryFile(trajectoryLocalArgs,trajectoryAnalysisArgs)

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



  Trajectories:::countTrajectories(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs)

  row<-getTrajectoryFromTrajectoryFile(trajectoryLocalArgs,trajectoryAnalysisArgs,trajectory_concept_ids=c(317009,255848))
  #Test that the trajectory comes out from the trajectory file
  testthat::expect_equal(nrow(row),1)
  #Test that the trajectory count is 500
  testthat::expect_equal(row$trajectory.count,500)
  testthat::expect_equal(row$length,2)

  row<-getTrajectoryFromTrajectoryFile(trajectoryLocalArgs,trajectoryAnalysisArgs,trajectory_concept_ids=c(255848,317009))

})
