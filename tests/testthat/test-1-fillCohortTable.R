context("Filling in cohor table")


testthat::test_that("Filling in cohort table with fulldb buit-in study", {

  eunomia <-setUpEunomia() #also fills in trajectoryLocalArgs
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=100) #in analysis, use 100 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848),n=20) #The only data is 20x asthma->pneumonia pair

  trajectoryAnalysisArgs <- Trajectories::createTrajectoryAnalysisArgs(mode="DISCOVERY",
                                                                       minimumDaysBetweenEvents = 1,
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
                                                                       RRrangeToSkip=c(0.9,1.1))


  #Create output folder for this analysis
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  Trajectories::InitLogger(logfile = file.path(outputFolder,'log.txt'), threshold = logger:::DEBUG)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)



  #First check: is cohort table filled in correctly (should be 100 rows)
  res<-querySql(connection, paste0('SELECT COUNT(*) as TOTAL FROM test_cohort where cohort_definition_id=1;'))
  testthat::expect_equal(res$TOTAL, 100)



})


testthat::test_that("Spliting cohort to DISCOVERY and VALIDATION set", {
  eunomia <-setUpEunomia() #also fills in trajectoryLocalArgs
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=40) #in analysis, use 100 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848),n=20) #The only data is 20x asthma->pneumonia pair


  trajectoryAnalysisArgs <- Trajectories::createTrajectoryAnalysisArgs(mode="DISCOVERY",
                                                                       minimumDaysBetweenEvents = 1,
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
                                                                       RRrangeToSkip=c(0.9,1.1))


  #Create output folder for this analysis
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  Trajectories::InitLogger(logfile = file.path(outputFolder,'log.txt'), threshold = logger:::DEBUG)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)



  # Assign 50% of the event-periods from the cohort to validation set (discovery set=data in cohort table where cohort_id=1; validation set=data in cohort table where cohort_id=2)
  Trajectories::createValidationSet(connection=connection,
                                    trajectoryAnalysisArgs,
                                    trajectoryLocalArgs,
                                    size=0.25)

  #First check: is cohort table filled in correctly (should be 100 rows)
  res<-querySql(connection, paste0('SELECT COUNT(*) as TOTAL FROM test_cohort where cohort_definition_id=1;'))
  testthat::expect_equal(res$TOTAL, 30)

  res<-querySql(connection, paste0('SELECT COUNT(*) as TOTAL FROM test_cohort where cohort_definition_id=2;'))
  testthat::expect_equal(res$TOTAL, 10)


})

