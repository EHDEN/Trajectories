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
  limitToNumPatients(connection,n=500) #in analysis, use 500 patients. Big enough number is needed for having sufficient amount of events on same disharge_date
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2011-12-31')
  person_ids<-addConditionEventTrajectory(connection,event_concept_ids=c(317009,255848,4299128),n=20) # Add asthma->pneumonia pair for 20 patients
  addRandomEvents(connection,n_per_person_range=c(0,10),exclude_concept_ids=c(317009,255848,4299128)) # Add up to 10 random events per each patient, except events 317009 and 255848


  trajectoryAnalysisArgs <- Trajectories::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
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
                                                                       cohortName="test",
                                                                       RRrangeToSkip=c(0,1))


  #Create output folder for this analysis
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  # Set up logger
  Trajectories::InitLogger(logfile = file.path(outputFolder,'log.txt'), threshold = logger:::INFO)

  #Remove output files (if exist from previous run)
  removeTestableOutputFiles(trajectoryLocalArgs,trajectoryAnalysisArgs)
  removeTrajectoryFile(trajectoryLocalArgs,trajectoryAnalysisArgs,concept_id=317009,concept_name='Asthma')

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)

  # Create database tables of all event pairs (patient level data + summary statistics)
  Trajectories::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)

  Trajectories::runDiscoveryAnalysis(connection=connection,
                                     trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                     trajectoryLocalArgs=trajectoryLocalArgs)

  # Draw plots for specific events (uses database connection and result tables in the database for trajectory alignments)
  Trajectories::PlotTrajectoriesGraphForEvents(connection,
                                               trajectoryAnalysisArgs,
                                               trajectoryLocalArgs,
                                               eventIds=c(317009),
                                               skipOutputTables = F)


  row<-getTrajectoryFromTrajectoryFile(trajectoryLocalArgs,trajectoryAnalysisArgs,concept_id=317009,concept_name='Asthma',trajectory_concept_ids=c(317009,255848,4299128))
  #Test that the trajectory comes out from the trajectory file
  testthat::expect_equal(nrow(row),1)
  #Test that the trajectory count is 20
  testthat::expect_equal(row$exact_count,20)
  testthat::expect_equal(row$total_count,20)

})
