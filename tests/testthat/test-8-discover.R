context("Testing discover()")
library(Trajectories)
library(DatabaseConnector)
library(SqlRender)


testthat::test_that("Test that discover() runs fully without any errors", {

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



  trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema = "temp_schema",
                                                                 prefixForResultTableNames = "",
                                                                 cdmDatabaseSchema = 'main',
                                                                 vocabDatabaseSchema = 'main',
                                                                 resultsSchema = 'main',
                                                                 sqlRole = F,
                                                                 inputFolder=system.file("extdata", "selftest-discover", package = "Trajectories"), # Full path to input folder that contains SQL file for cohort definition and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "T2D", package = "Trajectories")
                                                                 mainOutputFolder=getwd(), #Working directory
                                                                 databaseHumanReadableName='Eunomia')


  # ##################################################
  # RUN DISCOVERY ANALYSIS
  # ##################################################

  Trajectories::discover(connection,
                         trajectoryLocalArgs,
                         createCohort=T,
                         validationSetSize=0.5,
                         createEventPairsTable=T,
                         runDiscoveryAnalysis=T,
                         createFilteredFullgraphs=T,
                         runTrajectoryAnalysis = T,
                         selfValidate=T,
                         cleanup=T)


  #Nothing to test here. We just wanted to make sure that running discover() does not produce any errors

})
