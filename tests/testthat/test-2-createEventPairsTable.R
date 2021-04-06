context("Creating event pair tables")

library(Trajectories)

testthat::test_that("Filling in cohort table with fulldb buit-in study (no events)", {


  eunomia <-setUpEunomia() #also fills in trajectoryLocalArgs
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  clearConditionsTable(connection)
  limitToNumPatients(connection,n=100) #in analysis, use 100 patients
  limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')

  trajectoryAnalysisArgs <- createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
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
  outputFolder<-GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)



  createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)



  # There are no events in conditions table, all summary tables should be empty
  res<-querySql(connection, glue::glue('SELECT COUNT(*) AS TOTAL FROM {trajectoryLocalArgs$resultsSchema}.{trajectoryLocalArgs$prefixForResultTableNames}E2_summary'))
  testthat::expect_equal(res$TOTAL, 0)



})

testthat::test_that("Filling in cohort table with fulldb buit-in study (there are some events)", {


  eunomia <-setUpEunomia() #also fills in trajectoryLocalArgs
  connection<-eunomia$connection
  trajectoryLocalArgs<-eunomia$trajectoryLocalArgs
  #clearConditionsTable(connection)
  #limitToNumPatients(connection,n=100) #in analysis, use 100 patients
  #limitToConcepts(connection)
  setObservationPeriodForAll(connection,startdate='2010-01-01',enddate='2012-12-31')

  trajectoryAnalysisArgs <- createTrajectoryAnalysisArgs(minimumDaysBetweenEvents = 1,
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
  outputFolder<-GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  Trajectories::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)



  createEventPairsTable(connection=connection,
                        trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                        trajectoryLocalArgs=trajectoryLocalArgs)



  # Get number of rows from E1E2_model table (should be >10 rows)
  res<-querySql(connection, glue::glue('SELECT COUNT(*) AS TOTAL FROM {trajectoryLocalArgs$resultsSchema}.{trajectoryLocalArgs$prefixForResultTableNames}E1E2_model'))
  testthat::expect_gt(res$TOTAL, 10) #in Sulev's test there was 31 rows



})

