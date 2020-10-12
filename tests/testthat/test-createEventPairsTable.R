context("Creating event pair tables")

library(Trajectories)

test_that("Filling in cohort table with fulldb buit-in study", {


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
                                                                       packageName='Trajectories',
                                                                       cohortName="test")


  #Create output folder for this analysis
  outputFolder<-GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  createCohortTable(connection,
                    trajectoryAnalysisArgs,
                    trajectoryLocalArgs)

  # Fill cohort table with example cohort data
  fillCohortTable(connection=connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs)


  createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)



  # There are no events in conditions table, all summary tables should be empty
  res<-querySql(connection, glue::glue('SELECT COUNT(*) AS TOTAL FROM {trajectoryLocalArgs$resultsSchema}.{trajectoryLocalArgs$prefixForResultTableNames}d2_summary'))
  expect_equal(res$TOTAL, 0)



})
