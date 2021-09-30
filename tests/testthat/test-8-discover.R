context("Testing discover()")
library(Trajectories)
library(DatabaseConnector)
library(SqlRender)


testthat::test_that("Test that discover() runs fully without any errors", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection

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
