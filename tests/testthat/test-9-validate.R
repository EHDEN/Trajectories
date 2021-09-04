context("Testing validate()")
library(Trajectories)
library(DatabaseConnector)
library(SqlRender)


testthat::test_that("Test that validate() runs fully without any errors", {

  eunomia <-setUpEunomia()
  connection<-eunomia$connection

  trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema = "temp_schema",
                                                                 prefixForResultTableNames = "",
                                                                 cdmDatabaseSchema = 'main',
                                                                 vocabDatabaseSchema = 'main',
                                                                 resultsSchema = 'main',
                                                                 sqlRole = F,
                                                                 inputFolder=system.file("extdata", "selftest-validate", package = "Trajectories"), # Full path to input folder that contains SQL file for cohort definition and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "T2D", package = "Trajectories")
                                                                 mainOutputFolder=getwd(), #Working directory
                                                                 databaseHumanReadableName='Eunomia')


  # ##################################################
  # RUN DISCOVERY ANALYSIS
  # ##################################################

  Trajectories::validate(connection,
                         trajectoryLocalArgs,
                         createCohort=T,
                         createEventPairsTable=T,
                         runValidationAnalysis=T,
                         createFilteredFullgraphs=T,
                         createGraphsForSelectedEvents = T,
                         cleanup=T)


  #Nothing to test here. We just wanted to make sure that running validate() does not produce any errors

})
