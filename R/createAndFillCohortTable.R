if(!require(drat)){
  install.packages("drat")
  library(drat)
}

drat::addRepo("OHDSI")
if(!require(CohortGenerator)){
  install.packages("CohortGenerator")
  library(CohortGenerator)
}

if(!require(CirceR)){
  install.packages("CirceR")
  library(CirceR)
}

#' Creates and fills cohort table that is going to be used by the Trajectories package
#'
#' @inheritParams GetOutputFolder
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#'
#' @return
#'
#' @examples
createAndFillCohortTable<-function(connection,
                          trajectoryAnalysisArgs,
                          trajectoryLocalArgs) {

  # First construct a cohort definition set: an empty
  # data frame with the cohorts to generate
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
  cohortJsonFileName <- "cohort.json"
  cohortJsonFilePath <- file.path(trajectoryLocalArgs$inputFolder, cohortJsonFileName)
  cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))

  # Here we read in the JSON in order to create the SQL
  # using [CirceR](https://ohdsi.github.io/CirceR/)
  # If you have your JSON and SQL stored differenly, you can
  # modify this to read your JSON/SQL files however you require

  cohortJson <- readChar(cohortJsonFilePath, file.info(cohortJsonFilePath)$size)
  cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
  cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = TRUE))
  cohortsToCreate <- rbind(cohortsToCreate, data.frame(cohortId = 1,
                                                       cohortName = "Hypertension",
                                                       sql = cohortSql,
                                                       stringsAsFactors = FALSE))

  # Create the cohort tables to hold the cohort generation results
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "Hypertension")
  CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                      cohortDatabaseSchema = "main",
                                      cohortTableNames = cohortTableNames)

  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                                         cdmDatabaseSchema = "main",
                                                         tempEmulationSchema = "temp_schema",
                                                         cohortDatabaseSchema = "main",
                                                         cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "Hypertension"),
                                                         cohortDefinitionSet = cohortsToCreate)

  # Get the cohort counts
  cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                                   cohortDatabaseSchema = "main",
                                                   cohortTable = cohortTableNames$cohortTable)
  ParallelLogger::logInfo('There are ', cohortCounts, ' rows in this cohort in the cohort table.')
}
