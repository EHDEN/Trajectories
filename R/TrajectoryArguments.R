
#' Creates an object to hold analysis-specific data
#'
#' @param minimumDaysBetweenEvents The smallest number of days between 2 events of the patient that can be considered as event pair. Usually we have used 1.
#' @param maximumDaysBetweenEvents The maximum number of days between 2 events of the patient that can be considered as event pair. Ususally we have not really limited it so we have used 3650 (10 years)
#' @param minPatientsPerEventPair Minimum number of people having event1 -> event2 progression to be included in analysis. Can be used for limiting analysis to frequent event pairs only. However, it does not throw less frequent diagnosis pairs out of the (control group) data and therefore, does not affect the statistical significance.
#' @param addConditions TRUE/FALSE parameter to indicate whether events from Condition_occurrence table should be included in the analysis
#' @param addObservations TRUE/FALSE parameter to indicate whether events from Condition_occurrence table should be included in the analysis
#' @param addProcedures TRUE/FALSE parameter to indicate whether events from Procedure_occurrence table should be included in the analysis
#' @param addDrugExposures TRUE/FALSE parameter to indicate whether events from Drug_exposure table should be included in the analysis
#' @param addDrugEras TRUE/FALSE parameter to indicate whether events from Drug_era table should be included in the analysis. NB! use either addDrugEras=T or addDrugExposures=T (not both) as it leads to analysis duplication...
#' @param addBirths TRUE/FALSE parameter to indicate whether births events should be included in the analysis.
#' @param addDeaths TRUE/FALSE parameter to indicate whether events from Death table should be included in the analysis.
#' @param daysBeforeIndexDate 0 or any positive number that indicates for how many days before index date of the cohort the events are included in the analysis. In case one wants to include all events before index date, use value Inf
#' @param packageName Do not use/edit, this is required by SqlRender::loadRenderTranslateSql
#' @param cohortName Reader-friendly short description of the cohort. Used in graph titles and file names (can contain spaces)
#' @param description This is a placeholder for any description of the study/cohort/analysis. For instance, it would be wise to descibe here what kind of cohort is that and what the analysis does.
#'
#' @return
#' @export
#'
#' @examples
createTrajectoryAnalysisArgs <- function(minimumDaysBetweenEvents,
                                         maximumDaysBetweenEvents,
                                         minPatientsPerEventPair,
                                         addConditions,
                                         addObservations,
                                         addProcedures,
                                         addDrugExposures,
                                         addDrugEras,
                                         addBirths,
                                         addDeaths,
                                         daysBeforeIndexDate,
                                         packageName = 'Trajectories',
                                         cohortName,
                                         description = '') {


  if(addDrugExposures==T & addDrugEras==T) stop("Error in createTrajectoryAnalysisArgs(): parameters values for 'addDrugExposures' and 'addDrugEras' are TRUE but both of them cannot be TRUE at the same time (choose one of them or set both to FALSE)")

  value <- list(minimumDaysBetweenEvents=minimumDaysBetweenEvents,maximumDaysBetweenEvents=maximumDaysBetweenEvents, minPatientsPerEventPair=minPatientsPerEventPair,
                addConditions=addConditions,addObservations=addObservations,addProcedures=addProcedures,addDrugExposures=addDrugExposures,
                addDrugEras=addDrugEras,addBirths=addBirths,addDeaths=addDeaths,
                daysBeforeIndexDate=daysBeforeIndexDate,packageName=packageName,cohortName=cohortName,description=description)
  class(value) <- 'TrajectoryAnalysisArgs'
  return(value)

}

#' Creates an object to hold local database-specific parameters
#'
#' @param cdmDatabaseSchema Schema containing source data in OMOP CDM format
#' @param vocabDatabaseSchema Schema containing OMOP vocabulary
#' @param resultsSchema Schema the user has writing access to (used to write analysis tables into)
#' @param oracleTempSchema In case you are using oracle, schema for temporary tables need to be specified. A schema where temp tables can be created in Oracle. Otherwise leave it as it is (is not used)
#' @param sqlRole Role to use in SQL for writing tables in 'resultsSchema'. It should also have access to 'cdmDatabaseSchema' and 'vocabDatabaseSchema'. Set to FALSE (or F) if setting to a specific role is not needed. It should be safe to use F if you have no idea of what the SQL roles mean.
#' @param prefixForResultTableNames Table prefix that is used for all output tables to avoid any collision with existing table names. An empty string is also allowed.
#' @param cohortTableSchema Schema where cohort table is located
#' @param cohortTable Name of the cohort table in cohortTableSchema
#' @param cohortId ID of the cohort (in cohortTable) that will be used for the analysis (default value is 1)
#' @param inputFolder Full path to input folder that contains SQL file for cohort definition and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "RA", package = "Trajectories") which is also the default value
#' @param mainOutputFolder The output folder path. This is the folder where the final results are produced into. Use full path and do NOT add trailing slash! The folder must already exist. Default value is the default working directory.
#' @param databaseHumanReadableName In the future, it will be added to the titles of the graph to indicate what data is this. Use something short. Currently this parameter is not used.
#'
#' @return
#' @export
#'
#' @examples
createTrajectoryLocalArgs <- function(cdmDatabaseSchema,
                                      vocabDatabaseSchema,
                                      resultsSchema,
                                      oracleTempSchema,
                                      sqlRole=F,
                                      prefixForResultTableNames='',
                                      cohortTableSchema,
                                      cohortTable,
                                      cohortId=1,
                                      inputFolder=system.file("extdata", "RA", package = "Trajectories"),
                                      mainOutputFolder=getwd(),
                                      databaseHumanReadableName='My database') {

  #Sanity checks
  if (!dir.exists(inputFolder)) stop(paste0("ERROR in createTrajectoryLocalArgs(): inputFolder '",inputFolder,"' does not exist."))
  if (!dir.exists(mainOutputFolder)) stop(paste0("ERROR in createTrajectoryLocalArgs(): mainOutputFolder '",mainOutputFolder,"' does not exist."))
  if (!file.exists(file.path(inputFolder,'cohort.sql'))) stop(paste0("ERROR in createTrajectoryLocalArgs(): there is no 'cohort.sql' file in inputFolder '",inputFolder,"'."))
  #if (!file.exists(paste0(inputFolder,'/trajectoryAnalysisArgs.json'))) stop(paste0("ERROR in createTrajectoryLocalArgs(): there is no 'trajectoryAnalysisArgs.json' file in inputFolder '",inputFolder,"'."))

  value <- list(cdmDatabaseSchema=cdmDatabaseSchema,vocabDatabaseSchema=vocabDatabaseSchema,
                resultsSchema=resultsSchema,oracleTempSchema=oracleTempSchema,sqlRole=sqlRole,
                prefixForResultTableNames=prefixForResultTableNames, cohortTableSchema=cohortTableSchema,
                cohortTable=cohortTable,cohortId=cohortId,
                inputFolder=inputFolder,
                mainOutputFolder=mainOutputFolder, databaseHumanReadableName=databaseHumanReadableName)
  class(value) <- 'TrajectoryLocalArgs'
  return(value)

}

#' Checks whether the object is of type TrajectoryLocalArgs
#'
#' @param x Any R object
#'
#' @return
#' @export
#'
#' @examples
is.TrajectoryLocalArgs <- function(x) {
  inherits(x, "TrajectoryLocalArgs")
}

#' Checks whether the object is of type TrajectoryAnalysisArgs
#'
#' @param x Any R object
#'
#' @return
#' @export
#'
#' @examples
is.TrajectoryAnalysisArgs <- function(x) {
  inherits(x, "TrajectoryAnalysisArgs")
}

#' Writes trajectoryAnalysisArgs object to JSON file
#'
#' @param trajectoryAnalysisArgs Object created by Trajectories::createTrajectoryAnalysisArgs() method
#' @param filepath Full path to the output file. Should have .json extension as this is actually a JSON file.
#'
#' @return
#' @export
#'
#' @examples
TrajectoryAnalysisArgsToJson<-function(trajectoryAnalysisArgs, filepath) {
  print(paste0("Saving 'trajectoryAnalysisArgs' data in JSON format to ",filepath,"..."))

  if(!Trajectories::is.TrajectoryAnalysisArgs(trajectoryAnalysisArgs)) stop("Something is not right. 'trajectoryAnalysisArgs' is not an object from class 'TrajectoryAnalysisArgs'")

  library(jsonlite)
  json<-toJSON(trajectoryAnalysisArgs, force=T, pretty=T)

  fileConn<-file(filepath)
  writeLines(json, fileConn)
  close(fileConn)

  print("...done.")
}

#' Reads trajectoryAnalysisArgs object from JSON file
#'
#' @param filepath Full path to JSON file
#'
#' @return
#' @export
#'
#' @examples
TrajectoryAnalysisArgsFromJson<-function(filepath) {
  library(jsonlite)

  print(paste0("Loading 'trajectoryAnalysisArgs' object from JSON file ",filepath,"..."))
  r.obj<-fromJSON(filepath)
  trajectoryAnalysisArgs<-Trajectories::createTrajectoryAnalysisArgs(minimumDaysBetweenEvents=r.obj$minimumDaysBetweenEvents,
                               maximumDaysBetweenEvents=r.obj$maximumDaysBetweenEvents,
                               minPatientsPerEventPair=r.obj$minPatientsPerEventPair,
                               addConditions=r.obj$addConditions,
                               addObservations=r.obj$addObservations,
                               addProcedures=r.obj$addProcedures,
                               addDrugExposures=r.obj$addDrugExposures,
                               addDrugEras=r.obj$addDrugEras,
                               addBirths=r.obj$addBirths,
                               addDeaths=r.obj$addDeaths,
                               daysBeforeIndexDate=r.obj$daysBeforeIndexDate,
                               packageName=r.obj$packageName,
                               cohortName=r.obj$cohortName,
                               description=r.obj$description)
  print('...done.')
  return(trajectoryAnalysisArgs)
}

#' Searches for trajectoryAnalysisArgs.json file from inputFolder (defined in trajectoryLocalArgs), creates trajectoryAnalysisArgs object from it and returns it.
#'
#' @param trajectoryLocalArgs Object created by Trajectories::createTrajectoryLocalArgs() method
#'
#' @return TrajectoryLocalArgs object
#' @export
#'
#' @examples
TrajectoryAnalysisArgsFromInputFolder<-function(trajectoryLocalArgs) {
  trajectoryAnalysisArgs<-Trajectories::TrajectoryAnalysisArgsFromJson(file.path(trajectoryLocalArgs$inputFolder,"trajectoryAnalysisArgs.json"))
  return(trajectoryAnalysisArgs)
}

#' Returns full path to output folder for the results. If createIfMissing=T, then creates the folder by combining mainOutputFolder, database name and analysis name (requires that mainOutputFolder already exists)
#'
#' @param trajectoryLocalArgs  Object created by Trajectories::createTrajectoryLocalArgs() method
#' @param trajectoryAnalysisArgs  Object created by Trajectories::createTrajectoryAnalysisArgs() method
#' @param createIfMissing If TRUE, then creates necessary folder if missing.
#'
#' @return Full output path
#' @export
#'
#' @examples
GetOutputFolder<-function(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F) {

  outputFolder<-trajectoryLocalArgs$mainOutputFolder
  subFolder1=make.names(trajectoryLocalArgs$databaseHumanReadableName)
  subFolder2=make.names(trajectoryAnalysisArgs$cohortName)

  if (!dir.exists(outputFolder)) stop(paste0("ERROR in GetOutputFolder(): trajectoryLocalArgs$mainOutputFolder='",mainOutputFolder,"' does not exist."))

  outputFolderPrev=outputFolder
  outputFolder <- file.path(outputFolder, subFolder1)
  if (!dir.exists(outputFolder)){
    if(createIfMissing==F) stop(paste0("ERROR in GetOutputFolder(): There is no '",subFolder1,"' subfolder in '",outputFolderPrev,"' folder. Cannot create the folder either as parameter 'createIfMissing=F'."))
    dir.create(outputFolder)
    print(paste0('Created folder for database results: ',outputFolder))
  } else {
    #print(paste0('Folder for database results already exists: ',outputFolder))
  }

  outputFolderPrev=outputFolder
  outputFolder <- file.path(outputFolder, subFolder2)
  if (!dir.exists(outputFolder)){
    if(createIfMissing==F) stop(paste0("ERROR in GetOutputFolder(): There is no '",subFolder1,"' subfolder in '",outputFolderPrev,"' folder. Cannot create the folder either as parameter 'createIfMissing=F'."))
    dir.create(outputFolder)
    print(paste0('Created folder for analysis results: ',outputFolder))
  } else {
    #print(paste0('Folder for analysis results already exists: ',outputFolder))
  }

  if(createIfMissing==T) print(paste0("Output folder set to ",outputFolder))
  return(outputFolder)

}
