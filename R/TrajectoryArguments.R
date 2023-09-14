requireNamespace("jsonlite", quietly = TRUE)

#' Creates an object to hold analysis-specific data
#'
#' @param mode Indicates whether the analysis is run in DISCOVERY or VALIDATION mode. In VALIDATION mode, the package tries to validate predefined event pairs. In DISCOVERY mode, it tries to identify all directional event pairs from the data.
#' @param minimumDaysBetweenEvents The smallest number of days between two events of the patient that can be considered as event pair. Usually we have used 1 but 0 is also possible. The smaller the number is, the more time the calculation takes.
#' @param maximumDaysBetweenEvents The maximum number of days between two events of the patient that can be considered as event pair. Ususally we have not really limited it so we have used 3650 (10 years)
#' @param minPatientsPerEventPair Minimum number of people having event1 -> event2 (directional) progression (satisfying minimumDaysBetweenEvents and maximumDaysBetweenEvents requirements) to be included in analysis. If the value is >=1, it is considered as the absolute count of event pairs. If the value is less than 1, the value is considered as prevalence among the cohort size. For instance, if you have 1000 persons in the cohort and the value is 0.05, each event pair must occur at least 1000x0.05=50 times. Can be used for limiting analysis to frequent event pairs only. However, it does not affect control group matching and therefore, either the p-value.
#' @param daysBeforeIndexDate 0 or any positive number that indicates for how many days before index date of the cohort the events are included in the analysis. In case one wants to include all events before index date, use value Inf
#' @param cohortName Reader-friendly short description of the cohort. Used in graph titles and file names (can contain spaces)
#' @param RRrangeToSkip
#' @param eventIdsForGraphs
#' @param description This is a placeholder for any description of the study/cohort/analysis. For instance, it would be wise to descibe here what kind of cohort is that and what the analysis does.
#'
#' @return TrajectoryAnalysisArgs object
#'
#' @examples
createTrajectoryAnalysisArgs <- function(mode='DISCOVERY',
                                         minimumDaysBetweenEvents=1,
                                         maximumDaysBetweenEvents=3650,
                                         minPatientsPerEventPair=10,
                                         daysBeforeIndexDate=Inf,
                                         RRrangeToSkip=c(0,1.2),
                                         cohortName = 'My sample cohort',
                                         description = '',
                                         eventIdsForGraphs=NA) {


  if(!mode %in% c('DISCOVERY','VALIDATION')) stop("Error in createTrajectoryAnalysisArgs(): unknown value for MODE parameter: {mode}")

  if(RRrangeToSkip[2]<RRrangeToSkip[1]) {
    ParallelLogger::logError("Error in RRrangeToSkip=c(",RRrangeToSkip[1],",",RRrangeToSkip[2],") value: the start value of the range ('",RRrangeToSkip[1],"') can't be larger than the end value ('",RRrangeToSkip[2],"'). Check your analysis parameters.")
    stop()
  }
  if(RRrangeToSkip[1]<0 | RRrangeToSkip[1]>1) {
    ParallelLogger::logError("Error in RRrangeToSkip=c(",RRrangeToSkip[1],",",RRrangeToSkip[2],") value: The first value of the range must be in the range 0..1.")
    stop()
  }
  if(RRrangeToSkip[2]<1) {
    ParallelLogger::logError("Error in RRrangeToSkip=c(",RRrangeToSkip[1],",",RRrangeToSkip[2],") value: The second value of the range must be greater than 1.")
    stop()
  }
  if(!is.numeric(minimumDaysBetweenEvents)) {
    ParallelLogger::logError("Error in parameters: minimumDaysBetweenEvents value '",minimumDaysBetweenEvents,"' is not numeric.")
    stop()
  }
  if(minimumDaysBetweenEvents<0) {
    ParallelLogger::logError("Error in parameters: minimumDaysBetweenEvents value '",minimumDaysBetweenEvents,"' is negative which is not allowed.")
    stop()
  }
  if(!is.numeric(maximumDaysBetweenEvents)) {
    ParallelLogger::logError("Error in parameters: maximumDaysBetweenEvents value '",maximumDaysBetweenEvents,"' is not numeric.")
    stop()
  }
  if(maximumDaysBetweenEvents<0) {
    ParallelLogger::logError("Error in parameters: maximumDaysBetweenEvents value '",maximumDaysBetweenEvents,"' is negative which is not allowed.")
    stop()
  }
  if(minimumDaysBetweenEvents>maximumDaysBetweenEvents) {
    ParallelLogger::logError("Error in parameters: minimumDaysBetweenEvents value '",maximumDaysBetweenEvents,"' is larger than maximumDaysBetweenEvents value '",maximumDaysBetweenEvents,"' which is not allowed.")
    stop()
  }

  value <- list(mode=mode,minimumDaysBetweenEvents=minimumDaysBetweenEvents,maximumDaysBetweenEvents=maximumDaysBetweenEvents, minPatientsPerEventPair=minPatientsPerEventPair,
                daysBeforeIndexDate=daysBeforeIndexDate,
                RRrangeToSkip=RRrangeToSkip,
                cohortName=cohortName,description=description,eventIdsForGraphs=eventIdsForGraphs)
  class(value) <- 'TrajectoryAnalysisArgs'
  return(value)

}

#' Creates an object to hold local database-specific parameters
#'
#' @param cdmDatabaseSchema Schema containing source data in OMOP CDM format
#' @param vocabDatabaseSchema Schema containing OMOP vocabulary
#' @param resultsSchema Schema the user has writing access to (used to write analysis tables into)
#' @param prefixForResultTableNames Table prefix that is used for all output tables to avoid any collision with existing table names. An empty string is also allowed.
#' @param cohortTableSchema Schema where cohort table is located
#' @param cohortTable Name of the cohort table in cohortTableSchema
#' @param inputFolder Full path to input folder that contains SQL file for cohort definition (SQL Server format) and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "RA", package = "Trajectories") which is also the default value. In case your cohort data already exists in the database and you do not need to build it from scratch, set the value to FALSE.
#' @param mainOutputFolder The output folder path. This is the folder where the final results are produced into. Use full path and do NOT add trailing slash! The folder must already exist. Default value is the default working directory.
#' @param databaseHumanReadableName In the future, it will be added to the titles of the graph to indicate what data is this. Use something short. Currently this parameter is not used.
#'
#' @return TrajectoryLocalArgs object
#' @export
#'
#' @examples
createTrajectoryLocalArgs <- function(cdmDatabaseSchema,
                                      vocabDatabaseSchema,
                                      cohortDatabaseSchema,
                                      cohortTableName,
                                      resultsSchema,
                                      prefixForResultTableNames='',
                                      inputFolder=system.file("extdata", "RA", package = "Trajectories"),
                                      mainOutputFolder=getwd(),
                                      databaseHumanReadableName='My database') {

  #Sanity checks
  if(!is.logical(inputFolder)) {
    if (!dir.exists(inputFolder)) stop(paste0("ERROR in createTrajectoryLocalArgs(): inputFolder '",inputFolder,"' does not exist."))
  }
  if (!dir.exists(mainOutputFolder)) stop(paste0("ERROR in createTrajectoryLocalArgs(): mainOutputFolder '",mainOutputFolder,"' does not exist."))
  #if (!file.exists(paste0(inputFolder,'/trajectoryAnalysisArgs.json'))) stop(paste0("ERROR in createTrajectoryLocalArgs(): there is no 'trajectoryAnalysisArgs.json' file in inputFolder '",inputFolder,"'."))

  value <- list(cdmDatabaseSchema=cdmDatabaseSchema,
                vocabDatabaseSchema=vocabDatabaseSchema,
                resultsSchema=resultsSchema,
                cohortDatabaseSchema=cohortDatabaseSchema,
                cohortTableName=cohortTableName,
                prefixForResultTableNames=prefixForResultTableNames,
                inputFolder=inputFolder,
                mainOutputFolder=mainOutputFolder,
                databaseHumanReadableName=databaseHumanReadableName)
  class(value) <- 'TrajectoryLocalArgs'
  return(value)

}

#' Checks whether the object is of type TrajectoryLocalArgs
#'
#' @param x Any R object
#'
#' @return
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
#'
#' @examples
is.TrajectoryAnalysisArgs <- function(x) {
  inherits(x, "TrajectoryAnalysisArgs")
}

#' Writes trajectoryAnalysisArgs object to JSON file
#'
#' @param trajectoryAnalysisArgs Object created by Trajectories:::createTrajectoryAnalysisArgs() method
#' @param filepath Full path to the output file. Should have .json extension as this is actually a JSON file.
#'
#' @return
#'
#' @examples
TrajectoryAnalysisArgsToJson<-function(trajectoryAnalysisArgs, filepath) {
  ParallelLogger::logInfo("Saving 'trajectoryAnalysisArgs' data in JSON format to ",filepath,"...")

  if(!Trajectories:::is.TrajectoryAnalysisArgs(trajectoryAnalysisArgs)) stop("Something is not right. 'trajectoryAnalysisArgs' is not an object from class 'TrajectoryAnalysisArgs'")

  json<-jsonlite::toJSON(trajectoryAnalysisArgs, force=T, pretty=T)

  fileConn<-file(filepath)
  writeLines(json, fileConn)
  close(fileConn)

  ParallelLogger::logInfo("...done.")
}

#' Reads trajectoryAnalysisArgs object from JSON file
#'
#' @param filepath Full path to JSON file
#'
#' @return TrajectoryAnalysisArgs object
#'
#' @examples
TrajectoryAnalysisArgsFromJson<-function(filepath) {

  ParallelLogger::logInfo("Loading 'trajectoryAnalysisArgs' object from JSON file ",filepath,"...")
  r.obj<-jsonlite::fromJSON(filepath)

  #defaulting parameters if missing from JSON
  defaults=list(
    mode='DISCOVERY', #allowed values: 'DISCOVERY' or 'VALIDATION'
    minimumDaysBetweenEvents=1,
    maximumDaysBetweenEvents=3650,
    minPatientsPerEventPair=10,
    daysBeforeIndexDate=Inf,
    RRrangeToSkip=c(0,1.2),
    cohortName = 'My sample cohort',
    description = '',
    eventIdsForGraphs=NA
  )

  vals_for_obj=list()
  for(param in names(defaults)) {
    if(!param %in% names(r.obj)) {
      ParallelLogger::logWarn("'",param,"' parameter not given in JSON. Defaulting its value to ",defaults[param])
      vals_for_obj[[param]]=defaults[[param]]
    } else {
      vals_for_obj[[param]]=r.obj[[param]]
    }
  }

  trajectoryAnalysisArgs<-Trajectories:::createTrajectoryAnalysisArgs(
                              mode=vals_for_obj[['mode']],
                              minimumDaysBetweenEvents=vals_for_obj[['minimumDaysBetweenEvents']],
                               maximumDaysBetweenEvents=vals_for_obj[['maximumDaysBetweenEvents']],
                               minPatientsPerEventPair=vals_for_obj[['minPatientsPerEventPair']],
                               daysBeforeIndexDate=vals_for_obj[['daysBeforeIndexDate']],
                               RRrangeToSkip=vals_for_obj[['RRrangeToSkip']],
                               cohortName=vals_for_obj[['cohortName']],
                               description=vals_for_obj[['description']],
                               eventIdsForGraphs=vals_for_obj[['eventIdsForGraphs']])
  ParallelLogger::logInfo('...done.')
  return(trajectoryAnalysisArgs)
}

#' Searches for trajectoryAnalysisArgs.json file from inputFolder (defined in trajectoryLocalArgs), creates trajectoryAnalysisArgs object from it and returns it.
#'
#' @inheritParams GetOutputFolder
#'
#' @return TrajectoryLocalArgs object
#'
#' @examples
TrajectoryAnalysisArgsFromInputFolder<-function(trajectoryLocalArgs) {
  trajectoryAnalysisArgs<-Trajectories:::TrajectoryAnalysisArgsFromJson(file.path(trajectoryLocalArgs$inputFolder,"trajectoryAnalysisArgs.json"))

  if(Trajectories:::IsValidationMode(trajectoryAnalysisArgs,verbose=T)) {
    f=file.path(trajectoryLocalArgs$inputFolder,'event_pairs_for_validation.tsv')
    if(!file.exists(f)) {
      ParallelLogger::logError("The package is run in VALIDATION mode, but file 'event_pairs_for_validation.tsv' does not exist in input folder ",trajectoryLocalArgs$inputFolder,".")
      stop()
    }
  }


  #Create output folder for this analysis
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  # Set up logger
  Trajectories:::InitLogger(logfile = file.path(outputFolder,'logs',paste0(format(Sys.time(), "%Y%m%d-%H%M%S"),"-log.txt")), threshold = get('LOGGER_THRESHOLD', envir=TRAJECTORIES.CONSTANTS))

  return(trajectoryAnalysisArgs)
}

#' Returns full path to output folder for the results.
#'
#' Basically combines the value of mainOutputFolder, database name, and analysis name to get the output folder. Checks also that the folder exists. If createIfMissing=T, then creates the necessary subfolders under mainOutputFolder.
#'
#' @param trajectoryLocalArgs  Object created by Trajectories:::createTrajectoryLocalArgs() method
#' @param trajectoryAnalysisArgs  Object created by Trajectories:::createTrajectoryAnalysisArgs() method
#' @param createIfMissing If TRUE, then creates necessary folder if missing.
#'
#' @return Full output path
#'
#' @examples
GetOutputFolder<-function(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F) {

  outputFolder<-trajectoryLocalArgs$mainOutputFolder
  subFolder1=make.names(trajectoryLocalArgs$databaseHumanReadableName)
  subFolder2=make.names(trajectoryAnalysisArgs$cohortName)
  subFolder3=trajectoryAnalysisArgs$mode
  subFolder4="logs"

  if (!dir.exists(outputFolder)) stop(paste0("ERROR in GetOutputFolder(): trajectoryLocalArgs$mainOutputFolder='",mainOutputFolder,"' does not exist."))

  outputFolderPrev=outputFolder
  outputFolder <- file.path(outputFolder, subFolder1)
  if (!dir.exists(outputFolder)){
    if(createIfMissing==F) stop(paste0("ERROR in GetOutputFolder(): There is no '",subFolder1,"' subfolder in '",outputFolderPrev,"' folder. Cannot create the folder either as parameter 'createIfMissing=F'."))
    dir.create(outputFolder)
    print(paste0('Created folder for database results: ',outputFolder)) #do not use logger::log_... here as logger is not yet initialized
  } else {
    #print(paste0('Folder for database results already exists: ',outputFolder))
  }

  outputFolderPrev=outputFolder
  outputFolder <- file.path(outputFolder, subFolder2)
  if (!dir.exists(outputFolder)){
    if(createIfMissing==F) stop(paste0("ERROR in GetOutputFolder(): There is no '",subFolder2,"' subfolder in '",outputFolderPrev,"' folder. Cannot create the folder either as parameter 'createIfMissing=F'."))
    dir.create(outputFolder)
    print(paste0('Created folder for database results: ',outputFolder)) #do not use logger::log_... here as logger is not yet initialized
  } else {
    #print(paste0('Folder for analysis results already exists: ',outputFolder))
  }

  outputFolderPrev=outputFolder
  outputFolder <- file.path(outputFolder, subFolder3)
  if (!dir.exists(outputFolder)){
    if(createIfMissing==F) stop(paste0("ERROR in GetOutputFolder(): There is no '",subFolder3,"' subfolder in '",outputFolderPrev,"' folder. Cannot create the folder either as parameter 'createIfMissing=F'."))
    dir.create(outputFolder)
    print(paste0('Created folder for database results: ',outputFolder)) #do not use logger::log_... here as logger is not yet initialized
  } else {
    #print(paste0('Folder for analysis results already exists: ',outputFolder))
  }

  if(createIfMissing==T) ParallelLogger::logInfo("Output folder set to ",outputFolder)

  #subfolders

  subfolder='logs'
      outputFolderPrev=outputFolder
      outputFolder2 <- file.path(outputFolder, subfolder)
      if (!dir.exists(outputFolder2)){
        if(createIfMissing==F) stop(paste0("ERROR in GetOutputFolder(): There is no '",subfolder,"' subfolder in '",outputFolderPrev,"' folder. Cannot create the folder either as parameter 'createIfMissing=F'."))
        dir.create(outputFolder2)
        print(paste0('Created folder for logs: ',outputFolder2)) #do not use logger::log_... here as logger is not yet initialized
      } else {
        #print(paste0('Folder for analysis results already exists: ',outputFolder))
      }

  subfolder='figures'
      outputFolderPrev=outputFolder
      outputFolder2 <- file.path(outputFolder, subfolder)
      if (!dir.exists(outputFolder2)){
        if(createIfMissing==F) stop(paste0("ERROR in GetOutputFolder(): There is no '",subfolder,"' subfolder in '",outputFolderPrev,"' folder. Cannot create the folder either as parameter 'createIfMissing=F'."))
        dir.create(outputFolder2)
        print(paste0('Created folder for figures: ',outputFolder2)) #do not use logger::log_... here as logger is not yet initialized
      } else {
        #print(paste0('Folder for analysis results already exists: ',outputFolder))
      }

  subfolder='tables'
    outputFolderPrev=outputFolder
    outputFolder2 <- file.path(outputFolder, subfolder)
    if (!dir.exists(outputFolder2)){
      if(createIfMissing==F) stop(paste0("ERROR in GetOutputFolder(): There is no '",subfolder,"' subfolder in '",outputFolderPrev,"' folder. Cannot create the folder either as parameter 'createIfMissing=F'."))
      dir.create(outputFolder2)
      print(paste0('Created folder for tables: ',outputFolder2)) #do not use logger::log_... here as logger is not yet initialized
    } else {
      #print(paste0('Folder for analysis results already exists: ',outputFolder))
    }


  return(outputFolder)

}

#' Returns TRUE if the package is run in validation mode.
#'
#' @param trajectoryAnalysisArgs Object created by Trajectories:::createTrajectoryAnalysisArgs() method
#' @param verbose If TRUE, outputs some info in INFO/DEBUG log level. Otherwise, returns the results silently.
#'
#' @return
#'
#' @examples
IsValidationMode<-function(trajectoryAnalysisArgs, verbose=F) {
  if(verbose) {
    if(trajectoryAnalysisArgs$mode=='VALIDATION') {
      ParallelLogger::logInfo("The package is run in VALIDATION MODE")
    } else {
      ParallelLogger::logInfo("The package is run in DISCOVERY MODE")
    }
  }
  return(trajectoryAnalysisArgs$mode=='VALIDATION')
}

