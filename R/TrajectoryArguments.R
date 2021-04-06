
#' Creates an object to hold analysis-specific data
#'
#' @param mode Indicates whether the analysis is run in DISCOVERY or VALIDATION mode. In VALIDATION mode, the package tries to validate predefined event pairs. In DISCOVERY mode, it tries to identify all directional event pairs from the data.
#' @param minimumDaysBetweenEvents The smallest number of days between 2 events of the patient that can be considered as event pair. Usually we have used 1.
#' @param maximumDaysBetweenEvents The maximum number of days between 2 events of the patient that can be considered as event pair. Ususally we have not really limited it so we have used 3650 (10 years)
#' @param minPatientsPerEventPair Minimum number of people having event1 -> event2 (directional) progression (satisfying minimumDaysBetweenEvents and maximumDaysBetweenEvents requirements) to be included in analysis. If the value is >=1, it is considered as the absolute count of event pairs. If the value is less than 1, the value is considered as prevalence among the cohort size. For instance, if you have 1000 persons in the cohort and the value is 0.05, each event pair must occur at least 1000x0.05=50 times. Can be used for limiting analysis to frequent event pairs only. However, it does not throw less frequent diagnosis pairs out of the (control group) data and therefore, does not affect the statistical significance.
#' @param addConditions TRUE/FALSE parameter to indicate whether events from Condition_occurrence table should be included in the analysis
#' @param addObservations TRUE/FALSE parameter to indicate whether events from Condition_occurrence table should be included in the analysis
#' @param addProcedures TRUE/FALSE parameter to indicate whether events from Procedure_occurrence table should be included in the analysis
#' @param addDrugExposures TRUE/FALSE parameter to indicate whether events from Drug_exposure table should be included in the analysis. In most of the cases, prefer using addDrugEras instead as the particular RxNorm codes may differ in various databases (leading to no replication) but drug_era is always on ingredient level (active compound) and it also fills gaps between close events.
#' @param addDrugEras TRUE/FALSE parameter to indicate whether events from Drug_era table should be included in the analysis. NB! use either addDrugEras=T or addDrugExposures=T (not both) as it leads to analysis duplication...
#' @param addBirths TRUE/FALSE parameter to indicate whether births events should be included in the analysis.
#' @param addDeaths TRUE/FALSE parameter to indicate whether events from Death table should be included in the analysis.
#' @param daysBeforeIndexDate 0 or any positive number that indicates for how many days before index date of the cohort the events are included in the analysis. In case one wants to include all events before index date, use value Inf
#' @param RRrangeToSkip": Range of relative risks (RR) that are skipped from the analysis. The minimum value for the range is 0. E.g RRrangeToSkip=c(0,1) searches for RR>1 only (event pairs where the first event increases the risk of the second event). To skip RR with very small effect, it is recommended to use RRrangeToSkip=c(0,1.1) or even RRrangeToSkip=c(0,1.2) in DISCOVERY mode. In case one is interested in pairs with decreasing risk also, it is recommended to use the range something like RRrangeToSkip=c(0.8,1.2) (analyse all pairs that have RR<0.8 or R>=1.2). If you don't want to skip anything, use RRrangeToSkip=c(1,1) (analyses all pairs that have RR<1 or RR>=1 - that means, all pairs)
#' @param packageName Do not use/edit, this is required by SqlRender::loadRenderTranslateSql
#' @param cohortName Reader-friendly short description of the cohort. Used in graph titles and file names (can contain spaces)
#' @param description This is a placeholder for any description of the study/cohort/analysis. For instance, it would be wise to descibe here what kind of cohort is that and what the analysis does.
#' @param eventIdsForGraph List of exact concept ID-s of the events that are used to align actual trajectories in the end of analysis. Can be left not defined (NA)
#'
#' @return TrajectoryAnalysisArgs object
#' @export
#'
#' @examples
createTrajectoryAnalysisArgs <- function(mode='DISCOVERY',
                                         minimumDaysBetweenEvents=1,
                                         maximumDaysBetweenEvents=3650,
                                         minPatientsPerEventPair=10,
                                         addConditions=T,
                                         addObservations=F,
                                         addProcedures=F,
                                         addDrugExposures=F,
                                         addDrugEras=F,
                                         addBirths=F,
                                         addDeaths=T,
                                         daysBeforeIndexDate=Inf,
                                         RRrangeToSkip=c(0,1.2),
                                         cohortName = 'My sample cohort',
                                         description = '',
                                         eventIdsForGraphs=NA) {


  if(!mode %in% c('DISCOVERY','VALIDATION')) stop("Error in createTrajectoryAnalysisArgs(): unknown value for MODE parameter: {mode}")

  if(addDrugExposures==T & addDrugEras==T) stop("Error in createTrajectoryAnalysisArgs(): parameters values for 'addDrugExposures' and 'addDrugEras' are TRUE but both of them cannot be TRUE at the same time (choose one of them or set both to FALSE)")

  if(mode=='VALIDATION') {
    f=file.path(trajectoryLocalArgs$inputFolder,'event_pairs_for_validation.tsv')
    if(!file.exists(f)) {
      logger::log_error(paste0("The package is run in VALIDATION mode, but file 'event_pairs_for_validation.tsv' does not exist in input folder ",trajectoryLocalArgs$inputFolder,"."))
      stop()
    }
  }

  if(RRrangeToSkip[2]<RRrangeToSkip[1]) {
    logger::log_error(paste0("Error in RRrangeToSkip=c(",RRrangeToSkip[1],",",RRrangeToSkip[2],") value: the start value of the range ('",RRrangeToSkip[1],"') can't be larger than the end value ('",RRrangeToSkip[2],"'). Check your analysis parameters."))
    stop()
  }

  value <- list(mode=mode,minimumDaysBetweenEvents=minimumDaysBetweenEvents,maximumDaysBetweenEvents=maximumDaysBetweenEvents, minPatientsPerEventPair=minPatientsPerEventPair,
                addConditions=addConditions,addObservations=addObservations,addProcedures=addProcedures,addDrugExposures=addDrugExposures,
                addDrugEras=addDrugEras,addBirths=addBirths,addDeaths=addDeaths,
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
#' @param oracleTempSchema In case you are using oracle, schema for temporary tables need to be specified. A schema where temp tables can be created in Oracle. Otherwise leave it as it is (is not used)
#' @param sqlRole Role to use in SQL for writing tables in 'resultsSchema'. It should also have access to 'cdmDatabaseSchema' and 'vocabDatabaseSchema'. Set to FALSE (or F) if setting to a specific role is not needed. It should be safe to use F if you have no idea of what the SQL roles mean.
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
                                      resultsSchema,
                                      oracleTempSchema,
                                      sqlRole=F,
                                      prefixForResultTableNames='',
                                      #cohortTableSchema,
                                      #cohortTable,
                                      #cohortId=1, #use 1 for discovery studies and 2 for validation studies
                                      inputFolder=system.file("extdata", "RA", package = "Trajectories"),
                                      mainOutputFolder=getwd(),
                                      databaseHumanReadableName='My database') {

  #Sanity checks
  if(!is.logical(inputFolder)) {
    if (!dir.exists(inputFolder)) stop(paste0("ERROR in createTrajectoryLocalArgs(): inputFolder '",inputFolder,"' does not exist."))
    if (!file.exists(file.path(inputFolder,'cohort.sql'))) stop(paste0("ERROR in createTrajectoryLocalArgs(): there is no 'cohort.sql' file in inputFolder '",inputFolder,"'."))
  }
  if (!dir.exists(mainOutputFolder)) stop(paste0("ERROR in createTrajectoryLocalArgs(): mainOutputFolder '",mainOutputFolder,"' does not exist."))
  #if (!file.exists(paste0(inputFolder,'/trajectoryAnalysisArgs.json'))) stop(paste0("ERROR in createTrajectoryLocalArgs(): there is no 'trajectoryAnalysisArgs.json' file in inputFolder '",inputFolder,"'."))

  value <- list(cdmDatabaseSchema=cdmDatabaseSchema,vocabDatabaseSchema=vocabDatabaseSchema,
                resultsSchema=resultsSchema,oracleTempSchema=oracleTempSchema,sqlRole=sqlRole,
                prefixForResultTableNames=prefixForResultTableNames,
                #cohortTableSchema=cohortTableSchema,
                #cohortTable=cohortTable,
                #cohortId=cohortId,
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
  logger::log_info(paste0("Saving 'trajectoryAnalysisArgs' data in JSON format to ",filepath,"..."))

  if(!Trajectories::is.TrajectoryAnalysisArgs(trajectoryAnalysisArgs)) stop("Something is not right. 'trajectoryAnalysisArgs' is not an object from class 'TrajectoryAnalysisArgs'")

  library(jsonlite)
  json<-jsonlite::toJSON(trajectoryAnalysisArgs, force=T, pretty=T)

  fileConn<-file(filepath)
  writeLines(json, fileConn)
  close(fileConn)

  logger::log_info("...done.")
}

#' Reads trajectoryAnalysisArgs object from JSON file
#'
#' @param filepath Full path to JSON file
#'
#' @return TrajectoryAnalysisArgs object
#' @export
#'
#' @examples
TrajectoryAnalysisArgsFromJson<-function(filepath) {
  library(jsonlite)

  logger::log_info(paste0("Loading 'trajectoryAnalysisArgs' object from JSON file ",filepath,"..."))
  r.obj<-fromJSON(filepath)

  #defaulting parameters if missing from JSON
  defaults=list(
    mode='DISCOVERY', #allowed values: 'DISCOVERY' or 'VALIDATION'
    minimumDaysBetweenEvents=1,
    maximumDaysBetweenEvents=3650,
    minPatientsPerEventPair=10,
    addConditions=T,
    addObservations=F,
    addProcedures=F,
    addDrugExposures=F,
    addDrugEras=F,
    addBirths=F,
    addDeaths=T,
    daysBeforeIndexDate=Inf,
    RRrangeToSkip=c(0,1.2),
    cohortName = 'My sample cohort',
    description = '',
    eventIdsForGraphs=NA
  )

  vals_for_obj=list()
  for(param in names(defaults)) {
    if(!param %in% names(r.obj)) {
      logger::log_warn("'{param}' parameter not given in JSON. Defaulting its value to {defaults[param]}")
      vals_for_obj[[param]]=defaults[[param]]
    } else {
      vals_for_obj[[param]]=r.obj[[param]]
    }
  }

  trajectoryAnalysisArgs<-Trajectories::createTrajectoryAnalysisArgs(
                              mode=vals_for_obj[['mode']],
                              minimumDaysBetweenEvents=vals_for_obj[['minimumDaysBetweenEvents']],
                               maximumDaysBetweenEvents=vals_for_obj[['maximumDaysBetweenEvents']],
                               minPatientsPerEventPair=vals_for_obj[['minPatientsPerEventPair']],
                               addConditions=vals_for_obj[['addConditions']],
                               addObservations=vals_for_obj[['addObservations']],
                               addProcedures=vals_for_obj[['addProcedures']],
                               addDrugExposures=vals_for_obj[['addDrugExposures']],
                               addDrugEras=vals_for_obj[['addDrugEras']],
                               addBirths=vals_for_obj[['addBirths']],
                               addDeaths=vals_for_obj[['addDeaths']],
                               daysBeforeIndexDate=vals_for_obj[['daysBeforeIndexDate']],
                               RRrangeToSkip=vals_for_obj[['RRrangeToSkip']],
                               cohortName=vals_for_obj[['cohortName']],
                               description=vals_for_obj[['description']],
                               eventIdsForGraphs=vals_for_obj[['eventIdsForGraphs']])
  logger::log_info('...done.')
  return(trajectoryAnalysisArgs)
}

#' Searches for trajectoryAnalysisArgs.json file from inputFolder (defined in trajectoryLocalArgs), creates trajectoryAnalysisArgs object from it and returns it.
#'
#' @inheritParams GetOutputFolder
#'
#' @return TrajectoryLocalArgs object
#' @export
#'
#' @examples
TrajectoryAnalysisArgsFromInputFolder<-function(trajectoryLocalArgs) {
  trajectoryAnalysisArgs<-Trajectories::TrajectoryAnalysisArgsFromJson(file.path(trajectoryLocalArgs$inputFolder,"trajectoryAnalysisArgs.json"))

  Trajectories::IsValidationMode(trajectoryAnalysisArgs,verbose=T)

  #Create output folder for this analysis
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=T)

  # Set up logger
  Trajectories::InitLogger(logfile = file.path(outputFolder,'logs',paste0(format(Sys.time(), "%Y%m%d-%H%M%S"),"-log.txt")), threshold = logger:::INFO)

  return(trajectoryAnalysisArgs)
}

#' Returns full path to output folder for the results.
#'
#' Basically combines the value of mainOutputFolder, database name, and analysis name to get the output folder. Checks also that the folder exists. If createIfMissing=T, then creates the necessary subfolders under mainOutputFolder.
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

  if(createIfMissing==T) logger::log_info(paste0("Output folder set to ",outputFolder))

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
#' @param trajectoryAnalysisArgs Object created by Trajectories::createTrajectoryAnalysisArgs() method
#' @param verbose If TRUE, outputs some info in INFO/DEBUG log level. Otherwise, returns the results silently.
#'
#' @return
#' @export
#'
#' @examples
IsValidationMode<-function(trajectoryAnalysisArgs, verbose=F) {
  if(verbose) {
    if(trajectoryAnalysisArgs$mode=='VALIDATION') {
      logger::log_info("The package is run in VALIDATION MODE")
    } else {
      logger::log_info("The package is run in DISCOVERY MODE")
    }
  }
  return(trajectoryAnalysisArgs$mode=='VALIDATION')
}

