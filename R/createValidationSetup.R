#' Creates setup file for the validation of the results in a separate database
#'
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#'
#' @return
#'
#' @examples
createValidationSetup<-function(trajectoryAnalysisArgs,
                                trajectoryLocalArgs
                               ) {
  ParallelLogger::logInfo("Creating setup file for the validation of the results in a separate database...")

  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # If a folder for validation setup already exists, remove it (together with all "old" files)
  outputFolderForValidationSetup <- file.path(outputFolder, 'validation_setup')
  if (dir.exists(outputFolderForValidationSetup)) {
    unlink(outputFolderForValidationSetup, recursive = TRUE)
    ParallelLogger::logDebug("Removed 'old' folder for validation setup: ",outputFolderForValidationSetup)
  }

  # Create a new folder for validation setup
  dir.create(outputFolderForValidationSetup)
  ParallelLogger::logDebug('Created a new folder for validation setup: ',outputFolderForValidationSetup)


  # Copy cohort definition file into it
  f<-file.path(trajectoryLocalArgs$inputFolder,'cohort.sql')
  if (!dir.exists(trajectoryLocalArgs$inputFolder)) ParallelLogger::logError("ERROR in createValidationSetup(): trajectoryLocalArgs$inputFolder '",inputFolder,"' does not exist.")
  if (!file.exists(f)) ParallelLogger::logError("ERROR in createValidationSetup(): there is no 'cohort.sql' file in inputFolder '",trajectoryLocalArgs$inputFolder,"'.")
  to=file.path(outputFolderForValidationSetup,'cohort.sql')
  file.copy(from=f, to=to)
  ParallelLogger::logDebug(f," copied to ",to)

  # Save trajectoryAnalysisArgs into it
  trajectoryAnalysisArgsToSave=trajectoryAnalysisArgs
  trajectoryAnalysisArgsToSave$mode<-'VALIDATION' #set mode to VALIDATION
  Trajectories:::TrajectoryAnalysisArgsToJson(trajectoryAnalysisArgsToSave, file.path(outputFolderForValidationSetup,"trajectoryAnalysisArgs.json"))

  # Copy event pairs into it
  eventPairResultsFilename = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  eventPairResultsFilenameNew = file.path(outputFolderForValidationSetup,'event_pairs_for_validation.tsv')
  e = read.csv2(file = eventPairResultsFilename, sep = '\t', header = TRUE, as.is=T)
  e2 <- e %>% dplyr::select(E1_CONCEPT_ID,E2_CONCEPT_ID,E1_NAME,E2_NAME,E1_DOMAIN,E2_DOMAIN,RR_IN_PREVIOUS_STUDY=RR)
  e2$RR_IN_PREVIOUS_STUDY=round(as.numeric(e2$RR_IN_PREVIOUS_STUDY),3)
  write.table(e2, file=eventPairResultsFilenameNew, quote=FALSE, sep='\t', col.names = NA)
  ParallelLogger::logDebug("Selected columns of '",eventPairResultsFilename,"' copied to '",eventPairResultsFilenameNew,"'")


  ParallelLogger::logInfo('...done. Validation setup created to folder ',outputFolderForValidationSetup)
}
