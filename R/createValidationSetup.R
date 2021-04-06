library(SqlRender)

#' Creates setup file for the validation of the results in a separate database
#'
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#'
#' @return
#' @export
#'
#' @examples
createValidationSetup<-function(trajectoryAnalysisArgs,
                                trajectoryLocalArgs
                               ) {
  logger::log_info("Creating setup file for the validation of the results in a separate database...")

  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)

  # If a folder for validation setup already exists, remove it (together with all "old" files)
  outputFolderForValidationSetup <- file.path(outputFolder, 'validation_setup')
  if (dir.exists(outputFolderForValidationSetup)) {
    unlink(outputFolderForValidationSetup, recursive = TRUE)
    logger::log_debug(paste0("Removed 'old' folder for validation setup: ",outputFolderForValidationSetup))
  }

  # Create a new folder for validation setup
  dir.create(outputFolderForValidationSetup)
  logger::log_debug(paste0('Created a new folder for validation setup: ',outputFolderForValidationSetup))


  # Copy cohort definition file into it
  f<-file.path(trajectoryLocalArgs$inputFolder,'cohort.sql')
  if (!dir.exists(trajectoryLocalArgs$inputFolder)) logger::log_error(paste0("ERROR in createValidationSetup(): trajectoryLocalArgs$inputFolder '",inputFolder,"' does not exist."))
  if (!file.exists(f)) logger::log_error(paste0("ERROR in createValidationSetup(): there is no 'cohort.sql' file in inputFolder '",trajectoryLocalArgs$inputFolder,"'."))
  to=file.path(outputFolderForValidationSetup,'cohort.sql')
  file.copy(from=f, to=to)
  logger::log_debug("{f} copied to {to}")

  # Save trajectoryAnalysisArgs into it
  trajectoryAnalysisArgsToSave=trajectoryAnalysisArgs
  trajectoryAnalysisArgsToSave$mode<-'VALIDATION' #set mode to VALIDATION
  Trajectories::TrajectoryAnalysisArgsToJson(trajectoryAnalysisArgsToSave, file.path(outputFolderForValidationSetup,"trajectoryAnalysisArgs.json"))

  # Copy event pairs into it
  eventPairResultsFilename = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  eventPairResultsFilenameNew = file.path(outputFolderForValidationSetup,'event_pairs_for_validation.tsv')
  e = read.csv2(file = eventPairResultsFilename, sep = '\t', header = TRUE, as.is=T)
  e2 <- e %>% select(E1_CONCEPT_ID,E2_CONCEPT_ID,E1_NAME,E2_NAME,E1_DOMAIN,E2_DOMAIN,RR_IN_PREVIOUS_STUDY=RR)
  e2$RR_IN_PREVIOUS_STUDY=round(as.numeric(e2$RR_IN_PREVIOUS_STUDY),3)
  write.table(e2, file=eventPairResultsFilenameNew, quote=FALSE, sep='\t', col.names = NA)
  logger::log_debug("Selected columns of '{eventPairResultsFilename}' copied to '{eventPairResultsFilenameNew}'")


  logger::log_info("...done. Validation setup created to folder {outputFolderForValidationSetup}")
}
