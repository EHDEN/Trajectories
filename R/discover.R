#' The main function to run the Trajectory package in discovery mode - that is, to identify the temporal event sequences in the cohort of your local OMOP CDM
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param createCohort Builds a study cohort in the database. If validationSetSize is set to non-zero, splits the cohort into disovery and validation set.
#' @param validationSetSize If set to non-zero, splits the whole cohort into discovery and (self-)validation set. Is meaningful only if selfValidate=T. Allowed values are in range 0..1.
#' @param createEventPairsTable Builds all event pairs and necessary data tables in the database for the analysis. Also clears all results from the database if they exist.
#' @param runDiscoveryAnalysis Run the actual directionality analysis of all event pairs.
#' @param forceRecalculationOfAnalysis Forces deleting previous results from the database and rerunning the whole discovery analysis. Useful mostly in case something goes wrong and you need to force the recalculation (it is, when debugging). In normal circumstances using F is safe.
#' @param createFilteredFullgraphs Builds graphs based on the results.
#' @param runTrajectoryAnalysis If TRUE, runs the trajectory analysis - puts the actual trajectories to the graph
#' @param selfValidate Normally, set to F/FALSE as it is always better to validate your results in another database. However, if you want to validate your results in your own database, then set selfValidate=T and validationSetSize=some meaningful proportion (for example, 0.5). In such case, the discovery analysis is actually conducted on half of the data and the results are then validated on another half.
#' @param cleanup Drops tables from the database that were created during various stages of the analysis.
#
#' @return
#' @export
#'
#' @examples
discover <- function(connection,
                     trajectoryLocalArgs,
                     createCohort=T,
                     validationSetSize=0,  # used only if createCohort=T (validation set created while building cohort)
                     createEventPairsTable=T,
                     runDiscoveryAnalysis=T,
                     forceRecalculationOfAnalysis=F,
                     createFilteredFullgraphs=T,
                     runTrajectoryAnalysis=T,
                     selfValidate=F,
                     cleanup=F
                      ) {

  trajectoryAnalysisArgs<-Trajectories:::TrajectoryAnalysisArgsFromInputFolder(trajectoryLocalArgs) #also creates output folder for results, logs, etc.

  #Check that the mode is "DISCOVERY"
  if(Trajectories:::IsValidationMode(trajectoryAnalysisArgs,verbose=T)==T) stop('trajectoryAnalysisArg$mode=VALIDATION in study settings file ',trajectoryLocalArgs$inputFolder,'. Cannot run discover() method for analysis that is designed for validation mode. Perhaps you wanted to use validate() instead?')
  #Check that validationSetSize is between 0 and 1
  if(validationSetSize<0 | validationSetSize>1) stop('validationSetSize=',validationSetSize,' but it should be in the range 0..1')
  if(selfValidate==T & validationSetSize==0) stop('Cannot run validation of the results (requested by selfValidate=T) as validationSetSize=',validationSetSize)


  ##############################################################################################################

  # BUILD A COHORT BASED ON COHORT DEFINITION SQL AND THEN DIVIDE IT TO 2 SETS: DISCOVERY & VALIDATION SET

  ##############################################################################################################

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  if(createCohort) {
    Trajectories:::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)

    # Assign ...% of the event-periods from the cohort to validation set (discovery set=data in cohort table where cohort_id=1; validation set=data in cohort table where cohort_id=2)
    if(validationSetSize>0) Trajectories:::createValidationSet(connection=connection,
                                      trajectoryAnalysisArgs,
                                      trajectoryLocalArgs,
                                      size=0)
  }

  ##############################################################################################################

  # RUN DISCOVERY ANALYSIS

  ##############################################################################################################



  # Create database tables of all event pairs (patient level data + summary statistics). Uses cohort_id depending on the running mode of the package
  if(createEventPairsTable) Trajectories:::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)


  # Detect statistically significant directional event pairs and write the results to eventPairResultsFilename. Also creates validation setup.
  if(runDiscoveryAnalysis) Trajectories:::runDiscoveryAnalysis(connection,
                                     trajectoryAnalysisArgs,
                                     trajectoryLocalArgs,
                                     forceRecalculation=forceRecalculationOfAnalysis)



  # Draw unfiltered graphs of the discovery results (not limited to specific concept_id-s)
  if(createFilteredFullgraphs) Trajectories:::createFilteredFullgraphs(connection,
                                         trajectoryAnalysisArgs,
                                         trajectoryLocalArgs)

  # Run trajectory analysis
  if(runTrajectoryAnalysis) Trajectories:::align(connection,
                                                 trajectoryAnalysisArgs,
                                                 trajectoryLocalArgs)

  # Draw graphs for selected events (event ID-s taken from trajectoryAnalysisArgs$eventIdsForGraphs)
  #if(createGraphsForSelectedEvents) Trajectories:::PlotTrajectoriesGraphForEvents(connection,
  #                                                                                trajectoryAnalysisArgs,
  #                                                                                trajectoryLocalArgs,
  #                                                                                eventIds=trajectoryAnalysisArgs$eventIdsForGraphs)

  ##############################################################################################################

  # VALIDATE THE RESULTS OF DISCOVERY ANALYSIS (RUN VALIDATION ANALYSIS)

  ##############################################################################################################


  if(selfValidate) {

    # Load setup from output-folder-of-discovery-analysis/validation_setup folder
    trajectoryLocalArgs$inputFolder=file.path(Trajectories:::GetOutputFolder(trajectoryLocalArgs=trajectoryLocalArgs, trajectoryAnalysisArgs=trajectoryAnalysisArgs, createIfMissing = F),"validation_setup")

    Trajectories::validate(connection,
                           trajectoryLocalArgs,
                           createCohort=T,
                           createEventPairsTable=T,
                           runValidationAnalysis=T,
                           createFilteredFullgraphs=F,
                           createGraphsForSelectedEvents=F,
                           cleanup=F)

  }


  ########### CLEANUP: DROP ANALYSIS TABLES IF THERE IS NO NEED FOR THESE RESULTS ANYMORE ###########

  # Cleanup database after analysis
  if(cleanup) Trajectories:::dbCleanup(connection=connection,
                          trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                          trajectoryLocalArgs=trajectoryLocalArgs)

}

