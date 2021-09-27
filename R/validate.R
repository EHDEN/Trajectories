#' The main function to run the Trajectory package in validation mode - that is, to validate someone's results in your local OMOP CDM
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param createCohort Builds a study cohort in the database. Normally set to T/TRUE but for debugging or running the analysis step-by-step one can set it to F/FALSE as well.
#' @param createEventPairsTable Builds all event pairs and necessary data tables in the database for the analysis. Normally set to T/TRUE but for debugging or running the analysis step-by-step one can set it to F/FALSE as well.
#' @param runValidationAnalysis Run the actual directionality analysis of all event pairs. Normally set to T/TRUE but for debugging or running the analysis step-by-step one can set it to F/FALSE as well.
#' @param forceRecalculationOfAnalysis Used only when runValidationAnalysis=T. If TRUE, forces deleting previous results from the database and rerunning the whole validation analysis. Useful mostly in case something goes wrong and you need to force the recalculation (it is, when debugging). In normal circumstances using FALSE is safe.
#' @param createFilteredFullgraphs Builds graphs based on the results. Normally set to T/TRUE but for debugging or running the analysis step-by-step one can set it to F/FALSE as well.
#' @param runTrajectoryAnalysis If TRUE, runs the trajectory analysis - puts the actual trajectories to the graph
#' @param cleanup Drops tables from the database that were created during various stages of the analysis. Normally set to T/TRUE but for debugging or running the analysis step-by-step one can set it to F/FALSE as well.
#'
#' @return
#' @export
#'
#' @examples
validate <- function(connection,
                     trajectoryLocalArgs,
                     createCohort=T,
                     createEventPairsTable=T,
                     runValidationAnalysis=T,
                     forceRecalculationOfAnalysis=F,
                     createFilteredFullgraphs=T,
                     runTrajectoryAnalysis=F,
                     cleanup=T
                      ) {

  trajectoryAnalysisArgs<-Trajectories:::TrajectoryAnalysisArgsFromInputFolder(trajectoryLocalArgs) #also creates output folder for results, logs, etc.

  #Check that the mode is "VALIDATION"
  if(Trajectories:::IsValidationMode(trajectoryAnalysisArgs,verbose=T)==F) stop('trajectoryAnalysisArg$mode=DISCOVERY in study settings file ',trajectoryLocalArgs$inputFolder,'. Cannot run validate() method for analysis that is designed for discovery mode. Perhaps you wanted to use discover() instead?')

  ##############################################################################################################

  # BUILD A COHORT BASED ON COHORT DEFINITION SQL AND THEN DIVIDE IT TO 2 SETS: DISCOVERY & VALIDATION SET

  ##############################################################################################################

  # Create new cohort table for this package to results schema & fill it in (all having cohort_id=1 in cohort data)
  if(createCohort) {
    Trajectories:::createAndFillCohortTable(connection=connection,
                                         trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                         trajectoryLocalArgs=trajectoryLocalArgs)

    # Assign all event-periods from the cohort to validation set (UPDATE all to cohort_id=2)
    Trajectories:::createValidationSet(connection=connection,
                                      trajectoryAnalysisArgs,
                                      trajectoryLocalArgs,
                                      size=1)
  }

  ##############################################################################################################

  # RUN VALIDATION ANALYSIS

  ##############################################################################################################



  # Create database tables of all event pairs (patient level data + summary statistics). Uses cohort_id depending on the running mode of the package
  if(createEventPairsTable) Trajectories:::createEventPairsTable(connection=connection,
                                      trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                      trajectoryLocalArgs=trajectoryLocalArgs)


  # Detect statistically significant directional event pairs and write the results to eventPairResultsFilename. Also creates validation setup.
  if(runValidationAnalysis) Trajectories:::runValidationAnalysis(connection,
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

  ########### CLEANUP: DROP ANALYSIS TABLES IF THERE IS NO NEED FOR THESE RESULTS ANYMORE ###########

  # Cleanup database after analysis
  if(cleanup) Trajectories:::dbCleanup(connection=connection,
                          trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                          trajectoryLocalArgs=trajectoryLocalArgs)

}

