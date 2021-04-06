#' Creates plots for the analysis results obtained by runEventPairAnalysis()
#'
#' Analysis results, obtained by runEventPairAnalysis(), have to exist in output folder, set by Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs).
#' This function also alignes actual trajectories to the graph, therefore it requires database connection.
#' If you provide an eventName (OMOP concept name) or list of event names as input, the graph that crosses that event is shown. If you do not provide it (eventName=NA), the function automatically takes
#' most prevalent top5 events and builds graph for each of them.
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param eventIds Exact concept id-s as a list that is used for analyzing/plotting trajectories. If not specified (NA) (recommended) creates trajectories for top 5 events.
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param limitOfNodes Max number of events in the consrtructed graph. Used to limit the analysis to most important events only.
#' @param skipOutputTables If set to TRUE, no output data tables are made (the PDF graphs only).
#' @inheritParams createTrajectoriesGraph
#'
#' @return
#' @export
#'
#' @examples
PlotTrajectoriesGraphForEvents<-function(connection,
                       trajectoryAnalysisArgs,
                       trajectoryLocalArgs,
                       eventIds=NA,
                       limitOfNodes=30,
                       skipOutputTables=T
                       ) {


  library(stringi)
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  eventPairResultsFilename = file.path(outputFolder,'tables','event_pairs_directional.tsv')

  # create TrajectoriesGraph object from event pairs
  g<-Trajectories::createTrajectoriesGraph(eventPairResultsFilename=eventPairResultsFilename)

  cohortName=trajectoryAnalysisArgs$cohortName


  if(is.na(eventIds[1])) {
    logger::log_info('Going to plot graphs for 5 most-prevalent events...')
    eventIds<-getTop5Events(g)$concept_id
  } else {
    logger::log_info('Going to plot graphs for {length(eventIds)} selected events...')
  }

  #build graphs for the selected events
  for(eventId in eventIds) {

    logger::log_info(paste0('Building graph for the folloing event id: {eventId}'))

    #check that event id is present in g
    if(eventId %in% igraph::V(g)$concept_id) {
      Trajectories:::PlotTrajectoriesGraphForEvent(connection,
                                    trajectoryAnalysisArgs,
                                    trajectoryLocalArgs,
                                    g=g,
                                    eventId=eventId,
                                    limitOfNodes=30,
                                    skipOutputTables=skipOutputTables)

    } else {
      logger::log_warn('Cannot plot trajectories through {eventId} as there is no such event in the graph (skipping).')
    }

  } #for eventIds

  logger::log_info('All graphs saved.')

}
