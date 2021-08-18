#' Creates full graph plots from event pairs (not filtered to specific concept id-s)
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#'
#' @return
#' @export
#'
#' @examples
createFilteredFullgraphs<-function(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs) {
  library(stringi)

  logger::log_info('Creating a plot of full graph (built from all event pairs)...')

  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  eventPairResultsFilename = file.path(outputFolder,'tables','event_pairs_directional.tsv')

  # create igraph object from event pairs
  g<-Trajectories::createTrajectoriesGraph(eventPairResultsFilename=eventPairResultsFilename)

  if(length(igraph::V(g))==0) {
    logger::log_warn('The graph contains 0 events, skip plotting figures.')
    return()
  }

  cohortName=trajectoryAnalysisArgs$cohortName


  # create a plot of all event pairs (no filtering)
  title=paste0('All significant directional event pairs among ',cohortName,' patients')
  #Truncate the title for file name if it is too long
  truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  Trajectories::plotTrajectoriesGraph(g,
                                      layout=igraph::layout_with_graphopt(g),
                                      linknumbers=round(100*igraph::E(g)$prob),
                                      linklabels=paste0(round(100*igraph::E(g)$prob),'%'),
                                      outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                      title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M'))
                                      )

  # Remove low-probability event pairs (keep 20, 50, 100 event pairs with highest probability)
  s=c(20,50,100)
  s<-s[s<length(E(g))] #if the graph does not have that many edges, skip drawing the plot

  for(limitOfLinks in s) {
    logger::log_info('Creating a plot of the same graph, but filtered to {limitOfLinks} high-probability pairs only...')
    #limitOfLinks=50
    title=paste0(limitOfLinks,' high-probability event pairs among ',cohortName,' patients')
    h<-Trajectories::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g, limitOfLinks=limitOfLinks,edge_param_to_sort_by='prob')
    #Truncate the title for file name if it is too long
    truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
    Trajectories::plotTrajectoriesGraph(h,
                                        layout=igraph::layout_with_graphopt(h),
                                        linknumbers=round(100*igraph::E(h)$prob),
                                        linklabels=paste0(round(igraph::E(h)$prob*100),"%"),
                                        outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                        title=paste0(title,"\n",
                                        format(Sys.time(), '%d %B %Y %H:%M')))
  }






  # Take the events/edges from full graph, find the actual counts of these edges and make a graph of these numbers
  # create a plot of all event pairs (no filtering)
  title=paste0('All actual sequences on the graph of significant directional event pairs among ',cohortName,' patients')
  #Truncate the title for file name if it is too long
  truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  g2<-Trajectories::alignActualTrajectoriesToGraphFull(connection,
                                                       trajectoryAnalysisArgs,
                                                       trajectoryLocalArgs,
                                                       g)
  Trajectories::plotTrajectoriesGraph(g2,
                                      layout=igraph::layout_with_graphopt(g2),
                                      nodesizes=igraph::V(g2)$alignedTrajsCount,
                                      linknumbers=igraph::E(g2)$alignedTrajsProb,
                                      linklabels=paste0(round(100*igraph::E(g2)$alignedTrajsProb),'%'),
                                      outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                      title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M'))
  )

  # Remove low-probability event pairs (keep 20, 50, 100 event pairs with highest probability)
  s=c(20,50,100)
  s<-s[s<length(E(g2))] #if the graph does not have that many edges, skip drawing the plot

  for(limitOfLinks in s) {
    logger::log_info('Creating a plot of the same graph, but filtered to {limitOfLinks} high-probability pairs only...')
    #limitOfLinks=50
    title=paste0('Actual most prevalent ',limitOfLinks,' sequences among ',cohortName,' patients')
    h<-Trajectories::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g2, limitOfLinks=limitOfLinks,edge_param_to_sort_by='alignedTrajsCount')
    #Truncate the title for file name if it is too long
    truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
    Trajectories::plotTrajectoriesGraph(h,
                                        layout=igraph::layout_with_graphopt(h),
                                        nodesizes=igraph::V(h)$alignedTrajsCount,
                                        linknumbers=igraph::E(h)$alignedTrajsCount,
                                        linklabels=paste0(round(100*igraph::E(h)$alignedTrajsProb),'%'),
                                        outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                        title=paste0(title,"\n",
                                                     format(Sys.time(), '%d %B %Y %H:%M')))
  }


}
