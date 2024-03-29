requireNamespace("stringi", quietly = TRUE)

#' Creates full graph plots from event pairs (not filtered to specific concept id-s)
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#'
#' @return
#'
#' @examples
createFilteredFullgraphs<-function(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs) {

  ParallelLogger::logInfo('Creating a plot of full graph (built from all event pairs)...')

  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  eventPairResultsFilename = file.path(outputFolder,'tables','event_pairs_directional.tsv')

  # create igraph object from event pairs
  g<-Trajectories:::createTrajectoriesGraph(eventPairResultsFilename=eventPairResultsFilename)

  if(length(igraph::V(g))==0) {
    ParallelLogger::logWarn('The graph contains 0 events, skip plotting figures.')
    return()
  }

  cohortName=trajectoryAnalysisArgs$cohortName


  # create a plot of all event pairs (no filtering)
  title=paste0('All significant directional event pairs among ',cohortName,' patients, probability')
  #Truncate the title for file name if it is too long
  truncated_title=ifelse(stringi::stri_length(title)<=200,title,paste(substr(title,1,200)))
  Trajectories:::plotTrajectoriesGraph(g,
                                      layout=igraph::layout_with_graphopt(g),
                                      linknumbers=round(100*igraph::E(g)$prob),
                                      linklabels=paste0(round(100*igraph::E(g)$prob),'%'),
                                      outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                      title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M'))
                                      )

  title=paste0('All significant directional event pairs among ',cohortName,' patients, RR')
  #Truncate the title for file name if it is too long
  truncated_title=ifelse(stringi::stri_length(title)<=200,title,paste(substr(title,1,200)))
  Trajectories:::plotTrajectoriesGraph(g,
                                      layout=igraph::layout_with_graphopt(g),
                                      linknumbers=round(100*igraph::E(g)$effect),
                                      linklabels=paste0(round(igraph::E(g)$effect,1),'x'),
                                      outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                      title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M'))
  )

  # Remove low-probability event pairs (keep 20, 50, 100 event pairs with highest probability)
  s=c(20,50,100)
  s<-s[s<length(igraph::E(g))] #if the graph does not have that many edges, skip drawing the plot

  for(limitOfLinks in s) {
    ParallelLogger::logInfo('Creating a plot of the same graph, but filtered to ',limitOfLinks,' high-probability pairs only...')
    #limitOfLinks=50
    title=paste0(limitOfLinks,' high-probability event pairs among ',cohortName,' patients, probability')
    h<-Trajectories:::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g, limitOfLinks=limitOfLinks,edge_param_to_sort_by='prob')
    #Truncate the title for file name if it is too long
    truncated_title=ifelse(stringi::stri_length(title)<=200,title,paste(substr(title,1,200)))
    Trajectories:::plotTrajectoriesGraph(h,
                                        layout=igraph::layout_with_graphopt(h),
                                        linknumbers=round(100*igraph::E(h)$prob),
                                        linklabels=paste0(round(igraph::E(h)$prob*100),"%"),
                                        outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                        title=paste0(title,"\n",
                                        format(Sys.time(), '%d %B %Y %H:%M')))

    title=paste0(limitOfLinks,' high-probability event pairs among ',cohortName,' patients, RR')
    h<-Trajectories:::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g, limitOfLinks=limitOfLinks,edge_param_to_sort_by='prob')
    #Truncate the title for file name if it is too long
    truncated_title=ifelse(stringi::stri_length(title)<=200,title,paste(substr(title,1,200)))
    Trajectories:::plotTrajectoriesGraph(h,
                                        layout=igraph::layout_with_graphopt(h),
                                        linknumbers=round(100*igraph::E(h)$effect),
                                        linklabels=paste0(round(igraph::E(h)$effect,1),'x'),
                                        outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                        title=paste0(title,"\n",
                                                     format(Sys.time(), '%d %B %Y %H:%M')))
  }







  # Remove low-probability event pairs (keep 20, 50, 100 event pairs with highest probability)
  s=c(20,50,100)
  s<-s[s<length(igraph::E(g))] #if the graph does not have that many edges, skip drawing the plot

  for(limitOfLinks in s) {
    ParallelLogger::logInfo('Creating a plot of the same graph, but filtered to ',limitOfLinks,' most prevalent pairs only...')
    #limitOfLinks=50
    title=paste0('Actual most prevalent ',limitOfLinks,' sequences among ',cohortName,' patients, count')
    h<-Trajectories:::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g, limitOfLinks=limitOfLinks,edge_param_to_sort_by='count')
    countlabels<-igraph::E(h)$count
    thousands<-which(countlabels>=1000 & countlabels<1000000)
    millions<-which(countlabels>=1000000)
    if(sum(thousands)>0) countlabels[thousands]<-paste0(round(countlabels[thousands]/1000,1),'K')
    if(sum(millions)>0)countlabels[millions]<-paste0(round(countlabels[millions]/1000000,1),'M')

    #Truncate the title for file name if it is too long
    truncated_title=ifelse(stringi::stri_length(title)<=200,title,paste(substr(title,1,200)))
    Trajectories:::plotTrajectoriesGraph(h,
                                        layout=igraph::layout_with_graphopt(h),
                                        nodesizes=igraph::V(h)$count,
                                        linknumbers=igraph::E(h)$count,
                                        linklabels=countlabels,
                                        outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                        title=paste0(title,"\n",
                                                     format(Sys.time(), '%d %B %Y %H:%M')))

    title=paste0('Actual most prevalent ',limitOfLinks,' sequences among ',cohortName,' patients, RR')
    h<-Trajectories:::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g, limitOfLinks=limitOfLinks,edge_param_to_sort_by='count')

    #Truncate the title for file name if it is too long
    truncated_title=ifelse(stringi::stri_length(title)<=200,title,paste(substr(title,1,200)))
    Trajectories:::plotTrajectoriesGraph(h,
                                         layout=igraph::layout_with_graphopt(h),
                                         nodesizes=igraph::V(h)$count,
                                         linknumbers=round(100*igraph::E(h)$effect),
                                         linklabels=paste0(round(igraph::E(h)$effect,1),'x'),
                                         outputPdfFullpath=file.path(outputFolder,'figures',paste0(make.names(truncated_title),'.pdf')),
                                         title=paste0(title,"\n",
                                                      format(Sys.time(), '%d %B %Y %H:%M')))
  }


}
