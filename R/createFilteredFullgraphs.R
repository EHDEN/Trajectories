#' Creates full graph plots from event pairs (not filtered to specific concept id-s)
#'
#' @param connection
#' @param trajectoryAnalysisArgs
#' @param trajectoryLocalArgs
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
  eventPairResultsFilename = file.path(outputFolder,'event_pairs.tsv')

  # create igraph object from event pairs
  g<-Trajectories::createTrajectoriesGraph(eventPairResultsFilename)

  cohortName=trajectoryAnalysisArgs$cohortName


  # create a plot of all event pairs (no filtering)
  title=paste0('All significant directional event pairs among ',cohortName,' patients')
  #Truncate the title for file name if it is too long
  truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  Trajectories::plotTrajectoriesGraph(g,layout=layout_with_fr,linknumbers=round(100*E(g)$prob),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))

  # Remove low-probability event pairs (keep 20, 50, 100 event pairs with highest probability)
  s=c(20,50,100)

  for(limitOfLinks in s) {
    logger::log_info('Creating a plot of the same graph, but filtered to {limitOfLinks} high-probability pairs only...')
    #limitOfLinks=50
    title=paste0(limitOfLinks,' high-probability event pairs among ',cohortName,' patients')
    h<-Trajectories::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g, limitOfLinks=limitOfLinks,edge_param_to_sort_by='prob')
    #Truncate the title for file name if it is too long
    truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
    Trajectories::plotTrajectoriesGraph(h,layout=layout_with_fr,linknumbers=round(100*E(h)$prob),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
  }
}