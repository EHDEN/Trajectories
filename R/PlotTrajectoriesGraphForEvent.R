
#' Plots graph for a specific event
#'
#' @param connection
#' @param trajectoryAnalysisArgs
#' @param trajectoryLocalArgs
#' @param g
#' @param eventId
#' @param limitOfNodes
#'
#' @return
#' @export
#'
#' @examples
PlotTrajectoriesGraphForEvent<-function(connection,
                       trajectoryAnalysisArgs,
                       trajectoryLocalArgs,
                       g,
                       eventId,
                       limitOfNodes=50) {

  if(!is.numeric(eventId)) {
    stop('Error: Event ID is not numeric')
  }
  if(!eventId %in% V(g)$concept_id) {
    msg=glue::glue('Event with concept_id={eventId} is missing from TrajectoriesGraph object. The plot for this is not created (skipping).')
    logger::log_warn(msg)
    warning(msg)
    return()
  }

  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  cohortName=trajectoryAnalysisArgs$cohortName
  EVENTNAME=V(g)[V(g)$concept_id==eventId]$name

  logger::log_info("Creating graphs for concept_id={eventId} ('{EVENTNAME}')...")

  edge_param_to_sort_by="prob"

  # Construct most-likely trajectories that go through EVENT
  if(limitOfNodes==F) {
    title=paste0("Constructed most-likely trajectories of ",cohortName," patients through\n",EVENTNAME,"\n(based on ",V(g)[V(g)$concept_id==eventId]$count," patients and all directional event pairs)")
    aligned_to_title="constructed graph of all event pairs"
    filename_template=paste0(ifelse(stri_length(EVENTNAME)<=20,EVENTNAME,paste(substr(EVENTNAME,1,20))), eventId, '.constructed') #add event ID to file name as the beginning of the concept name might not be unique
  } else {
    title=paste0("Constructed most-likely trajectories of ",cohortName," patients through\n",EVENTNAME,"\n(based on ",V(g)[V(g)$concept_id==eventId]$count," patients and limited to ",limitOfNodes," events)")
    aligned_to_title=paste0("constructed graph of ",limitOfNodes," events")
    filename_template=paste0(ifelse(stri_length(EVENTNAME)<=20,EVENTNAME,paste(substr(EVENTNAME,1,20))), eventId, ".constructed.limit",limitOfNodes," events") #add event ID to file name as the beginning of the concept name might not be unique
  }
  logger::log_info(" Step 1: Constructing filtered graph for event {eventId}: '{title}'...")
  constructed.graph<-Trajectories::filterTrajectoriesGraphCrossingEvent(g, eventname = EVENTNAME, limitOfNodes=limitOfNodes, edge_param_to_sort_by=edge_param_to_sort_by)
  #Truncate the title for file name if it is too long
  #truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  filename=file.path(outputFolder,paste0(make.names(filename_template),'.pdf'))
  Trajectories::plotTrajectoriesGraph(constructed.graph,layout=layout_with_fr,linknumbers=round(E(constructed.graph)$prob*100),outputPdfFullpath=filename,title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
  logger::log_info(' ...done. File saved to {filename}.')


  logger::log_info(' Step 2: Aligning actual patient trajectories to this graph...')
  #align actual trajectories to constructed graph
  limitOfTrajs=NA
  h<-Trajectories::alignActualTrajectoriesToGraph (connection=connection,
                                                   trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                                   trajectoryLocalArgs=trajectoryLocalArgs,
                                                   g=constructed.graph,
                                                   eventname=EVENTNAME,
                                                   limit=limitOfTrajs,
                                                   filename=file.path(outputFolder,paste0(make.names(filename_template),'.trajs.csv')))
  #remove edges and nodes with count=0
  #Update 15 Oct 2020: do not remove them, we need them to draw out as gray (otherwise it is not clear which events and trajectories were analyzed)
  #h<-h-E(h)[E(h)$alignedTrajsCount==0]
  #h<-h-V(h)[V(h)$alignedTrajsCount==0]
  #previous actions "take away" TrajectoriesGraph class. Put it back
  class(h)<-c("TrajectoriesGraph","igraph")

  filename_e=file.path(outputFolder,paste0(make.names(filename_template),'.edges.csv'))
  filename_v=file.path(outputFolder,paste0(make.names(filename_template),'.vertices.csv'))
  logger::log_info(' Step 3: Saving graph data to edge/vertex files: {filename_e} and {filename_v}...')
  x<-igraph::as_data_frame(h, what='edges')
  y<-igraph::as_data_frame(h, what='vertices')
  write.table(x,file=filename_e,quote=FALSE, sep="\t", col.names = NA)
  write.table(y,file=filename_v,quote=FALSE, sep="\t", col.names = NA)
  logger::log_info(' ...done.')


  logger::log_info(' Step 4: Printing aligned graph 1/2 (counts): ')
  #One difficult thing to understand now is that V(h)$alignedTrajsCount for the EVENTNAME holds the number of trajectories that go through the EVENTNAME, not the prevalence of EVENTNAME
  #We've found that this is difficult to understand and for EVENTNAME it is better to show the actual prevalence/count instead (and calculate all frequencies based on that)
  title=paste0(ifelse(is.na(limitOfTrajs),'All ',''),V(h)[V(h)$concept_id==eventId]$count," actual trajectories of ",cohortName," patients having/passing\n",EVENTNAME," (EVENT),\naligned to ",aligned_to_title," (trajectory count on edge)")
  #Truncate the title for file name if it is too long
  #truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  filename=paste0(filename_template,".aligned.counts")
  filename=file.path(outputFolder,paste0(make.names(filename),'.pdf'))
  Trajectories::plotTrajectoriesGraph(h,layout=layout_with_fr,nodesizes=V(h)$alignedTrajsCount,linknumbers=round(E(h)$alignedTrajsCount),outputPdfFullpath=filename,title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
  logger::log_info(' ...done. File saved to {filename}.')


  logger::log_info(' Step 4: Printing aligned graph 2/2 (frequencies): ')
  #E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[ends(h,E(h),names=F)[,1]]$alignedTrajsCount
  E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[V(h)$concept_id==eventId]$count #probability relative to EVENTNAME COUNT (it is challenging to understand if we take it relative to EVENTNAME alignedTrajsCount because some trajectories are not being analyzed at all as it was the constructec graph)
  #set edges and nodes with frequency<0.5% to 0 (to get displayed as gray)
  nodesizes=V(h)$alignedTrajsCount
  nodesizes[V(h)$alignedTrajsCount/V(h)[V(h)$concept_id==eventId]$count<0.005]<-0
  E(h)[E(h)$alignedTrajsProb<0.005]$alignedTrajsProb=0
  #previous actions "take away" TrajectoriesGraph class. Put it back
  class(h)<-c("TrajectoriesGraph","igraph")

  title=paste0(ifelse(is.na(limitOfTrajs),'All ',''),V(h)[V(h)$concept_id==eventId]$count," actual trajectories of ",cohortName," patients having/passing\n",EVENTNAME," (EVENT),\naligned to ",aligned_to_title," (frequency relative to EVENT given on edges)")
  #Truncate the title for file name if it is too long
  filename=paste0(filename_template,".aligned.freqs")
  filename=file.path(outputFolder,paste0(make.names(filename),'.pdf'))
  Trajectories::plotTrajectoriesGraph(h,layout=layout_with_fr,nodesizes=nodesizes,linknumbers=round(E(h)$alignedTrajsProb*100),outputPdfFullpath=filename,title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
  logger::log_info(' ...done. File saved to {filename}.')



  #align actual trajectories to full graph (takes looong time)
  #limitOfTrajs=10000
  #h<-Trajectories::alignActualTrajectoriesToGraph (connection=connection,
  #                                                 trajectoryAnalysisArgs=trajectoryAnalysisArgs,
  #                                                 trajectoryLocalArgs=trajectoryLocalArgs,
  #                                                 g=g,
  #                                                 eventname=EVENTNAME,
  #                                                 limit=limitOfTrajs)
  #remove edges and nodes with count=0
  #h<-h-E(h)[E(h)$alignedTrajsCount==0]
  #h<-h-V(h)[V(h)$alignedTrajsCount==0]
  #E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[V(h)$name==EVENTNAME]$alignedTrajsCount #probability relative to EVENTNAME
  #title=paste0(ifelse(is.na(limitOfTrajs),'All ',limitOfTrajs)," actual trajectories of ",cohortName," patients having/passing\n",EVENTNAME," (EVENT),\naligned to full graph (frequency relative to EVENT given on edge)")
  #Truncate the title for file name if it is too long
  #truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  #Trajectories::plotTrajectoriesGraph(h,layout=layout_with_fr,nodesizes=V(h)$alignedTrajsCount,linknumbers=round(E(h)$alignedTrajsProb*100),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))

  #x<-igraph::as_data_frame(h, what='edges')
  #y<-igraph::as_data_frame(h, what='vertices')
  #write.table(x,file=file.path(outputFolder,paste0(make.names(truncated_title),'.edges.csv')),quote=FALSE, sep="\t", col.names = NA)
  #write.table(y,file=file.path(outputFolder,paste0(make.names(truncated_title),'.vertices.csv')),quote=FALSE, sep="\t", col.names = NA)

  return()

}
