
#' Plots graph for a specific event
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param g igraph object
#' @param eventId ID of the event in the event pair
#' @param limitOfNodes limit of the number of nodes to include on the graph
#' @param skipOutputTables If set to T, no output data tables are made (the PDF graphs only).
#'
#' @return
#'
#' @examples
PlotTrajectoriesGraphForEvent<-function(connection,
                       trajectoryAnalysisArgs,
                       trajectoryLocalArgs,
                       g,
                       eventId=443732, #Disorder due to type 2 diabetes mellitus
                       limitOfNodes=30,
                       skipOutputTables=T) {

  #if(!is.numeric(eventId)) {
  #  stop('Error: Event ID is not numeric')
  #}
  if(!eventId %in% igraph::V(g)$concept_id) {
    msg=glue::glue('Event with concept_id={eventId} is missing from TrajectoriesGraph object. The plot for this is not created (skipping).')
    logger::log_warn(msg)
    warning(msg)
    return()
  }

  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  cohortName=trajectoryAnalysisArgs$cohortName
  EVENTNAME=igraph::V(g)[igraph::V(g)$concept_id==eventId]$concept_name

  logger::log_info("Creating graphs for concept_id={eventId} ('{EVENTNAME}')...")

  edge_param_to_sort_by="prob"

  # Construct most-likely trajectories that go through EVENT
  if(limitOfNodes==F) {
    title=paste0("Constructed most-likely trajectories of ",cohortName," patients through\n",EVENTNAME,"\n(based on ",igraph::V(g)[igraph::V(g)$concept_id==eventId]$count," patients and all directional event pairs)")
    aligned_to_title="constructed graph of all event pairs"
    filename_template=paste0(ifelse(stri_length(EVENTNAME)<=20,EVENTNAME,paste(substr(EVENTNAME,1,20))), eventId, '.constructed') #add event ID to file name as the beginning of the concept name might not be unique
  } else {
    title=paste0("Constructed most-likely trajectories of ",cohortName," patients through\n",EVENTNAME,"\n(based on ",igraph::V(g)[igraph::V(g)$concept_id==eventId]$count," patients and limited to ",limitOfNodes," events)")
    aligned_to_title=paste0("constructed graph of ",limitOfNodes," events")
    filename_template=paste0(ifelse(stri_length(EVENTNAME)<=20,EVENTNAME,paste(substr(EVENTNAME,1,20))), eventId, ".constructed.limit",limitOfNodes," events") #add event ID to file name as the beginning of the concept name might not be unique
  }
  logger::log_info(" Step 1: Constructing filtered graph for event {eventId}: '{title}'...")
  constructed.graph<-Trajectories::filterTrajectoriesGraphCrossingEvent(g,
                                                                        eventname = eventId,
                                                                        limitOfNodes=limitOfNodes,
                                                                        edge_param_to_sort_by=edge_param_to_sort_by)
  #Truncate the title for file name if it is too long
  #truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  filename=file.path(outputFolder,'figures',paste0(make.names(filename_template),'.pdf'))
  Trajectories::plotTrajectoriesGraph(constructed.graph,
                                      layout=igraph::layout_with_fr,
                                      linknumbers=round(igraph::E(constructed.graph)$prob*100),
                                      linklabels=paste0(round(igraph::E(constructed.graph)$prob*100),"%"),
                                      outputPdfFullpath=filename,
                                      title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
  logger::log_info(' ...done. File saved to {filename}.')


  logger::log_info(' Step 2: Aligning actual patient trajectories to this graph...')
  #align actual trajectories to constructed graph
  limitOfTrajs=NA
  h<-Trajectories::alignActualTrajectoriesToGraph (connection=connection,
                                                   trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                                   trajectoryLocalArgs=trajectoryLocalArgs,
                                                   g=constructed.graph,
                                                   eventid=eventId,
                                                   limit=limitOfTrajs,
                                                   filename=file.path(outputFolder,'tables',paste0(make.names(filename_template),'.trajs.csv')),
                                                   filename_interpretation =file.path(outputFolder,'tables',paste0(make.names(filename_template),'.trajs_interpretation.txt')) )
  #remove edges and nodes with count=0
  #Update 15 Oct 2020: do not remove them, we need them to draw out as gray (otherwise it is not clear which events and trajectories were analyzed)
  #h<-h-E(h)[E(h)$alignedTrajsCount==0]
  #h<-h-V(h)[V(h)$alignedTrajsCount==0]
  #previous actions "take away" TrajectoriesGraph class. Put it back
  class(h)<-c("TrajectoriesGraph","igraph")

  if(skipOutputTables==F) {
    filename_e=file.path(outputFolder,'tables',paste0(make.names(filename_template),'.edges.csv'))
    filename_v=file.path(outputFolder,'tables',paste0(make.names(filename_template),'.vertices.csv'))
    logger::log_info(' Step 3: Saving graph data to edge/vertex files: {filename_e} and {filename_v}...')
    x<-igraph::as_data_frame(h, what='edges')
    y<-igraph::as_data_frame(h, what='vertices')
    write.table(x,file=filename_e,quote=FALSE, sep="\t", col.names = NA)
    write.table(y,file=filename_v,quote=FALSE, sep="\t", col.names = NA)
    logger::log_info(' ...done.')
  } else {
    logger::log_info(' Step 3: (skipping as skipOutputTables=T)')
  }

  logger::log_info(' Step 4: Printing aligned graph: ')
  #One difficult thing to understand now is that V(h)$alignedTrajsCount for the EVENTNAME holds the number of trajectories that go through the EVENTNAME, not the prevalence of EVENTNAME
  #We've found that this is difficult to understand and for EVENTNAME it is better to show the actual prevalence/count instead (and calculate all frequencies based on that)
  title=paste0(ifelse(is.na(limitOfTrajs),'All ',''),igraph::V(h)[igraph::V(h)$concept_id==eventId]$count," actual trajectories of ",cohortName," patients having/passing\n",EVENTNAME," (EVENT),\naligned to ",aligned_to_title," (count + frequency relative to EVENT given on edges)")
  #Truncate the title for file name if it is too long
  #truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  filename=paste0(filename_template,".aligned")
  filename=file.path(outputFolder,'figures',paste0(make.names(filename),'.pdf'))
  Trajectories::plotTrajectoriesGraph(h,
                                      layout=igraph::layout_with_fr,
                                      nodesizes=igraph::V(h)$alignedTrajsCount,
                                      linknumbers=round(igraph::E(h)$alignedTrajsCount),
                                      linklabels=paste0(igraph::E(h)$alignedTrajsCount," (",round(igraph::E(h)$alignedTrajsProb*100),"%)"),
                                      outputPdfFullpath=filename,
                                      title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
  logger::log_info(' ...done. File saved to {filename}.')

  return()

}
