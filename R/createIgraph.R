library(igraph)
library(dplyr)

#' Creates igraph plots for the analysis results
#'
#' Analysis results have to exist in output folder, set by Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs).
#' This function also alignes actual trajectories to the graph, therefore it requires database connection.
#'
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param eventName Exact concept name that is used for building trajectories. Must exist in event pairs data table. If not specified (NA) (recommended) creates trajectories for top 5 events.
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#'
#' @return
#' @export
#'
#' @examples
createIgraph<-function(connection,
                       trajectoryAnalysisArgs,
                       trajectoryLocalArgs,
                       eventName=NA) {

  library(stringi)

  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  eventPairResultsFilename = file.path(outputFolder,'event_pairs.tsv')

  # create igraph object from event pairs
  g<-Trajectories::createGraph(eventPairResultsFilename)


  cohortName=trajectoryAnalysisArgs$cohortName






  # create a plot of all event pairs (no filtering)
  title=paste0('All significant directional event pairs among ',cohortName,' patients')
  #Truncate the title for file name if it is too long
  truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  Trajectories::plotIgraph(g,layout=layout_with_fr,linknumbers=round(100*E(g)$prob),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))

  # Remove low-probability event pairs (keep 50 event pairs with highest probability)
  s=c(20,50)
  for(limitOfLinks in s) {
    #limitOfLinks=50
    h<-Trajectories::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g, limitOfLinks=limitOfLinks,edge_param_to_sort_by='prob')
    title=paste0(limitOfLinks,' high-probability event pairs among ',cohortName,' patients')
    #Truncate the title for file name if it is too long
    truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
    Trajectories::plotIgraph(h,layout=layout_with_fr,linknumbers=round(100*E(h)$prob),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
  }


  # Construct most-likely trajectories that go through EVENT
  limitOfNodes=30
  edge_param_to_sort_by="prob"

  if(is.na(eventName)) {
    top5events<-Trajectories::getTop5Events(g)$name
  } else {
    top5events<-c(eventName)
  }

  #build graphs for the selected events
  for(EVENTNAME in top5events) {

    log_info(paste0('Building graph for: {EVENTNAME}'))

    #check that eventname is present in g
    if(EVENTNAME %in% V(g)$name) {

      constructed.graph<-Trajectories::filterIgraphCrossingEvent(g, eventname = EVENTNAME, limitOfNodes=limitOfNodes, edge_param_to_sort_by=edge_param_to_sort_by)
      title=paste0("Constructed most-likely trajectories in ",cohortName," cohort through\n",EVENTNAME,"\n(based on ",V(g)[V(g)$name==EVENTNAME]$count," patients and limited to ",limitOfNodes," events)")
      #Truncate the title for file name if it is too long
      truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
      Trajectories::plotIgraph(constructed.graph,layout=layout_with_fr,linknumbers=round(E(constructed.graph)$prob*100),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))


      #align actual trajectories to constructed graph
      limitOfTrajs=NA
      h<-Trajectories::alignActualTrajectoriesToGraph (connection=connection,
                                                       trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                                       trajectoryLocalArgs=trajectoryLocalArgs,
                                                       g=constructed.graph,
                                                       eventname=EVENTNAME,
                                                       limit=limitOfTrajs)
      #remove edges and nodes with count=0
      h<-h-E(h)[E(h)$alignedTrajsCount==0]
      h<-h-V(h)[V(h)$alignedTrajsCount==0]
      #E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[ends(h,E(h),names=F)[,1]]$alignedTrajsCount
      E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[V(h)$name==EVENTNAME]$alignedTrajsCount #probability relative to EVENTNAME
      title=paste0(ifelse(is.na(limitOfTrajs),'All ',''),V(h)[V(h)$name==EVENTNAME]$alignedTrajsCount," actual trajectories of ",cohortName," patients having/passing\n",EVENTNAME," (EVENT),\naligned to constructed graph of ",limitOfNodes," events (frequency relative to EVENT given on edges)")
      #Truncate the title for file name if it is too long
      truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))

      Trajectories::plotIgraph(h,layout=layout_with_fr,nodesizes=V(h)$alignedTrajsCount,linknumbers=round(E(h)$alignedTrajsProb*100),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
      title=paste0(ifelse(is.na(limitOfTrajs),'All ',''),V(h)[V(h)$name==EVENTNAME]$alignedTrajsCount," actual trajectories of ",cohortName," patients having/passing\n",EVENTNAME," (EVENT),\naligned to constructed graph of ",limitOfNodes," events (trajectory count on edge)")
      #Truncate the title for file name if it is too long
      truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
      Trajectories::plotIgraph(h,layout=layout_with_fr,nodesizes=V(h)$alignedTrajsCount,linknumbers=round(E(h)$alignedTrajsCount),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))


      x<-igraph::as_data_frame(h, what='edges')
      y<-igraph::as_data_frame(h, what='vertices')
      write.table(x,file=file.path(outputFolder,paste0(make.names(truncated_title),'.edges.csv')),quote=FALSE, sep="\t", col.names = NA)
      write.table(y,file=file.path(outputFolder,paste0(make.names(truncated_title),'.vertices.csv')),quote=FALSE, sep="\t", col.names = NA)


      #align actual trajectories to full graph (takes looong time)
      limitOfTrajs=10000
      h<-Trajectories::alignActualTrajectoriesToGraph (connection=connection,
                                                       trajectoryAnalysisArgs=trajectoryAnalysisArgs,
                                                       trajectoryLocalArgs=trajectoryLocalArgs,
                                                       g=g,
                                                       eventname=EVENTNAME,
                                                       limit=limitOfTrajs)
      #remove edges and nodes with count=0
      h<-h-E(h)[E(h)$alignedTrajsCount==0]
      h<-h-V(h)[V(h)$alignedTrajsCount==0]
      #E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[ends(h,E(h),names=F)[,1]]$alignedTrajsCount
      E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[V(h)$name==EVENTNAME]$alignedTrajsCount #probability relative to EVENTNAME
      title=paste0(ifelse(is.na(limitOfTrajs),'All ',limitOfTrajs)," actual trajectories of ",cohortName," patients having/passing\n",EVENTNAME," (EVENT),\naligned to full graph (frequency relative to EVENT given on edge)")
      #Truncate the title for file name if it is too long
      truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
      Trajectories::plotIgraph(h,layout=layout_with_fr,nodesizes=V(h)$alignedTrajsCount,linknumbers=round(E(h)$alignedTrajsProb*100),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))

      x<-igraph::as_data_frame(h, what='edges')
      y<-igraph::as_data_frame(h, what='vertices')
      write.table(x,file=file.path(outputFolder,paste0(make.names(truncated_title),'.edges.csv')),quote=FALSE, sep="\t", col.names = NA)
      write.table(y,file=file.path(outputFolder,paste0(make.names(truncated_title),'.vertices.csv')),quote=FALSE, sep="\t", col.names = NA)


    } else {
      log_warn(paste0('Cannot filter trajectories through {EVENTNAME} as the graph does not contain any links with that event. Return unfiltered graph.'))
    }

  } #for top5events

  log_info('All graphs saved.')

}

#' Creates a subgraph of edges that go through EVETNAME. Starts building it from EVENTNAME, adds most probable edge. Then takes both events together, adds most probable edge out from these two etc. Limits to limitOfNodes nodes.
#'
#' @param g
#' @param eventname
#' @param limitOfNodes
#'
#' @return
#' @export
#'
#' @examples
filterIgraphCrossingEvent <-function(g, eventname='clopidogrel',limitOfNodes=F, edge_param_to_sort_by=c('effect','numcohortExact','numcohortCustom','effectCount','prob')) {

  logger::log_info(paste0("Filtering trajectories from the graph that occur before or after {eventname} (sorted by ",edge_param_to_sort_by,')...'))
  if(limitOfNodes!=F) {log_info(paste0('  (also limiting to ',limitOfNodes,' nodes)'))}

  #check that eventname is present in g
  if(!eventname %in% V(g)$name) {
    logger::log_warn(paste0('Cannot filter trajectories through {eventname} as the graph does not contain any links with that event. Return unfiltered graph.'))
    return(g)
  }

  #If limitOfNodes=F, still give to it a numeric value (no limit = number of initial nodes/events)
  limitOfNodes=ifelse(limitOfNodes==F,length(E(g)),limitOfNodes)

  # add first item to stack
  stack<-data.frame(event=c(eventname),
                    edgeid=0,
                    effect=Inf,
                    prob=1,
                    totalprob=1,
                    numcohortExact=Inf,
                    #numcohortCustom=Inf,
                    effectCount=Inf,
                    direction=c('both'),
                    stringsAsFactors=FALSE)
  edge.ids<-c()
  i=1
  #logger::log_level(logger:::DEBUG)
  nodelist=data.frame()
  while(i<=nrow(stack) & i<limitOfNodes) {
    logger::log_debug(paste0('Stack size: ',i,'. Total probability of last element: ',round(100*tail(stack,n=1)$totalprob),'%'))
    #print(stack)
    #print(paste('i=',i))
    #if(i==30) stop()
    #for the analyzable node, find its incoming and/or outgoing edges

    if (i==1 | stack[i,'direction']=='ancestor') {
      #find incoming edges for node
      x<-incident_edges(g, v = V(g)[stack[stack$direction %in% c('both','ancestor'),'event']], mode = c("in"))
      if(!'numcohortCustom' %in% edge_attr_names(g)) {
        cc<-rep(0,length(x))
      } else {
        cc<-x$numcohortCustom
      }
      #put nodes to nodelist dataframe
      for(name in names(x)) {
        y<-x[[name]]
        if(length(y)>0) nodelist<-rbind(nodelist,
                                        data.frame(event=tail_of(g,y)$name,
                                                   edgeid=get.edge.ids(g, as.vector(t(ends(g,y, names=F)))),
                                                   effect=y$effect,
                                                   prob=y$prob,
                                                   #totalprob=y$prob*(data.frame(event=as.character(stack$event), prob=stack$totalprob, stringsAsFactors=F) %>% right_join( data.frame(from=ends(g,y, names=T)[,2], stringsAsFactors=F), by=c('event'='from')) %>% select(totalprob=prob)),
                                                   totalprob=y$prob*(stack %>% filter(direction %in% c('both','ancestor')) %>% select (event,prob) %>% right_join( data.frame(from=ends(g,y, names=T)[,2], stringsAsFactors=F), by=c('event'='from')) %>% select(totalprob=prob)),
                                                   numcohortExact=y$numcohortExact,
                                                   effectCount=y$effectCount,
                                                   direction='ancestor'))
      }
      #print(paste0('Nodelist after adding ancestors of ',stack[i,'event'],':'))
      #print(nodelist)
    }
    if (i==1 | stack[i,'direction']=='descendant') {
      #find outgoing edges from node
      x<-incident_edges(g, v = V(g)[stack[stack$direction %in% c('both','descendant'),'event']], mode = c("out"))
      if(!'numcohortCustom' %in% edge_attr_names(g)) {
        cc<-rep(0,length(x))
      } else {
        cc<-x$numcohortCustom
      }
      #put nodes to nodelist dataframe
      for(name in names(x)) {
        y<-x[[name]]
        if(length(y)>0) nodelist<-rbind(nodelist,
                                        data.frame(event=head_of(g,y)$name,
                                                   edgeid=get.edge.ids(g, as.vector(t(ends(g,y, names=F)))),
                                                   effect=y$effect,
                                                   prob=y$prob,
                                                   #totalprob=y$prob*(data.frame(event=as.character(stack$event), prob=stack$totalprob, stringsAsFactors=F) %>% right_join( data.frame(from=ends(g,y, names=T)[,1], stringsAsFactors=F), by=c('event'='from')) %>% select(totalprob=prob)),
                                                   totalprob=y$prob*(stack %>% filter(direction %in% c('both','descendant')) %>% select (event,prob) %>% right_join( data.frame(from=ends(g,y, names=T)[,1], stringsAsFactors=F), by=c('event'='from')) %>% select(totalprob=prob)),
                                                   numcohortExact=y$numcohortExact,
                                                   effectCount=y$effectCount,
                                                   direction='descendant'))
      }#print(paste0('Nodelist after adding descendants of ',stack[i,'event'],':'))
      #print(nodelist)
    }
    if(nrow(nodelist)>0) {
      #sorts nodes by decreasing order of effect/effectCount/numcohortsCustom or numcohortsExact
      nodelist<-nodelist %>% arrange(-totalprob)
      #avoid duplicates - if duplicate nodes exist, take the one with larger probability
      #Update: duplicates are allowed!
      #nodelist <- nodelist %>% group_by(event) %>% mutate(max_prob=max(totalprob)) %>% ungroup() %>% filter(totalprob==max_prob) %>% select(-max_prob)
      #remove edges that are already added to edgelist
      nodelist <- nodelist %>% filter(!edgeid %in% edge.ids)

      if(nrow(nodelist)>0) {
        for(j in 1:nrow(nodelist)) {
          n<-nodelist[j,]
          #add edge (even if the node already exists)
          if(!n$edgeid %in% edge.ids) edge.ids<-c(edge.ids,n$edgeid)
          #add node to the stack if it does not exist in the stack, remove from nodelist and break the loop
          if(!n$event %in% stack$event) {
            stack<-rbind(stack,n)
            nodelist<-nodelist[-j,]
            break;
          }
        }
      }


    }

    i<-i+1


    if(i>10000) {
      stop('Some error with building igraph. Too many iterations. Exiting to prevent infinite loop...')
    }
  }

  #create a new graph of selected nodes
  #g2 <- induced.subgraph(graph=g,vids=V(g)[stack$event])
  #create a new graph of selected edges
  g2 <- subgraph.edges(g, edge.ids[edge.ids>0], delete.vertices = T)

  log_info(paste0('...done. The resulting graph contains ',length(V(g2)),' events and ',length(E(g2)),' links between them.'))

  #the length of longest path is the diameter of the graph
  f<-farthest_vertices(g2, directed = TRUE, weights=NA)
  if(limitOfNodes==F) {
    logger::log_info(paste0('The longest trajectory has a length ',f$distance," (between '{f$vertices[1]$name}' and '{f$vertices[2]$name}')"))
  } else {
    logger::log_info(paste0('The longest trajectory (within the limit of ',limitOfNodes,' edges) has a length ',f$distance," (between '{f$vertices[1]$name}' and '{f$vertices[2]$name}')"))
  }
  #print(paste(shortest_paths(g2,from=V(g2)[f$vertices[1]],to=V(g2)[f$vertices[2]])$vpath))

  return(g2)
}



#' Takes top5 nodes from the graph (by count) and returns the corresponding node list
#'
#' @param g
#'
#' @return
#' @export
#'
#' @examples
getTop5Events<-function(g) {

  conceptorder<-order(-V(g)$count)
  conceptorder<-conceptorder[seq(1,min(length(conceptorder),5))]
  return(V(g)[conceptorder])

}


#' Title
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

  logger:log_info('Creating a plot of full graph (built from all event pairs)...')

  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  eventPairResultsFilename = file.path(outputFolder,'event_pairs.tsv')

  # create igraph object from event pairs
  g<-Trajectories::createGraph(eventPairResultsFilename)

  cohortName=trajectoryAnalysisArgs$cohortName


  # create a plot of all event pairs (no filtering)
  title=paste0('All significant directional event pairs among ',cohortName,' patients')
  #Truncate the title for file name if it is too long
  truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
  Trajectories::plotIgraph(g,layout=layout_with_fr,linknumbers=round(100*E(g)$prob),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))

  # Remove low-probability event pairs (keep 20, 50, 100 event pairs with highest probability)
  s=c(20,50,100)

  graphcounter=1
  for(limitOfLinks in s) {
    logger:log_info('Creating a plot of the same graph, but filtered to {limitOfLinks} high-probability pairs only...')
    graphcounter=graphcounter+1
    #limitOfLinks=50
    title=paste0(limitOfLinks,' high-probability event pairs among ',cohortName,' patients')
    h<-Trajectories::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g, limitOfLinks=limitOfLinks,edge_param_to_sort_by='prob')
    #Truncate the title for file name if it is too long
    truncated_title=ifelse(stri_length(title)<=200,title,paste(substr(title,1,200)))
    Trajectories::plotIgraph(h,layout=layout_with_fr,linknumbers=round(100*E(h)$prob),outputPdfFullpath=file.path(outputFolder,paste0(make.names(truncated_title),'.pdf')),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
  }
}
