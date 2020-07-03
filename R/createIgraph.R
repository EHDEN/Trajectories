library(ff)
library(ffbase)
library(igraph)
library(dplyr)
#' Title
#'
#' @param eventPairResultsFilename
#' @param packageName
#' @param connection
#' @param resultsSchema
#' @param prefixForResultTableNames
#' @param sqlRole Database role that is used when creating tables to 'resultsSchema'. Set to F if a specific role is not needed.
#' @param cohortName
#' @param eventName Exact concept name that is used for building trajectories. Must exist in event pairs data table. If not specified (NA) (recommended) creates trajectories for top 5 events.
#' @param outputFolder Output folder (should exist) without trailing slash
#'
#' @return
#' @export
#'
#' @examples
createIgraph<-function(packageName,
                       connection,
                       sqlRole=F,
                       resultsSchema,
                       prefixForResultTableNames,
                       eventPairResultsFilename,
                       outputFolder,
                       cohortName="",
                       eventName=NA) {

  # create igraph object from event pairs
  g<-Trajectories::createGraph(eventPairResultsFilename)


  COHORTNAME=cohortName
  OUTPUTFOLDER=paste0(outputFolder,"/")





  # create plot of all event pairs (no filtering)
  title=paste0('All significant directional event pairs among ',COHORTNAME,' patients')
  Trajectories::plotIgraph(g,layout=layout_with_fr,linknumbers=round(100*E(g)$prob),outputPdfFullpath=paste0(OUTPUTFOLDER,make.names(title),'.pdf'),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))

  # Remove low-probability event pairs (keep 50 event pairs with highest probability)
  s=c(20,50)
  for(limitOfLinks in s) {
    #limitOfLinks=50
    h<-Trajectories::filterIgraphRemoveLowEffectLinksAndOrphanNodes(g, limitOfLinks=limitOfLinks,edge_param_to_sort_by='prob')
    title=paste0(limitOfLinks,' high-probability event pairs among ',COHORTNAME,' patients')
    Trajectories::plotIgraph(h,layout=layout_with_fr,linknumbers=round(100*E(h)$prob),outputPdfFullpath=paste0(OUTPUTFOLDER,make.names(title),'.pdf'),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
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

    #check that eventname is present in g
    if(EVENTNAME %in% V(g)$name) {

      constructed.graph<-Trajectories::filterIgraphCrossingEvent(g, eventname = EVENTNAME, limitOfNodes=limitOfNodes, edge_param_to_sort_by=edge_param_to_sort_by)
      title=paste0("Constructed most-likely trajectories in ",COHORTNAME," cohort through\n",EVENTNAME,"\n(based on ",V(g)[V(g)$name==EVENTNAME]$count," patients and limited to ",limitOfNodes," events)")
      Trajectories::plotIgraph(constructed.graph,layout=layout_with_fr,linknumbers=round(E(constructed.graph)$prob*100),outputPdfFullpath=paste0(OUTPUTFOLDER,make.names(title),'.pdf'),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))


      #align actual trajectories to constructed graph
      limitOfTrajs=NA
      h<-Trajectories::alignActualTrajectoriesToGraph (packageName=packageName,
                                                              connection=connection,
                                                              sqlRole=sqlRole,
                                                              resultsSchema=resultsSchema,
                                                              prefixForResultTableNames=prefixForResultTableNames,
                                                              g=constructed.graph,
                                                              eventname=EVENTNAME,
                                                              limit=limitOfTrajs)
      #remove edges and nodes with count=0
      h<-h-E(h)[E(h)$alignedTrajsCount==0]
      h<-h-V(h)[V(h)$alignedTrajsCount==0]
      #E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[ends(h,E(h),names=F)[,1]]$alignedTrajsCount
      E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[V(h)$name==EVENTNAME]$alignedTrajsCount #probability relative to EVENTNAME
      title=paste0(ifelse(is.na(limitOfTrajs),'All ',''),V(h)[V(h)$name==EVENTNAME]$alignedTrajsCount," actual trajectories of ",COHORTNAME," patients having/passing\n",EVENTNAME," (EVENT),\naligned to constructed graph of ",limitOfNodes," events (frequency relative to EVENT given on edges)")
      Trajectories::plotIgraph(h,layout=layout_with_fr,nodesizes=V(h)$alignedTrajsCount,linknumbers=round(E(h)$alignedTrajsProb*100),outputPdfFullpath=paste0(OUTPUTFOLDER,make.names(title),'.pdf'),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))
      title=paste0(ifelse(is.na(limitOfTrajs),'All ',''),V(h)[V(h)$name==EVENTNAME]$alignedTrajsCount," actual trajectories of ",COHORTNAME," patients having/passing\n",EVENTNAME," (EVENT),\naligned to constructed graph of ",limitOfNodes," events (trajectory count on edge)")
      Trajectories::plotIgraph(h,layout=layout_with_fr,nodesizes=V(h)$alignedTrajsCount,linknumbers=round(E(h)$alignedTrajsCount),outputPdfFullpath=paste0(OUTPUTFOLDER,make.names(title),'.pdf'),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))


      x<-igraph::as_data_frame(h, what='edges')
      y<-igraph::as_data_frame(h, what='vertices')
      write.table(x,file=paste0(OUTPUTFOLDER,make.names(title),'.edges.csv'),quote=FALSE, sep="\t", col.names = NA)
      write.table(y,file=paste0(OUTPUTFOLDER,make.names(title),'.vertices.csv'),quote=FALSE, sep="\t", col.names = NA)


      #align actual trajectories to full graph (takes looong time)
      limitOfTrajs=10000
      h<-Trajectories::alignActualTrajectoriesToGraph (packageName=packageName,
                                                              connection=connection,
                                                              sqlRole=sqlRole,
                                                              resultsSchema=resultsSchema,
                                                              prefixForResultTableNames=prefixForResultTableNames,
                                                              g=g,
                                                              eventname=EVENTNAME,
                                                              limit=limitOfTrajs)
      #remove edges and nodes with count=0
      h<-h-E(h)[E(h)$alignedTrajsCount==0]
      h<-h-V(h)[V(h)$alignedTrajsCount==0]
      #E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[ends(h,E(h),names=F)[,1]]$alignedTrajsCount
      E(h)$alignedTrajsProb=E(h)$alignedTrajsCount/V(h)[V(h)$name==EVENTNAME]$alignedTrajsCount #probability relative to EVENTNAME
      title=paste0(ifelse(is.na(limitOfTrajs),'All ',limitOfTrajs)," actual trajectories of ",COHORTNAME," patients having/passing\n",EVENTNAME," (EVENT),\naligned to full graph (frequency relative to EVENT given on edge)")
      Trajectories::plotIgraph(h,layout=layout_with_fr,nodesizes=V(h)$alignedTrajsCount,linknumbers=round(E(h)$alignedTrajsProb*100),outputPdfFullpath=paste0(OUTPUTFOLDER,make.names(title),'.pdf'),title=paste0(title,"\n",format(Sys.time(), '%d %B %Y %H:%M')))

      x<-igraph::as_data_frame(h, what='edges')
      y<-igraph::as_data_frame(h, what='vertices')
      write.table(x,file=paste0(OUTPUTFOLDER,make.names(title),'.edges.csv'),quote=FALSE, sep="\t", col.names = NA)
      write.table(y,file=paste0(OUTPUTFOLDER,make.names(title),'.vertices.csv'),quote=FALSE, sep="\t", col.names = NA)


    } else {
      print(paste0('Cannot filter trajectories through ',EVENTNAME,' as the graph does not contain any links with that event. Return unfiltered graph.'))
    }

  } #for top5events

print('All graphs saved.')

}


#' Title
#'
#' @param g
#'
#' @return
#' @export
#'
#' @examples
filterIgraphRemoveLowEffectLinks<-function(g, edge_param_to_sort_by=c('effect','numcohortExact','numcohortCustom')) {
  print('Filtering graph by removing links with small effect size so that all events are still kept connected.')

  #there is something to filter when there is at least 3 edges in the cluster
  if(length(E(g))<=2) return(g)

  orig_num_nodes=length(V(g))

  newgraph<-g
  #sort edges by effect size (increasing)


  #The initial graph might be not connected (it may contain several unconnected clusters)
  #We anayze the graph cluster-by-cluster
  #Within each cluster (cluster is a part of a graph which is (weakly) connected), we try to remove edges (starting with smallest effect size) so that the cluster is still connected
  #the following command assigns a cluster for each element
  clu <- components(g,mode="weak")
  #if there are more than 1 cluster
  for(cluster in 1:clu$no) {
    subgraph<-induced_subgraph(g,vids=V(g)[clu$membership==cluster])
    if(length(E(g))>3) { #there is something to filter/optimize when there is at least 3 edges in the cluster
      new.subgraph<-subgraph
      if(edge_param_to_sort_by=='numcohortExact') {
        print('Sort by numcohortExact')
        filtered_edges<-E(subgraph)[order(E(subgraph)$numcohortExact, decreasing=F)]
      } else if(edge_param_to_sort_by=='numcohortCustom') {
        print('Sort by numcohortCustom')
        filtered_edges<-E(subgraph)[order(E(subgraph)$numcohortCustom, decreasing=F)]
      } else {
        print('Sort by effect')
        filtered_edges<-E(subgraph)[order(E(subgraph)$effect, decreasing=F)]
      }
      for(e in filtered_edges) {
        #rint(paste0('Analyzing ',tail_of(subgraph,e)$name,'->',head_of(subgraph,e)$name,' (effect=',round(E(subgraph)[e]$effect,3),', numcohortExact=',round(E(subgraph)[e]$numcohortExact,3),')...'))
        new.subgraph.candidate<-new.subgraph - edge(paste0(tail_of(subgraph,e)$name,'|',head_of(subgraph,e)$name))
        if(is_connected(new.subgraph.candidate, mode="weak")) {
          #print(paste0('Removing ',tail_of(subgraph,e)$name,'->',head_of(subgraph,e)$name,' (effect=',round(E(subgraph)[e]$effect,3),', numcohortExact=',round(E(subgraph)[e]$numcohortExact,3),') as the graph is still weakly connected'))
          new.subgraph<-new.subgraph.candidate
          newgraph<-newgraph - edge(paste0(tail_of(subgraph,e)$name,'|',head_of(subgraph,e)$name))
        }
        if(length(V(newgraph))<orig_num_nodes) {
          stop('SOMETHING IS WRONG IN FUNCTION filterIgraphRemoveLowEffectLinks')
        }
      }
    }
    #plot(newgraph)


  }


  print(paste0('...done. The resulting graph contains ',length(V(newgraph)),' events and ',length(E(newgraph)),' links between them.'))
  return(newgraph)
}







#' Adds numcohortCustom value to graph edges - actual number of people (out of all people who have EVENTNAME) on that edge
#'
#' @param g
#' @param eventname
#' @param actualTrajs
#'
#' @return
#' @export
#'
#' @examples
addNumcohortCustom <- function(g, eventname, actualTrajs) {


  #actualTrajs<-read.csv2.ffdf(file = "/Users/sulevr/temp/cohorts_of_pairs.tsv", sep = '\t', header = TRUE) #we use ff-package here to avoid reading the whole file into memory

  print(paste0('Aligning actual event trajectories (of people who have actually had <',eventname,'>) to the graph (',length(E(g)),' edges in total)...'))


  #concept_id of eventid
  concept_id<-V(g)[eventname]$concept_id


  #find cohorts that go through eventname (these are the cohorts that we use for each pair)
  cohorts<-unique(actualTrajs[actualTrajs$EVENT1_CONCEPT_ID==concept_id | actualTrajs$EVENT2_CONCEPT_ID==concept_id,'COHORT_ID'])[]

      ##remove all other cohorts from actualTrajs
  tt<-actualTrajs[actualTrajs$COHORT_ID[] %in% cohorts,]


  for(i in 1:length(E(g))) {
    e<-ends(g,E(g)[i],names=F)
    all_cohorts_in_this_pair<- tt[    tt$EVENT1_CONCEPT_ID==V(g)$concept_id[e[1,1]] & tt$EVENT2_CONCEPT_ID==V(g)$concept_id[e[1,2]] ,'COHORT_ID']
    num_actual_cohorts<-length(all_cohorts_in_this_pair)
    E(g)[i]$numcohortCustom=num_actual_cohorts
  }
  print('...done. Actual counts saved to numcohortCustom element of the graph edges.')


  return(g)
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

  print(paste0('Filtering trajectories from the graph that occur before or after ',eventname,' (sorted by ',edge_param_to_sort_by,')...'))
  if(limitOfNodes!=F) {print(paste0('  (also limiting to ',limitOfNodes,' nodes)'))}

  #check that eventname is present in g
  if(!eventname %in% V(g)$name) {
    print(paste0('Cannot filter trajectories through ',eventname,' as the graph does not contain any links with that event. Return unfiltered graph.'))
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
  while(i<=nrow(stack) & i<limitOfNodes) {
    print(paste0('Stack size: ',i,'. Total probability of last element: ',round(100*tail(stack,n=1)$totalprob),'%'))
    #print(stack)
    #print(paste('i=',i))
    #if(i==30) stop()
    #for the analyzable node, find its incoming and/or outgoing edges
    nodelist=data.frame()
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
          #add node to the stack if it does not exist in the stack and break the loop
          if(!n$event %in% stack$event) {
            stack<-rbind(stack,n)
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

  print(paste0('...done. The resulting graph contains ',length(V(g2)),' events and ',length(E(g2)),' links between them.'))

  #the length of longest path is the diameter of the graph
  f<-farthest_vertices(g2, directed = TRUE, weights=NA)
  if(limitOfNodes==F) {
    print(paste0('The longest trajectory has a length ',f$distance,' (between ',f$vertices[1]$name,' and ',f$vertices[2]$name,'):'))
  } else {
    print(paste0('The longest trajectory (within the limit of ',limitOfNodes,' edges) has a length ',f$distance,' (between ',f$vertices[1]$name,' and ',f$vertices[2]$name,'):'))
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
