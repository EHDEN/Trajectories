
#' Creates a subgraph of edges that go through EVETNAME. Starts building it from EVENTNAME, adds most probable edge. Then takes both events together, adds most probable edge out from these two etc. Limits to limitOfNodes nodes.
#'
#' @param g igraph object
#' @param eventname name of the event
#' @param limitOfNodes limitOfNodes
#' @param edge_param_to_sort_by edge_param_to_sort_by
#'
#' @return
#' @export
#'
#' @examples
filterTrajectoriesGraphCrossingEvent <-function(g, eventname=4283892,limitOfNodes=F, edge_param_to_sort_by=c('effect','numcohortExact','numcohortCustom','effectCount','prob')) {

  logger::log_info(paste0("Filtering trajectories from the graph that occur before or after {eventname} (sorted by ",edge_param_to_sort_by,')...'))
  if(limitOfNodes!=F) {log_info(paste0('  (also limiting to ',limitOfNodes,' nodes)'))}

  if(!inherits(g, 'TrajectoriesGraph')) stop('Error in filterTrajectoriesGraphCrossingEvent(): object g is not class TrajectoriesGraph object')

  #check that event concept_id is present in g
  eventname=as.character(eventname)
  if(!eventname %in% names(igraph::V(g))) {
    logger::log_warn(paste0('Cannot filter trajectories through {eventname} as the graph does not contain any links with that event. Return unfiltered graph.'))
    return(g)
  }

  #If limitOfNodes=F, still give to it a numeric value (no limit = number of initial nodes/events)
  limitOfNodes=ifelse(limitOfNodes==F,length(igraph::E(g)),limitOfNodes)

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
      x<-igraph::incident_edges(g, v = igraph::V(g)[stack[stack$direction %in% c('both','ancestor'),'event']], mode = c("in"))
      if(!'numcohortCustom' %in% igraph::edge_attr_names(g)) {
        cc<-rep(0,length(x))
      } else {
        cc<-x$numcohortCustom
      }
      #put nodes to nodelist dataframe
      for(name in names(x)) {
        y<-x[[name]]
        if(length(y)>0) nodelist<-rbind(nodelist,
                                        data.frame(event=as.character(igraph::tail_of(g,y)$name),
                                                   edgeid=igraph::get.edge.ids(g, as.vector(t(igraph::ends(g,y, names=F)))),
                                                   effect=y$effect,
                                                   prob=y$prob,
                                                   #totalprob=y$prob*(data.frame(event=as.character(stack$event), prob=stack$totalprob, stringsAsFactors=F) %>% right_join( data.frame(from=ends(g,y, names=T)[,2], stringsAsFactors=F), by=c('event'='from')) %>% select(totalprob=prob)),
                                                   totalprob=y$prob*(stack %>% filter(direction %in% c('both','ancestor')) %>% select (event,prob) %>% right_join( data.frame(from=as.character(igraph::ends(g,y, names=T)[,2]), stringsAsFactors=F), by=c('event'='from')) %>% select(totalprob=prob)),
                                                   numcohortExact=y$numcohortExact,
                                                   effectCount=y$effectCount,
                                                   direction='ancestor'))
      }
      #print(paste0('Nodelist after adding ancestors of ',stack[i,'event'],':'))
      #print(nodelist)
    }
    if (i==1 | stack[i,'direction']=='descendant') {
      #find outgoing edges from node
      x<-igraph::incident_edges(g, v = igraph::V(g)[stack[stack$direction %in% c('both','descendant'),'event']], mode = c("out"))
      if(!'numcohortCustom' %in% igraph::edge_attr_names(g)) {
        cc<-rep(0,length(x))
      } else {
        cc<-x$numcohortCustom
      }
      #put nodes to nodelist dataframe
      for(name in names(x)) {
        y<-x[[name]]
        if(length(y)>0) nodelist<-rbind(nodelist,
                                        data.frame(event=as.character(igraph::head_of(g,y)$name),
                                                   edgeid=igraph::get.edge.ids(g, as.vector(t(igraph::ends(g,y, names=F)))),
                                                   effect=y$effect,
                                                   prob=y$prob,
                                                   #totalprob=y$prob*(data.frame(event=as.character(stack$event), prob=stack$totalprob, stringsAsFactors=F) %>% right_join( data.frame(from=ends(g,y, names=T)[,1], stringsAsFactors=F), by=c('event'='from')) %>% select(totalprob=prob)),
                                                   totalprob=y$prob*(stack %>% filter(direction %in% c('both','descendant')) %>% select (event,prob) %>% right_join( data.frame(from=as.character(igraph::ends(g,y, names=T)[,1]), stringsAsFactors=F), by=c('event'='from')) %>% select(totalprob=prob)),
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
  g2 <- igraph::subgraph.edges(g, edge.ids[edge.ids>0], delete.vertices = T)

  log_info(paste0('...done. The resulting graph contains ',length(igraph::V(g2)),' events and ',length(igraph::E(g2)),' links between them.'))

  #the length of longest path is the diameter of the graph
  f<-igraph::farthest_vertices(g2, directed = TRUE, weights=NA)
  if(limitOfNodes==F) {
    logger::log_info(paste0('The longest trajectory has a length ',f$distance," (between '{f$vertices[1]$name}' and '{f$vertices[2]$name}')"))
  } else {
    logger::log_info(paste0('The longest trajectory (within the limit of ',limitOfNodes,' edges) has a length ',f$distance," (between '{f$vertices[1]$name}' and '{f$vertices[2]$name}')"))
  }
  #print(paste(shortest_paths(g2,from=V(g2)[f$vertices[1]],to=V(g2)[f$vertices[2]])$vpath))

  # make it of the class TrajectoriesGraph which is derived from the class igraph
  class(g2) <- c("TrajectoriesGraph","igraph")
  return(g2)
}
