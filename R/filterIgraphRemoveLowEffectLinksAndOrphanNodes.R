#' Reduces trajectories igraph object by removing low effect links and orphan nodes
#'
#' @param g igraph object
#' @param limitOfLinks limitOfLinks
#' @param edge_param_to_sort_by edge_param_to_sort_by
#'
#' @return
#'
#' @examples
filterIgraphRemoveLowEffectLinksAndOrphanNodes<-function(g, limitOfLinks=20,edge_param_to_sort_by='effect') {
  ParallelLogger::logInfo('Filtering graph by removing links with small effect/people size and orphan nodes...')

  if(!inherits(g, 'TrajectoriesGraph')) stop('Error in filterIgraphRemoveLowEffectLinksAndOrphanNodes(): object g is not class TrajectoriesGraph object')

  #there is something to filter when there is at least 3 edges in the cluster
  if(length(igraph::E(g))<=2) return(g)

  if(limitOfLinks>length(igraph::E(g))) limitOfLinks=length(igraph::E(g))

  ParallelLogger::logInfo('...The original graph contains ',length(igraph::V(g)),' events and ',length(igraph::E(g)),' links between them...')

  if(!edge_param_to_sort_by %in% igraph::edge_attr_names(g)) {
    ParallelLogger::logError('Error in filterIgraphRemoveLowEffectLinksAndOrphanNodes(): edge_param_to_sort_by="',edge_param_to_sort_by,'" but there is no such edge attribute in igraph object')
  }

  #igraph does not support Edge reordering. Therefore, to reorder, we create a new graph (with reordered edges)
  if(edge_param_to_sort_by=='effect') {
    o<-order(abs(igraph::E(g)$effect-1), decreasing=T)
  } else {
    o<-order(igraph::edge_attr(g, edge_param_to_sort_by), decreasing=T)
  }

  #create a new graph from reordered edges (take only first limitOfLinks edges)
  g=igraph::graph_from_data_frame(d=igraph::as_data_frame(
    g,
    what=("edges"))[o,] %>% head(limitOfLinks),
    vertices=igraph::as_data_frame(g,what="vertices")
  )

  #remove all orphan nodes
  degrees<-igraph::degree(g, v=igraph::V(g), mode='all')
  g <- g - igraph::V(g)[which(degrees==0)]

  ParallelLogger::logInfo('...done. The resulting graph contains ',length(igraph::V(g)),' events and ',length(igraph::E(g)),' links between them.')

  class(g)<-c('TrajectoriesGraph','igraph')
  return(g)
}

