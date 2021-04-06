#' Reduces trajectories igraph object by removing low effect links and orphan nodes
#'
#' @param g igraph object
#' @param limitOfLinks limitOfLinks
#' @param edge_param_to_sort_by edge_param_to_sort_by
#'
#' @return
#' @export
#'
#' @examples
filterIgraphRemoveLowEffectLinksAndOrphanNodes<-function(g, limitOfLinks=20,edge_param_to_sort_by=c('effect','numcohortExact','numcohortCustom','effectCount','prob')) {
  logger::log_info('Filtering graph by removing links with small effect/people size and orphan nodes...')

  if(!inherits(g, 'TrajectoriesGraph')) stop('Error in filterIgraphRemoveLowEffectLinksAndOrphanNodes(): object g is not class TrajectoriesGraph object')

  #there is something to filter when there is at least 3 edges in the cluster
  if(length(igraph::E(g))<=2) return(g)

  if(limitOfLinks>length(igraph::E(g))) limitOfLinks=length(igraph::E(g))

  logger::log_info(paste0('...The original graph contains ',length(igraph::V(g)),' events and ',length(igraph::E(g)),' links between them...'))

  #igraph does not support Edge reordering. Therefore, to reorder, we create a new graph (with reordered edges)
  if(edge_param_to_sort_by=='numcohortExact') {
    #log_debug('Sort by numcohortExact')
    o<-order(igraph::E(g)$numcohortExact, decreasing=T)
  } else if(edge_param_to_sort_by=='numcohortCustom') {
    #log_debug('Sort by numcohortCustom')
    o<-order(igraph::E(g)$numcohortCustom, decreasing=T)
  } else if(edge_param_to_sort_by=='effectCount') {
    log_debug('Sort by effectCount')
    o<-order(igraph::E(g)$effectCount, decreasing=T)
  } else if(edge_param_to_sort_by=='prob') {
    log_debug('Sort by prob')
    o<-order(igraph::E(g)$prob, decreasing=T)
  } else if(edge_param_to_sort_by=='actualTrajsProb') {
    log_debug('Sort by actualTrajsProb')
    o<-order(igraph::E(g)$actualTrajsProb, decreasing=T)
  } else {
    #log_debug('Sort by effect')
    o<-order(abs(igraph::E(g)$effect-1), decreasing=T)
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

  logger::log_info(paste0('...done. The resulting graph contains ',length(igraph::V(g)),' events and ',length(igraph::E(g)),' links between them.'))

  class(g)<-c('TrajectoriesGraph','igraph')
  return(g)
}

