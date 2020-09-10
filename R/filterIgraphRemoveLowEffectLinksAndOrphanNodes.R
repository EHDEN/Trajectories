#' Title
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
  print('Filtering graph by removing links with small effect/people size and orphan nodes...')

  #there is something to filter when there is at least 3 edges in the cluster
  if(length(E(g))<=2) return(g)

  if(limitOfLinks>length(E(g))) limitOfLinks=length(E(g))

  print(paste0('...The original graph contains ',length(V(g)),' events and ',length(E(g)),' links between them...'))

  #igraph does not support Edge reordering. Therefore, to reorder, we create a new graph (with reordered edges)
  if(edge_param_to_sort_by=='numcohortExact') {
    #print('Sort by numcohortExact')
    o<-order(E(g)$numcohortExact, decreasing=T)
  } else if(edge_param_to_sort_by=='numcohortCustom') {
    #print('Sort by numcohortCustom')
    o<-order(E(g)$numcohortCustom, decreasing=T)
  } else if(edge_param_to_sort_by=='effectCount') {
    print('Sort by effectCount')
    o<-order(E(g)$effectCount, decreasing=T)
  } else if(edge_param_to_sort_by=='prob') {
    print('Sort by prob')
    o<-order(E(g)$prob, decreasing=T)
  } else if(edge_param_to_sort_by=='actualTrajsProb') {
    print('Sort by actualTrajsProb')
    o<-order(E(g)$actualTrajsProb, decreasing=T)
  } else {
    #print('Sort by effect')
    o<-order(E(g)$effect, decreasing=T)
  }
  #create a new graph from reordered edges (take only first limitOfLinks edges)
  g=graph_from_data_frame(d=igraph::as_data_frame(
    g,
    what=("edges"))[o,] %>% head(limitOfLinks),
    vertices=igraph::as_data_frame(g,what="vertices")
  )

  #remove all orphan nodes
  degrees<-degree(g, v=V(g), mode='all')
  g <- g - V(g)[which(degrees==0)]

  print(paste0('...done. The resulting graph contains ',length(V(g)),' events and ',length(E(g)),' links between them.'))
  return(g)
}

