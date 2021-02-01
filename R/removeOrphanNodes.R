#' Removes orphan nodes from igraph/TrajectoriesGraph object
#'
#' @param g igraph object
#'
#' @return
#' @export
#'
#' @examples
removeOrphanNodes<-function(g) {

  if(!inherits(g, 'igraph')) stop('Error in removeOrphanNodes(): object g is not inherited from igraph class')

  degrees<-igraph::degree(g, v=V(g), mode='all')
  g <- g - V(g)[which(degrees==0)]

}
