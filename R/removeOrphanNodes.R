library(SqlRender)
library(igraph)

#' Creates force network graph from the event pairs
#'
#' @param g igraph object
#'
#' @return
#' @export
#'
#' @examples
removeOrphanNodes<-function(g) {

  degrees<-degree(g, v=V(g), mode='all')
  g <- g - V(g)[which(degrees==0)]

}
