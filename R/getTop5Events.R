#' Takes most prevalent top5 nodes from the graph (by count) and returns the corresponding node list
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