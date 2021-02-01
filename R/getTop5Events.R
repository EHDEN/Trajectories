getTop5Events<-function(g) {

  conceptorder<-order(-igraph::V(g)$count)
  conceptorder<-conceptorder[seq(1,min(length(conceptorder),5))]
  return(igraph::V(g)[conceptorder])

}
