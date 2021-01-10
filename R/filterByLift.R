#' Filtering edges by lift
#'
#' @param g igraph object
#' @param lift.threshold lift.threshold
#'
#' @return
#' @export
#'
#' @examples
filterByLift <- function(g, lift.threshold=2) {
  DEBUG=F

  log_info(paste0('Analyzing graph by lift to remove transient edges having lift < ',lift.threshold,'...'))

  EDGE_REMOVED=T
  num_edges_removed=0

  last_A_id<-0

  s<-0
  while(EDGE_REMOVED==T) {
    #print(s)
    s<-s+1
    EDGE_REMOVED=F
    for(A_id in (last_A_id+1):length(V(g))) {

      A_name=V(g)[A_id]$name
      if(DEBUG) log_debug(paste0('Analyzing ',A_name,'...'))
      #all outgoing edges from initial node A
      edges_from_A <- incident(g, v = V(g)[A_id], mode = c("out"))
      B_and_C<-head_of(g, edges_from_A)
      if(length(B_and_C)>0) {
        if(DEBUG) log_debug(paste0('Its descendants: ',paste(V(g)[B_and_C]$name, collapse=", ")))
        for(j in 1:length(B_and_C)) {
          #print(j)
          B_name=B_and_C[j]$name
          B_id<-which(V(g)$name==B_name)
          p_AB=edges_from_A[j]$prob
          if(DEBUG) log_debug(paste0('  Analyzing ',A_name,'->',B_name,' (prob=',round(p_AB*100,1),'%)...'))
          #find outgoing edges from B
          edges_from_B <- incident(g, v = V(g)[B_id], mode = c("out"))
          C<-head_of(g, edges_from_B)
          if(length(C)>0) {
            for(k in 1:length(C)) {
              C_name=C[k]$name
              C_id=which(V(g)$name==C_name)
              p_BC=edges_from_B[k]$prob

              pACpBC=p_AB*p_BC

              if(DEBUG) log_debug(paste0('     Analyzing ',B_name,'->',C_name,' (prob=',round(p_BC*100,1),'%) (from ',A_name,' prob=',round(pACpBC*100,1),'%)...'))
              if(V(g)[C_id] %in% B_and_C) {

                p_AC=E(g)$prob[E(g)$e1_concept_id==V(g)[A_id]$concept_id & E(g)$e2_concept_id==V(g)[C_id]$concept_id]
                if(DEBUG) log_debug(paste0('     There also exists a direct link from ',A_name,' to ',C_name,' (prob=',round(p_AC*100,1),'%)...'))
                lift=p_AC/pACpBC
                #print(paste0('     ',lift))
                if(lift<lift.threshold) {
                  log_info(paste0('     ',A_id,'/',length(V(g)),' REMOVE direct link from ',A_name,' to ',C_name,' (prob=',round(p_AC*100,1),'%) as there is also a link through ',B_name,' (prob=',round(pACpBC*100,1),'%) (lift=',round(lift,1),')...'))
                  edge_to_remove=paste0(A_name,'|',C_name)
                  g<-g %>% delete_edges(edge_to_remove)
                  EDGE_REMOVED=T
                  num_edges_removed<-num_edges_removed+1
                  break #start from the beginning
                }
              }
            }
            if(EDGE_REMOVED==T) break
          }

        }
        if(EDGE_REMOVED==T) break
      }
      #This node was analyzed so that no edge was removed. Write it down to avoid reanalyzing nodes up to this point when some edges will be moved in next node
      last_A_id<-min(A_id,length(V(g))-1)
    }
    if(s>1000) {
      stop('Too many iterations in function filterByLift()')
    }
  } #while
  log_info(paste0(num_edges_removed,' edges removed from the graph by lift analysis, ',length(E(g)),' edges remaining.'))
  return(g)
}

