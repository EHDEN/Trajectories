#' Builds a TrajectoriesGraph object based on event pairs data in "eventPairResultsFilename" file
#'
#' It is actually an igraph object but has some specific attributes (counts, colors) necessary for Trajectories package
#'
#' Vertex attributes:
#' (ID=concept_name), concept_id, count, color, labelcolor
#'
#' Edge attributes:
#' e1 (name), e1_concept_id, e2 (name), e2_concept_id, e1_count, effect, prob, numcohortExact. numcohortExact is a number of event periods that had E1->E2 as immediate order (no intermediate events).
#'
#' @param eventPairResultsFilename Full path to event pairs file
#'
#' @return
#'
#' @examples
createTrajectoriesGraph<-function(eventPairResultsFilename) {

  ParallelLogger::logInfo('Creating TrajectoriesGraph object based on event pairs data from the following file: ',eventPairResultsFilename,'...')

  # Links are coming from eventPairResultsFilename (event1->event2 pairs)
  e = read.csv2(file = eventPairResultsFilename, sep = '\t', header = TRUE, as.is=T)

  # convert chr columns to numeric
  e$RR<-as.numeric(e$RR)

  ParallelLogger::logInfo('There are ',nrow(e),' event pairs in the file.')

  #apply minimum RR threshold (there is no sense to draw graphs for decreasing risks - hard to interpret)
  e <- e %>% dplyr::filter(RR > 1)
  ParallelLogger::logInfo(nrow(e),' pairs remained after applying relative risk > 1 filter.')

  ParallelLogger::logInfo('Building TrajectoriesGraph object from these pairs...')


  edges <- e %>%
    dplyr::mutate(from=as.character(E1_CONCEPT_ID),
                  to=as.character(E2_CONCEPT_ID),
                  prob=E1_BEFORE_E2_COUNT_IN_EVENTS/E1_COUNT_IN_EVENTS,
                  numcohortExact=NA,
                  alignedTrajsProb=NA) %>%
      dplyr::select(from,
                    to,
                    e1_concept_id=E1_CONCEPT_ID,
                    e2_concept_id=E2_CONCEPT_ID,
                    e1=E1_NAME,
                    e2=E2_NAME,
                    e1_count=E1_COUNT_IN_EVENTS,
                    prob,
                    effect=RR,
                    numcohortExact,
                    alignedTrajsProb)

  e1s <- e %>%
    dplyr::select(
      concept_id=E1_CONCEPT_ID,
      concept_name=E1_NAME,
      domain=E1_DOMAIN,
      count=E1_COUNT_IN_EVENTS) %>%
    unique()

  e2s <- e %>%
    dplyr::select(
      concept_id=E2_CONCEPT_ID,
      concept_name=E2_NAME,
      domain=E2_DOMAIN,
      count=E2_COUNT_IN_EVENTS) %>%
    unique()

  vertices <- rbind(e1s,e2s) %>%
    dplyr::group_by(concept_id, concept_name, domain) %>% #we could also do simply unique() here to get one row per the same event as the numbers should be the same,
    dplyr::summarise(count=max(count), .groups = "drop")  #but to avoid any surprises we do group_by and count=max()

  #Add colors
  # predefined colors
  COLORS=list(Condition='#F1948A', #red
              Observation='#85C1E9', #blue
              Procedure='#ccc502',#yellow
              Drug='#8fba75', #green
              Unknown='#bbbbbb') #gray
  #Drug='#ABEBC6') #green
  LABELCOLORS=list(Condition='#7B241C',
                   Observation='#21618C',
                   Procedure='#7D6608',
                   Drug='#145A32', #green
                   Unknown='#666666')
  vertices <- vertices %>%
     dplyr::mutate(name=as.character(concept_id),
                   concept_id=concept_id, #this is needed, otherwise somehow the concept_id column disappears
                   color = recode(domain, !!!COLORS, .default = '#bbbbbb', .missing='#bbbbbb'),
                   labelcolor = recode(domain, !!!LABELCOLORS, .default = '#666666', .missing='#666666')
                   ) %>%
    dplyr::select(name,concept_id,concept_name,domain,count,color,labelcolor)


  #Build a graph object from edges and vertices
  g<-igraph::graph_from_data_frame(edges, directed=T, vertices=vertices)


  # ADD COLORS TO EDGES ALSO (based on the end node of the edge)

  # if there are at least 1 edge
  if(igraph::gsize(g)>0) {

    igraph::E(g)$effectCount=igraph::E(g)$e1_count*igraph::E(g)$effect

    #make edge color equal to target node color
    edge.end <- igraph::ends(g, es=igraph::E(g), names=F)[,2] #outputs the end node id of each edge
    igraph::E(g)$color <- igraph::V(g)$color[edge.end]
    #but make it a bit lighter
    rgb1<-sapply(igraph::E(g)$color ,col2rgb)

    rgb1<-rgb1*1.2
    rgb1<-ifelse(rgb1>255,255,rgb1)

    rgb2<-rgb(rgb1[1,],rgb1[2,],rgb1[3,],alpha=1,maxColorValue=255)
    igraph::E(g)$color <- rgb2
  }

  ParallelLogger::logInfo('...done. Full graph contains ',igraph::gsize(g),' links between ',igraph::gorder(g),' events.')

  # make it of the class TrajectoriesGraph which is derived from the class igraph
  class(g) <- c("TrajectoriesGraph","igraph")

  return(g)

}
