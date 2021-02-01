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
#' @export
#'
#' @examples
createTrajectoriesGraph<-function(eventPairResultsFilename) {

  logger::log_info('Creating TrajectoriesGraph object based on event pairs data from the following file: {eventPairResultsFilename}...')

  # Links are coming from eventPairResultsFilename (event1->event2 pairs)
  e = read.csv2(file = eventPairResultsFilename, sep = '\t', header = TRUE, as.is=T)

  # convert chr columns to numeric
  e$RR<-as.numeric(e$RR)
  #e$AVG_NUMBER_OF_DAYS_BETWEEN_E1_AND_E2<-as.numeric(e$AVG_NUMBER_OF_DAYS_BETWEEN_E1_AND_E2)

  #apply minimum RR threshold (there is no sense to draw graphs for decreasing risks - hard to interpret)
  e <- e %>% filter(RR > 1)
  logger::log_info('Applying relative risk filter: only pairs having relative risk > 1 are used for building the trajectories graph.')

  # calculate max event count for scaling
  max_event_count<-max(e$E1_COUNT_IN_EVENTS,e$E2_COUNT_IN_EVENTS)

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

  # BUILD A GRAPH!
  # create empty graph
  g <- igraph::make_empty_graph(directed = TRUE)
  #add edges between these nodes
  if(nrow(e)>0) {
    for(i in seq(1,nrow(e))) {
      e1<-e[i,'E1_NAME']
      e2<-e[i,'E2_NAME']
      # add vertexes if not exist already
      if(!e1 %in% V(g)$name) {g <- g + igraph::vertices(e1,
                                                concept_id=e[i,'E1_CONCEPT_ID'],
                                                count=e[i,'E1_COUNT_IN_EVENTS'],
                                                #size=e[i,'E1_COUNT']/max_event_count,
                                                color=if(is.na(e[i,'E1_DOMAIN']) | !e[i,'E1_DOMAIN'] %in% names(COLORS)) {COLORS[['Unknown']]} else {COLORS[[e[i,'E1_DOMAIN']]]},
                                                labelcolor=if(is.na(e[i,'E1_DOMAIN']) | !e[i,'E1_DOMAIN'] %in% names(LABELCOLORS)) {LABELCOLORS[['Unknown']]} else {LABELCOLORS[[e[i,'E1_DOMAIN']]]}
                                                #, age=AGES[AGES$event==e1,'AGE_FOR_GRAPH']
                                                )
      }
      if(!e2 %in% V(g)$name) {g <- g + igraph::vertices(e2,
                                                concept_id=e[i,'E2_CONCEPT_ID'],
                                                count=e[i,'E2_COUNT_IN_EVENTS'],
                                                #size=e[i,'E2_COUNT']/max_event_count,
                                                color=if(is.na(e[i,'E2_DOMAIN']) | !e[i,'E2_DOMAIN'] %in% names(COLORS)) {COLORS[['Unknown']]} else {COLORS[[e[i,'E2_DOMAIN']]]},
                                                labelcolor=if(is.na(e[i,'E2_DOMAIN']) | !e[i,'E2_DOMAIN'] %in% names(LABELCOLORS)) {LABELCOLORS[['Unknown']]} else {LABELCOLORS[[e[i,'E2_DOMAIN']]]}
                                                #, age=AGES[AGES$event==e2,'AGE_FOR_GRAPH']
                                                )
      }
      # add edge
      g <- g + igraph::edge(e1,
                    e2,
                    e1=e1,
                    e1_concept_id=e[i,'E1_CONCEPT_ID'],
                    e2_concept_id=e[i,'E2_CONCEPT_ID'],
                    e2=e2,
                    e1_count=e[i,'E1_COUNT_IN_EVENTS'],
                    effect=e[i,'RR'],
                    prob=e[i,'E1_BEFORE_E2_COUNT_IN_EVENTS']/e[i,'E1_COUNT_IN_EVENTS'],
                    #weight=1/e[i,'EVENT_PAIR_EFFECT'], #opposite to the effect size. Do not use weight attribute, as it has a special meaning in igraph and we do not want to use this automatically
                    #numcohortExact=e[i,'EVENTPERIOD_COUNT_HAVING_E2_RIGHT_AFTER_E1'] #number of event periods that had E1->E2 as immediate order (no intermediate events)
                    numcohortExact=NA
                    #numcohort=e[i,'EVENT1_EVENT2_COHORT_COUNT']/e[i,'EVENT1_COUNT']*e[i,'EVENT1_COUNT']/max_event_count #number of cohorts that had E1->E2, adjusted to but may have had intermediate events also
                    #numcohort=e[i,'EVENT1_EVENT2_COHORT_COUNT']/e[i,'EVENT1_COUNT']*e[i,'EVENT1_COUNT']/max_event_count #number of cohorts that had E1->E2, but may have had intermediate events also
      )
    }
  }

  logger::log_info(paste('Full graph contains',gsize(g),'links between',gorder(g),'events'))

  #Normalized numcohortExact
  E(g)$normalizedNumcohortExact = (E(g)$numcohortExact-min(E(g)$numcohortExact))/(max(E(g)$numcohortExact)-min(E(g)$numcohortExact))

  #Effect*event1_count
  E(g)$effectCount=E(g)$e1_count*E(g)$effect

  #make edge color equal to target node color
  edge.end <- ends(g, es=E(g), names=F)[,2] #outputs the end node id of each edge
  E(g)$color <- V(g)$color[edge.end]
  #but make it a bit lighter
  rgb1<-sapply(E(g)$color ,col2rgb)

  rgb1<-rgb1*1.2
  rgb1<-ifelse(rgb1>255,255,rgb1)

  rgb2<-rgb(rgb1[1,],rgb1[2,],rgb1[3,],alpha=1,maxColorValue=255)
  E(g)$color <- rgb2

  # make it of the class TrajectoriesGraph which is derived from the class igraph
  class(g) <- c("TrajectoriesGraph","igraph")

  return(g)

}
