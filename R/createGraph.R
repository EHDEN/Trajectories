library(ff)
library(ffbase)
library(igraph)
library(dplyr)
#' Builds an igraph object based on event pairs data in "eventPairResultsFilename" file
#'
#' @param eventPairResultsFilename Full path to event pairs file
#'
#' @return
#' @export
#'
#' @examples
createGraph<-function(eventPairResultsFilename) {

  print(paste0('Creating igraph object based on event pairs data in the following file: ',eventPairResultsFilename,'...'))

  # Links are coming from eventPairResultsFilename (event1->event2 pairs)
  e = read.csv2(file = eventPairResultsFilename, sep = '\t', header = TRUE, as.is=T)

  #remove everything with births (remove from here later, just for debugging)
  #e<-e[e$EVENT1_NAME!='Pneumonia',]

  #actual trajectories
  #t = read.csv2.ffdf(file = eventPairCohortsFilename, sep = '\t', header = TRUE) #we use ff-package here to avoid reading the whole file into memory
  #ffload(eventPairCohortsFilename, overwrite=TRUE) #loads an object "trajs" from file

  # convert chr columns to numeric
  e$EVENT_PAIR_EFFECT<-as.numeric(e$EVENT_PAIR_EFFECT)
  e$AVG_NUMBER_OF_DAYS_BETWEEN_EVENTS<-as.numeric(e$AVG_NUMBER_OF_DAYS_BETWEEN_EVENTS)


  # An approximate age for each event is calculated
  # -----
  #In each event pair the age of event1 might be different
  #For a graph, we use the minimum date of the following: MIN( min(25% quantile if given), min (event2_estimated_age if given))
  # first, find out what is the lowest age_q25 for that event, if the event is among event1
  x<-e %>% group_by(EVENT1_NAME) %>% mutate(min_q25_age_of_event1=quantile(Q25_AGE_OF_COHORT_EVENT1_OCCURS_FIRST,c(0.25))) %>% ungroup() %>% select (event=EVENT1_NAME,age=min_q25_age_of_event1)
  # second, find out what is the expected age for that event, if the event is among event2 (calculated from event1 age)
  e$event2_estimated_age <- e$Q25_AGE_OF_COHORT_EVENT1_OCCURS_FIRST+round(e$AVG_NUMBER_OF_DAYS_BETWEEN_EVENTS/365.25)
  y<-e %>% group_by(EVENT2_NAME) %>% mutate(min_estimated_age=quantile(event2_estimated_age,c(0.25))) %>% ungroup() %>% select (event=EVENT2_NAME,age=min_estimated_age)
  # Finally, take the minimum value of those 2
  AGES<-as.data.frame(rbind(x,y) %>% group_by(event) %>% mutate(AGE_FOR_GRAPH=min(age)) %>% ungroup() %>% select (event,AGE_FOR_GRAPH) %>% unique())
  # End of age calculation

  # calculate max event count for scaling
  max_event_count<-max(e$EVENT1_COUNT,e$EVENT2_COUNT)

  # predefined colors
  COLORS=list(Condition='#F1948A', #red
              Observation='#85C1E9', #blue
              Procedure='#ccc502',#yellow
              Drug='#8fba75') #green
              #Drug='#ABEBC6') #green
  LABELCOLORS=list(Condition='#7B241C',
                   Observation='#21618C',
                   Procedure='#7D6608',
                   Drug='#145A32')

  # BUILD A GRAPH!
  # create empty graph
  g <- make_empty_graph(directed = TRUE)
  #add edges between these nodes
  if(nrow(e)>0) {
    for(i in seq(1,nrow(e))) {
      e1<-e[i,'EVENT1_NAME']
      e2<-e[i,'EVENT2_NAME']
      # add vertexes if not exist already
      if(!e1 %in% V(g)$name) {g <- g + vertices(e1,
                                                concept_id=e[i,'EVENT1_CONCEPT_ID'],
                                                count=e[i,'EVENT1_COUNT'],
                                                #size=e[i,'EVENT1_COUNT']/max_event_count,
                                                color=COLORS[[e[i,'EVENT1_DOMAIN']]],
                                                labelcolor=LABELCOLORS[[e[i,'EVENT1_DOMAIN']]],
                                                age=AGES[AGES$event==e1,'AGE_FOR_GRAPH'])
      }
      if(!e2 %in% V(g)$name) {g <- g + vertices(e2,
                                                concept_id=e[i,'EVENT2_CONCEPT_ID'],
                                                count=e[i,'EVENT2_COUNT'],
                                                #size=e[i,'EVENT2_COUNT']/max_event_count,
                                                color=COLORS[[e[i,'EVENT2_DOMAIN']]],
                                                labelcolor=LABELCOLORS[[e[i,'EVENT2_DOMAIN']]],
                                                age=AGES[AGES$event==e2,'AGE_FOR_GRAPH'])
      }
      # add edge
      g <- g + edge(e1,
                    e2,
                    e1=e1,
                    e1_concept_id=e[i,'EVENT1_CONCEPT_ID'],
                    e2_concept_id=e[i,'EVENT2_CONCEPT_ID'],
                    e2=e2,
                    e1_count=e[i,'EVENT1_COUNT'],
                    effect=e[i,'EVENT_PAIR_EFFECT'],
                    prob=e[i,'EVENT1_EVENT2_COHORT_COUNT']/e[i,'EVENT1_COUNT'],
                    #weight=1/e[i,'EVENT_PAIR_EFFECT'], #opposite to the effect size. Do not use weight attribute, as it has a special meaning in igraph and we do not want to use this automatically
                    numcohortExact=e[i,'COHORT_COUNT_HAVING_E2_RIGHT_AFTER_E1'] #number of cohorts that had E1->E2 as immediate order (no intermediate events)
                    #numcohort=e[i,'EVENT1_EVENT2_COHORT_COUNT']/e[i,'EVENT1_COUNT']*e[i,'EVENT1_COUNT']/max_event_count #number of cohorts that had E1->E2, adjusted to but may have had intermediate events also
                    #numcohort=e[i,'EVENT1_EVENT2_COHORT_COUNT']/e[i,'EVENT1_COUNT']*e[i,'EVENT1_COUNT']/max_event_count #number of cohorts that had E1->E2, but may have had intermediate events also
      )
    }
  }

  print(paste('Full graph contains',gsize(g),'links between',gorder(g),'events'))

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

  return(g)

}
