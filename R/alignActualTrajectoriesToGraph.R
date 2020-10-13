#' Adds numcohortCustom value to graph edges - actual number of people (out of all people who have EVENTNAME) on that edge
#'
#' @param g An igrpah object that is created by specific graph functions in this package
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param limit Max number of trajectories to align (to limit the analysis). Set to NA if no limit.
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param eventname Event name (concept name) through which the trajectories are analyzed
#'
#' @return
#' @export
#'
#' @examples
alignActualTrajectoriesToGraph <- function(connection,
                                           trajectoryAnalysisArgs,
                                           trajectoryLocalArgs,
                                           g,
                                           eventname,
                                           limit=1000) {

  if(!inherits(g, 'TrajectoriesGraph')) stop('Error in filterIgraphRemoveLowEffectLinksAndOrphanNodes(): object g is not class TrajectoriesGraph object')

  DEBUG=F

  if(is.na(limit)) limit=0

  #check that eventname is present in g
  if(!eventname %in% V(g)$name) {
    logger::log_warn(paste0('Cannot align trajectories through {eventname} as the graph does not contain any links with that event. Return unaligned graph.'))
    return(g)
  }

  eventid=V(g)[V(g)$name==eventname]$concept_id

  #First, put event pairs of the graph into table

  logger::log_info(paste0('Putting ',length(E(g)),' event pairs of the graph into database to align to: {eventname}...'))

  e<-igraph::as_data_frame(g,what="edges")
  edges<- e %>% select(e1_concept_id,e2_concept_id)


    #but before actual TABLE CREATE there is an extra step: if sqlRole is given, set session to correct role before creating the table
  Trajectories::setRole(connection,trajectoryLocalArgs$sqlRole)

  #26 Sep 2020: Can't use simply insertTable here because in Eunomia package it does not solve schema name correctly. That's why this is commented out and we manually create SQL here :(
  #insertTable(connection, tablename, edges, tempTable=F, progressBar=T)

  #Create empty table manually
  RenderedSql <- Trajectories::loadRenderTranslateSql("create_mylinks_table.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=attr(connection, "dbms"),
                                                   resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                   prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection, RenderedSql)

  #Fill with data
  tablename<-paste0(trajectoryLocalArgs$resultsSchema,'.',trajectoryLocalArgs$prefixForResultTableNames,'mylinks')
  insertSql <- paste("INSERT INTO ",
                     tablename,
                     " (e1_concept_id, e2_concept_id) VALUES ",
                     paste(paste("(",paste(edges$e1_concept_id, edges$e2_concept_id, sep=","),")") , collapse=","),
                     ";",
                     sep = "")
  RenderedSql <- SqlRender::translate(insertSql,targetDialect=attr(connection, "dbms"))
  DatabaseConnector::executeSql(connection, RenderedSql)

  #querySql(connection, paste0("SELECT COUNT(*) FROM main.mylinks"))


  #querySql(connection, paste0("SELECT * FROM sqlite_master"))


  #align trajectories to graph (to get the exact E1->E2 counts with no intermediate events)
  #Takes all trajectories that pass the event-pairs of that graph. Leaves out intermediate events that are not given in the graph.
  logger::log_info('Extracting actual sequences of these events from database...')
  RenderedSql <- Trajectories::loadRenderTranslateSql("map_actual_trajs_to_graph2.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=attr(connection, "dbms"),
                                                   resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                   prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                   eventid=eventid,
                                                   limit=limit
  )
  DatabaseConnector::executeSql(connection, RenderedSql)
  logger::log_info('... done.')

  #Read in counts between E1->E2
  logger::log_info('Reading cohort-ids of these sequences...')

  RenderedSql <- Trajectories::loadRenderTranslateSql("get_actual_trajs_to_graph_persons.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=attr(connection, "dbms"),
                                                   resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                   prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )
  res<-DatabaseConnector::querySql(connection, RenderedSql)

  #clear all numcohortCustom values
  E(g)$alignedTrajsCount<-0
  V(g)$alignedTrajsCount<-0

  log_info("Aligning trajectories to graph of '{eventname}'...")

  #create chunks - in each chunk, 100 persons
  chunks<-split(res$COHORT_ID, ceiling(seq_along(res$COHORT_ID)/100))
  for(i in 1:length(chunks)) {
    chunk<-chunks[[i]]
    logger::log_debug(paste0('Aligning ',length(chunk),' trajectories to graph (',i,'/',length(chunks),')...'))

    RenderedSql <- Trajectories::loadRenderTranslateSql("get_actual_trajs_to_graph2.sql",
                                                     packageName=trajectoryAnalysisArgs$packageName,
                                                     dbms=attr(connection, "dbms"),
                                                     resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                     prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                     cohortids=paste(chunk,collapse=",")
    )
    res<-DatabaseConnector::querySql(connection, RenderedSql)
    #log_info(paste0('DB query done, executing R...'))

    for(cohort_id in unique(res$COHORT_ID)) {
      d<-res[res$COHORT_ID==cohort_id,]
      #on which cohort day is the index event?
      indexevent_cohort_day<-unique(c(d[d$E1_IS_INDEXEVENT==1,'E1_COHORT_DAY'],d[d$E2_IS_INDEXEVENT==1,'E2_COHORT_DAY'])) #if the code breaks here, it may be because there are several records for the same concept_ids. It has happened when one used both addDrugEras=T AND addDrugExposures=T... One should not use it like this.
      d$cohort_day_relative_to_indexevent = d$E1_COHORT_DAY-indexevent_cohort_day
      #d<-d %>% arrange( -INDEXEVENT,abs(cohort_day_relative_to_indexevent) )
      d <-d[order(-(d$E1_IS_INDEXEVENT+d$E2_IS_INDEXEVENT), abs(d$cohort_day_relative_to_indexevent)),]
      #for debugging:
      if(DEBUG) {
        v<-igraph::as_data_frame(g, what="vertices") %>% select(concept_id,name)
        w<-d %>% left_join(v, by=c("E1_CONCEPT_ID"="concept_id")) %>% rename(E1=name)
        w<-w %>% left_join(v, by=c("E2_CONCEPT_ID"="concept_id")) %>% rename(E2=name)
        x<-graph_from_data_frame(w %>% select(E1,E2), directed = TRUE, vertices = NULL)
        #plot(x,layout=layout_)
        plot(x,main=paste0("Actual trajectories of cohort ",cohort_id))
      }

      visitednodes=c()
      for(j in 1:nrow(d)) {
        r<-d[j,]

        #check if the first event is indexevent (if yes, add it to visitednodes)
        if(r$E1_IS_INDEXEVENT==1 & (!r$E1_CONCEPT_ID %in% visitednodes)) {
          visitednodes<-c(visitednodes,r$E1_CONCEPT_ID)
        } else if(r$E2_IS_INDEXEVENT==1 & (!r$E2_CONCEPT_ID %in% visitednodes)) {
          visitednodes<-c(visitednodes,r$E2_CONCEPT_ID)
        }

        #check that the first event is in visitevent (if not then we cannot add this graph)
        if(
          (r$cohort_day_relative_to_indexevent>=0 & r$E1_CONCEPT_ID %in% visitednodes)
          |
          (r$cohort_day_relative_to_indexevent<0 & r$E2_CONCEPT_ID %in% visitednodes)
        ) {
          #check that the edge exists in our graph
          eid<-which(E(g)$e1_concept_id==r$E1_CONCEPT_ID & E(g)$e2_concept_id==r$E2_CONCEPT_ID)
          if(length(eid)==1) {
            #yes, it exists. Add +1 to edge count
            E(g)[eid]$alignedTrajsCount<-E(g)[eid]$alignedTrajsCount+1
            # Also +1 to the node
            if(r$cohort_day_relative_to_indexevent>=0) {
              # and also add to visitednode
              visitednodes<-c(visitednodes,r$E2_CONCEPT_ID)
            } else {
              # and also add to visitednode
              visitednodes<-c(visitednodes,r$E1_CONCEPT_ID)
            }

            if(DEBUG) logger::log_debug(paste0('Added ',r$E1_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E1_CONCEPT_ID]$name,'->',r$E2_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E2_CONCEPT_ID]$name,' (cohort ',cohort_id,') to graph (current count of edge: ',E(g)[eid]$alignedTrajsCount,')'))
          } else {
            if(DEBUG) logger::log_debug(paste0('Cannot add ',r$E1_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E1_CONCEPT_ID]$name,'->',r$E2_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E2_CONCEPT_ID]$name,' (cohort ',cohort_id,') as this edge is not part of the graph'))
          }
        } else {
          if(DEBUG) logger::log_debug(paste0('Cannot add ',r$E1_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E1_CONCEPT_ID]$name,'->',r$E2_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E2_CONCEPT_ID]$name,' as ',r$E1_CONCEPT_ID,' is not in visitednodes of cohort ',cohort_id))
        }

      } #for j
      # add +1 to all visited nodes
      if(length(visitednodes)>0) V(g)[V(g)$concept_id %in% visitednodes]$alignedTrajsCount<- V(g)[V(g)$concept_id %in% visitednodes]$alignedTrajsCount+1
      if(DEBUG) logger::log_debug(paste0('alignedTrajsCount of {eventname} after cohort ',cohort_id,': ',V(g)[V(g)$name==eventname]$alignedTrajsCount))
    } #for cohort_id

  } #for i




  # make it of the class TrajectoriesGraph which is derived from the class igraph
  class(g) <- c("TrajectoriesGraph","igraph")

  return(g)
}


