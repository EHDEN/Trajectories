#' Adds numcohortCustom value to graph edges - actual number of people (out of all people who have EVENTNAME) on that edge
#'
#' @param g An igrpah object that is created by specific graph functions in this package
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param limit Max number of trajectories to align (to limit the analysis). Set to NA if no limit.
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param eventname Event name (concept name) through which the trajectories are analyzed
#' @param filename Full path tforoutput file for trajectory counts
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
                                           limit=1000,
                                           filename=file.path(getwd(),'trajectories.csv')) {

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


  #querySql(connection, paste0("SELECT COUNT(*) FROM sqlite_master"))

  #For interpretation purposes (done later), we get some counts first (not actually used in the alignment, but required for better interpretation)
  INTERPRETER=list()
  sql<-"SELECT COUNT(*) AS COHORTSIZE FROM @resultsSchema.@prefiXmycohort;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))
  INTERPRETER[['cohortsize']]=res$COHORTSIZE

  sql<-"SELECT E1_COUNT FROM @resultsSchema.@prefiXE1E2_model WHERE E1_CONCEPT_ID=@eventid LIMIT 1;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames, eventid=eventid)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))
  INTERPRETER[['event_count_in_eventperiods']]=res$E1_COUNT

  logger::log_info(paste0("Out of ",INTERPRETER[['cohortsize']]," event-periods in the analysis, event '",eventname,"' is present in ",INTERPRETER[['event_count_in_eventperiods']],' of them.'))

  #align trajectories to graph (to get the exact E1->E2 counts with no intermediate events)
  #Takes all trajectories that pass any event of that graph. Leaves out intermediate events that are not given in the graph.
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
  res2<-DatabaseConnector::querySql(connection, RenderedSql)

  INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]=length(res2$EVENTPERIOD_ID)
  if(INTERPRETER[['event_count_in_eventperiods']] != INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]) {
    diff=INTERPRETER[['event_count_in_eventperiods']] - INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]
    diff_perc=round(diff/INTERPRETER[['event_count_in_eventperiods']]*100,1)
    logger::log_warn("{diff} ({diff_perc}%) event-periods do not have any other event of a selected graph besides '{eventname}' (they do not produce any trajectory).")
    if(diff_perc>25) logger::log_warn('This is quite a high proportion, indicating that the graph as a model might not reflect the actual trajectories.')
  }



  #clear all numcohortCustom values
  E(g)$alignedTrajsCount<-0
  V(g)$alignedTrajsCount<-0

  #Create a list of concept names (for faster search later)
  Node.names<-igraph::as_data_frame(g, what="vertices") %>% select(concept_id,name)

  logger::log_info("Aligning trajectories to graph of '{eventname}'...")
  all_trajs<-data.frame()
  number_of_single_node_trajs=0

  #create chunks - in each chunk, 100 persons
  chunks<-split(res2$EVENTPERIOD_ID, ceiling(seq_along(res2$EVENTPERIOD_ID)/100))
  for(i in 1:length(chunks)) {
    print(paste('i=',i))
    chunk<-chunks[[i]]
    logger::log_debug(paste0('Aligning ',length(chunk),' trajectories to graph (',i,'/',length(chunks),')...'))

    RenderedSql <- Trajectories::loadRenderTranslateSql("get_actual_trajs_to_graph2.sql",
                                                     packageName=trajectoryAnalysisArgs$packageName,
                                                     dbms=attr(connection, "dbms"),
                                                     resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                     prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                     eventperiodids=paste(chunk,collapse=",")
    )
    res<-DatabaseConnector::querySql(connection, RenderedSql)

    k<-0
    for(cohort_id in chunk) {
      k<-k+1
      print(paste0('Aligning event-period #',(i-1)*100+k,": ",cohort_id))
      d<-res[res$EVENTPERIOD_ID==cohort_id,]
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
        plot(x,main=paste0("Actual trajectories of eventperiod ",cohort_id))
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

            logger::log_debug(paste0('Added ',r$E1_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E1_CONCEPT_ID]$name,'->',r$E2_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E2_CONCEPT_ID]$name,' (eventperiod ',cohort_id,') to graph (current count of edge: ',E(g)[eid]$alignedTrajsCount,')'))
          } else {
            logger::log_debug(paste0('Cannot add ',r$E1_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E1_CONCEPT_ID]$name,'->',r$E2_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E2_CONCEPT_ID]$name,' (eventperiod ',cohort_id,') as this edge is not part of the graph'))
          }
        } else {
          logger::log_debug(paste0('Cannot add ',r$E1_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E1_CONCEPT_ID]$name,'->',r$E2_CONCEPT_ID,':',V(g)[V(g)$concept_id==r$E2_CONCEPT_ID]$name,' as ',r$E1_CONCEPT_ID,' is not in visitednodes of eventperiod ',cohort_id))
        }

      } #for j
      # add +1 to all visited nodes
      if(length(visitednodes)>0) V(g)[V(g)$concept_id %in% visitednodes]$alignedTrajsCount<- V(g)[V(g)$concept_id %in% visitednodes]$alignedTrajsCount+1




      logger::log_debug(paste0('alignedTrajsCount of {eventname} after eventperiod ',cohort_id,': ',V(g)[V(g)$name==eventname]$alignedTrajsCount))
      #visitednodes.str<-paste0(c(visitednodes.names$node),collapse="\t")
      #l<-length(visitednodes)
      #logger::log_info("{i}:{cohort_id}:len={l}: {visitednodes.str}")
      all_trajs<-rbind(all_trajs, data.frame( trajectory= I(list(visitednodes)))) #this I(list(...)) is here to prevent R to flatten the list. See https://stackoverflow.com/questions/51307970/how-to-store-vector-in-dataframe-in-r

      if(length(visitednodes)<=1) {
        number_of_single_node_trajs<-number_of_single_node_trajs+1
      }

    } #for cohort_id

  } #for i

  E(g)$alignedTrajsProb=E(g)$alignedTrajsCount/V(g)[V(g)$concept_id==eventId]$count


  INTERPRETER[['number_of_single_node_trajs']]=number_of_single_node_trajs
  diff_perc=round(INTERPRETER[['number_of_single_node_trajs']]/INTERPRETER[['event_count_in_eventperiods']]*100,1)
  logger::log_info("{number_of_single_node_trajs} event-periods ({diff_perc}%) had other events (in addition to '{eventname}') of a selected graph, but their trajectories do not match any edge of the graph, so they do not form any trajectories on that graph.")
  if(diff_perc>25) logger::log_warn('This is quite a high proportion, indicating that the graph as a model might not reflect the actual trajectories.')
  if(nrow(all_trajs)!=INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]) logger::log_warn(paste0('Something is not right in alignActualTrajectoriesToGraph() method: nrow(all_trajs)=',nrow(all_trajs)," is not equal to INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]=",INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]," but it should"))

  INTERPRETER[['number_of_trajs_on_graph']]=INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]-INTERPRETER[['number_of_single_node_trajs']]
  diff_perc=round(INTERPRETER[['number_of_trajs_on_graph']]/INTERPRETER[['event_count_in_eventperiods']]*100,1)
  logger::log_info(paste0("Remaining ",INTERPRETER[['number_of_trajs_on_graph']]," ({diff_perc}%) trajectories through '{eventname}' are shown on the resulting graph."))

  #Group trajs by count, add names
  logger::log_info(' Step 3: Saving counts of aligned trajectories to file: {filename}...')
  all_trajs$trajectory.str=sapply(all_trajs$trajectory, paste, collapse="-")
  d<-all_trajs %>% group_by(trajectory.str) %>% summarise(n=n()) %>% arrange(desc(n)) %>% select(n=n, trajectory=trajectory.str)
  d$trajectory.str<-NA
  d$length<-NA
  d$index_of_eventname<-NA
  d$num_events_before_eventname<-NA
  d$num_events_after_eventname<-NA
  idx_of_first_traj_having_one_event_after_eventname<-NA
  idx_of_first_traj_having_two_events_after_eventname<-NA

  #Find names for each event
  n<-strsplit(d$trajectory,'-',fixed=T)
  for(i in 1:length(n)) {
    d[i,'length']<-length(n[[i]])

    #which element is index event
    d[i,'index_of_eventname']<-which(n[[i]]==eventId)

    #how many elements before eventname
    d[i,'num_events_before_eventname']<-1-d[i,'index_of_eventname']

    #how many elements after eventname
    d[i,'num_events_after_eventname']<- d[i,'length']-d[i,'index_of_eventname']
    if(is.na(idx_of_first_traj_having_one_event_after_eventname) & d[i,'num_events_after_eventname']>=1) idx_of_first_traj_having_one_event_after_eventname=i
    if(is.na(idx_of_first_traj_having_two_events_after_eventname) & d[i,'num_events_after_eventname']>=2) idx_of_first_traj_having_two_events_after_eventname=i


    i.names <- data.frame(concept_id=as.numeric(n[[i]])) %>%
      left_join(Node.names, by=c("concept_id"="concept_id")) %>% rename(concept_name=name) %>%
      mutate(namelength=stri_length(concept_name)) %>%
      mutate(node=paste0(concept_id,":\"",  ifelse(namelength<=20, concept_name, paste0(substr(concept_name,1,20), "...")),"\"")) %>%
      select(node)
    i.as.str<-paste0(c(i.names$node),collapse=" -> ")
    d[i,'trajectory.str']<-i.as.str
  }
  write.table(d,file=filename,quote=FALSE, sep="\t", row.names=F, col.names=F)
  logger::log_info(' ...done.')

  if(!is.na(idx_of_first_traj_having_two_events_after_eventname)) {
    node1=eventId
    node2=n[[idx_of_first_traj_having_two_events_after_eventname]][as.numeric(d[idx_of_first_traj_having_two_events_after_eventname,'index_of_eventname']+1)]
    node3=n[[idx_of_first_traj_having_two_events_after_eventname]][as.numeric(d[idx_of_first_traj_having_two_events_after_eventname,'index_of_eventname']+2)]

    edge2<-E(g)[which(E(g)$e1_concept_id==node1 & E(g)$e2_concept_id==node2)]
    print(paste0(edge2$alignedTrajsCount," event-periods (",round(edge2$alignedTrajsProb*100,1),"%) have '",V(g)[V(g)$concept_id==node2]$name,"' right after '",V(g)[V(g)$concept_id==eventid]$name,"'"))

    edge3<-E(g)[which(E(g)$e1_concept_id==node2 & E(g)$e2_concept_id==node3)]
    print(paste0(edge3$alignedTrajsCount," event-periods (",round(edge3$alignedTrajsProb*100,1),"%) have '",V(g)[V(g)$concept_id==node3]$name,"' right after '",V(g)[V(g)$concept_id==node2]$name,"'"))
    TODO: should explain that not all of these node2->node3 patients had node1 somewhere in their path (but does not mean that all of them had... it right before...)

  }


  # make it of the class TrajectoriesGraph which is derived from the class igraph
  class(g) <- c("TrajectoriesGraph","igraph")

  return(g)
}


