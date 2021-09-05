requireNamespace("igraph", quietly = TRUE)

#' Adds numcohortCustom value to graph edges - actual number of people (out of all people who have eventId) on that edge
#'
#' @param g An igrpah object that is created by specific graph functions in this package
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param limit Max number of trajectories to align (to limit the analysis). Set to NA if no limit.
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param eventid Event name (concept ID) through which the trajectories are analyzed
#' @param filename Full path to output file for trajectory counts. Set to NA to skip this.
#' @param filename_interpretation Full path to output file for textual interpretation of the output graph. Set to NA to skip this.
#'
#' @return
#'
#' @examples
alignActualTrajectoriesToGraph <- function(connection,
                                           trajectoryAnalysisArgs,
                                           trajectoryLocalArgs,
                                           g,
                                           eventid,
                                           limit=1000,
                                           filename=file.path(getwd(),'trajectories.csv'),
                                           filename_interpretation=file.path(getwd(),'trajectories_interpretation.txt')) {

  if(!inherits(g, 'TrajectoriesGraph')) stop('Error in filterIgraphRemoveLowEffectLinksAndOrphanNodes(): object g is not class TrajectoriesGraph object')

  DEBUG=F

  if(is.na(limit)) limit=0

  #check that eventid is present in g
  if(!eventid %in% igraph::V(g)$concept_id) {
    ParallelLogger::logWarn('Cannot align trajectories through ',eventid,' as the graph does not contain any links with that event. Return unaligned graph.')
    return(g)
  }

  eventname=igraph::V(g)[igraph::V(g)$concept_id==eventid]$name

  #First, put event pairs of the graph into table

  ParallelLogger::logInfo('Putting ',length(igraph::E(g)),' event pairs of the graph into database to align to: ',eventname,'...')

  e<-igraph::as_data_frame(g,what="edges")
  edges<- e %>% dplyr::select(e1_concept_id,e2_concept_id)


  #but before actual TABLE CREATE there is an extra step: if sqlRole is given, set session to correct role before creating the table
  Trajectories:::setRole(connection,trajectoryLocalArgs$sqlRole)

  #26 Sep 2020: Can't use simply insertTable here because in Eunomia package it does not solve schema name correctly. That's why this is commented out and we manually create SQL here :(
  #insertTable(connection, tablename, edges, tempTable=F, progressBar=T)

  #Create empty table manually
  sqlfilename="create_mylinks_table.sql"
  if(is.character(edges$e1_concept_id)) sqlfilename="create_mylinks_table-brunak.sql" # This hocus-pocus is just for handling character-based CONCEP_ID-s.
  RenderedSql <- Trajectories:::loadRenderTranslateSql(sqlfilename,
                                                  packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
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
                     paste(paste0("(",paste(
                       #edges$e1_concept_id,
                       #if(is.character(edges$e1_concept_id)) {paste0("'",edges$e1_concept_id,"'")} else {edges$e1_concept_id}, #this hocus-pocus is just for handling character-based CONCEP_ID-s. This normally does not happen, but in case someone is tricking a bit and tries to use source_values for the analysis, then here we want to make sure that the code does not break
                       DBI::dbQuoteString(connection, edges %>% dplyr::mutate(e1_concept_id=dplyr::if_else(is.na(e1_concept_id),'NULL',as.character(e1_concept_id))) %>% dplyr::pull(e1_concept_id)  ),
                       #edges$e2_concept_id,
                       #if(is.character(edges$e2_concept_id)) {paste0("'",edges$e2_concept_id,"'")} else {edges$e2_concept_id},
                       DBI::dbQuoteString(connection, edges %>% dplyr::mutate(e2_concept_id=dplyr::if_else(is.na(e2_concept_id),'NULL',as.character(e2_concept_id))) %>% dplyr::pull(e2_concept_id)  ),
                       sep=","),")") , collapse=","),
                     ";",
                     sep = "")
  RenderedSql <- SqlRender::translate(insertSql,targetDialect=attr(connection, "dbms"))
  DatabaseConnector::executeSql(connection, RenderedSql)

  #querySql(connection, paste0("SELECT COUNT(*) FROM main.mylinks"))


  #querySql(connection, paste0("SELECT COUNT(*) FROM sqlite_master"))

  #For interpretation purposes (done later), we get some counts first (not actually used in the alignment, but required for better interpretation)
  INTERPRETER=list()
  INTERPRETER.MSG=c()
  sql<-"SELECT COUNT(*) AS COHORTSIZE FROM @resultsSchema.@prefiXmycohort;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))
  INTERPRETER[['cohortsize']]=res$COHORTSIZE

  #event1 might be given as E1 and E2 (or both) in pairs
  sql<-"SELECT
          CASE WHEN E1_CONCEPT_ID=@eventid THEN E1_COUNT_IN_EVENTS ELSE E2_COUNT_IN_EVENTS END AS E_COUNT
        FROM @resultsSchema.@prefiXE1E2_model WHERE E1_CONCEPT_ID=@eventid OR E2_CONCEPT_ID=@eventid
  LIMIT 1;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                   eventid=if(is.character(eventid)) {paste0("'",eventid,"'")} else {eventid})
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))

  INTERPRETER[['event_count_in_eventperiods']]=res$E_COUNT

  msg=paste0("Out of ",INTERPRETER[['cohortsize']]," event-periods in the analysis, event '",eventname,"' is present in ",INTERPRETER[['event_count_in_eventperiods']],' of them.')
  ParallelLogger::logInfo(msg)
  INTERPRETER.MSG=c(INTERPRETER.MSG,msg)

  #align trajectories to graph (to get the exact E1->E2 counts with no intermediate events)
  #Takes all trajectories that pass any event of that graph. Leaves out intermediate events that are not given in the graph.
  ParallelLogger::logInfo('Extracting actual sequences of these events from database...')
  RenderedSql <- Trajectories:::loadRenderTranslateSql("map_actual_trajs_to_graph2.sql",
                                                  packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                   dbms=attr(connection, "dbms"),
                                                   resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                   prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                   eventid=if(is.character(eventid)) {paste0("'",eventid,"'")} else {eventid},
                                                   limit=limit
  )
  DatabaseConnector::executeSql(connection, RenderedSql)
  ParallelLogger::logInfo('... done.')

  #Read in counts between E1->E2
  ParallelLogger::logInfo('Reading cohort-ids of these sequences...')

  RenderedSql <- Trajectories:::loadRenderTranslateSql("get_actual_trajs_to_graph_persons.sql",
                                                   packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                   dbms=attr(connection, "dbms"),
                                                   resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                   prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )
  res2<-DatabaseConnector::querySql(connection, RenderedSql)

  INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]=length(res2$EVENTPERIOD_ID)
  if(INTERPRETER[['event_count_in_eventperiods']] != INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]) {
    diff=INTERPRETER[['event_count_in_eventperiods']] - INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]
    diff_perc=round(diff/INTERPRETER[['event_count_in_eventperiods']]*100,1)
    msg=paste0("Out of these ",INTERPRETER[['event_count_in_eventperiods']]," event-periods, ",diff," (",diff_perc,"%) event-periods do not have any other event of a selected graph besides '",eventname,"' (they do not produce any trajectory).")
    ParallelLogger::logInfo(msg)
    INTERPRETER.MSG=c(INTERPRETER.MSG,msg)
    if(diff_perc>25) {
      msg='This is quite a high proportion, indicating that the graph as a model might not reflect the actual trajectories comprehensively.'
      ParallelLogger::logInfo(msg)
      INTERPRETER.MSG=c(INTERPRETER.MSG,msg)
    }
  }



  #clear all numcohortCustom values
  igraph::E(g)$alignedTrajsCount<-0
  igraph::V(g)$alignedTrajsCount<-0

  #Create a list of concept names (for faster search later)
  Node.names<-igraph::as_data_frame(g, what="vertices") %>% dplyr::select(concept_id,name)

  ParallelLogger::logInfo("Aligning trajectories to the graph of '",eventname,"'...")
  all_trajs<-data.frame()
  number_of_single_node_trajs=0

  #create chunks - in each chunk, 100 persons
  chunks<-split(res2$EVENTPERIOD_ID, ceiling(seq_along(res2$EVENTPERIOD_ID)/100))
  for(i in 1:length(chunks)) {
    #print(paste('i=',i))
    chunk<-chunks[[i]]
    ParallelLogger::logInfo('Aligning ',length(chunk),' event-periods to the graph (',i,'/',length(chunks),')...')

    RenderedSql <- Trajectories:::loadRenderTranslateSql("get_actual_trajs_to_graph2.sql",
                                                        packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                     dbms=attr(connection, "dbms"),
                                                     resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                     prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                     eventperiodids=paste(chunk,collapse=",")
    )
    res<-DatabaseConnector::querySql(connection, RenderedSql)

    k<-0
    for(cohort_id in chunk) {
      k<-k+1
      #print(paste0('Aligning event-period #',(i-1)*100+k,": ",cohort_id))
      d<-res[res$EVENTPERIOD_ID==cohort_id,]
      #on which cohort day is the index event?
      indexevent_cohort_day<-unique(c(d[d$E1_IS_INDEXEVENT==1,'E1_COHORT_DAY'],d[d$E2_IS_INDEXEVENT==1,'E2_COHORT_DAY'])) #if the code breaks here, it may be because there are several records for the same concept_ids. It has happened when one used both addDrugEras=T AND addDrugExposures=T... One should not use it like this.
      d$cohort_day_relative_to_indexevent = d$E1_COHORT_DAY-indexevent_cohort_day
      #d<-d %>% arrange( -INDEXEVENT,abs(cohort_day_relative_to_indexevent) )
      d <-d[order(-(d$E1_IS_INDEXEVENT+d$E2_IS_INDEXEVENT), abs(d$cohort_day_relative_to_indexevent)),]
      #for debugging:
      if(DEBUG) {
        v<-igraph::as_data_frame(g, what="vertices") %>% dplyr::select(concept_id,name)
        w<-d %>% dplyr::left_join(v, by=c("E1_CONCEPT_ID"="concept_id")) %>% dplyr::rename(E1=name)
        w<-w %>% dplyr::left_join(v, by=c("E2_CONCEPT_ID"="concept_id")) %>% dplyr::rename(E2=name)
        x<-graph_from_data_frame(w %>% dplyr::select(E1,E2), directed = TRUE, vertices = NULL)
        #plot(x,layout=layout_)
        plot(x,main=paste0("Actual trajectories of eventperiod ",cohort_id))
      }

      visitednodes=c() #just a list of visited nodes, not in the right order!
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
          eid<-which(igraph::E(g)$e1_concept_id==r$E1_CONCEPT_ID & igraph::E(g)$e2_concept_id==r$E2_CONCEPT_ID)
          if(length(eid)==1) {
            #yes, it exists. Add +1 to edge count
            igraph::E(g)[eid]$alignedTrajsCount<-igraph::E(g)[eid]$alignedTrajsCount+1
            # Also +1 to the node
            if(r$cohort_day_relative_to_indexevent>=0) {
              # and also add to visitednode
              visitednodes<-c(visitednodes,r$E2_CONCEPT_ID)
            } else {
              # and also add to visitednode
              visitednodes<-c(visitednodes,r$E1_CONCEPT_ID)
            }

            ParallelLogger::logDebug('Added ',r$E1_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==r$E1_CONCEPT_ID]$name,'->',r$E2_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==r$E2_CONCEPT_ID]$name,' (eventperiod ',cohort_id,') to graph (current count of edge: ',igraph::E(g)[eid]$alignedTrajsCount,')')
          } else {
            ParallelLogger::logDebug('Cannot add ',r$E1_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==r$E1_CONCEPT_ID]$name,'->',r$E2_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==r$E2_CONCEPT_ID]$name,' (eventperiod ',cohort_id,') as this edge is not part of the graph')
          }
        } else {
          ParallelLogger::logDebug('Cannot add ',r$E1_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==r$E1_CONCEPT_ID]$name,'->',r$E2_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==r$E2_CONCEPT_ID]$name,' as ',r$E1_CONCEPT_ID,' is not in visitednodes of eventperiod ',cohort_id)
        }

      } #for j
      # add +1 to all visited nodes
      if(length(visitednodes)>0) igraph::V(g)[igraph::V(g)$concept_id %in% visitednodes]$alignedTrajsCount<- igraph::V(g)[igraph::V(g)$concept_id %in% visitednodes]$alignedTrajsCount+1




      ParallelLogger::logDebug('alignedTrajsCount of ',eventname,' after eventperiod ',cohort_id,': ',igraph::V(g)[igraph::V(g)$name==eventname]$alignedTrajsCount)
      #visitednodes.str<-paste0(c(visitednodes.names$node),collapse="\t")
      #l<-length(visitednodes)

      #visited nodes in right order
      visited_edges_ordered<-d %>%
        dplyr::arrange(E1_COHORT_DAY) %>%
        dplyr::filter(E1_CONCEPT_ID %in% visitednodes & E2_CONCEPT_ID %in% visitednodes)
      e1_days<-visited_edges_ordered %>% dplyr::select(DAYNO=E1_COHORT_DAY,CONCEPT_ID=E1_CONCEPT_ID)
      e2_days<-visited_edges_ordered %>% dplyr::select(DAYNO=E2_COHORT_DAY,CONCEPT_ID=E2_CONCEPT_ID)
      visited_nodes_ordered<-rbind(e1_days,e2_days) %>% unique() %>% dplyr::arrange(DAYNO,CONCEPT_ID) %>% dplyr::group_by(DAYNO) %>% dplyr::summarise(CONCEPT_GROUPED=paste(CONCEPT_ID,collapse="&")) %>% dplyr::pull(CONCEPT_GROUPED)
      #for 0-length trajectories, add eventid manually to visitednodes (do not come automatically as there is no event pair in that case)
      if(length(visited_nodes_ordered)==0) visited_nodes_ordered=c(eventid)
      trajectory.str=paste(visited_nodes_ordered,collapse="-")
      if(nrow(all_trajs)>0 & trajectory.str %in% all_trajs$trajectory.str) {
        idx<-which(all_trajs$trajectory.str==trajectory.str)
        all_trajs[idx,'exact_count'] <- all_trajs[idx,'exact_count'] + 1
      } else {
        all_trajs<-rbind(all_trajs, data.frame( trajectory= I(list(visited_nodes_ordered)), trajectory.str=trajectory.str, exact_count=1)) #this I(list(...)) is here to prevent R to flatten the list. See https://stackoverflow.com/questions/51307970/how-to-store-vector-in-dataframe-in-r
      }

      if(length(visitednodes)<=1) {
        number_of_single_node_trajs<-number_of_single_node_trajs+1
      }

    } #for cohort_id

  } #for i

  igraph::E(g)$alignedTrajsProb=igraph::E(g)$alignedTrajsCount/igraph::V(g)[igraph::V(g)$concept_id==eventid]$count


  INTERPRETER[['number_of_single_node_trajs']]=number_of_single_node_trajs
  diff_perc=round(INTERPRETER[['number_of_single_node_trajs']]/INTERPRETER[['event_count_in_eventperiods']]*100,1)
  msg=paste0(number_of_single_node_trajs," event-periods (",diff_perc,"%) had other events (in addition to '",eventname,"') of a selected graph but their trajectories do not match any edge of the graph, so they do not form any trajectories on that graph.")
  ParallelLogger::logInfo(msg)
  INTERPRETER.MSG=c(INTERPRETER.MSG,msg)
  if(diff_perc>25) {
    msg='This is quite a high proportion, indicating that the graph as a model might not reflect the actual trajectories comprehensively.'
    ParallelLogger::logInfo(msg)
    INTERPRETER.MSG=c(INTERPRETER.MSG,msg)
  }
  if(sum(all_trajs$exact_count)!=INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]) {
    msg=paste0('Something is not right in alignActualTrajectoriesToGraph() method: sum(all_trajs$exact_count)=',nrow(all_trajs)," is not equal to INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]=",INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]," but it should")
    ParallelLogger::logWarn(msg)
    INTERPRETER.MSG=c(INTERPRETER.MSG,msg)
  }

  INTERPRETER[['number_of_trajs_on_graph']]=INTERPRETER[['event_count_in_eventperiods_of_selected_graph']]-INTERPRETER[['number_of_single_node_trajs']]
  diff_perc=round(INTERPRETER[['number_of_trajs_on_graph']]/INTERPRETER[['event_count_in_eventperiods']]*100,1)
  msg=paste0("Remaining ",INTERPRETER[['number_of_trajs_on_graph']]," (",diff_perc,"%) trajectories through '",eventname,"' are shown on the resulting graph.")
  ParallelLogger::logInfo(msg)
  INTERPRETER.MSG=c(INTERPRETER.MSG,msg)

  #add names to trajectories
  all_trajs<-all_trajs %>% dplyr::arrange(-exact_count)
  all_trajs$trajectory.str<-as.character(all_trajs$trajectory.str)
  d<-all_trajs
  d$total_count<-d$exact_count
  d$length<-as.numeric(lapply(d$trajectory,length))
  d$is_subtrajectory_of<-NA
  #d$index_of_eventname<-NA
  #d$num_events_before_eventname<-NA
  #d$num_events_after_eventname<-NA
  #idx_of_first_traj_having_one_event_after_eventname<-NA
  idx_of_first_traj_having_two_events_after_eventname<-NA
  d$trajectory.names.str<-""

  #Find names for each event
  Node.names$concept_id=as.character(Node.names$concept_id)
  for(i in 1:nrow(d)) {
    # print(i)
    n<-d[i,'trajectory']
    d[i,'length']<-length(n[[1]])

    #which element is index event (or contains index event as elements may be combinations of several events)
    elem_eventnames_as_str=c()
    for(e in 1:length(n[[1]])) {
      elem=n[[1]][e]
      elem_events=strsplit(as.character(elem),"&",fixed=TRUE)[[1]]
      if(eventid %in% elem_events) d[i,'index_of_eventname']<-e

      elem_eventnames <- data.frame(concept_id=as.character(elem_events), stringsAsFactors=FALSE) %>%
        dplyr::left_join(Node.names, by=c("concept_id"="concept_id")) %>% dplyr::rename(concept_name=name) %>%
        dplyr::mutate(namelength=stringi::stri_length(concept_name)) %>%
        dplyr::mutate(node=paste0(concept_id,":\"",  ifelse(namelength<=20, concept_name, paste0(substr(concept_name,1,20), "...")),"\"")) %>%
        dplyr::pull(node)
      elem_eventnames_as_str=c(elem_eventnames_as_str, paste(elem_eventnames,collapse="&"))
    }
    d[i,'trajectory.names.str']<-paste0(elem_eventnames_as_str,collapse=" -> ")
  }


  #some trajectories are sub-trajectories of the others. Find the actual count of them
  #make sure that trajectories are still ordered by count desc (trajectories with larger count cannot be sub-trajectories of smaller count)
  ParallelLogger::logInfo('Finding actual counts of each trajectory by considering also sub-trajectories...')
  d<-d %>% dplyr::arrange(-exact_count,-length)
  d <- d %>% dplyr::mutate(id=dplyr::row_number())
  d <- d %>% dplyr::select(id,trajectory,trajectory.str,exact_count,total_count,length,is_subtrajectory_of,trajectory.names.str)
  if(nrow(d)>1) {
    for(i in 1:(nrow(d)-1)) {
      #print(i)

      subtraj_ids=c()

      #print(length(d$trajectory[[i]]))
      ParallelLogger::logDebug('Counting ',d$trajectory.str[[i]],'...')
      for(j in (1+i):nrow(d)) {
        ParallelLogger::logDebug('...Checking whether ',d$trajectory.str[[i]],' (count=',d$total_count[i],') is a sub-trajectory of ',d$trajectory.str[[j]],' (count=',d$total_count[j],')...')

        if(length(d$trajectory[[j]])>=length(d$trajectory[[i]])) {

          #regex/state-machine logic here
          state = 1
          for (event in d$trajectory[[j]]) {
            eventsplitted<-stringi::stri_split_fixed(event,"&")[[1]] #split_fixed here because some event might be given as "evevn1/event2" occurring on same day
            if(state == 1) {
              if(all(stringi::stri_split_fixed(d$trajectory[[i]][1],"&")[[1]] %in% eventsplitted)) {
                state = state+1
              }
            } else if(state > 1) {
              if(all(stringi::stri_split_fixed(d$trajectory[[i]][state],"&")[[1]] %in% eventsplitted)) {
                state = state+1
              } else {
                state=1
              }
            }
            if(state>length(d$trajectory[[i]])) {
              ParallelLogger::logDebug('...yes, it is.')
              subtraj_ids<-c(subtraj_ids,j)
              #print(subtraj_ids)
              d$total_count[i]<-d$total_count[i]+d$total_count[j]
              ParallelLogger::logDebug('New count of ',d$trajectory.str[[i]],' is ',d$total_count[i])
              break
            }
          }
        } # if(length(d$trajectory.str[[j]])>d$trajectory.str[[i]])

      } # for j

      d$is_subtrajectory_of[i]<-paste(subtraj_ids,collapse=",")
    } #for i
  }

  if(!is.na(filename)) {
    ParallelLogger::logInfo(' Step 3: Saving counts of aligned trajectories to file: {filename}...')
    write.table(d,file=filename,quote=FALSE, sep="\t", row.names=F, col.names=T)
    ParallelLogger::logInfo(' ...done.')
  }


  if(!is.na(filename_interpretation)) {
    ParallelLogger::logInfo(' Step 4: Saving graph interpretation to file: {filename_interpretation}...')
    #most frequent event pair
    most_frequent_edge<-igraph::E(g)[which(igraph::E(g)$alignedTrajsCount==max(igraph::E(g)$alignedTrajsCount))][1] # [1] here to take the first in case there are several equally frequent edges
    msg=paste0("For example, ",most_frequent_edge$alignedTrajsCount," event-periods (",round(most_frequent_edge$alignedTrajsProb*100,1),"%) have '",igraph::V(g)[igraph::V(g)$concept_id==most_frequent_edge$e2_concept_id]$name,"' right after '",igraph::V(g)[igraph::V(g)$concept_id==most_frequent_edge$e1_concept_id]$name,"'")
    INTERPRETER.MSG=c(INTERPRETER.MSG,msg)

    #Save graph interpretation to file
    fileConn<-file(filename_interpretation)
    writeLines(INTERPRETER.MSG, fileConn)
    close(fileConn)
    ParallelLogger::logInfo(' ...done.')
  }


  # make it of the class TrajectoriesGraph which is derived from the class igraph
  class(g) <- c("TrajectoriesGraph","igraph")

  return(g)
}


