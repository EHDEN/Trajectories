requireNamespace("igraph", quietly = TRUE)



#' Creates and fills alignment tables to the database for a given graph.
#' Particularly, creates table "graph_events" for events and table "graph_event_pairs" for the pairs
#'
#' @param connection
#' @param trajectoryAnalysisArgs
#' @param trajectoryLocalArgs
#' @param g  TrajectoriesGraph object
#'
#' @return TrajectoriesGraph object with filled E(g)$alignedTrajsCount, E(g)$alignedTrajsProb and V(g)$alignedTrajsCount values
#'
#' @examples
createAlignmentTableNew <- function(connection,
                                    trajectoryAnalysisArgs,
                                    trajectoryLocalArgs,
                                    g) {

  if(!inherits(g, 'TrajectoriesGraph')) stop('Error in createAlignmentTables(): object g is not class TrajectoriesGraph object')

  DEBUG=F

  ParallelLogger::logInfo('Creating alignment tables by filtering full event data for the graph containing ',length(igraph::E(g)),' event pairs...')

  #First, put event pairs of the graph into table
  ParallelLogger::logInfo('Putting ',length(igraph::E(g)),' event pairs of the graph into database.')
  e<-igraph::as_data_frame(g,what="edges")
  edges<- e %>% dplyr::select(e1_concept_id,e2_concept_id)

  #26 Sep 2020: Can't use simply SqlRender::insertTable here because in Eunomia package it does not solve schema name correctly. Therefore, currently using workaround function from Trajectories package
  Trajectories:::insertTable(connection=connection,
                             databaseSchema=trajectoryLocalArgs$resultsSchema,
                             tableName=paste0(trajectoryLocalArgs$prefixForResultTableNames,'mylinks'),
                             data=edges,
                             dropTableIfExists=T,
                             tempTable=F,
                             progressBar=T)

  #querySql(connection, paste0("SELECT COUNT(*) FROM ",tablename))

  #align trajectories to graph (to get the exact E1->E2 counts with no intermediate events)
  #Creates table "graph_events" for events and also pairs of these events to "graph_event_pairs" (does not include pairs between events that occur on the same day)
  ParallelLogger::logInfo('Filtering these event pairs in the database and extracting counts, and finally writing the counts back to the database...')
  RenderedSql <- Trajectories:::loadRenderTranslateSql("map_actual_trajs_to_graph4.sql",
                                                       packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                       dbms=attr(connection, "dbms"),
                                                       resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                       prefiX = trajectoryLocalArgs$prefixForResultTableNames
  )

  DatabaseConnector::executeSql(connection, RenderedSql)
  ParallelLogger::logInfo('... done.')

  #Get counts of all events
  tablename<-paste0(trajectoryLocalArgs$resultsSchema,'.',trajectoryLocalArgs$prefixForResultTableNames,'graph_events')
  actual_events<-querySql(connection, paste0("SELECT e as concept_id,COUNT(*) AS count FROM ",tablename,' GROUP BY e ORDER BY COUNT(*) DESC'))

  #Get counts of all event pairs
  tablename<-paste0(trajectoryLocalArgs$resultsSchema,'.',trajectoryLocalArgs$prefixForResultTableNames,'graph_event_pairs')
  actual_event_pairs<-querySql(connection, paste0("SELECT e1_concept_id,e2_concept_id,COUNT(*) AS count FROM ",tablename,' GROUP BY e1_concept_id,e2_concept_id ORDER BY COUNT(*) DESC'))

  #update alignedTrajsCount values
  v<-igraph::as_data_frame(g,what="vertices")
  e<-igraph::as_data_frame(g,what="edges")
  if('COUNT' %in% colnames(v)) v <- v %>% dplyr::select(-COUNT) #remove this to prevent old COUNT existing and leading to multiple COUNT columns after join
  if('count' %in% colnames(v)) v <- v %>% dplyr::select(-count) #remove this to prevent old COUNT existing and leading to multiple COUNT columns after join
  v <- v %>%
    dplyr::left_join(actual_events, by=c('concept_id'='CONCEPT_ID')) %>%
    dplyr::mutate(COUNT=dplyr::if_else(is.na(COUNT),as.integer(0),as.integer(COUNT)))
  e <- e %>%
    dplyr::left_join(actual_event_pairs, by=c('e1_concept_id'='E1_CONCEPT_ID', 'e2_concept_id'='E2_CONCEPT_ID')) %>%
    dplyr::mutate(COUNT=dplyr::if_else(is.na(COUNT),as.integer(0),as.integer(COUNT)))
  g2 <- igraph::graph_from_data_frame(e, directed=TRUE, vertices=v)

  #For reporting purposes create temporarily a filtered graph also (where edge count>0):
  g3 <- igraph::subgraph.edges(g2, igraph::E(g2)[igraph::E(g2)$COUNT>0], delete.vertices = TRUE)

  ParallelLogger::logInfo('Done. Out of ',length(igraph::V(g)),' events and ',length(igraph::E(g)),' pairs in original graph, ',length(igraph::V(g3)),' events and ',length(igraph::E(g3)),' pairs remained after applying count>0 filter.')

  # make it of the class TrajectoriesGraph which is derived from the class igraph
  class(g2) <- c("TrajectoriesGraph","igraph")

  return(g2)

}

getTrajectoryObjects<-function(component) {
  visited_nodes<-igraph::as_data_frame(component, what="vertices")
  visited_edges_ordered<-igraph::as_data_frame(component, what="edges") %>% arrange(E1_COHORT_DAY,E2_COHORT_DAY)
  e1_days<-visited_edges_ordered %>% dplyr::select(DAYNO=E1_COHORT_DAY,CONCEPT_ID=from)
  e2_days<-visited_edges_ordered %>% dplyr::select(DAYNO=E2_COHORT_DAY,CONCEPT_ID=to)
  visited_nodes_ordered<-rbind(e1_days,e2_days) %>% unique() %>% dplyr::arrange(DAYNO,CONCEPT_ID) %>% dplyr::group_by(DAYNO) %>% dplyr::summarise(CONCEPT_GROUPED=paste(CONCEPT_ID,collapse="&")) %>% dplyr::pull(CONCEPT_GROUPED)
  #for 0-length trajectories, add eventid manually to visitednodes (do not come automatically as there is no event pair in that case)
  if(length(visited_nodes_ordered)==0) {
    #there should be a single node (that form a single-node trajectory)
    if(nrow(visited_nodes)==1) {
      visited_nodes_ordered<-c(visited_nodes[1,'name'])
    } else {
      ParallelLogger::logWarn('ERROR in getTrajectoryObjects(): there are no visited events created from pairs, but there are ',nrow(visited_nodes),' events (nodes) in the decomposed graph. Should not happen normally.')
    }
    visited_nodes_ordered=c(eventid)
  }
  trajectory.str=paste(visited_nodes_ordered,collapse="-")
  return(
    list(
      trajectory= I(list(visited_nodes_ordered)), #this I(list(...)) is here to prevent R to flatten the list. See https://stackoverflow.com/questions/51307970/how-to-store-vector-in-dataframe-in-r
      trajectory.str=trajectory.str
    )
  )
}

addToAllTrajs<-function(all_trajs,trajObjects) {
  traj.obj<-trajObjects$trajectory
  traj.str<-trajObjects$trajectory.str
  if(nrow(all_trajs)==0) {
    all_trajs=data.frame(trajectory.str=traj.str, trajectory=traj.obj, exact_count=1)
  } else {
    if(traj.str %in% all_trajs$trajectory.str) {
      idx <- which(all_trajs$trajectory.str==traj.str)
      all_trajs[idx, 'exact_count'] <- all_trajs[idx, 'exact_count']+1
    } else {
      all_trajs <- rbind(all_trajs, data.frame(trajectory.str=traj.str, trajectory=traj.obj, exact_count=1))
    }
  }
  return(all_trajs)
}

saveAllTrajs<-function(allTrajObjects,trajectoryAnalysisArgs,trajectoryLocalArgs) {

    #order by count desc
  allTrajObjects <- allTrajObjects %>% dplyr::arrange(-trajectory.count,-length)

    #reorder columns - id column to first
  allTrajObjects <- allTrajObjects %>% dplyr::select(id,tidyselect::everything())

    #convert trajectory.str.names from list to chr
  allTrajObjects$trajectory.str.names<-unlist(allTrajObjects$trajectory.str.names)

    outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F)
    xlsxfile=file.path( outputFolder,'tables','trajectory_counts.xlsx')
    rdatafile=file.path(outputFolder,'tables','trajectory_counts.RData')

    ParallelLogger::logInfo('Saving trajectory counts to Excel-file: ',xlsxfile,'...')
    openxlsx::write.xlsx(allTrajObjects,file=xlsxfile, overwrite=T)
    ParallelLogger::logInfo(' ...done.')

    ParallelLogger::logInfo('Saving trajectory counts to R-object-file: ',rdatafile,'...')
    save(allTrajObjects, file=rdatafile)
    ParallelLogger::logInfo(' ...done.')
}

addEventNamesToTrajectories<-function(all_trajs, Node.names) {
  ParallelLogger::logInfo('Adding event names to trajectory descriptions (may take some time if the number of different trajectories is large)...')

  all_trajs <- all_trajs %>% tibble::add_column(trajectory.str.names=NA)
  all_trajs$trajectory.str.names<-lapply(all_trajs$trajectory.str,Trajectories:::getTrajectoryStringWithNames, Node.names=Node.names)

  ParallelLogger::logInfo('...done.')
  return(all_trajs)
}

getTrajectoryStringWithNames<-function(trajectory.str,Node.names) {

  trajectory=strsplit(as.character(trajectory.str),"-",fixed=TRUE)[[1]]

  elem_eventnames_as_str=c()
  for(e in 1:length(trajectory)) {
    elem=trajectory[e]
    elem_events=strsplit(as.character(elem),"&",fixed=TRUE)

    elem_eventnames <- data.frame(concept_id=as.character(elem_events[[1]]), stringsAsFactors=FALSE) %>%
      dplyr::left_join(Node.names, by=c("concept_id"="concept_id")) %>%
      dplyr::mutate(namelength=stringi::stri_length(concept_name)) %>%
      dplyr::mutate(node=paste0(concept_id,":\"",  ifelse(namelength<=20, concept_name, paste0(substr(concept_name,1,20), "...")),"\"")) %>%
      dplyr::pull(node)
    elem_eventnames_as_str=c(elem_eventnames_as_str, paste(elem_eventnames,collapse="&"))
  }
  trajectory.names.str<-paste0(elem_eventnames_as_str,collapse=" -> ")
  return(trajectory.names.str)
}


#' Title
#'
#' @param all_trajs Must have columns trajectory.str, trajectory.count, length
#'
#' @return
#'
#' @examples
detectSubgraphs<-function(all_trajs) {
  stopifnot("trajectory.str" %in% colnames(all_trajs))
  stopifnot("trajectory.count" %in% colnames(all_trajs))
  stopifnot("length" %in% colnames(all_trajs))

  ParallelLogger::logInfo('Detecting sub-trajectories...')
  d<-all_trajs
  d <- d %>% tibble::add_column(is_subtrajectory_of=NA)
  d$trajectory=strsplit(as.character(d$trajectory.str),"-",fixed=TRUE)

  #make sure that trajectories are still ordered by count desc (trajectories with larger count cannot be sub-trajectories of smaller count)
  d<-d %>% dplyr::arrange(-trajectory.count,-length)

  #d <- d %>% dplyr::select(id,trajectory,trajectory.str,exact_count,total_count,length,is_subtrajectory_of,trajectory.names.str)
  if(nrow(d)>1) {

    starttime=Sys.time()
    for(i in 1:(nrow(d)-1)) {
      #print(i)



      ParallelLogger::logInfo('Grouping trajectories ',i,'/',(nrow(d)-1),' for: ',d$trajectory.str[[i]],', total progress ',
                              round(100*(i/(nrow(d)-1))),'%, ETA: ',Trajectories:::estimatedTimeRemaining(progress_perc=i/(nrow(d)-1),starttime=starttime),
                              ')...')



      #i can be a sub-trajectory of j only if:
      #a) i's length <= j's length; and
      #b) all elements of i are present in j
      # The problem with (b) is that elements can combine several elements (e.g A01&A02 in j, but only A01 in i). Therefore, the fast check does not take elements with & into account.

      comparator <- d[(1+i):nrow(d),] %>% filter(length>=d$length[i])
      i_elems_without_combined=grep('&',d$trajectory[[i]],invert=T,fixed=T,value=T)
      j_elems_without_combined=comparator$trajectory[!grepl('&',comparator$trajectory,fixed=T)]
      comparator <- comparator[unlist(lapply(j_elems_without_combined,function(x) {all(i_elems_without_combined %in% x)})),]
      comparator<-comparator %>% dplyr::arrange(-trajectory.count,-length)

      subtraj_ids=c()
      if(nrow(comparator)>0){
        for(j in 1:nrow(comparator)) {

          #regex/state-machine logic here
          state = 1
          for (event in comparator$trajectory[[j]]) {
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
              #ParallelLogger::logInfo('...yes, it is.')
              subtraj_ids<-c(subtraj_ids,comparator$id[j])
              break
            }
          }
          #} # if(length(d$trajectory.str[[j]])>d$trajectory.str[[i]])

        } # for j
      }

      d$is_subtrajectory_of[i]<-paste(subtraj_ids,collapse=",")
    } #for i
  }

  d <- d %>% dplyr::select(-trajectory)

  ParallelLogger::logInfo('...detection of sub-trajectories completed.')
  return(d)
}

removeOldTrajectoryResults<-function(trajectoryAnalysisArgs,trajectoryLocalArgs) {
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F)
  xlsxfile=file.path( outputFolder,'tables','trajectory_counts.xlsx')
  rdatafile=file.path(outputFolder,'tables','trajectory_counts.RData')
  if(file.exists(xlsxfile)) file.remove(xlsxfile)
  if(file.exists(rdatafile)) file.remove(rdatafile)
}




getTrajectoryObjectsNew<-function(visited_concepts_ordered,traj_count) {
  trajectory.str=paste(visited_concepts_ordered,collapse="-")
  return(
    data.frame(
      trajectory.str=trajectory.str,
      trajectory.count=traj_count
    )
  )
}


addToAllTrajsNew<-function(all_trajs,trajObjects) {
  if(nrow(all_trajs)==0) {
    all_trajs=trajObjects
  } else {
    if(any(trajObjects$trajectory.str %in% all_trajs$trajectory.str)) {
      stop(paste0('ERROR in addToAllTrajsNew: trying to add',trajObjects$trajectory.str,' but it is already in all_trajs.'))
    } else {
      all_trajs <- rbind(all_trajs, trajObjects)
    }
  }
  return(all_trajs)
}

getEventperiodsOfEdge<-function(connection,trajectoryLocalArgs, g, edge, limit_to_eventperiods=c()) {

  edge.start.concept_id=as.numeric(igraph::ends(g,edge)[1,1])
  edge.end.concept_id=as.numeric(igraph::ends(g,edge)[1,2])
  #When reaching this point, the edge has count >= MIN.TRAJ.COUNT
  #However, we need to identify the eventperiod_ids

  sql="SELECT DISTINCT EVENTPERIOD_ID FROM @resultsSchema.@prefiXgraph_event_pairs
        WHERE
          e1_concept_id=@e1
          AND
          e2_concept_id=@e2
          AND
          e2_cohort_day >= e1_cohort_day"
  if(length(limit_to_eventperiods)>0) {

    #Can't use simply SqlRender::insertTable here because in Eunomia package it does not solve schema name correctly. Therefore, currently using workaround function from Trajectories package
    Trajectories:::insertTable(connection=connection,
                               databaseSchema=trajectoryLocalArgs$resultsSchema,
                               tableName=paste0(trajectoryLocalArgs$prefixForResultTableNames,'traj_eventperiods'),
                               data=data.frame(EVENTPERIOD_ID=limit_to_eventperiods),
                               dropTableIfExists=T,
                               tempTable=F,
                               progressBar=F)


    sql<-paste0(sql," AND eventperiod_id IN (SELECT EVENTPERIOD_ID FROM @resultsSchema.@prefiXtraj_eventperiods)")
  }
  sql<-paste0(sql,";")
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames, e1=edge.start.concept_id, e2=edge.end.concept_id)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))

  eventperiod_ids=res$EVENTPERIOD_ID
  return(eventperiod_ids)
}

expandTrajectory<-function(connection,trajectoryLocalArgs,g,MIN.TRAJ.COUNT,traj,eventperiod_ids=c()) {
  allTrajObjects=data.frame(
    trajectory.str=c(),
    trajectory.count=c()
  )

  logSpacer=paste0(rep("  ",length(traj)),collapse="")

  ParallelLogger::logInfo(logSpacer,'Expanding trajectory ',paste0(traj,collapse="-"),'...')

  #Get the last element of the trajectory
  if(length(traj)==0) stop('ERROR in expandTrajectory: length(traj)=0 which shoult not happen. You should not call this function without a single node as trajectory.')
  if(length(traj)>10) stop('ERROR in expandTrajectory: length(traj)>10. Is everything OK?')


  #if(length(eventperiod_ids)==0) {
  #  traj.count=0
  #} else {
  #  traj.count=length(eventperiod_ids)
  #}
  last.concept.id<-traj[length(traj)]
  node=igraph::V(g)[igraph::V(g)$concept_id==last.concept.id]

  #find outgoing edges from the node
  outgoing.edges <- igraph::incident(g, node, mode="out")
  if(length(outgoing.edges)==0) {
    ParallelLogger::logDebug(logSpacer,'...no outgoing edges found from ',last.concept.id,' (or their count<',MIN.TRAJ.COUNT,'). Therefore, the trajectory counting stops here.')
    return(allTrajObjects)
  } else {

    #loop over edges
    for(e.idx in 1:length(outgoing.edges)) {
      edge=outgoing.edges[e.idx]
      edge.end.concept_id=as.numeric(igraph::ends(g,edge)[1,2])

      #if the current trajectory is A->B->C and the new node to be added is D, but C->D or B->C->D is already tested and any of these found count<MIN.TRAJ.COUNT then there is no need to test A->B->C->D
      #The following method isTrajRareBasedOnPrevCounts() automatically updates the global list of rare trajectories also
      if(!Trajectories:::isTrajRareBasedOnPrevCounts(traj,edge.end.concept_id)) {

        edge.eventperiod_ids=Trajectories:::getEventperiodsOfEdge(connection, trajectoryLocalArgs, g, edge, limit_to_eventperiods=eventperiod_ids)

        if(length(edge.eventperiod_ids)>=MIN.TRAJ.COUNT) {
          #Can expand the trajectory to new event
          new.traj<-c(traj,edge.end.concept_id)

          trajObjects<-getTrajectoryObjectsNew(new.traj,traj_count=length(edge.eventperiod_ids))
          allTrajObjects<-Trajectories:::addToAllTrajsNew(allTrajObjects,trajObjects)
          ParallelLogger::logDebug(logSpacer,'...added trajectory ',paste0(new.traj, collapse="-"),' (count=',length(edge.eventperiod_ids),').')


          #nested call to expand trajectory from this event further
          trajObjects<-Trajectories:::expandTrajectory(connection, trajectoryLocalArgs,g,MIN.TRAJ.COUNT,new.traj,eventperiod_ids=edge.eventperiod_ids)

          #add result to all trajObjects as we continue the search from other edges as well
          if(nrow(trajObjects)>0) {
            allTrajObjects<-Trajectories:::addToAllTrajsNew(allTrajObjects,trajObjects)
          }

        } else {
          ParallelLogger::logDebug(logSpacer,'...the number of trajectories that reach ',paste0(traj,collapse="-"),'->',edge.end.concept_id,' is ',length(edge.eventperiod_ids),' which is less than MIN.TRAJ.COUNT. Therefore, the trajectory counting stops here.')
          #update global list of rare trajectories
          new.traj<-c(traj,edge.end.concept_id)
          rare_trajectories <- get('RARE_TRAJECTORIES', envir=TRAJECTORIES.CONSTANTS)
          rare_trajectories <- c(rare_trajectories, paste0(new.traj,collapse="-"))
          assign('RARE_TRAJECTORIES', rare_trajectories, envir=TRAJECTORIES.CONSTANTS)
        }
      } else {
        ParallelLogger::logDebug(logSpacer,'...based on previous counts, the event is rare. No need to expand it any further.')
      }

    } #for
    ParallelLogger::logDebug(logSpacer,'End of for-loop. Returning allTrajObjects')
    #print(allTrajObjects)

    # All outgoing edges scanned through
    return(allTrajObjects)

  }
}

addTrajLength<-function(allTrajObjects) {
  trajectory.lengths <- lapply(strsplit(as.character(allTrajObjects$trajectory.str),"-",fixed=TRUE),length)
  allTrajObjects <- allTrajObjects %>% tibble::add_column(length=NA)
  if(length(trajectory.lengths)>0) allTrajObjects$length=unlist(trajectory.lengths)
  return(allTrajObjects)
}


#if the current trajectory is A->B->C and the new node to be added is D, but C->D or B->C->D is already tested and any of these found count<MIN.TRAJ.COUNT then there is no need to test A->B->C->D
isTrajRareBasedOnPrevCounts<-function(traj,edge.end.concept_id) {

  new.traj=c(traj,edge.end.concept_id)
  ParallelLogger::logDebug('isTrajRareBasedOnPrevCounts: Testing whether ',paste0(new.traj,collapse="-"),' has a low count based on the counts of its subtrajectories...')

  rare_trajectories <- get('RARE_TRAJECTORIES', envir=TRAJECTORIES.CONSTANTS)

  #For 2-element trajectory should never be considered as rare before calculation
  if(length(traj)<=1) return(FALSE)

  for(i in 1:(length(traj)-1)) {
    traj_to_test <- c(traj[(length(traj)-i+1):length(traj)], edge.end.concept_id)
    traj_to_test.as.str=paste0(traj_to_test,collapse="-")
    ParallelLogger::logDebug('isTrajRareBasedOnPrevCounts: Testing whether ',traj_to_test.as.str,' is in global list of rare trajectories...')
    if(traj_to_test.as.str %in% rare_trajectories) {
      ParallelLogger::logDebug('isTrajRareBasedOnPrevCounts: ',paste0(new.traj,collapse="-"),' has a low count based on the previously calculated counts of ',traj_to_test.as.str)

      #update global list of rare trajectories also
      rare_trajectories <- c(rare_trajectories, paste0(new.traj,collapse="-"))
      assign('RARE_TRAJECTORIES', rare_trajectories, envir=TRAJECTORIES.CONSTANTS)

      return(TRUE)
    } else {
      ParallelLogger::logDebug('isTrajRareBasedOnPrevCounts: ...no, it is not.')
    }
  }
  ParallelLogger::logDebug('isTrajRareBasedOnPrevCounts: based on the counts of subtrajectories, we cannot say that ',paste0(new.traj,collapse="-"),' has a low count. Need to calculate it!')
  return(FALSE)

}

countTrajectories<-function(connection,
                 trajectoryAnalysisArgs,
                 trajectoryLocalArgs) {

  ParallelLogger::logInfo('Starting counting of longer trajectories...')

  Trajectories:::removeOldTrajectoryResults(trajectoryAnalysisArgs,trajectoryLocalArgs)

  allTrajObjects=data.frame(
    trajectory.str=character(),
    trajectory.count=numeric()
  )

  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F)
  eventPairResultsFilename = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  g<-Trajectories:::createTrajectoriesGraph(eventPairResultsFilename=eventPairResultsFilename)

  if(length(igraph::E(g))>0) {

    #Prepare data tables in the database
    g<-Trajectories:::createAlignmentTableNew(connection,
                                              trajectoryAnalysisArgs,
                                              trajectoryLocalArgs,
                                              g)

    # APPLY MINIMIUM COUNT THRESHOLD
    #MIN.TRAJ.COUNT=20 #Do not count trajectories that have less than this (in Siggaard et al. 2020 the number was 20)
    MIN.TRAJ.COUNT<-Trajectories:::getMinPatientsPerEventPair(connection,
                                                              trajectoryAnalysisArgs,
                                                              trajectoryLocalArgs)
    g <- igraph::subgraph.edges(g, igraph::E(g)[igraph::E(g)$COUNT>=MIN.TRAJ.COUNT], delete.vertices = TRUE)
    ParallelLogger::logInfo(length(igraph::V(g)),' events and ',length(igraph::E(g)),' pairs remained after applying count>=',MIN.TRAJ.COUNT,' filter.')


    if(length(igraph::E(g))>0) {

      # Topological sort: first nodes to left (can be used only if the graph is acyclic)
      if(igraph::is_dag(g)) {
        node_order<-igraph::topo_sort(g, mode="in") #start from the right (last nodes). Should add some speed performance
      } else {
        #In casea graph is cyclic, topo_sort returns partial node list as result. Therefore, the we take the partial node list and add to it all the remaining nodes
        node_order_short<-suppressWarnings(igraph::topo_sort(g, mode="in"))
        if(length(node_order_short)<length(igraph::V(g)))
        node_order<- c(node_order_short, igraph::V(g)[setdiff(igraph::V(g),node_order_short)]) #add the remaining nodes
      }
      #make sure that no nodes are left out
      if(length(node_order)!=length(igraph::V(g))) {
        ParallelLogger::logInfo('Something is not right in countTrajectories(): the graph has ',length(igraph::V(g)),' edges but in the node order there are only ',length(node_order),' nodes.')
      }

      #Loop through all nodes and count the trajectory patterns
      assign('RARE_TRAJECTORIES', c(), envir=TRAJECTORIES.CONSTANTS) #clear previous counts of rare trajectories
      starttime=Sys.time()
      for(i in 1:length(node_order)) {
        node=node_order[i]
        concept_id=node$concept_id
        ParallelLogger::logInfo('Counting trajectories ',i,'/',length(node_order),' starting with ',concept_id,' (event count=',node$COUNT,'), total progress ',
                                round(100*(i/length(node_order))),'%, ETA: ',Trajectories:::estimatedTimeRemaining(progress_perc=(i-1)/length(node_order),starttime=starttime),
                                '...')


        trajObjects<-Trajectories:::expandTrajectory(connection,trajectoryLocalArgs,g,MIN.TRAJ.COUNT=MIN.TRAJ.COUNT,traj=c(concept_id),eventperiod_ids=c())
        if(nrow(trajObjects)>0) {
          allTrajObjects<-Trajectories:::addToAllTrajsNew(allTrajObjects,trajObjects)
          #print('allTrajObjects is now:')
          #print(allTrajObjects)
        }
      } #for


    } else {
      ParallelLogger::logInfo('Size of the graph after limiting by the count>=',MIN.TRAJ.COUNT,' is 0, nothing to count...')
    }



  } else {
    ParallelLogger::logInfo('Size of the graph is 0, nothing to count...')
  }



  #load('all_trajectories.R')

  #sort by: count descending
  allTrajObjects <- allTrajObjects %>% arrange(-trajectory.count)

  #Add trajectory length column
  allTrajObjects <- allTrajObjects %>% Trajectories:::addTrajLength()

  #Add id column
  allTrajObjects <- allTrajObjects %>% mutate(id=row_number())

  #detect subgraphs
  #To prevent very resource demanding calculations, limit the scope of subgraph detection if the number of trajectories is very big
  all_trajs_left<-allTrajObjects
  MAX_NUM_TRAJS_TO_COUNT=1000
  if(nrow(allTrajObjects)>MAX_NUM_TRAJS_TO_COUNT) {
    ParallelLogger::logInfo('Due to the large number of different trajectories (n=',nrow(all_trajs_left),'), limiting subgraph detection analysis to the subset only...')
    n=1
    while(nrow(all_trajs_left)>MAX_NUM_TRAJS_TO_COUNT) {
      n=n+1
      all_trajs_left<-all_trajs_left %>% dplyr::filter(trajectory.count>=n)
    }
    ParallelLogger::logInfo('Subgraph detection analysis limited to the trajectories that occur at least ',n,' times (n=',nrow(all_trajs_left),')...')

    #detect subgraphs of this subset
    all_trajs_left<-Trajectories:::detectSubgraphs(all_trajs_left)

    #put the results back to the original allTrajObjects dataframe
    if('is_subtrajectory_of' %in% colnames(allTrajObjects)) allTrajObjects <- allTrajObjects %>% dplyr::select(-is_subtrajectory_of)
    allTrajObjects <- allTrajObjects %>% dplyr::left_join(all_trajs_left %>% dplyr::select(id, is_subtrajectory_of) %>% dplyr::mutate(subtrajectory_analyzed=T), by=c('id'='id'))
    allTrajObjects <- allTrajObjects %>% mutate(
      is_subtrajectory_of=ifelse(subtrajectory_analyzed==T,is_subtrajectory_of,NA) ) %>%
      select(-subtrajectory_analyzed)

  } else {
    allTrajObjects<-Trajectories:::detectSubgraphs(allTrajObjects)

  }

  #Add node names
  Node.names<-igraph::as_data_frame(g, what="vertices") %>% dplyr::select(concept_id,concept_name) %>% mutate(concept_id=as.character(concept_id))
  allTrajObjects <- allTrajObjects %>% Trajectories:::addEventNamesToTrajectories(Node.names)

  #Save trajectories to TSV and RData file
  Trajectories:::saveAllTrajs(allTrajObjects,trajectoryAnalysisArgs,trajectoryLocalArgs)

  ParallelLogger::logInfo('Trajectory counting completed.')
}

