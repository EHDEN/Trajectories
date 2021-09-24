requireNamespace("igraph", quietly = TRUE)



#' Creates and fills alignment tables to the database for a given graph.
#' Particularly, creates table "graph_events" for events and table "graph_event_pairs" for the pairs
#' Note that these pairs are not "between any combination" of events like in discovery/validation analysis,
#' but specifically between the events that are occurring temporally next to each other.
#' For example, A->B->C produces pairs A->B and B->C only.
#'
#' Also note that "graph_event_pairs" does not include pairs between events that occur on the same day.
#' In case B and C occur on the same day on A->B/C->D trajectory, the pairs A->B, A->C, A->D, B->D, C->D are created.
#' Therefore, a single eventperiod can still produce several "paths" in the graph from A to D.
#'
#' In case a patient has A->B->C->D, but there are only edges A->B, A->D, C->D on the graph, it also produces 2 separate trajectories: A->B and C->D
#'
#' @param connection
#' @param trajectoryAnalysisArgs
#' @param trajectoryLocalArgs
#' @param g  TrajectoriesGraph object
#'
#' @return TrajectoriesGraph object with filled E(g)$alignedTrajsCount, E(g)$alignedTrajsProb and V(g)$alignedTrajsCount values
#' @export
#'
#' @examples
createAlignmentTables <- function(connection,
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
  #Takes all trajectories that pass any event of that graph. Leaves out intermediate events that are not given in the graph.
  #Creates table "graph_events" for events and also pairs of these events to "graph_event_pairs" (does not include pairs between events that occur on the same day)
  ParallelLogger::logInfo('Filtering these event pairs in the database and extracting counts, and finally writing the counts back to the database...')
  RenderedSql <- Trajectories:::loadRenderTranslateSql("map_actual_trajs_to_graph3.sql",
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

  #Calculate percentages
  #Get cohortsize (needed for calculating the percentages later)
  sql<-"SELECT COUNT(*) AS COHORTSIZE FROM @resultsSchema.@prefiXmycohort;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))
  COHORTSIZE=res$COHORTSIZE
  actual_events$PERC <- actual_events$COUNT/COHORTSIZE
  actual_event_pairs$PERC <- actual_event_pairs$COUNT/COHORTSIZE

  #clear all alignedTrajsCount values
  igraph::E(g)$alignedTrajsCount<-0
  igraph::V(g)$alignedTrajsCount<-0
  igraph::E(g)$alignedTrajsProb<-0


  #update alignedTrajsCount values
  v<-igraph::as_data_frame(g,what="vertices")
  e<-igraph::as_data_frame(g,what="edges")
  if('COUNT' %in% colnames(v)) v <- v %>% dplyr::select(-COUNT) #remove this to prevent old COUNT existing and leading to multiple COUNT columns after join
  if('count' %in% colnames(v)) v <- v %>% dplyr::select(-count) #remove this to prevent old COUNT existing and leading to multiple COUNT columns after join
  v <- v %>%
    dplyr::left_join(actual_events, by=c('concept_id'='CONCEPT_ID')) %>%
    dplyr::mutate(alignedTrajsCount=dplyr::if_else(is.na(COUNT),as.integer(0),as.integer(COUNT)))
  e <- e %>%
    dplyr::left_join(actual_event_pairs, by=c('e1_concept_id'='E1_CONCEPT_ID', 'e2_concept_id'='E2_CONCEPT_ID')) %>%
    dplyr::mutate(alignedTrajsCount=dplyr::if_else(is.na(COUNT),as.integer(0),as.integer(COUNT)))
  g2 <- igraph::graph_from_data_frame(e, directed=TRUE, vertices=v)


  #create alignedTrajsProb values
  edge.start <- igraph::ends(g2, es=igraph::E(g2), names=F)[,1] #outputs the start node id of each edge
  igraph::E(g2)$alignedTrajsProb <- igraph::E(g2)$alignedTrajsCount/igraph::V(g2)$alignedTrajsCount[edge.start]


  #For reporting purposes create temporarily a filtered graph also (where edge count>0):
  g3 <- igraph::subgraph.edges(g2, igraph::E(g2)[igraph::E(g2)$alignedTrajsCount>0], delete.vertices = TRUE)

  ParallelLogger::logInfo('Done. Out of ',length(igraph::V(g)),' events and ',length(igraph::E(g)),' pairs in original graph, ',length(igraph::V(g3)),' events and ',length(igraph::E(g3)),' pairs remained after applying count>0 filter.')

  # make it of the class TrajectoriesGraph which is derived from the class igraph
  class(g2) <- c("TrajectoriesGraph","igraph")


  return(g2)
}

#' Takes actual event trajectories of the cohort and puts them to the given graph. Counts all different trajectories. Expects that the graph is created by createAlignmentTables() method which has created necessary database tables
#'
#' @param connection
#' @param trajectoryAnalysisArgs
#' @param trajectoryLocalArgs
#' @param g TrajectoriesGraph object, that is created by createAlignmentTables() method (that ensures that necessary database tables exist)
#'
#' @return
#' @export
#'
#' @examples
alignTrajectoriesToGraph<-function(connection,
                         trajectoryAnalysisArgs,
                         trajectoryLocalArgs,
                         g) {

  DEBUG=F

  if(!inherits(g, 'TrajectoriesGraph')) stop('Error in alignTrajectoriesToGraph(): object g is not class TrajectoriesGraph object')
  if(any(is.na(igraph::E(g)$alignedTrajsCount)==T)) stop("Error in alignTrajectoriesToGraph(): edges of g does not have attribute alignedTrajsCount. Is this object created by createAlignmentTables() method as required?")


  #Create a list of concept names (for faster search later when adding names to the concept_id-s)
  Node.names<-igraph::as_data_frame(g, what="vertices") %>% dplyr::select(concept_id,concept_name) %>% mutate(concept_id=as.character(concept_id))

  all_trajs<-data.frame()
  number_of_single_node_trajs=0

  #Get all eventperiods
  sql<-"SELECT DISTINCT(EVENTPERIOD_ID) AS EVENTPERIOD_ID FROM @resultsSchema.@prefiXgraph_events ORDER BY EVENTPERIOD_ID;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))
  EVENTPERIOD_IDS=res$EVENTPERIOD_ID

  #create chunks - in each chunk, 100 persons
  chunks<-split(EVENTPERIOD_IDS, ceiling(seq_along(EVENTPERIOD_IDS)/100))
  if(length(chunks)>1) {
    starttime=Sys.time()
    for(i in 1:length(chunks)) {
      ParallelLogger::logDebug('i=',i)
      chunk<-chunks[[i]]
      ParallelLogger::logInfo('Putting ',length(chunk),' event-periods to the graph (',i,'/',length(chunks),', total progress ',
                              round(100*(i/length(chunks))),'%, ETA: ',Trajectories:::estimatedTimeRemaining(progress_perc=(i-1)/length(chunks),starttime=starttime),
                              ')...')

      #Get event pairs of these eventperiods (note that the trajectory of one patient can be split into several sections (some trajectories) )
      RenderedSql <- Trajectories:::loadRenderTranslateSql("get_actual_trajs_to_graph2.sql",
                                                           packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                           dbms=attr(connection, "dbms"),
                                                           resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                           prefiX = trajectoryLocalArgs$prefixForResultTableNames,
                                                           eventperiodids=paste(chunk,collapse=",")
      )
      res<-DatabaseConnector::querySql(connection, RenderedSql)


      for(k in 1:length(chunk)) {
        cohort_id<-chunk[k]
        ParallelLogger::logDebug('Aligning event-period #',(i-1)*100+k,": ",cohort_id)
        pairs<-res %>% dplyr::filter(EVENTPERIOD_ID==get('cohort_id'))

        if(nrow(pairs)==0) { #this is a "unconnected events" eventperiod, no event pairs constructed
          number_of_single_node_trajs=number_of_single_node_trajs+1
          ParallelLogger::logDebug('Eventperiod ',cohort_id,' does not produce any trajectories on that graph.')

        } else {

          #A graph of this eventperiod
          v<-igraph::as_data_frame(g, what="vertices") %>% dplyr::select(concept_id,name)
          w<-pairs %>% dplyr::left_join(v, by=c("E1_CONCEPT_ID"="concept_id")) %>% dplyr::rename(E1=name)
          w<-w %>% dplyr::left_join(v, by=c("E2_CONCEPT_ID"="concept_id")) %>% dplyr::rename(E2=name)
          x<-igraph::graph_from_data_frame(w %>% dplyr::select(E1,E2,E1_COHORT_DAY,E2_COHORT_DAY), directed = TRUE, vertices = NULL)

          if(DEBUG) {
            #plot(x,layout=layout_)
            plot(x,main=paste0("Actual trajectories of eventperiod ",cohort_id))

            #Actual order of the events
            sql="SELECT CONCEPT_ID,date from @resultsSchema.@prefiXevents WHERE EVENTPERIOD_ID=@id AND CONCEPT_ID in (
                  select e1_concept_id as e from @resultsSchema.@prefiXmylinks
                  union
                  select e2_concept_id as e from @resultsSchema.@prefiXmylinks) ORDER BY date;"
            RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames, id=cohort_id)
            RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
            res2<-c(DatabaseConnector::querySql(connection, RenderedSql))
            eventtable<-data.frame(date=res2$DATE, event=res2$CONCEPT_ID)
            eventtable

            #Actual order of the graph events
            sql="SELECT e AS CONCEPT_ID,cohort_day AS DATE from @resultsSchema.@prefiXgraph_events WHERE EVENTPERIOD_ID=@id AND e in (
                  select e1_concept_id as e from @resultsSchema.@prefiXmylinks
                  union
                  select e2_concept_id as e from @resultsSchema.@prefiXmylinks) ORDER BY cohort_day;"
            RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames, id=cohort_id)
            RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
            res2<-c(DatabaseConnector::querySql(connection, RenderedSql))
            eventtable<-data.frame(date=res2$DATE, event=res2$CONCEPT_ID)
            eventtable

            for(j in 1:nrow(pairs)) {
              pair<-pairs[j,]
              #check that the edge/pair exists in our graph
              eid<-which(igraph::E(g)$e1_concept_id==pair$E1_CONCEPT_ID & igraph::E(g)$e2_concept_id==pair$E2_CONCEPT_ID)
              if(length(eid)==1) {
                ParallelLogger::logDebug('Added ',pair$E1_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==pair$E1_CONCEPT_ID]$name,'->',pair$E2_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==pair$E2_CONCEPT_ID]$name,' (eventperiod ',cohort_id,') to graph')
              } else {
                ParallelLogger::logWarn('Cannot add ',pair$E1_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==pair$E1_CONCEPT_ID]$name,'->',pair$E2_CONCEPT_ID,':',igraph::V(g)[igraph::V(g)$concept_id==pair$E2_CONCEPT_ID]$name,' (eventperiod ',cohort_id,') as this edge is not part of the graph. Should not happen, actually.')
              }

            } #for pair
          } #if DEBUG

          #break graph into components (a graph may contain unconnected subgraphs - each is a separate trajectory. Add each trajectory to all_trajs)
          components <- igraph::decompose(x, mode='weak', min.vertices=0)
          for(m in 1:length(components)) {
            component=components[[m]]
            #skip single-event components (with no edges)
            if(length(igraph::E(component))>0) {
              trajObjects<-Trajectories:::getTrajectoryObjects(component)
              all_trajs <- all_trajs %>% Trajectories:::addToAllTrajs(trajObjects)
            }
          } #for m

        } #if-else

      } #for k

    } #for i
  } # if(length(chunks)>1)

  ParallelLogger::logInfo("In total, ",nrow(all_trajs),' different trajectories on the given graph were identified.')

  #Calculate trajectory lengths
  all_trajs$length<-as.numeric(lapply(all_trajs$trajectory,length))

  #Add id-s
  all_trajs <- all_trajs %>%
    dplyr::arrange(-exact_count,-length) %>%
    dplyr::mutate(id=dplyr::row_number())
  all_trajs$total_count=all_trajs$exact_count

  #Save trajectories to TSV and RData file
  Trajectories:::saveAllTrajs(all_trajs,trajectoryAnalysisArgs,trajectoryLocalArgs)

  #Save trajectories to TSV and RData file
  Trajectories:::saveAllTrajs(all_trajs,trajectoryAnalysisArgs,trajectoryLocalArgs)

  #Add column trajectory.names.str
  all_trajs <- all_trajs %>% Trajectories:::addEventNamesToTrajectories(Node.names)

  #detect subgraphs
  #To prevent very resource demanding calculations, limit the scope of subgraph detection if the number of trajectories is very big
  all_trajs_left<-all_trajs
  MAX_NUM_TRAJS_TO_COUNT=10000
  if(nrow(all_trajs)>MAX_NUM_TRAJS_TO_COUNT) {
    ParallelLogger::logInfo('Due to the large number of different trajectories (n=',nrow(all_trajs_left),'), limiting subgraph detection analysis to the subset only...')
    n=1
    while(nrow(all_trajs_left)>MAX_NUM_TRAJS_TO_COUNT) {
     n=n+1
     all_trajs_left<-all_trajs_left %>% dplyr::filter(exact_count>=n)
    }
    ParallelLogger::logInfo('Subgraph detection analysis limited to the trajectories that occur at least ',n,' times (n=',nrow(all_trajs_left),')...')

    #detect subgraphs of this subset
    all_trajs_left<-Trajectories:::detectSubgraphs(all_trajs_left)

    #put the results back to the original all_trajs dataframe
    if('total_count' %in% colnames(all_trajs)) all_trajs <- all_trajs %>% dplyr::select(-total_count)
    if('is_subtrajectory_of' %in% colnames(all_trajs)) all_trajs <- all_trajs %>% dplyr::select(-is_subtrajectory_of)
    all_trajs <- dplyr::left_join(all_trajs_left %>% dplyr::select(id, approx_total_count=total_count, is_subtrajectory_of), by=c('id'='id'))

  } else {
    all_trajs<-Trajectories:::detectSubgraphs(all_trajs)

  }


  #Save trajectories to TSV and RData file
  Trajectories:::saveAllTrajs(all_trajs,trajectoryAnalysisArgs,trajectoryLocalArgs)

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

saveAllTrajs<-function(all_trajs,trajectoryAnalysisArgs,trajectoryLocalArgs) {

    if(!'exact_count' %in% colnames(all_trajs)) stop("ERROR in saveAllTrajs(): all_trajs does not have column all_trajs")
    if(!'length' %in% colnames(all_trajs)) stop("ERROR in saveAllTrajs(): all_trajs does not have column length")

    #order by exact_count desc
    all_trajs <- all_trajs %>% dplyr::arrange(-exact_count,-length)

    #reorder columns - id column to first
    all_trajs <- all_trajs %>% dplyr::select(id,tidyselect::everything())

    #convert trajectory.str.names from list to chr
    all_trajs$trajectory.str.names<-unlist(all_trajs$trajectory.str.names)

    outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F)
    xlsxfile=file.path( outputFolder,'tables','trajectory_counts.xlsx')
    rdatafile=file.path(outputFolder,'tables','trajectory_counts.RData')

    ParallelLogger::logInfo('Saving trajectory counts to Excel-file: ',xlsxfile,'...')
    openxlsx::write.xlsx(all_trajs %>% select(-trajectory),file=xlsxfile, overwrite=T) #skip "trajectory" column as this is a list
    ParallelLogger::logInfo(' ...done.')

    ParallelLogger::logInfo('Saving trajectory counts to R-object-file: ',rdatafile,'...')
    save(all_trajs, file=rdatafile)
    ParallelLogger::logInfo(' ...done.')
}

addEventNamesToTrajectories<-function(all_trajs, Node.names) {
  ParallelLogger::logInfo('Adding event names to trajectory descriptions (may take some time if the number of different trajectories is large)...')

  all_trajs$trajectory.str.names<-lapply(all_trajs$trajectory,Trajectories:::getTrajectoryStringWithNames, Node.names=Node.names)

  ParallelLogger::logInfo('...done.')
  return(all_trajs)
}

getTrajectoryStringWithNames<-function(trajectory,Node.names) {
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

detectSubgraphs<-function(all_trajs) {
  ParallelLogger::logInfo('Finding actual counts of each trajectory by considering also sub-trajectories...')
  d<-all_trajs
  d$total_count=d$exact_count
  d$is_subtrajectory_of=NA

  #make sure that trajectories are still ordered by count desc (trajectories with larger count cannot be sub-trajectories of smaller count)
  d<-d %>% dplyr::arrange(-exact_count,-length)

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
      comparator<-comparator %>% dplyr::arrange(-exact_count,-length)

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
              #print(subtraj_ids)
              d$total_count[i]<-d$total_count[i]+comparator$total_count[j]
              #ParallelLogger::logInfo('...Checking whether ',d$trajectory.str[[i]],' (count=',d$total_count[i],') is a sub-trajectory of ',comparator$trajectory.str[[j]],' (count=',comparator$total_count[j],')...')
              #ParallelLogger::logInfo('New count of ',d$trajectory.str[[i]],' is ',d$total_count[i])
              break
            }
          }
          #} # if(length(d$trajectory.str[[j]])>d$trajectory.str[[i]])

        } # for j
      }

      d$is_subtrajectory_of[i]<-paste(subtraj_ids,collapse=",")
    } #for i
  }
  return(d)
}

removeOldTrajectoryResults<-function(trajectoryAnalysisArgs,trajectoryLocalArgs) {
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F)
  xlsxfile=file.path( outputFolder,'tables','trajectory_counts.xlsx')
  rdatafile=file.path(outputFolder,'tables','trajectory_counts.RData')
  if(file.exists(xlsxfile)) file.remove(xlsxfile)
  if(file.exists(rdatafile)) file.remove(rdatafile)
}

align<-function(connection,
                trajectoryAnalysisArgs,
                trajectoryLocalArgs) {

  ParallelLogger::logInfo('Start the trajectory alignment to the given graph...')

  Trajectories:::removeOldTrajectoryResults(trajectoryAnalysisArgs,trajectoryLocalArgs)

  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F)
  eventPairResultsFilename = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  g<-Trajectories:::createTrajectoriesGraph(eventPairResultsFilename=eventPairResultsFilename)

  #Prepare data tables in the database
  g<-Trajectories:::createAlignmentTables(connection,
                                          trajectoryAnalysisArgs,
                                          trajectoryLocalArgs,
                           g)

  #Do the actual alignment and trajectory counting
  Trajectories:::alignTrajectoriesToGraph(connection,
                                     trajectoryAnalysisArgs,
                                     trajectoryLocalArgs,
                                     g)

  ParallelLogger::logInfo('Trajectory aligment completed.')
}


