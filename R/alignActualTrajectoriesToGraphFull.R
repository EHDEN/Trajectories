requireNamespace("igraph", quietly = TRUE)

#' Adds numcohortCustom value to graph nodes and edges - actual number of people on that edge/node
#'
#' @param g An igrpah object that is created by specific graph functions in this package
#' @param connection Database connection object created by createConnectionDetails() method in DatabaseConnector package
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#'
#' @return
#'
#' @examples
alignActualTrajectoriesToGraphFull <- function(connection,
                                           trajectoryAnalysisArgs,
                                           trajectoryLocalArgs,
                                           g) {

  if(!inherits(g, 'TrajectoriesGraph')) stop('Error in filterIgraphRemoveLowEffectLinksAndOrphanNodes(): object g is not class TrajectoriesGraph object')

  DEBUG=F

  ParallelLogger::logInfo('Aligning actual trajectories to the graph of ',length(igraph::E(g)),' event pairs.')

  #First, put event pairs of the graph into table

  ParallelLogger::logInfo('Putting ',length(igraph::E(g)),' event pairs of the graph into database.')

  e<-igraph::as_data_frame(g,what="edges")
  edges<- e %>% dplyr::select(e1_concept_id,e2_concept_id)


  #but before actual TABLE CREATE there is an extra step: if sqlRole is given, set session to correct role before creating the table
  Trajectories:::setRole(connection,trajectoryLocalArgs$sqlRole)

  #26 Sep 2020: Can't use simply insertTable here because in Eunomia package it does not solve schema name correctly. Therefore, currently using workaround function from Trajectories package
  Trajectories:::insertTable(connection=connection,
                             databaseSchema=trajectoryLocalArgs$resultsSchema,
                             tableName=paste0(trajectoryLocalArgs$prefixForResultTableNames,'mylinks'),
                             data=edges,
                             dropTableIfExists=T,
                             tempTable=F,
                             progressBar=T)

  #querySql(connection, paste0("SELECT COUNT(*) FROM ",tablename))

  #Get cohortsize
  sql<-"SELECT COUNT(*) AS COHORTSIZE FROM @resultsSchema.@prefiXmycohort;"
  RenderedSql <- SqlRender::render(sql, resultsSchema=trajectoryLocalArgs$resultsSchema, prefiX = trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(RenderedSql,targetDialect=attr(connection, "dbms"))
  res<-c(DatabaseConnector::querySql(connection, RenderedSql))
  COHORTSIZE=res$COHORTSIZE


  #align trajectories to graph (to get the exact E1->E2 counts with no intermediate events)
  #Takes all trajectories that pass any event of that graph. Leaves out intermediate events that are not given in the graph.
  ParallelLogger::logInfo('Extracting actual sequences of these events from database...')
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

  ParallelLogger::logInfo('Done. Out of ',length(igraph::V(g)),' events and ',length(igraph::E(g)),' pairs in original graph ',length(igraph::V(g3)),' events and ',length(igraph::E(g3)),' pairs remained after applying count>0 filter.')

  # make it of the class TrajectoriesGraph which is derived from the class igraph
  class(g2) <- c("TrajectoriesGraph","igraph")


  return(g2)
}


