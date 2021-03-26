create_nodes_and_edges = function(tg) {
  #Base filtering and remove isolated nodes
  tg = tg %>%
    activate(edges) %>%
    filter(!is.na(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS)) %>%
    filter(value > 1.5) %>%
    activate(nodes) %>%
    filter(!node_is_isolated())


  # Separate out edges and node data frames
  tg_nodes <-
    tg %>%
    activate(nodes) %>%
    data.frame() %>%
    tibble::rowid_to_column("rowid")

  tg_edges <-
    tg %>%
    activate(edges) %>%
    data.frame()

  named_edge_list <-
    tg_edges %>%
    # Rename from nodes
    left_join(tg_nodes, by = c("from" = "rowid")) %>%
    rename(from_row = from) %>%
    rename(from = id) %>%
    # Rename to nodes
    left_join(tg_nodes, by = c("to" = "rowid")) %>%
    rename(to_row = to) %>%
    rename(to = id) %>%
    select(
      from,
      from_row,
      to,
      to_row,
      value,
      importance,
      RR,
      E1_AND_E2_TOGETHER_COUNT_IN_EVENTS
    )


  tg_nodes <- select(tg_nodes,-rowid) %>%
    mutate(title = CodeDescription)

  print(head(tg_nodes))
  print(head(named_edge_list))
  return(list("nodes" = tg_nodes, "edges" = named_edge_list))
}

filter_nodes_and_edges = function(tg, filter, selected_icd_codes) {
  flog.info("Filtering dataset")
  use_for_weight <- filter@use_for_weight
  RR_effect_value <- filter@RR_effect_value
  E1E2Together_effect_value <- filter@E1E2Together_effect_value
  importance_value <- filter@importance_value

  #Filter by weight
  tg = tg %>%
    activate(edges) %>%
    filter(RR > RR_effect_value) %>%
    filter(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS > E1E2Together_effect_value) %>%
    mutate(value = !!as.symbol(use_for_weight))


  #Filter by importance
  tg = tg %>%
    activate(edges) %>%
    mutate(importance = centrality_edge_betweenness()) %>%
    filter(importance >= importance_value)

  #Filter by icd codes
  print(selected_icd_codes)
  if (length(selected_icd_codes)) {
    chosen_nodes = activate(tg, nodes) %>%
      pull(id) %>% {
        which(. %in% selected_icd_codes)
      }

    #print(chosen_nodes)
    print(list())

    tg = tg %>%
      activate(nodes) %>%
      mutate(dist_to_node = node_distance_to(nodes = chosen_nodes, mode =
                                               "all")) %>%
      filter(dist_to_node < 3)
  }

  return(create_nodes_and_edges(tg))
}
