create_nodes_and_edges = function(tg) {
  #Remove isolated nodes
  tg = tg %>%
    activate(edges) %>%
    filter(!is.na(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS)) %>%
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
    select(from,
           from_row,
           to,
           to_row,
           value,
           RR,
           E1_AND_E2_TOGETHER_COUNT_IN_EVENTS)


  tg_nodes <- select(tg_nodes, -rowid)

  print(head(tg_nodes))
  print(head(named_edge_list))
  return(list("nodes" = tg_nodes, "edges" = named_edge_list))
}

filter_nodes_and_edges = function(tg,
                                  filter,
                                  selected_id_codes,
                                  selected_groups) {
  logger::log_info("Filtering dataset")
  use_for_weight <- filter@use_for_weight
  RR_effect_value <- filter@RR_effect_value
  E1E2Together_effect_value <- filter@E1E2Together_effect_value
  importance_value <- filter@importance_value
  max_distance <- filter@max_distance

  #Filter by weight
  tg = tg %>%
    activate(edges) %>%
    filter(RR > RR_effect_value) %>%
    filter(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS > E1E2Together_effect_value) %>%
    mutate(value = !!as.symbol(use_for_weight))

  #Filter by selected id codes
  #Code is selected if it's distance to any selected code is smaller than max distance
  if (length(selected_id_codes)) {
    chosen_nodes = activate(tg, nodes) %>%
      pull(id) %>% {
        which(. %in% selected_id_codes)
      }
    print(chosen_nodes[1])
    tg = activate(tg, nodes) %>%
      mutate(dist_to_node = node_distance_to(chosen_nodes[1], mode = "all"))

    for (node in chosen_nodes[-1]) {
      tg = activate(tg, nodes) %>%
        mutate(
          dist_to_node = ifelse(
            node_distance_to(node, mode = "all") < dist_to_node,
            node_distance_to(node, mode = "all") ,
            dist_to_node
          )
        )
    }

    tg = activate(tg, nodes) %>%
      filter(dist_to_node <= max_distance)
  }

  #Filter by betweenness centrality
  tg = tg %>%
    activate(nodes) %>%
    mutate(importance = centrality_betweenness()) %>%
    filter(importance >= importance_value)

  #Filter by group
  if (length(selected_groups)) {
    tg = tg %>%
      activate(nodes) %>%
      filter(group  %in% selected_groups)
  }

  return(create_nodes_and_edges(tg))
}
