#Used if given data is missing or invalid
test_data_source = "Data/event_pairs_tested.xlsx"

GraphFilter <- setClass(
  "GraphFilter",
  slots = c(
    use_for_weight = "character",
    RR_effect_value = "numeric",
    E1E2Together_effect_value = "numeric",
    importance_value = "numeric",
    selected_id_codes = "list",
    use_network_view = "logical",
    max_distance = "numeric"
  )
)

get_test_data = function() {
  test_data = as.data.frame(read_xlsx(test_data_source))
  logger::log_info("Got test data from: %s", test_data_source)
  return(test_data)
}

make_links_from_data = function(data) {
  logger::log_info("Making edges from data")

  # Prepare edges
  edges = data %>%
    rename(from = E1_CONCEPT_ID, to = E2_CONCEPT_ID) %>%
    mutate(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS = coalesce(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS, 0)) %>%
    mutate(RR = coalesce(RR, 0)) %>%
    filter(RR >= 1) %>%
    select(from, to, RR, E1_AND_E2_TOGETHER_COUNT_IN_EVENTS)
  print(edges)
  return(as.data.frame(edges))
}

make_nodes_from_data = function(data) {
  logger::log_info("Making nodes from data")

  e1_nodes = tibble(id = data$E1_CONCEPT_ID, title = data$E1_NAME, label = data$E1_NAME, group = data$E1_DOMAIN)
  e2_nodes = tibble(id = data$E2_CONCEPT_ID, title = data$E2_NAME, label = data$E2_NAME, group = data$E2_DOMAIN)

  nodes = full_join(e1_nodes, e2_nodes) %>%
    distinct(id, .keep_all = TRUE)

  nodes = as.data.frame(nodes[order(nodes$id),])
  print(nodes)
  return(nodes)
}

format_given_data = function(data) {
  data %>%
    mutate(E1_CONCEPT_ID = as.character(E1_CONCEPT_ID)) %>%
    mutate(E2_CONCEPT_ID = as.character(E2_CONCEPT_ID))
}


