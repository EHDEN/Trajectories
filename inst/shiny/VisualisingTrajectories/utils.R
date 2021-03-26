test_data_source = "Data/event_pairs_tested.xlsx"

load("Data/icd10cm2019.rda", verbose = T) # From icd R package
icd = icd10cm2019 %>%
  map(~ as.character(.x)) %>%
  as_tibble()

GraphFilter <- setClass(
  "GraphFilter",
  slots = c(
    use_for_weight = "character",
    RR_effect_value = "numeric",
    E1E2Together_effect_value = "numeric",
    importance_value = "numeric",
    selected_icd_codes = "list",
    use_network_view = "logical"
  )
)

get_test_data = function() {
  test_data = as.data.frame(read_xlsx(test_data_source))
  flog.info("Got test data from: %s", test_data_source)
  return(test_data)
}

make_links_from_data = function(data) {
  flog.info("Making edges from data")

  # Prepare edges
  edges = data %>%
    rename(from = E1_CONCEPT_ID, to = E2_CONCEPT_ID) %>%
    mutate(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS = coalesce(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS, 0)) %>%
    mutate(RR = coalesce(RR, 0)) %>%
    select(from, to, RR, E1_AND_E2_TOGETHER_COUNT_IN_EVENTS)

  return(as.data.frame(edges))
}

make_nodes_from_data = function(data) {
  flog.info("Making nodes from data")

  nodes = tibble(name = unique(c(
    data$E1_CONCEPT_ID, data$E2_CONCEPT_ID
  )))


  nodes = nodes %>%
    left_join(icd, by = c("name" = "code")) %>%
    select(name, Description = short_desc, Chapter = chapter) %>%
    mutate(CodeDescription = paste(name, ifelse(
      is.na(Description), "",  paste(" - ", Description)
    ))) %>%
    group_by(Chapter) %>%
    mutate(ChapterNew = str_c(min(name), " - ", max(name), ": ", Chapter)) %>%
    ungroup() %>%
    mutate(Chapter = ChapterNew) %>%

    select(-ChapterNew) %>%
    rename(id = name) %>%
    rename(group = Chapter)

  nodes = as.data.frame(nodes[order(nodes$id), ])
  print(nodes)
  return(nodes)
}
