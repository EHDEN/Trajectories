test_data_source = "Data/event_pairs_tested.xlsx"

load("Data/icd10cm2019.rda", verbose = T) # From icd R package
icd = icd10cm2019 %>%
  map( ~ as.character(.x)) %>%
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
    select(from, to, RR, E1_AND_E2_TOGETHER_COUNT_IN_EVENTS)
  print(edges)
  return(as.data.frame(edges))
}

make_nodes_from_data = function(data, domain_hash, name_hash) {
  logger::log_info("Making nodes from data")

  nodes = tibble(name = unique(c(
    data$E1_CONCEPT_ID, data$E2_CONCEPT_ID
  )))

  #Give icd values to nodes
  nodes = nodes %>%
    left_join(icd, by = c("name" = "code")) %>%
    select(name, Description = short_desc, Chapter = chapter) %>%
    mutate(CodeDescription = case_when(
      !is.na(Description) ~ paste(name, " - ", Description),
      TRUE ~ ""
    )) %>%
    group_by(Chapter) %>%

    mutate(ChapterNew = str_c(min(name), " - ", max(name), ": ", Chapter)) %>%
    ungroup() %>%
    mutate(Chapter = ChapterNew) %>%
    select(-ChapterNew,-Description) %>%
    rename(id = name) %>%
    rename(group = Chapter)

  #If node does not belong to icd group add group and description based on info from given data
  for (r in 1:nrow(nodes))
  {
    row = nodes[r, ]
    if (is.na(row$group)) {
      row$group = domain_hash[[row$id]]
    }
    if (is.na(row$CodeDescription) | row$CodeDescription == "") {
      row$CodeDescription = name_hash[[row$id]]
    }
    nodes[r, ] = row
  }

  nodes = as.data.frame(nodes[order(nodes$id),])
  print(nodes)
  return(nodes)
}

format_given_data = function(data) {
  data %>%
    mutate(E1_CONCEPT_ID = as.character(E1_CONCEPT_ID)) %>%
    mutate(E2_CONCEPT_ID = as.character(E2_CONCEPT_ID))
}

get_domains_from_given_data = function(data) {
  domain_hash <- hash()
  for (r in 1:nrow(data))
  {
    row = data[r, ]
    if (!has.key(row$E1_CONCEPT_ID, domain_hash)) {
      domain_hash[[row$E1_CONCEPT_ID]] = row$E1_DOMAIN
    }
    if (!has.key(row$E2_CONCEPT_ID, domain_hash)) {
      domain_hash[[row$E2_CONCEPT_ID]] = row$E2_DOMAIN
    }

  }
  return(domain_hash)
}

get_names_from_given_data = function(data) {
  name_hash <- hash()
  for (r in 1:nrow(data))
  {
    row = data[r, ]
    if (!has.key(row$E1_CONCEPT_ID, name_hash)) {
      name_hash[[row$E1_CONCEPT_ID]] = row$E1_NAME
    }
    if (!has.key(row$E2_CONCEPT_ID, name_hash)) {
      name_hash[[row$E2_CONCEPT_ID]] = row$E2_NAME
    }

  }
  return(name_hash)
}
