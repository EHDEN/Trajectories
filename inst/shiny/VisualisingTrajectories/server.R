# Modified from Winston Chang,
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html
source("utils.R")
source("filter_network.R")
# Load R packages
library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library("data.table")
library(tidyverse)
library(tidygraph)
library(readxl)
library(visNetwork)
library(hash)


DEFAULT_COLUMNS_FOR_WEIGHT = c("RR", "E1_AND_E2_TOGETHER_COUNT_IN_EVENTS")
appDir <- getwd()

test_data = get_test_data()
data  <- getShinyOption("data", test_data)
data <- format_given_data(data)

# Define server function
server <- function(input, output, session) {
  logger::log_info("Loading Shiny server")

  #create nodes dataframe
  nodes <- make_nodes_from_data(data)
  #create links dataframe
  edges <- make_links_from_data(data)

  #Get filtered nodes and edges
  tg = tbl_graph(nodes, edges)

  nodesandedges <-
    reactive({
      id_codes = data.frame(input$selected_id_codes)
      graph_filter <-
        new(
          "GraphFilter",
          use_for_weight = ifelse(
            !is.null(input$use_for_weight),
            input$use_for_weight,
            "RR"
          ),
          RR_effect_value = ifelse(
            !is.null(input$RR_effect_value),
            input$RR_effect_value,
            0
          ),
          E1E2Together_effect_value = ifelse(
            !is.null(input$E1E2Together_effect_value),
            input$E1E2Together_effect_value,
            0
          ),
          importance_value = ifelse(
            !is.null(input$importance_value),
            input$importance_value,
            0
          ),
          use_network_view = ifelse(
            !is.null(input$network_view_switch),
            input$network_view_switch,
            TRUE
          ),
          max_distance = input$selected_distance
        )

      return(
        filter_nodes_and_edges(
          tg,
          graph_filter,
          input$selected_id_codes,
          input$selected_groups
        )
      )
    })

  output$table <- DT::renderDataTable({
    datatable(data, rownames = FALSE, options = list(
      scrollX = TRUE
    ))
  })

  #Renders visNetwork graph
  output$network <- renderVisNetwork({
    visNetwork(
      nodesandedges()$nodes,
      nodesandedges()$edges,
      width = "100%",
      height = "90vh"
    ) %>%
      visIgraphLayout() %>%
      visNodes(
        shape = "dot",
        color = list(highlight = "#FF8000"),
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(arrows = "to",
               color = list(highlight = "#C62F4B")) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 2)) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend(width = 0.2) %>%
      visExport(
        type = "png",
        name = "export-network",
        float = "left",
        label = "Save network",
        background = "white",
        style = ""
      ) %>%
      visLayout(randomSeed = 11) %>%
      visHierarchicalLayout(direction = "LR",
                            enabled = input$use_hierarchical_layout)
  })

  #Renders Sankey network
  output$sankeyNet <- renderPlotly({
    plot_ly(
      type = "sankey",
      orientation = "h",

      node = list(
        label = as.list(select(nodesandedges()$nodes, title))$title,
        pad = 15,
        thickness = 20,
        line = list(color = "black",
                    width = 0.5)
      ),

      link = list(
        source = as.list(select(nodesandedges()$edges, from_row))$from_row - 1,
        #R counts indexes from 1, whereas plotly from 0
        target = as.list(select(nodesandedges()$edges, to_row))$to_row - 1,
        value =  as.list(select(nodesandedges()$edges, value))$value
      )
    )
  })

  #Used for filtering using RR and E1&E2 together count.
  output$weight_slider <- renderUI({
    div(
      sliderInput(
        "RR_effect_value",
        "Relative risk",
        min = 1,
        max = max(edges %>% select(RR), na.rm = TRUE),
        value = 1
      ),

      sliderInput(
        "E1E2Together_effect_value",
        "Events together count",
        min = min(
          edges %>% select(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS),
          na.rm = TRUE
        ),
        max = max(
          edges %>% select(E1_AND_E2_TOGETHER_COUNT_IN_EVENTS),
          na.rm = TRUE
        ),
        value = 1
      )
    )


  })

  #Importance value is used to filter nodes using centrality_edge_betweenness
  output$importance_slider <- renderUI({
    sliderInput(
      "importance_value",
      label = div(class="tooltip-container", h4("Centrality value"),
                  dropMenu(
                    actionButton(
                      inputId = "centrality-tooltip-button",
                      class = "tooltip-button",
                      label = "",
                      icon = icon('info'),
                    ),
                    div(
                      span("Importance value is used to filter nodes using centrality betweenness measure.")
                    ),
                    placement = "top-end",
                    trigger = "mouseenter"
                  )
      ),
      min = 0,
      max = 100,
      #max(edges %>% select(!!as.symbol(input$use_for_weight)), na.rm = TRUE)
      value = 0
    )
  })

  #Selected weight is used for edgeweight.
  output$weight_radiobox <- renderUI({
    radioButtons(
      "use_for_weight",
      label = div(class="tooltip-container", h4("Use for edge weight"),
                  dropMenu(
                    actionButton(
                      inputId = "weight-selection-tooltip-button",
                      class = "tooltip-button",
                      label = "",
                      icon = icon('info'),
                    ),
                    div(
                      span("Graph's edge thickness is based on selected value.")
                    ),
                    placement = "top-end",
                    trigger = "mouseenter"
                  )
      ),
      choiceNames = c("Relative Risk", "Events together count"),
      choiceValues = DEFAULT_COLUMNS_FOR_WEIGHT,
      width = "100%"
    )
  })


  #Selects specific node ids
  output$id_selectinput <- renderUI({
    multiInput(
      inputId = "selected_id_codes",
      label = h4("Select id codes"),
      choices = NULL,
      choiceNames = nodes$title,
      choiceValues = nodes$id,
      width = "100%"
    )
  })

  #Selects specific groups
  output$groups_selectinput <- renderUI({
    selectInput(
      inputId = "selected_groups",
      label = h4("Select groups"),
      multiple = TRUE,
      choices = distinct(nodes, group)$group,
      width = "100%"
    )
  })

  #Resets filter options
  observeEvent(input$resetFilter, {
    logger::log_info("Filter has been reset")
    updateSliderInput(session, 'importance_value', value = 0)
    updateSliderInput(session, 'RR_effect_value', value = 1)
    updateSliderInput(session, 'E1E2Together_effect_value', value = 0)
    updateMultiInput(session, "selected_id_codes", selected = character(0))
    updateSelectInput(session, "selected_groups", selected = character(0))
    updateRadioButtons(session, "use_for_weight", selected = "RR")
    updateCheckboxInput(session, "use_hierarchical_layout", value = FALSE)
    updateNumericInput(session, "selected_distance", value = 3)
  })

  observeEvent(input$selected_id_codes, {
    nodes_selection <- input$selected_id_codes
    visNetworkProxy("network") %>%
      visSelectNodes(id = nodes_selection)
  })

}
