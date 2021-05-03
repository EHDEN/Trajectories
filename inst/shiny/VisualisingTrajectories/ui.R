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
library(geomnet)

#Define ui
ui <- dashboardPage(
  dashboardHeader(title = "Trajectories",
                  titleWidth = 350),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem(
        "Network",
        tabName = "network",
        icon = icon("project-diagram")
      ),
      menuItem("Raw Data", tabName = "raw_data", icon = icon("table"))
    ),
    switchInput(
      inputId = "network_view_switch",
      value = TRUE,
      onLabel = "Network view",
      offLabel = "Sankey view",
    ),
    conditionalPanel(
      condition = "input.network_view_switch == 1",
      checkboxInput("use_hierarchical_layout", "Use hierarchical layout", value = FALSE, width = NULL)
    ),
    uiOutput("id_selectinput"),
    numericInput("selected_distance", "Maximum distance to selected", 3, min= 0, max = 5),
    uiOutput("groups_selectinput"),
    uiOutput("weight_radiobox"),
    uiOutput('weight_slider'),
    uiOutput("importance_slider"),
    actionButton("resetFilter","Reset Filtering", class = "btn-info")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    tabItems(
      tabItem(
        tabName = "network",
        conditionalPanel(
          condition = "input.network_view_switch == 1",
          visNetworkOutput("network", width = "100%", height = "90vh")
        ),
        conditionalPanel(
          condition = "input.network_view_switch == 0",
          plotlyOutput("sankeyNet", width = "100%", height = "90vh")
        )
      ),
      tabItem(tabName = "raw_data",
              h1("Data table"),
              DT::dataTableOutput("table"))
    )
  )
)
