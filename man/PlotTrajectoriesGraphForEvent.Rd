% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotTrajectoriesGraphForEvent.R
\name{PlotTrajectoriesGraphForEvent}
\alias{PlotTrajectoriesGraphForEvent}
\title{Plots graph for a specific event}
\usage{
PlotTrajectoriesGraphForEvent(
  connection,
  trajectoryAnalysisArgs,
  trajectoryLocalArgs,
  g,
  eventId = 443732,
  limitOfNodes = 30,
  skipOutputTables = T
)
}
\arguments{
\item{connection}{Database connection object created by createConnectionDetails() method in DatabaseConnector package}

\item{trajectoryAnalysisArgs}{TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method}

\item{trajectoryLocalArgs}{TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method}

\item{g}{igraph object}

\item{eventId}{ID of the event in the event pair}

\item{limitOfNodes}{limit of the number of nodes to include on the graph}

\item{skipOutputTables}{If set to T, no output data tables are made (the PDF graphs only).}
}
\description{
Plots graph for a specific event
}
