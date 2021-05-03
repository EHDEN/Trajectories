#' Visualize data pairs using Shiny application
#'
#' @param data Data in xlsx format used for visualization. Has to contain columns E1_CONCEPT_ID and E2_CONCEPT_ID
#' @export
#' @examples
#' data <- as.data.frame(read.xlsx("inst/shiny/VisualisingTrajectories/Data/event_pairs_tested.xlsx"))
#' Trajectories::visualize_data_pairs(data)
visualize_data_pairs <- function(data) {
  ensure_installed("DT")
  ensure_installed("shiny")
  ensure_installed("shinydashboard")
  ensure_installed("shinyWidgets")
  ensure_installed("plotly")
  ensure_installed("data.table")
  ensure_installed("tidyverse")
  ensure_installed("tidygraph")
  ensure_installed("readxl")
  ensure_installed("visNetwork")
  ensure_installed("hash")

  Trajectories::InitLogger(logfile = file.path(".",'log.txt'), threshold = logger:::INFO)
  #data validation
  if (length(colnames(data)) < 3) {
    logger::log_error("Dataset does not contain enough columns")
    logger::log_info("Column names: ", colnames(data))
    stop()
  }
  if (!('E1_CONCEPT_ID' %in% colnames(data)) |
      !('E2_CONCEPT_ID' %in% colnames(data))) {
    logger::log_error("Dataset does not contain needed column E1_CONCEPT_ID or E2_CONCEPT_ID")
    logger::log_info("Column names: ", colnames(data))
    stop()
  }

  shiny::shinyOptions(data = data)
  shiny::runApp("./inst/shiny/VisualisingTrajectories")
}

# Code taken from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = 0) {
  installed_version <-
    tryCatch(
      utils::packageVersion(pkg),
      error = function(e)
        NA
    )
  ! is.na(installed_version) && installed_version >= version
}

# Code taken from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <-
      paste0(sQuote(pkg), " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}
