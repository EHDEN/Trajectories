#' Initializes logger for Trajectories package
#'
#' @param logfile Full path to log file
#' @param threshold log_threshold values from "logger" package. DEBUG, INFO, WARN, ERROR
#'
#' @return
#' @export
#'
#' @examples
InitLogger<-function(logfile, threshold=logger:::INFO) {
  library(logger)
  logger::log_threshold(threshold) #define the level of output log information (DEBUG, INFO, WARN, ERROR)
  logger::log_formatter(formatter_glue)
  logger::log_layout(layout_glue_colors) #colorful log messages
  logger::log_appender(appender_tee(logfile)) #log both to console and to file
  #logger::log_debug('I am a low level log message that will not be printed with a high log level threshold')
  #logger::log_warn('I am a higher level log message that is very likely to be printed')

  #delete old log file if exists
  if (file.exists(logfile)) {
    file.remove(logfile)
    logger::log_info(paste0('Old log file ',logfile,' removed.'))
  }

  s=file.create(logfile)
}

