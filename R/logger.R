#' Initializes logger for Trajectories package
#'
#' @param logfile Full path to log file
#' @param threshold log_threshold values from "ParallelLogger" package: TRACE, DEBUG, INFO, WARN, ERROR
#'
#' @return
#'
#' @examples
InitLogger<-function(logfile, threshold="INFO") {

  #Clear (remove) default loggers
  ParallelLogger::clearLoggers()

  #delete old log file if exists
  if (file.exists(logfile)) {
    file.remove(logfile)
    print(paste0('Old log file ',logfile,' removed.'))
  }
  #s=file.create(logfile)

  logger <- ParallelLogger::createLogger(name = "LOGGER",
                         threshold = threshold,
                         appenders = list(ParallelLogger::createConsoleAppender(layout=ParallelLogger::layoutTimestamp),
                                          ParallelLogger::createFileAppender(
                                            layout = ParallelLogger::layoutTimestamp,
                                            fileName=logfile,
                                            overwrite = FALSE
                                          )
                         ))

  ParallelLogger::registerLogger(logger)

  ParallelLogger::logInfo('Registered logger with threshold=',threshold,' and console + file appender (',logfile,')')
}
