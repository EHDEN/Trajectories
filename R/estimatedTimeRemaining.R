#' Returns estimated time remaining
#'
#' Returns estimated time remaining in a textual manner, based on the current progress percentage
#'
#' @param progress_perc The percentage of task progress
#' @param starttime Time when current task was started
#'
#' @return
#' @export
#'
#' @examples
estimatedTimeRemaining<-function(progress_perc=0.25,starttime=Sys.time()) {

  curTime=Sys.time()
  timePassed=curTime-starttime
  format(Sys.time(), "%a %b %d %X %Y")
  timePassedInSecs=as.numeric(difftime(curTime,starttime, units="secs"))

  timeRemaining=timePassedInSecs*(1-progress_perc)/progress_perc

  if(timeRemaining<60) {
    return(paste(round(timeRemaining),'sec'))
  } else if (timeRemaining<3600) {
    m<-floor(timeRemaining/60)
    s<-timeRemaining-60*m
    return(paste(m,'m',round(s),'s'))
  } else {
    h<-floor(timeRemaining/3600)
    m<-floor((timeRemaining-h*3600)/60)
    s<-timeRemaining-3600*h-60*m
    return(paste(h,'h',m,'m',round(s),'s'))
  }
}
