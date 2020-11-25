library(SqlRender)
#' Title
#'
#' @param connection
#' @param trajectoryAnalysisArgs
#' @param trajectoryLocalArgs
#'
#' @return
#' @export
#'
#' @examples
addBirthsChecker <- function(connection,
                             trajectoryAnalysisArgs,
                             trajectoryLocalArgs) {
  logger::log_info('Parameter addBirths is set to TRUE. Check whether person.birth_datetime is actually recorded in the data...')
  births_sql <- SqlRender::loadRenderTranslateSql("addBirthsChecker.sql",
                                                packageName=trajectoryAnalysisArgs$packageName,
                                                dbms=connection@dbms,
                                                cdmDatabaseSchema =  trajectoryLocalArgs$cdmDatabaseSchema
                                                )
  birthDateTimeCount = DatabaseConnector::querySql(connection, births_sql)
  if(birthDateTimeCount$BIRTHCOUNTS<1) {
    logger::log_warn("The column person.birth_datetime is empty, setting addBirths to FALSE")
    trajectoryAnalysisArgs$addBirths=F
  } else {
    logger::log_info('...OK')
  }
}
