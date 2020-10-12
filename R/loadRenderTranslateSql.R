library(SqlRender)
library(logger)

#' Copy of SqlRender::loadRenderTranslateSql() method to corrrectly solve pathToSql inside Trajectories package
#'
#' @param sqlFilename
#' @param packageName
#' @param dbms
#' @param ...
#' @param oracleTempSchema
#' @param warnOnMissingParameters
#'
#' @return
#' @export
#'
#' @examples
loadRenderTranslateSql<-function(sqlFilename,
                                 packageName,
                                 dbms = "sql server",
                                 ...,
                                 oracleTempSchema = NULL,
                                 warnOnMissingParameters = TRUE) {
  pathToSql <- system.file("sql", gsub(" ", "_", dbms),
                           sqlFilename,
                           package = packageName)
  mustTranslate <- !file.exists(pathToSql)
  if (mustTranslate) {
    # If DBMS-specific code does not exists, load SQL Server code and translate after rendering
    pathToSql <- system.file("sql", "sql_server",
                             sqlFilename,
                             package = packageName)
    #print(pathToSql)
    if (!file.exists(pathToSql)) {
      #print(paste('XXXSearched from :',system.file("", package = packageName)))
      #print(paste('XXXSearched from SQL:',system.file("sql", package = packageName)))
      #print(paste('XXXSearched from SQL/:',system.file("sql/", package = packageName)))
      #print(paste('XXXSearched from SQL/SQL_SERVER:',system.file("sql/sql_server", package = packageName)))

      stop("Cannot find '", sqlFilename, "' in the 'sql/sql_server' folder of the '", packageName , "' package (from '",pathToSql,"')")
    }
  }
  parameterizedSql <- readChar(pathToSql, file.info(pathToSql)$size)

  renderedSql <- render(sql = parameterizedSql[1], warnOnMissingParameters = warnOnMissingParameters, ...)

  if (mustTranslate)
    renderedSql <- translate(sql = renderedSql, targetDialect = dbms, oracleTempSchema = oracleTempSchema)

  renderedSql
}
