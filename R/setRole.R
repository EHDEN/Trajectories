#' This function switches role of the database session by simply executing SET ROLE ...;
#'
#' @param connection
#' @param sqlRole SQL role to be set
#'
#' @return
#' @export
#'
#' @examples
setRole <- function(connection,
                    sqlRole=F) {
  if(is.na(sqlRole) | is.logical(sqlRole) | sqlRole=='') {
    #sqlRole is not set or is set incorrectly, do nothing
  } else {
    RenderedSql=SqlRender::translate(sql = paste0("SET ROLE ",sqlRole,";"), targetDialect=attr(connection, "dbms"))
    DatabaseConnector::executeSql(connection, RenderedSql)
    print(paste0('Database role set to ',sqlRole))
  }
  invisible() #return NULL, but do not print it to console
}
