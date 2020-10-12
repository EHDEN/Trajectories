#library(usethis)
#use_test()
#' This function switches role of the database session by simply executing SET ROLE ...;
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param sqlRole SQL role to be set
#'
#' @return
#' @export
#'
#' @examples
setRole <- function(connection,
                    sqlRole) {

  statement_sqlserver =paste0("EXECUTE AS USER =  '",sqlRole,"';")
  statement_other = paste0("SET ROLE ",sqlRole,";")
  if(is.na(sqlRole) | is.logical(sqlRole) | sqlRole=='') {
    #sqlRole is not set or is set incorrectly, do nothing
  } else {
    RenderedSql=SqlRender::translate(sql =ifelse(connection@dbms=="sql server",statement_sqlserver,statement_other) , targetDialect=attr(connection, "dbms"))
    DatabaseConnector::executeSql(connection, RenderedSql)
    log_info(paste0('Database role set to ',sqlRole))
  }
  invisible() #return NULL, but do not print it to console
}
