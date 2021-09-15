#' Replica of insertTable, but to handle schema problem https://github.com/OHDSI/SqlRender/issues/258 for SqlRender
#'
#' @return
#'
#' @examples
insertTable <- function(connection,
                        databaseSchema = NULL,
                        ...) {

  args = list(...) # unpack, contains a='foo'

  if(connection@dbms=='sqlite') {
    databaseSchema = NULL #do not give databaseschema as input for SQLite as it does not work anyways
  }

  #Now, call insertTable() as usual
  y = do.call(DatabaseConnector::insertTable, c(connection, databaseSchema, args)) # repack arguments for call to g()

}
