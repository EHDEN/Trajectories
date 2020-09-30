library(SqlRender)
addBirthsChecker <- function(connection,
                             trajectoryAnalysisArgs,
                             trajectoryLocalArgs) {
  print('addBirths is True, checking data availability...')
  births_sql <- SqlRender::loadRenderTranslateSql("addBirthsChecker.sql",
                                                packageName=trajectoryAnalysisArgs$packageName,
                                                dbms=connection@dbms,
                                                cdmDatabaseSchema =  trajectoryLocalArgs$cdmDatabaseSchema
                                                )
  birthDateTimeCount = DatabaseConnector::querySql(connection, births_sql)
  if(birthDateTimeCount$BIRTHCOUNTS<1) {
    warning("the column person.birth_datetime is empty")
    } else {
      print("data is available for addBirths functionality.")
    }
  }
