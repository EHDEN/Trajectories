context("Tests with Eunomia package")
library(Trajectories)
library(DatabaseConnector)
library(stringr)
library(SqlRender)

#helper functions
clearConditionsTable<-function(connection) {
  executeSql(connection, "DELETE FROM CONDITION_OCCURRENCE;")
}

getPatientIds<-function(connection,n=2694,excludePatientIds=c()) {
  if(length(excludePatientIds)>0) {
    res<-querySql(connection, paste0('SELECT person_id FROM PERSON WHERE person_id NOT IN (',paste0(excludePatientIds,collapse=","),') ORDER BY random() LIMIT ',as.numeric(n),';'))
  } else {
    res<-querySql(connection, paste0('SELECT person_id FROM PERSON ORDER BY random() LIMIT ',as.numeric(n),';'))
  }
  return(res$PERSON_ID)
}

getObservationPeriod<-function(connection,person_id) {
  res<-querySql(connection, paste0('SELECT OBSERVATION_PERIOD_START_DATE,OBSERVATION_PERIOD_END_DATE FROM OBSERVATION_PERIOD WHERE person_id =',as.numeric(person_id),';'))
  return(res)
}

addConditionEventPairForPerson<-function(connection,event1_concept_id=133834,event2_concept_id=255848,person_id=6) {
  obsPeriod=getObservationPeriod(connection,person_id)
  startDate=obsPeriod$OBSERVATION_PERIOD_START_DATE
  endDate=obsPeriod$OBSERVATION_PERIOD_END_DATE
  event1Date=sample(seq(as.Date(startDate), as.Date(endDate), by="day"), 1)
  event2Date=sample(seq(as.Date(event1Date), as.Date(endDate), by="day"), 1)

  #For SQLite, we must convert dates to real numbers (otherwise it does not work with other REAL dates in Eunomia package)
  #Therefore, some magic is needed. First, convert to YYYYMMDD format and second, use SQL CONVERT(DATE,...) function.
  #Another problem is SqlRender translation bug https://github.com/OHDSI/SqlRender/issues/232 So we hardcode SQLite translation here
  event1Date<-str_replace_all(event1Date, "-", "")
  event2Date<-str_replace_all(event2Date, "-", "")

  executeSql(connection, paste0("INSERT INTO CONDITION_OCCURRENCE (person_id,condition_concept_id,condition_start_date) VALUES (",
                                person_id,",",
                                event1_concept_id,
                                ",",getSQLiteRealFromDateSQLstring(event1Date),");"),
             progressBar = F,
             reportOverallTime=F)
  executeSql(connection, paste0("INSERT INTO CONDITION_OCCURRENCE (person_id,condition_concept_id,condition_start_date) VALUES (",
                                person_id,",",
                                event2_concept_id,
                                ",",getSQLiteRealFromDateSQLstring(event2Date),");"),
             progressBar = F,
             reportOverallTime=F)
}

#For SQLite, we must convert dates to real numbers (otherwise it does not work with other REAL dates in Eunomia package)
#Therefore, some magic is needed. First, convert to YYYYMMDD format and second, use SQL CONVERT(DATE,...) function.
#Another problem is SqlRender translation bug https://github.com/OHDSI/SqlRender/issues/232 So we hardcode SQLite translation here
getSQLiteRealFromDateSQLstring<-function(dateAsString='2010-01-01') {
  dateAsString<-str_replace_all(dateAsString, "-", "")
  sql=paste0(" CAST(
             STRFTIME('%s',
             SUBSTR('",dateAsString,"', 1, 4) || '-' || SUBSTR('",dateAsString,"', 5, 2) || '-' || SUBSTR('",dateAsString,"', 7)

             ) AS REAL) ")
  return(sql)

}

addRandomEventForPeson<-function(connection,person_id=6,exclude_concept_ids=c(),include_concept_ids=c()) {

  obsPeriod=getObservationPeriod(connection,person_id)
  startDate=obsPeriod$OBSERVATION_PERIOD_START_DATE
  endDate=obsPeriod$OBSERVATION_PERIOD_END_DATE
  eventDate=sample(seq(as.Date(startDate), as.Date(endDate), by="day"), 1)

  concept_id=ifelse(length(include_concept_ids)>0,
                    include_concept_ids[base::sample(length(include_concept_ids),1)],
                    paste0("(SELECT concept_id FROM CONCEPT WHERE concept_id NOT IN (",paste(exclude_concept_ids,collapse=","),") AND DOMAIN_ID='Condition' ORDER BY random() LIMIT 1)"))

  sql=paste0("INSERT INTO CONDITION_OCCURRENCE (person_id,condition_concept_id,condition_start_date) VALUES (",
             person_id,",",
             concept_id,",",
             getSQLiteRealFromDateSQLstring(eventDate),");")

  #logger::log_info(sql)

  executeSql(connection, sql=sql, progressBar = F,
             reportOverallTime=F)
}

addRandomEvents<-function(connection,n_per_person_range=c(0,5),exclude_concept_ids=c(),include_concept_ids=c(),exclude_person_ids=c()) {
  print(paste0('Adding ',n_per_person_range[1],'-',n_per_person_range[2],' random events for each patients to data (exceptions possible)'))
  person_ids<-getPatientIds(connection)
  person_ids<-setdiff(person_ids,exclude_person_ids)
  for(person_id in person_ids) {
    num_events<-sample(n_per_person_range[1]:n_per_person_range[2], 1)
    if(num_events>0) {
      for(n in 1:num_events) {
        addRandomEventForPeson(connection,person_id,exclude_concept_ids=exclude_concept_ids,include_concept_ids=include_concept_ids)
      }
    }
  }
}


addConditionEventPair<-function(connection,event1_concept_id,event2_concept_id,n, excludePatientIds=c()) {
  print(paste0('Adding ',n,'x ',event1_concept_id,'->',event2_concept_id,' event pair to te data'))
  if(n>2694) warning("Error in tests: n>2694 but there are 2694 people in Eunomia database")
  person_ids<-getPatientIds(connection, n, excludePatientIds=excludePatientIds)
  for(person_id in person_ids) {
    #print(person_id)
    addConditionEventPairForPerson(connection,event1_concept_id,event2_concept_id,person_id)
  }
  return(person_ids)
}

setUpEunomia<-function() {
  if(!require(drat)){
    install.packages("drat")
    library(drat)
  }
  drat::addRepo("OHDSI")
  if(!require(Eunomia)){
    install.packages("Eunomia")
    library(Eunomia)
  }
  connectionDetails <- getEunomiaConnectionDetails()
  Eunomia::createCohorts(connectionDetails)

  connection <- DatabaseConnector::connect(connectionDetails)

  #on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit

  trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema = "temp_schema",
                                                                 prefixForResultTableNames = "",
                                                                 cdmDatabaseSchema = 'main',
                                                                 vocabDatabaseSchema = 'main',
                                                                 resultsSchema = 'main',
                                                                 sqlRole = F,
                                                                 cohortTableSchema='main',
                                                                 cohortTable='cohort',
                                                                 cohortId=1,
                                                                 inputFolder=system.file("extdata", "fulldb", package = "Trajectories"), # Full path to input folder that contains SQL file for cohort definition and optionally also trajectoryAnalysisArgs.json. You can use built-in folders of this package such as: inputFolder=system.file("extdata", "T2D", package = "Trajectories")
                                                                 #mainOutputFolder=tempdir(check=TRUE),
                                                                 mainOutputFolder=getwd(),
                                                                 databaseHumanReadableName='TEST')

  return(list(connection=connection,trajectoryLocalArgs=trajectoryLocalArgs))
}

setObservationPeriodForAll<-function(connection,startdate='2010-01-01',enddate='2012-12-31') {
  sql<-translate(paste0("UPDATE OBSERVATION_PERIOD SET OBSERVATION_PERIOD_START_DATE=",getSQLiteRealFromDateSQLstring(startdate),", OBSERVATION_PERIOD_END_DATE=",getSQLiteRealFromDateSQLstring(enddate),";"), targetDialect=connection@dbms)
  executeSql(connection, sql, progressBar = F,
             reportOverallTime=F)
}

limitToNumPatients<-function(connection,n=2694) {
  person_ids<-getPatientIds(connection,n)
  executeSql(connection, paste0("DELETE FROM OBSERVATION_PERIOD WHERE person_id NOT IN (",paste(person_ids,collapse=","),");"), progressBar = F,
             reportOverallTime=F)
  executeSql(connection, paste0("DELETE FROM PERSON WHERE person_id NOT IN (",paste(person_ids,collapse=","),");"), progressBar = F,
             reportOverallTime=F)
  executeSql(connection, paste0("DELETE FROM CONDITION_OCCURRENCE WHERE person_id NOT IN (",paste(person_ids,collapse=","),");"), progressBar = F,
             reportOverallTime=F)
}

limitToConcepts<-function(connection,concept_ids=c(9201, #inpatient visit
                                                   9202, #outpatient visit

                                                   255848, #Condition: Pneumonia
                                                   313217, #Condition: Atrial fibrillation
                                                   317009, #Condition: Asthma
                                                   378419, #Condition: Alzheime
                                                   4144583, #Condition: Diabetes mellitus due to cystic fibrosis
                                                   4001336,	#Condition:Concussion injury of brain
                                                   192671,	#Condition:Gastrointestinal hemorrhage
                                                   4029498,	#Condition:Seizure disorder
                                                   4132546,	#Condition:Traumatic brain injury
                                                   140673,	#Condition:Hypothyroidism
                                                   4027663,	#Condition:Peptic ulcer
                                                   4066995,	#Condition:Fracture of vertebral column with spinal cord injury
                                                   4043241,	#Condition:Familial Alzheimer's disease of early onset
                                                    4059173,	#Condition:Fracture of ankle
                                                    4084167,	#Condition:Acute allergic reaction
                                                    4056621,	#Condition:Recurrent urinary tract infection
                                                    4134304,	#Condition:Fracture subluxation of wrist
                                                    4299128,	#Condition:Third degree burn
                                                    4329847,	#Condition:Myocardial infarction
                                                    44782520,	#Condition:Chronic paralysis due to lesion of spinal cord

                                                   442116, #Observation: Allergy to eggs
                                                   4168004, #Observation: Burn injury
                                                   4219399, #Observation: Allergy to fish

                                                   1112807, #Drug: Aspirin
                                                   1124300, #Drug: Diclofenac
                                                   1149380, #Drug:fluticasone
                                                   1308738, #Drug: Vitamin B 12

                                                   4163872, #Procedure: Plain chest X-ray
                                                   4198190, #Procedure: Appendectomy
                                                   4246502 #Procedure: Neonatal screening
                        )) {
  executeSql(connection, paste0("DELETE FROM CONCEPT WHERE CONCEPT_ID NOT IN (",paste(concept_ids,collapse=","),");"), progressBar = F,
             reportOverallTime=F)
  executeSql(connection, paste0("DELETE FROM CONDITION_OCCURRENCE WHERE CONDITION_CONCEPT_ID NOT IN (",paste(concept_ids,collapse=","),");"), progressBar = F,
             reportOverallTime=F)
  executeSql(connection, paste0("DELETE FROM PROCEDURE_OCCURRENCE WHERE PROCEDURE_CONCEPT_ID NOT IN (",paste(concept_ids,collapse=","),");"), progressBar = F,
             reportOverallTime=F)
  executeSql(connection, paste0("DELETE FROM OBSERVATION WHERE OBSERVATION_CONCEPT_ID NOT IN (",paste(concept_ids,collapse=","),");"), progressBar = F,
             reportOverallTime=F)
  executeSql(connection, paste0("DELETE FROM DRUG_EXPOSURE WHERE DRUG_CONCEPT_ID NOT IN (",paste(concept_ids,collapse=","),");"), progressBar = F,
             reportOverallTime=F)

}

getEventPairsTableAsDataFrame<-function(trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs.tsv') {
  #Get output folder for this analysis
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F)
  eventPairResultsFilename = file.path(outputFolder,filename)
  #Get event_pairs.csv table
  event_pairs_data = read.csv2(file = eventPairResultsFilename, sep = '\t', header = TRUE, as.is=T)
  return(event_pairs_data)
}

getEventPairFromEventPairsTable<-function(event1_concept_id,event2_concept_id,trajectoryLocalArgs,trajectoryAnalysisArgs,filename='event_pairs.tsv') {
  e<-getEventPairsTableAsDataFrame(trajectoryLocalArgs,trajectoryAnalysisArgs,filename=filename)
  res<-e %>% filter(E1_CONCEPT_ID==event1_concept_id & E2_CONCEPT_ID==event2_concept_id)
  return(res)
}

removeTestableOutputFiles<-function(trajectoryLocalArgs,trajectoryAnalysisArgs) {
  #Get output folder for this analysis
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs,createIfMissing=F)

  filename="event_pairs.tsv"
  if(file.exists(file.path(outputFolder,filename))) file.remove(file.path(outputFolder,filename))

  filename="event_pairs_tested.tsv"
  if(file.exists(file.path(outputFolder,filename))) file.remove(file.path(outputFolder,filename))

}
