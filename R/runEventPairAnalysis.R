library(SqlRender)
library(logger)

#' Runs the analysis that detects statistically significant directional event pairs and writes the results to file. Data is taken from database and it is expected that the tables are created by function createEventPairsTable()
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param forceRecalculation Set to TRUE if you wish to recalculate p-values for all pairs again. If it is set to FALSE, it avoids overcalculating p-values for pairs that have been analyzed already.
#'
#' @return
#' @export
#'
#' @examples
runEventPairAnalysis<-function(connection,
                               trajectoryAnalysisArgs,
                               trajectoryLocalArgs,
                               forceRecalculation=F) {

  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  d1d2ModelResultsFilename = file.path(outputFolder,'event_pairs_tested.tsv')
  eventPairResultsFilename = file.path(outputFolder,'event_pairs.tsv')
  eventPairResultsStatsFilename = file.path(outputFolder,'event_pairs_stats.txt')

  logger::log_info(paste0("Detect statistically significant directional event pairs and write the results to ",eventPairResultsFilename,"..."))

  #Set SQL role of the database session
  Trajectories::setRole(connection, trajectoryLocalArgs$sqlRole)

  # Get number of all (frequent) event pairs from the database
  RenderedSql <- Trajectories::loadRenderTranslateSql("GetNumPairs.sql",
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames
  )
  num.dpairs = DatabaseConnector::querySql(connection, RenderedSql)
  num.dpairs <- num.dpairs$TOTAL

  #Leave out pairs that have E1_E2_EVENTPERIOD_COUNT==0 (some counts might be 0 in case of validation mode). Skipping them from the analysis also affects Bonferroni correction
  # Get number of all non-zero-count event pairs from the database
  RenderedSql <- Trajectories::loadRenderTranslateSql("GetNumPairsForAnalysis.sql",
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames
  )
  num.nonzero.dpairs = DatabaseConnector::querySql(connection, RenderedSql)
  num.nonzero.dpairs <- num.nonzero.dpairs$TOTAL

  if(num.dpairs!=num.nonzero.dpairs) {
    logger::log_info(paste0('There are ',num.dpairs,' event pairs in the original event pairs tabel. However, ',num.dpairs-num.nonzero.dpairs,' of them have count=0 and are therefore skipped from the remaining analysis.'))
  }

  # Determine p-value threshold of Bonferroni correction
  cutoff_pval = 0.05/num.nonzero.dpairs
  logger::log_info(paste0('There are ',num.nonzero.dpairs,' event pairs that are going to be analyzed.'))
  if(num.nonzero.dpairs==0) {
    logger::log_info('Nothing to analyze, exit analysis function.')
    return(1);
  }
  logger::log_info(paste0('We use Bonferroni multiple test correction, therefore p-value threshold 0.05/',num.nonzero.dpairs,'=',cutoff_pval,' is used in association analysis.'))
  logger::log_info(paste0('For directionality tests, a different p-value threshold is used. It depends on the actual number of significant associactions and will be calculated in the end of analysis.'))
  logger::log_info(paste0('To indicate the estimated number of significant event pairs while the anaylsis is running, we use the same (very conservative) p-value threshold as for association analysis.'))


  #Load all event pairs for the analysis
  # (also) Avoid recalculation if forceRecalculation=T and the data is already there (the analysis has already run up to some point)
  RenderedSql <- Trajectories::loadRenderTranslateSql("GetPairsForCalculation.sql",
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                      cutoffPval = cutoff_pval, #this is used only when forceRecalculation=T
                                                      forceRecalculation = ifelse(forceRecalculation==T,1,0)
  )
  dpairs_for_analysis = DatabaseConnector::querySql(connection, RenderedSql)

  if(forceRecalculation==T) {
    significant_pairs_count=0
    significant_directional_pairs_count=0
  } else {

    if(nrow(dpairs_for_analysis)<num.nonzero.dpairs) {
      logger::log_info(paste0("For ",num.nonzero.dpairs-nrow(dpairs_for_analysis))," event pairs, p-value is already calculated. They are skipped in this analysis run (skipping them do not affect Bonferroni corrected p-value threshold).")
    }
    RenderedSql <- Trajectories::loadRenderTranslateSql("GetNumPairsAssociated.sql",
                                                        packageName=trajectoryAnalysisArgs$packageName,
                                                        dbms=connection@dbms,
                                                        resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                        prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                        cutoffPval = cutoff_pval
    )
    significant_pairs_count = DatabaseConnector::querySql(connection, RenderedSql)
    significant_pairs_count<-significant_pairs_count$TOTAL

    RenderedSql <- Trajectories::loadRenderTranslateSql("GetNumPairsDirectional.sql",
                                                        packageName=trajectoryAnalysisArgs$packageName,
                                                        dbms=connection@dbms,
                                                        resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                        prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                        cutoffPval = cutoff_pval
    )
    significant_directional_pairs_count = DatabaseConnector::querySql(connection, RenderedSql)
    significant_directional_pairs_count<-significant_directional_pairs_count$TOTAL
    if(nrow(dpairs_for_analysis)<num.nonzero.dpairs) {
      logger::log_info(paste0("Among ",num.nonzero.dpairs-nrow(dpairs_for_analysis))," event pairs that have p-value already calculated, ",significant_pairs_count," are significantly associated and ",significant_directional_pairs_count," are directional.")
    }
  }

  starttime=Sys.time()
  # For each event pair, run the analysis
  if(nrow(dpairs_for_analysis)>0) {
    for(i in 1:nrow(dpairs_for_analysis))
    {
      diagnosis1 = dpairs_for_analysis[i,'E1_CONCEPT_ID']
      diagnosis2 = dpairs_for_analysis[i,'E2_CONCEPT_ID']

      rr_in_previous_study=dpairs_for_analysis[i,'RR_IN_PREVIOUS_STUDY']
      event_pair_pvalue=1

      logger::log_info(paste0('Analyzing event pair ',diagnosis1,' -> ',diagnosis2,' (total progress ',
                              round(100*i/nrow(dpairs_for_analysis)),
                              '%, # sign pairs: ',
                              significant_pairs_count,
                              ', at least ',
                              significant_directional_pairs_count,
                              ' directional, estimated time left: ',Trajectories::estimatedTimeRemaining(progress_perc=(i-1)/nrow(dpairs_for_analysis),starttime=starttime),
                              ')...'))




      # Extract necessary event1_concept_id->event2_concept_id data from table d1d2_analysistable
      RenderedSql <- Trajectories::loadRenderTranslateSql("5CaseControlStats.sql",
                                                          packageName=trajectoryAnalysisArgs$packageName,
                                                          dbms=connection@dbms,
                                                          event1=diagnosis1,
                                                          event2=diagnosis2,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      case_control = DatabaseConnector::querySql(connection, RenderedSql)



      # The code below is going to answer the following question:
      #       If there is no difference in event2_concept_id prevalence between case group (people having event1_concept_id) and control group
      #       (general population),
      #       what is the probability that we observe event2_concept_id in case group that many times?
      #       If the probability is significantly small, then the null hypothesis (no difference in prevalence)
      #       is rejected and we have found a significant event pair.
      #       If a significant event pair (event1_concept_id-event2_concept_id) is found, it is tested, whether also the direction is significant.



      # First of all, we have to find out, what is the expected prevalence of event2_concept_id (based on control group).
      # By null hypothesis we assume that event2_concept_id prevalence (called here 'expected_prob') among case and control group is the same.
      # Therefore, we could take the prevalence as: expected_prob=sum(case_control$CONTROL_D2)/sum(case_control$CONTROL_COUNT)
      # However, we have to also take into account that age, gender, discharge time distributions of case/control groups differ.
      # Therefore, we "adjust" the prevalence based on event1_concept_id distribution in case group:
      #   expected_prob= (event1_concept_id distribution in case group) * (event2_concept_id prevalence in control group)
      # The following lines of code do exactly that.

      # Add column "group_prob" which basically is the event1_concept_id count distribution over age, gender, discharge time in case group
      case_control$group_prob <- case_control$CASE_COUNT / sum(case_control$CASE_COUNT)

      # Add column "match_prob" which is event2_concept_id prevalence within this age, gender, discharge time group in general population
      case_control$match_prob <- case_control$CONTROL_D2/case_control$CONTROL_COUNT

      # Find "adjusted" expected prevalence of event2_concept_id in case group based on event1_concept_id distribution in case group:
      expected_prob <- sum(case_control$group_prob * case_control$match_prob)
      # Sometimes the arithmetics above gives a result (probability) slighlty greater than 1. Lets fix this.
      if (expected_prob > 1){
        expected_prob = 1
      }

      # In general population event2_concept_id occurs with probability p=expected_prob
      # Assume (null hypothesis) that in our case group, the probability is the same as in general population.
      # That is, in case group, declare prob=expected_prob
      # We are going to check whether the event2_concept_id prevalence in case group is different from this.
      # In our case group, we have 'observation_count=sum(case_control$CASE_COUNT)' diagnosis pairs. That is, we run 'observation_count' tests.
      # And we actually see event2_concept_id happening within case group 'observed_matches=sum(case_control$CASE_D2)' times.
      # Therefore:  If the expected prevalence of event2_concept_id in case group is 'expected_prob',
      #             what is the probability that we observe event2_concept_id in case group more than 'observed_matches-1'?
      # This question can be easily answered by pbinom(..., lower.tail=FALSE) which gives cumulative density function (cdf) as a result
      # Using lower.tail=FALSE is necessary, because otherwise pbinom would give the probability that we observe event2_concept_id in case group LESS than 'observed_matches'


      observed_matches <- sum(case_control$CASE_D2)
      observation_count <- sum(case_control$CASE_COUNT)
      logger::log_debug('Actual prevalence of D2 in (adjusted) control group is {round(expected_prob*100)}% (and in case group {round(observed_matches*100/sum(case_control$CASE_COUNT))}%). If the expected prevalence of event2_concept_id in case group is {expected_prob}, what is the probability that we observe event2_concept_id in case group more than {observed_matches-1} (we actually did {observed_matches})?')
      logger::log_debug('If the expected prevalence of event2_concept_id in case group is {round(expected_prob*100)}%, what is the probability that we observe event2_concept_id in case group more than {observed_matches-1}?')
      event_pair_pvalue <- Trajectories:::getPValueForAccociation(expected_prob,observation_count,observed_matches)

      #In case event_pair_pvalue <= cutoff_pval, the null hypothesis can be rejected and the prevalence of event2_concept_id among event1_concept_id patients
      #is significantly larger than in general population.
      #This means that we have found a significant event1_concept_id-event2_concept_id event pair!

      #What is the "relative risk" (effect) (how many times the event2_concept_id prevalence in case group is higher than in control group)
      event_pair_rr <- observed_matches / (observation_count*expected_prob)

      E2_COUNT_IN_CONTROL_GROUP = round(observation_count*expected_prob)

      #Calculate power
      powerAssociation=Trajectories:::getPowerAssociation(case_control,expected_prob,rr.threshold=ifelse(is.na(rr_in_previous_study),1.2,rr_in_previous_study))


      # Writing the results back to database
      RenderedSql <- Trajectories::loadRenderTranslateSql("6PvalInserter.sql",
                                                          packageName=trajectoryAnalysisArgs$packageName,
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          pval = event_pair_pvalue,
                                                          rr = event_pair_rr,
                                                          diag1 = diagnosis1,
                                                          diag2 = diagnosis2,
                                                          E2_COUNT_IN_CONTROL_GROUP = E2_COUNT_IN_CONTROL_GROUP,
                                                          power=ifelse(is.na(powerAssociation),'NULL',powerAssociation),
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      #print(power)
      #print(RenderedSql)
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)


      # In case the event1_concept_id-event2_concept_id event pair is significant, let's investigate whether the order of the events is also important
      if (event_pair_pvalue > cutoff_pval){

        logger::log_debug(paste0('Event pair ',diagnosis1,' -> ',diagnosis2,' is not significant.'))

      } else {
        significant_pairs_count <- significant_pairs_count + 1
        logger::log_debug(paste0('Event pair ',diagnosis1,' -> ',diagnosis2,' is significant. Testing its directionality...'))

        #Calculate in database: among people that have event1_concept_id and event2_concept_id pair, how many have date1<date2, date1=date2, date1>date2
        RenderedSql <- Trajectories::loadRenderTranslateSql("7DirectionCounts.sql",
                                                            packageName=trajectoryAnalysisArgs$packageName,
                                                            dbms=connection@dbms,
                                                            resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                            diag1 = diagnosis1,
                                                            diag2 = diagnosis2,
                                                            prefix =  trajectoryLocalArgs$prefixForResultTableNames
        )
        DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

        # Get calculation results from database
        RenderedSql <- Trajectories::loadRenderTranslateSql("8DpairReader.sql",
                                                            packageName=trajectoryAnalysisArgs$packageName,
                                                            dbms=connection@dbms,
                                                            resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                            diag1=diagnosis1,
                                                            diag2=diagnosis2,
                                                            prefix =  trajectoryLocalArgs$prefixForResultTableNames
        )
        direction_counts = DatabaseConnector::querySql(connection, RenderedSql)


        # If there is no significant direction in event pair event1_concept_id and event2_concept_id, then we expect to see event1_concept_id->event2_concept_id and event2_concept_id->event1_concept_id sequences
        # with the same frequency. Say, we would expect that event1_concept_id is the first diagnosis in 50% cases (prob=0.5).
        # Therefore: If the expected probability of seeing event1_concept_id as the first diagnosis is 0.5,
        #            what is the probability that we observe event1_concept_id as the first diagnosis more than 'direction_counts$people_count_event1_occurs_first-1' times?
        # This question can be easily answered by pbinom(..., lower.tail=FALSE) which gives cumulative density function (cdf) as a result
        # Using lower.tail=FALSE is necessary, because otherwise pbinom would give the probability that we observe event1_concept_id as first diagnosis  LESS than 'direction_counts$people_count_event1_occurs_first'
        event_pair_pvalue = Trajectories:::getPValueForDirection(direction_counts$EVENTPERIOD_COUNT_E1_OCCURS_FIRST,direction_counts$EVENTPERIOD_COUNT_E2_OCCURS_FIRST,direction_counts$EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY)

        #Calculate direction power
        powerDirection = Trajectories:::getPowerDirection(direction_counts$EVENTPERIOD_COUNT_E1_OCCURS_FIRST,direction_counts$EVENTPERIOD_COUNT_E2_OCCURS_FIRST,direction_counts$EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY,rr.threshold=1.2)


        # Store the pvalue to database
        RenderedSql <- Trajectories::loadRenderTranslateSql("9PvalInserterDirection.sql",
                                                            packageName=trajectoryAnalysisArgs$packageName,
                                                            dbms=connection@dbms,
                                                            resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                            pval = event_pair_pvalue,
                                                            diag1 = diagnosis1,
                                                            diag2 = diagnosis2,
                                                            powerDirection=ifelse(is.na(powerDirection),'NULL',powerDirection),
                                                            prefix =  trajectoryLocalArgs$prefixForResultTableNames
        )
        DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

        if (event_pair_pvalue > cutoff_pval){

          logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is not significant (might become significant after threshold adjustment).'))

        } else {

          significant_directional_pairs_count <- significant_directional_pairs_count + 1
          logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is significant.'))

        }

      }

      #} #if

    } # for
  } #if




  logger::log_info('Adjusting p-value cutoff for directionality tests...')
  logger::log_info(paste0('  Found ',significant_pairs_count,' significant event pairs.'))
  logger::log_info(paste0('  For all of them, we conducted directionality test.'))
  if(significant_pairs_count>0) {
    cutoff_pval_direction=0.05/significant_pairs_count
  } else {
    cutoff_pval_direction=0.05
  }
  logger::log_info(paste0('... Therefore, Bonferroni corrected p-value threshold for directionality tests is 0.05/',significant_pairs_count,'=',cutoff_pval_direction))

  # Update significance flag info in database
  RenderedSql <- Trajectories::loadRenderTranslateSql("updateSignificanceFlag.sql",
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                      cutoff_pval=cutoff_pval,
                                                      cutoff_pval_direction=cutoff_pval_direction
  )
  DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

  # Read in results
  RenderedSql <- Trajectories::loadRenderTranslateSql("11ResultsReader.sql",
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                      cutoff_val=cutoff_pval_direction,
                                                      rr = 1.0
  )
  selected_data = DatabaseConnector::querySql(connection, RenderedSql)
  significant_directional_pairs_count=nrow(selected_data)
  logger::log_info(paste0('There are ',significant_directional_pairs_count,' significant directional event pairs having p-val > ',cutoff_pval_direction,'.'))
  logger::log_info('Some of them may have very small relative risk. Therefore, we extract significant event pairs that have relative risk > 1.1')


  RenderedSql <- Trajectories::loadRenderTranslateSql('d1d2_model_reader.sql',
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames
  )
  d1d2_data = DatabaseConnector::querySql(connection, RenderedSql)
  # Write result table into file
  write.table(d1d2_data, file=d1d2ModelResultsFilename, quote=FALSE, sep='\t', col.names = NA)


  RenderedSql <- Trajectories::loadRenderTranslateSql("11ResultsReader.sql",
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                      cutoff_val=cutoff_pval_direction,
                                                      rr = 1.1
  )
  selected_data = DatabaseConnector::querySql(connection, RenderedSql)

  # Write result table into file
  write.table(selected_data, file=eventPairResultsFilename, quote=FALSE, sep='\t', col.names = NA)

  logger::log_info(paste0(nrow(d1d2_data),' event pairs were tested. All test results are written to {d1d2ModelResultsFilename}.'))
  logger::log_info(paste0('Out of ',significant_pairs_count,' significant event pairs, ',significant_directional_pairs_count,' have significant direction. Out of these, ',nrow(selected_data),' have relative risk > 1.1.'))
  logger::log_info(paste0('These ',nrow(selected_data),' event pairs were written to ',eventPairResultsFilename))


  # Show some event pair statistics:
  msg=c(paste('EVENT PAIR RESULTS STATS:'),
        paste('========================='),
        paste(format(Sys.time(), '%d %B %Y %H:%M')),
        paste(''),
        paste('Total number of event pairs in initial table:',num.dpairs),
        paste('Total number of event pairs analyzed (having non-zero count):',nrow(dpairs_for_analysis)),
        paste('Bonferroni corrected p-value threshold for association test:',cutoff_pval),
        paste('Bonferroni corrected p-value threshold for directionality test:',cutoff_pval_direction),
        paste('Number of significant event pairs:',significant_pairs_count),
        paste('Number of significant event pairs with significant direction:',significant_directional_pairs_count),
        paste('Number of significant event pairs with significant direction having relative risk > 1.1:',nrow(selected_data)),
        paste('These significant event pairs written to:',eventPairResultsFilename)

  )
  logger::log_info(paste(msg,collapse="\n"))

  # Print stats to file
  if(eventPairResultsStatsFilename!=F) {
    logger::log_info(paste0('Writing event pair statistics to ',eventPairResultsStatsFilename,'...'))

    fileConn<-file(eventPairResultsStatsFilename)
    writeLines(msg, fileConn)
    close(fileConn)

    logger::log_info('... done.')
  }

  logger::log_info('TASK COMPLETED: Detecting statistically significant directional event pairs completed successfully.')
}



# Functions for statistical power calculation

# Increases E2 counts in control group by 20% (alpha). Returns E2 counts in each age-sex bins
addSignalToBackground <- function(case_control, alpha=0.2){

  expected_prevalence=sum(case_control$group_prob * case_control$match_prob)
  if(expected_prevalence*(1+alpha)>1) {
    logger::log_info('Prevalence in control group is already {round(expected_prevalence*100)}%, cannot increase it by {round(alpha*100)}% to calculate power.')
    return(NA)
  }

  extra_total=ceiling(alpha*sum(case_control$CONTROL_D2))

  case_control<-case_control %>%
    mutate(CONTROL_D2_ELEVATED=round((1+alpha)*CONTROL_D2)) %>% #note that some bins might be overfilled after this step - need to limit them (next command)
    mutate(CONTROL_D2_ELEVATED=ifelse(CONTROL_D2_ELEVATED<=CONTROL_COUNT,CONTROL_D2_ELEVATED,CONTROL_COUNT))

  #How many additional E2 counts do we need to add
  extra_left = extra_total - (sum(case_control$CONTROL_D2_ELEVATED)-sum(case_control$CONTROL_D2))
  while(extra_left>0) {
    #From all bins where there are "room" for additional E2 occurrences, order them by count (desc) and add +1 for E2 count
    #Take into account the actual distribution of E2 prevalence in matched general population - case_control$group_prob * case_control$match_pro
    o<-order(case_control$group_prob * case_control$match_prob * (case_control$CONTROL_D2_ELEVATED < case_control$CONTROL_COUNT), decreasing = TRUE)
    index <- o[1:min(length(o),extra_left)]
    case_control$CONTROL_D2_ELEVATED[index] <- case_control$CONTROL_D2_ELEVATED[index] +1
    extra_left = extra_total - (sum(case_control$CONTROL_D2_ELEVATED)-sum(case_control$CONTROL_D2))
  }

  #By now, count of elevated E2 occurrences -> sum(m) <- should be exactly sum(case_control$CONTROL_D2)+extra
  #you may check that: sum(m)==sum(case_control$CONTROL_D2)+extra

  if(sum(case_control$CONTROL_D2_ELEVATED>case_control$CONTROL_COUNT)>0) stop('Error in addSignalToBackground. Increased E2 count in some bins > total count')

  logger::log_debug(paste0('Increased E2 counts in CONTROL group of case_control dataframe by 20%. Old count: ',sum(case_control$CONTROL_D2),'. New count: ',sum(case_control$CONTROL_D2_ELEVATED),'.'))
  return(case_control$CONTROL_D2_ELEVATED)

}

# Function for sampling positive E2 with alpha lift
#Create n case groups (by sampling) from the (20% elevated) general population and see how many E2 occurrencies you'd get in each sample/case group
sampleFromEleveatedBackgroundOld <- function(E2_count_in_background=c(), non_E2_count_in_background=c(), case_count=c(), n=1000){
  return(apply(mapply(rhyper, n, E2_count_in_background, non_E2_count_in_background, case_count), 1 , sum))
}


#If the expected prevalence of event2_concept_id in case group is {round(expected_prob*100)}%, what is the p-value that we observe event2_concept_id in case group more than {observed_matches-1}?
getPValueForAccociation<-function(expected_prob,observation_count,observed_matches) {
  logger::log_debug('Actual prevalence of D2 in (adjusted) control group is {round(expected_prob*100)}% (and in case group {round(observed_matches*100/observation_count)}%). If the expected prevalence of event2_concept_id in case group is {expected_prob}, what is the probability that we observe event2_concept_id in case group more than {observed_matches-1} (we actually did {observed_matches})?')
  logger::log_debug('If the expected prevalence of event2_concept_id in case group is {round(expected_prob*100)}%, what is the probability that we observe event2_concept_id in case group more than {observed_matches-1}?')
  event_pair_pvalue <- pbinom(q = ifelse(observed_matches==0,0,observed_matches-1), size = observation_count, prob = expected_prob, lower.tail=FALSE)
  return(event_pair_pvalue)
}

#pbinom(q = ifelse(eventperiod_count_event1_occurs_first_for_test==0,0,eventperiod_count_event1_occurs_first_for_test-1), size = total_tests, prob = 0.5, lower.tail=FALSE)
getPValueForDirection<-function(EVENTPERIOD_COUNT_E1_OCCURS_FIRST,EVENTPERIOD_COUNT_E2_OCCURS_FIRST,EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY) {
  total_tests = EVENTPERIOD_COUNT_E1_OCCURS_FIRST + EVENTPERIOD_COUNT_E2_OCCURS_FIRST + EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY #We also take into account the number of events on the same day to prevent problem, when 1000 events occur on same day, but 10 times E1 is before E2 and 1 times vice verca and this is significant.
  logger::log_debug('In case group, event1 occurs {EVENTPERIOD_COUNT_E1_OCCURS_FIRST} times as the first event and {EVENTPERIOD_COUNT_E2_OCCURS_FIRST} as the second event.')
  logger::log_debug('Both events occur on same day {EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY} times.')
  eventperiod_count_event1_occurs_first_for_test=EVENTPERIOD_COUNT_E1_OCCURS_FIRST + round(EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY/2)
  logger::log_debug('If the expected probability of event1 being the first diagnosis is 0.5, what is the probability that we observe event1 as the first event more than {EVENTPERIOD_COUNT_E1_OCCURS_FIRST}+{EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY}/2-1={eventperiod_count_event1_occurs_first_for_test-1} times out of {total_tests} trials?')
  event_pair_pvalue <- pbinom(q = ifelse(eventperiod_count_event1_occurs_first_for_test==0,0,eventperiod_count_event1_occurs_first_for_test-1), size = total_tests, prob = 0.5, lower.tail=FALSE)
  logger::log_debug('Answer: p-val={event_pair_pvalue}')
  return(event_pair_pvalue)
}

#Get power: the probability that we detect the association between E1 and E2 in case E1 increases the risk of getting E2 by 20% (relative risk threshold) when compared to (age-sex matched) general population
getPowerOld<-function(case_control,expected_prob,rr.threshold=1.2,cutoff_pval) {
  CONTROL_D2_ELEVATED <- Trajectories:::addSignalToBackground(case_control, alpha=(rr.threshold-1))
  #number of times to sample. n=1000 seems reasonable to get quite accurate estimate
  n=1000
  #Create n case groups (by sampling) from the (20% elevated) general population and see how many E2 occurrencies you'd get in each sample/case group
  observation_count <- sum(case_control$CASE_COUNT)
  observed_matches<-Trajectories:::sampleFromEleveatedBackgroundOld(E2_count_in_background=CONTROL_D2_ELEVATED,
                                                                    non_E2_count_in_background=case_control$CONTROL_COUNT - CONTROL_D2_ELEVATED,
                                                                    case_count=case_control$CASE_COUNT,
                                                                    n=n)
  #hist(observed_matches)

  #Calculate p-value for each of these sampled case groups
  p.vals <- mapply(Trajectories:::getPValueForAccociation,
                   expected_prob=rep(expected_prob,n),
                   observation_count=rep(observation_count,n),
                   observed_matches=observed_matches)
  #hist(p.vals,breaks=1000)

  #Calculate power: how many p-values are belo threshold?
  power=sum(p.vals<cutoff_pval)/n

  logger::log_debug('If E1 increased the prevalence of E2 by {round((rr.threshold-1)*100)}%, we would detect it with probability {round(power*100)}% (=power)')
  return(power)

}

#Get power: the probability that we detect the association between E1 and E2 in case E1 increases the risk of getting E2 by 20% (relative risk threshold) when compared to (age-sex matched) general population
getPowerAssociation<-function(case_control,expected_prob,rr.threshold=1.2) {


  #number of times to sample from general population. n=1000 seems reasonable to get quite accurate estimate
  n=1000
  control_group_size=sum(case_control$CONTROL_COUNT)
  case_group_size= sum(case_control$CASE_COUNT)
  elevated_E2_prevalence=expected_prob*rr.threshold

  elevated_E2_count_in_control_group <- ceiling(control_group_size*elevated_E2_prevalence)
  #check that it is not overfilled
  elevated_E2_count_in_control_group=ifelse(elevated_E2_count_in_control_group>control_group_size,control_group_size,elevated_E2_count_in_control_group)
  #sample 1000 new case groups from this elevated general population. Count, what is the E2 occurrence count in them
  observed_case_E2_counts<-rhyper(nn=n, m=elevated_E2_count_in_control_group, control_group_size-elevated_E2_count_in_control_group, case_group_size)

  #hist(observed_case_E2_counts)

  #Calculate p-value for each of these sampled case groups
  p.vals <- mapply(Trajectories:::getPValueForAccociation,
                   expected_prob=rep(expected_prob,n),
                   observation_count=rep(case_group_size,n),
                   observed_matches=observed_case_E2_counts)
  #hist(p.vals,breaks=1000)

  #Calculate power: how many p-values are belo threshold?
  #Note that we do not use corrected threshold here as we select the true threshold AFTER the power analysis. Therefore, conservatively, we use 0.05 here to leave out event pairs that do not exceed the threshold even without correction.
  #power=sum(p.vals<cutoff_pval)/n
  power=sum(p.vals<0.05)/n


  logger::log_debug('If E1 increased the prevalence of E2 by {round(rr.threshold,3)}x, we would detect it with given case group size n={case_group_size} with probability {round(power*100)}% (=power)')
  return(power)

}

#Get power of direction: the probability that we detect the direction E1->E2 in case E1->E2 is overrepresented by 20%
getPowerDirection<-function(EVENTPERIOD_COUNT_E1_OCCURS_FIRST,EVENTPERIOD_COUNT_E2_OCCURS_FIRST,EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY,rr.threshold=1.2) {

  n=1000

  total_tests=EVENTPERIOD_COUNT_E1_OCCURS_FIRST+EVENTPERIOD_COUNT_E2_OCCURS_FIRST+EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY

  elevated_signal_prob=rr.threshold/(1+rr.threshold) #if one direction has prevalence 55% (elevated signal prob) and the other 45%, then the signal strength is 20% (45% x 20%)

  #Sample this distribution 1000 times (from infinite distribution) and see how many elevated_e1_occurs_first-s you'll get (skip tests where EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY)
  elevated_e1_occurs_first = apply(replicate(n = n, expr=sample(c(1,0), total_tests-EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY, T, c(elevated_signal_prob,1-elevated_signal_prob)), simplify = T),2,sum)

  #Calculate p-value for each of these sampled case groups
  p.vals <- mapply(Trajectories:::getPValueForDirection,
                   EVENTPERIOD_COUNT_E1_OCCURS_FIRST=elevated_e1_occurs_first,
                   EVENTPERIOD_COUNT_E2_OCCURS_FIRST=total_tests-EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY-elevated_e1_occurs_first,
                   EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY=rep(EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY,n))
  #hist(p.vals,breaks=1000)

  #Calculate power: how many p-values are belo threshold?
  #Note that we do not use corrected threshold here as we select the true threshold AFTER the power analysis. Therefore, conservatively, we use 0.05 here to leave out event pairs that do not exceed the threshold even without correction.
  #power=sum(p.vals<cutoff_pval)/n
  power=sum(p.vals<0.05)/n

  logger::log_debug('If direction E1->E2 were overrepresented {round(rr.threshold,3)}x when compared to E2->E1, we would detect it with probability {round(power*100)}% (=power)')
  return(power)

}
