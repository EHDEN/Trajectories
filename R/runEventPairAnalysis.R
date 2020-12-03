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

  # Get all (frequent) event pairs from the database
  RenderedSql <- Trajectories::loadRenderTranslateSql("2GetPairs.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=connection@dbms,
                                                   resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                   prefix =  trajectoryLocalArgs$prefixForResultTableNames
  )
  dpairs = DatabaseConnector::querySql(connection, RenderedSql)

  #Leave out pairs that have E1_E2_EVENTPERIOD_COUNT==0 (some counts might be 0 in case of validation mode). Skip them from the analysis to not affect Bonferroni correction
  dpairs_for_analysis<-dpairs %>% filter(E1_E2_EVENTPERIOD_COUNT>0)

  if(nrow(dpairs)!=nrow(dpairs_for_analysis)) {
    logger::log_info(paste0('There are ',nrow(dpairs),' event pairs in the original event pairs tabel. However, ',nrow(dpairs)-nrow(dpairs_for_analysis),' have count=0 and are therefore skipped from the remaining analysis.'))
  }

  # Determine p-value threshold of Bonferroni correction
  cutoff_pval = 0.05/nrow(dpairs_for_analysis)
  logger::log_info(paste0('There are ',nrow(dpairs_for_analysis),' event pairs that are going to be analyzed.'))
  if(nrow(dpairs_for_analysis)==0) {
    logger::log_info('Nothing to analyze, exit analysis function.')
    return(1);
  }
  logger::log_info(paste0('We use Bonferroni multiple test correction, therefore p-value threshold 0.05/',nrow(dpairs_for_analysis),'=',cutoff_pval,' is used in association analysis.'))
  logger::log_info(paste0('For directionality tests, a different p-value threshold is used. It depends on the actual number of significant associactions and will be calculated in the end of analysis.'))
  logger::log_info(paste0('To indicate the estimated number of significant event pairs while the anaylsis is running, we use the same (very conservative) p-value threshold as for association analysis.'))

  significant_pairs_count=0
  significant_directional_pairs_count=0
  starttime=Sys.time()
  # For each event pair, run the analysis
  for(i in 1:nrow(dpairs_for_analysis))
    {
      diagnosis1 = dpairs_for_analysis[i,'E1_CONCEPT_ID']
      diagnosis2 = dpairs_for_analysis[i,'E2_CONCEPT_ID']
      event_pair_pvalue=1

      logger::log_info(paste0('Analyzing event pair ',diagnosis1,' -> ',diagnosis2,' (total progress ',
                                                                            round(100*i/nrow(dpairs_for_analysis)),
                                                                            '%, # sign pairs: ',
                                                                            significant_pairs_count,
                                                                            ', at least ',
                                                                            significant_directional_pairs_count,
                                                                            ' directional, estimated time left: ',Trajectories::estimatedTimeRemaining(progress_perc=(i-1)/nrow(dpairs_for_analysis),starttime=starttime),
                                                                            ')...'))

      # Avoid recalculation if the data is already there (the analysis has already run up to some point)
      if(forceRecalculation==F) {
        pairCheckingSql <- Trajectories::loadRenderTranslateSql("pvalueChecker.sql",
                                                                packageName=trajectoryAnalysisArgs$packageName,
                                                                dbms=connection@dbms,
                                                                resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                                prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                                diag1 = diagnosis1,
                                                                diag2 = diagnosis2)
        pvalueCheck = DatabaseConnector::querySql(connection, pairCheckingSql)
        if(!is.na(pvalueCheck$EVENT_PAIR_PVALUE) &
            ((pvalueCheck$EVENT_PAIR_PVALUE > cutoff_pval) | (pvalueCheck$EVENT_PAIR_PVALUE <= cutoff_pval & !is.na(pvalueCheck$DIRECTIONAL_EVENT_PAIR_PVALUE)))
           ) {

          if (pvalueCheck$EVENT_PAIR_PVALUE <= cutoff_pval) {

            significant_pairs_count <- significant_pairs_count + 1

            if (pvalueCheck$DIRECTIONAL_EVENT_PAIR_PVALUE <= cutoff_pval) {
              significant_directional_pairs_count <- significant_directional_pairs_count + 1
            }
          }
          logger::log_info(paste0("P-value already calculated for event pair ",diagnosis1," -> ",diagnosis2," (skipping)"))
          next #halts the processing of the current "for loop" iteration and continues with the next iteration

        }
      } #if(forceRecalculation==F)


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
      # We apply Laplace smoothing to solve the problem of (possible) zero probability.
      # That is: +1 observation for cases and +1 for controls (+2 for total)
      # Update 8 Oct 2020: As control group cant be smaller than case group, it always has count>0 and there is no need for Laplace smoothing
      #case_control$match_prob <- (case_control$CONTROL_D2+1) / (case_control$CONTROL_COUNT + 2)
      case_control$match_prob <- case_control$CONTROL_D2/case_control$CONTROL_COUNT


      #Check that control group is balanced enough to case group
      #case_control$CONTROL_CASE_RATIO=case_control$CONTROL_COUNT/case_control$CASE_COUNT
      #case_control$CONTROL_CASE_WEIGHTED_RATIO=case_control$CONTROL_CASE_RATIO*case_control$group_prob
      #m<-mean(case_control$CONTROL_CASE_WEIGHTED_RATIO) #mean of weighted CASE_CONTROL_RATIO
      #s<-sd(case_control$CONTROL_CASE_WEIGHTED_RATIO) #SD of weighted CASE_CONTROL_RATIO

      #check that 95% of differences lies withing 2 standard deviations of the mean
      #diff=abs(case_control$CONTROL_CASE_WEIGHTED_RATIO-m)
      #proportion=sum(diff>2*s)/length(diff)

      #CONTROL_CASE_RATIO_Q=quantile(case_control$CONTROL_CASE_RATIO,probs=c(0.1,0.9))
      #ratios from 10%...90%
      #x<-case_control[which(case_control$CONTROL_CASE_RATIO >= CONTROL_CASE_RATIO_Q[1] & case_control$CONTROL_CASE_RATIO <= CONTROL_CASE_RATIO_Q[2]),]

      #if(proportion>0.05) {
      #  logger::log_warn(paste0('Control group is inbalanced: ',round(proportion*100,0),'% of control/case count ratios are more than 2 standard deviations apart from mean ratio. Stop analyzing this pair.'))
      #  max_diff_i<-which(abs(case_control$CONTROL_CASE_WEIGHTED_RATIO-m)==max(abs(case_control$CONTROL_CASE_WEIGHTED_RATIO-m)))
      #  if(length(max_diff_i)>1) max_diff_i=max_diff_i[1]
      #  logger::log_warn(paste0('The biggest inbalance is in (GENDER=',case_control[max_diff_i,'GENDER'],'; AGE=',case_control[max_diff_i,'AGE'],'; DISCHARGE_TIME=',case_control[max_diff_i,'DISCHARGE_TIME'],') group: CASE COUNT=',case_control[max_diff_i,'CASE_COUNT'],' CONTROL COUNT=',case_control[max_diff_i,'CONTROL_COUNT']))
      #} else {


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
      logger::log_debug('Actual prevalence of D2 in (adjusted) control group is {round(expected_prob*100)}% (and in case group {round(observed_matches*100/sum(case_control$CASE_COUNT))}%). If the expected prevalence of event2_concept_id in case group is {expected_prob}, what is the probability that we observe event2_concept_id in case group more than {observed_matches-1} (we actually did {observed_matches})?')
      logger::log_debug('If the expected prevalence of event2_concept_id in case group is {round(expected_prob*100)}%, what is the probability that we observe event2_concept_id in case group more than {observed_matches-1}?')
      observation_count <- sum(case_control$CASE_COUNT)
      event_pair_pvalue <- pbinom(q = ifelse(observed_matches==0,0,observed_matches-1), size = observation_count, prob = expected_prob, lower.tail=FALSE)

      #In case event_pair_pvalue <= cutoff_pval, the null hypothesis can be rejected and the prevalence of event2_concept_id among event1_concept_id patients
      #is significantly larger than in general population.
      #This means that we have found a significant event1_concept_id-event2_concept_id event pair!

      #What is the "effect" (how many times the event2_concept_id prevalence in case group is higher than in control group)
      event_pair_effect <- observed_matches / (observation_count*expected_prob)

      E2_COUNT_IN_CONTROL_GROUP = round(observation_count*expected_prob)


      # Writing the results back to database
      RenderedSql <- Trajectories::loadRenderTranslateSql("6PvalInserter.sql",
                                                       packageName=trajectoryAnalysisArgs$packageName,
                                                       dbms=connection@dbms,
                                                       resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                       pval = event_pair_pvalue,
                                                       effect = event_pair_effect,
                                                       diag1 = diagnosis1,
                                                       diag2 = diagnosis2,
                                                       E2_COUNT_IN_CONTROL_GROUP = E2_COUNT_IN_CONTROL_GROUP,
                                                       prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
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
        eventperiod_count_event1_occurs_first = direction_counts$EVENTPERIOD_COUNT_E1_OCCURS_FIRST

        total_tests = direction_counts$EVENTPERIOD_COUNT_E1_OCCURS_FIRST + direction_counts$EVENTPERIOD_COUNT_E2_OCCURS_FIRST + direction_counts$EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY #We also take into account the number of events on the same day to prevent problem, when 1000 events occur on same day, but 10 times E1 is before E2 and 1 times vice verca and this is significant.
        logger::log_debug('In case group, event1 occurs {direction_counts$COHORT_COUNT_EVENT1_OCCURS_FIRST} times as the first event and {direction_counts$COHORT_COUNT_EVENT2_OCCURS_FIRST} as the second event.')
        logger::log_debug('Both events occur on same day {direction_counts$COHORT_COUNT_EVENT1_EVENT2_OCCUR_ON_SAME_DAY} times.')
        eventperiod_count_event1_occurs_first_for_test=direction_counts$EVENTPERIOD_COUNT_E1_OCCURS_FIRST + round(direction_counts$EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY/2)
        logger::log_debug('If the expected probability of event1 being the first diagnosis is 0.5, what is the probability that we observe event1 as the first event more than {direction_counts$COHORT_COUNT_EVENT1_OCCURS_FIRST}+{direction_counts$COHORT_COUNT_EVENT1_EVENT2_OCCUR_ON_SAME_DAY}/2-1={eventperiod_count_event1_occurs_first_for_test-1} times out of {total_tests} trials?')
        event_pair_pvalue <- pbinom(q = ifelse(eventperiod_count_event1_occurs_first_for_test==0,0,eventperiod_count_event1_occurs_first_for_test-1), size = total_tests, prob = 0.5, lower.tail=FALSE)
        logger::log_debug('Answer: p-val={event_pair_pvalue}')

        # Store the pvalue to database
        RenderedSql <- Trajectories::loadRenderTranslateSql("9PvalInserterDirection.sql",
                                                         packageName=trajectoryAnalysisArgs$packageName,
                                                         dbms=connection@dbms,
                                                         resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                         pval = event_pair_pvalue,
                                                         diag1 = diagnosis1,
                                                         diag2 = diagnosis2,
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
                                                   effectSize = 1.0
  )
  selected_data = DatabaseConnector::querySql(connection, RenderedSql)
  significant_directional_pairs_count=nrow(selected_data)
  logger::log_info(paste0('There are ',significant_directional_pairs_count,' significant directional event pairs having p-val > ',cutoff_pval_direction,'.'))
  logger::log_info('Some of them may have very small effect. Therefore, we extract significant event pairs that have effect size > 1.1')


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
                                                   effectSize = 1.1
  )
  selected_data = DatabaseConnector::querySql(connection, RenderedSql)

  # Write result table into file
  write.table(selected_data, file=eventPairResultsFilename, quote=FALSE, sep='\t', col.names = NA)

  logger::log_info(paste0(nrow(d1d2_data),' event pairs were tested. All test results are written to {d1d2ModelResultsFilename}.'))
  logger::log_info(paste0('Out of ',significant_pairs_count,' significant event pairs, ',significant_directional_pairs_count,' have significant direction. Out of these, ',nrow(selected_data),' have effect size > 1.1.'))
  logger::log_info(paste0('These ',nrow(selected_data),' event pairs were written to ',eventPairResultsFilename))


  # Show some event pair statistics:
  msg=c(paste('EVENT PAIR RESULTS STATS:'),
        paste('========================='),
        paste(format(Sys.time(), '%d %B %Y %H:%M')),
        paste(''),
        paste('Total number of event pairs in initial table:',nrow(dpairs)),
        paste('Total number of event pairs analyzed (having non-zero count):',nrow(dpairs_for_analysis)),
        paste('Bonferroni corrected p-value threshold for association test:',cutoff_pval),
        paste('Bonferroni corrected p-value threshold for directionality test:',cutoff_pval_direction),
        paste('Number of significant event pairs:',significant_pairs_count),
        paste('Number of significant event pairs with significant direction:',significant_directional_pairs_count),
        paste('Number of significant event pairs with significant direction having effect size > 1.1:',nrow(selected_data)),
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

