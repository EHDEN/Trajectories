library(SqlRender)
library(logger)

#' Runs the analysis that detects statistically significant directional event pairs and writes the results to file. Data is taken from database and it is expected that the tables are created by function createEventPairsTable()
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param forceRecalculation Set to TRUE if you wish to recalculate p-values for all pairs again. If it is set to FALSE, it avoids overcalculating p-values for pairs that have been analyzed already.
#' @param relativeRiskForPowerCalculations Relative risk that is used for power calculations (what is the power to detect that relative risk in these data). When run in validation mode, this parameter is ignored.
#'
#' @return
#' @export
#'
#' @examples
runEventPairAnalysis<-function(connection,
                               trajectoryAnalysisArgs,
                               trajectoryLocalArgs,
                               forceRecalculation=F,
                               relativeRiskForPowerCalculations=1.2) {

  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  d1d2ModelResultsFilename = file.path(outputFolder,'event_pairs_tested.tsv')
  eventPairResultsFilename = file.path(outputFolder,'event_pairs.tsv')

  logger::log_info(paste0("Detect statistically significant directional event pairs and write the results to ",eventPairResultsFilename,"..."))

  if(Trajectories::IsValidationMode(trajectoryLocalArgs)) logger::log_info('Parameter value for {relativeRiskForPowerCalculations} is ignored in runEventPairAnalysis() method as the package is running in VALIDATION mode and relative risk value in previous study is used instead.')

  #Set SQL role of the database session
  Trajectories::setRole(connection, trajectoryLocalArgs$sqlRole)

  logger::log_info("Matching case and control groups for p-value pre-filtering (most time consuming part, also calculates relative risk)...")
  Trajectories:::prepareEventPairsForStatisticalTesting(connection,
                                                   trajectoryAnalysisArgs,
                                                   trajectoryLocalArgs,
                                                   relativeRiskForPowerCalculations=relativeRiskForPowerCalculations,
                                                   forceRecalculation=forceRecalculation)
  #Association tests
  logger::log_info("Conducting p-value prefiltering. These are basically association tests, but direction is somewhat taken into account - outcome (E2) has to happen AFTER E1 (in case group)...")
  Trajectories:::runPrefilteringTests(connection,
                                  trajectoryAnalysisArgs,
                                  trajectoryLocalArgs,
                                  forceRecalculation=forceRecalculation)

  #Run directionality test for associated pairs
  logger::log_info("Running direction tests for pre-filtered event pairs...")
  Trajectories:::runDirectionTests(connection,
                                     trajectoryAnalysisArgs,
                                     trajectoryLocalArgs)



  # Read in results
  RenderedSql <- Trajectories::loadRenderTranslateSql("11ResultsReader.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=connection@dbms,
                                                   resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                   prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                   rr = 1.0
  )
  selected_data = DatabaseConnector::querySql(connection, RenderedSql)
  significant_directional_pairs_count=nrow(selected_data)
  #logger::log_info(paste0('There are ',significant_directional_pairs_count,' significant directional event pairs with relative risk > 1.'))
  #logger::log_info('Some of them may have very small relative risk (close to 1). Therefore, we extract significant event pairs that have relative risk > 1.1')


  RenderedSql <- Trajectories::loadRenderTranslateSql('d1d2_model_reader.sql',
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames
  )
  d1d2_data = DatabaseConnector::querySql(connection, RenderedSql)
  significant_pairs_count=sum(d1d2_data$EVENT_PAIR_PVALUE_SIGNIFICANT=='*', na.rm=T)
  # Write result table into file
  write.table(d1d2_data, file=d1d2ModelResultsFilename, quote=FALSE, sep='\t', col.names = NA)


  RenderedSql <- Trajectories::loadRenderTranslateSql("11ResultsReader.sql",
                                                   packageName=trajectoryAnalysisArgs$packageName,
                                                   dbms=connection@dbms,
                                                   resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                   prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                   rr = 1.1
  )
  selected_data = DatabaseConnector::querySql(connection, RenderedSql)

  # Write result table into file
  write.table(selected_data, file=eventPairResultsFilename, quote=FALSE, sep='\t', col.names = NA)

  logger::log_info(paste0(nrow(d1d2_data),' event pairs were tested. All test results are written to {d1d2ModelResultsFilename}.'))
  logger::log_info(paste0('Events out of ',significant_pairs_count,' pre-filtered pairs, ',significant_directional_pairs_count,' have also significant direction.'))
  logger::log_info("These directional event pairs are written to {eventPairResultsFilename}.")

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


# Gets relative risk and 95% CI
RRandCI<-function(cases_with_outcome,cases_total,expected_prob) {

  # Is based on https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_confidence_intervals/bs704_confidence_intervals8.html

  n1=cases_total
  x1=cases_with_outcome
  n2=cases_total #control group counts is a buit tricky. We do not use the actual count as the actual count is not matched to case group. Therefore we use matched "expected prevalence" and we use control count = case count
  x2=expected_prob*n2 #Seems that rounding to integer is not necessary
  if(x2>n2) x2=n2

  #Can't calculate if any of the numbers is 0
  if(any(c(x1,n1,x2,n2,expected_prob)==0)) return(c(NA,NA,NA))

  d<-matrix(c(x1,n1-x1,x2,n2-x2),ncol=2,byrow=T)

  rr <- (x1/n1)/expected_prob

  e_for_ln_rr=1.96*sqrt(((n1-x1)/x1/n1)+((n2-x2)/x2/n2))
  #the 95% CI for ln(rr) is ln(rr)+-e_for_ln_rr
  ci_for_ln_rr=c(log(rr)-e_for_ln_rr,log(rr)+e_for_ln_rr)
  #95% CI for rr is exp() of that
  ci_for_rr=exp(ci_for_ln_rr)

  return(c(rr,ci_for_rr[1],ci_for_rr[2]))

}


# Calculates all necessary input values for statistical tests
prepareEventPairsForStatisticalTesting<-function(connection,
                                                 trajectoryAnalysisArgs,
                                                 trajectoryLocalArgs,
                                                 relativeRiskForPowerCalculations=1.2,
                                                 forceRecalculation=F) {

  logger::log_info("Preparing event pairs data for statistical testing (extracting necessary counts, calculating power, etc.)...")

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
    logger::log_info(paste0('There are ',num.dpairs,' event pairs in the original event pairs table. However, ',num.dpairs-num.nonzero.dpairs,' of them have count=0 and are therefore skipped from the remaining analysis.'))
  }

  logger::log_info(paste0('There are ',num.nonzero.dpairs,' event pairs that are going to be analyzed.'))

  if(num.nonzero.dpairs==0) {
    logger::log_info('Nothing to analyze, exit analysis function.')
    return(1);
  }

  #Load all event pairs for the analysis
  # (also) Avoid recalculation if forceRecalculation=T and the data is already there (the analysis has already run up to some point)
  RenderedSql <- Trajectories::loadRenderTranslateSql("GetPairsForCalculation.sql",
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                      forceRecalculation = ifelse(forceRecalculation==T,1,0)
  )
  dpairs_for_analysis = DatabaseConnector::querySql(connection, RenderedSql)

  if(nrow(dpairs_for_analysis)<num.nonzero.dpairs) {
    logger::log_info(paste0("For ",num.nonzero.dpairs-nrow(dpairs_for_analysis))," event pairs, the data are already prepared. They are skipped in this preparation run.")
  }

  starttime=Sys.time()
  # For each event pair, run the analysis
  if(nrow(dpairs_for_analysis)>0) {
    for(i in 1:nrow(dpairs_for_analysis))
    {
      diagnosis1        <- dpairs_for_analysis[i,'E1_CONCEPT_ID']
      diagnosis2        <- dpairs_for_analysis[i,'E2_CONCEPT_ID']
      observation_count <- dpairs_for_analysis[i,'E1_COUNT_AS_FIRST_EVENT'] #case group size - eventperiod count where E1 is not the last event
      observed_matches  <- dpairs_for_analysis[i,'E1_E2_EVENTPERIOD_COUNT'] #Number of cases where E2 is followed by E1

      observation_count2 <- dpairs_for_analysis[i,'E1_COUNT'] #case group size - eventperiods where E1 is present (even if it is the last event)
      observed_matches2  <- dpairs_for_analysis[i,'E1_AND_E2_TOGETHER_COUNT'] #Number of eventperiods where E1 and E2 are both present (random order)

      rr_in_previous_study=dpairs_for_analysis[i,'RR_IN_PREVIOUS_STUDY']

      logger::log_info(paste0('Matching control group for event pair ',i,'/',nrow(dpairs_for_analysis),': ',diagnosis1,' -> ',diagnosis2,' (total progress ',
                              round(100*i/nrow(dpairs_for_analysis)),' ETA: ',Trajectories::estimatedTimeRemaining(progress_perc=(i-1)/nrow(dpairs_for_analysis),starttime=starttime),
                              ')...'))

      # Calculate
      # Expected E2 prevalence in matched control group
      # and
      # power - typically power for detecting RR=1.2 but in case RR_IN_PREVIOUS_STUDY is given, then the power to detect RR=RR_IN_PREVIOUS_STUDY
      z<-Trajectories:::getExpectedPrevalenceAndPower(diagnosis1,
                                                     diagnosis2,
                                                     rr_in_previous_study,
                                                     relativeRiskForPowerCalculations,
                                                     connection,
                                                     trajectoryAnalysisArgs,
                                                     trajectoryLocalArgs)
      expected_prob    <- z[1]
      powerAssociation <- z[2]

      # Approximate E2 count (relative to case group size) in matched control group. It is not used in any calculation, it is only for fast check if the results make sense
      E2_COUNT_IN_CONTROL_GROUP = round(observation_count*expected_prob)
      if(E2_COUNT_IN_CONTROL_GROUP>observation_count) E2_COUNT_IN_CONTROL_GROUP=observation_count

      #What is the "relative risk" (effect) (how many times the event2_concept_id prevalence in case group is higher than in control group) and its CI
      rr_and_ci=Trajectories:::RRandCI(cases_with_outcome=observed_matches,
                                       cases_total=observation_count,
                                       expected_prob=expected_prob)
      event_pair_rr=rr_and_ci[1]
      event_pair_rr_ci_lower=rr_and_ci[2]
      event_pair_rr_ci_upper=rr_and_ci[3]
      if(!is.na(event_pair_rr)) logger::log_debug("Relative risk {round(event_pair_rr,2)} (95%CI {round(event_pair_rr_ci_lower,2)}..{round(event_pair_rr_ci_upper,2)})")

      # Writing the results back to database
      RenderedSql <- Trajectories::loadRenderTranslateSql("insertDataForPrefilter.sql",
                                                          packageName=trajectoryAnalysisArgs$packageName,
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          rr = event_pair_rr,
                                                          rr_ci_lower=event_pair_rr_ci_lower,
                                                          rr_ci_upper=event_pair_rr_ci_upper,
                                                          expected_prob=expected_prob,
                                                          diag1 = diagnosis1,
                                                          diag2 = diagnosis2,
                                                          E2_COUNT_IN_CONTROL_GROUP = E2_COUNT_IN_CONTROL_GROUP,
                                                          power=ifelse(is.na(powerAssociation),'NULL',powerAssociation),
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      #print(power)
      #print(RenderedSql)
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

    } # for
  } #if

  logger::log_info('Pre-calculations done.')
}

getExpectedPrevalenceAndPower<-function(diagnosis1,
                                diagnosis2,
                                rr_in_previous_study,
                                relativeRiskForPowerCalculations,
                                connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs) {
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

  logger::log_debug('Expected prevalence of {diagnosis2} in case group (=people having prior {diagnosis1}) is {round(expected_prob*100,1)}%')

  powerAssociation=Trajectories:::getPowerAssociation(case_control,expected_prob,rr.threshold=ifelse(is.na(rr_in_previous_study),relativeRiskForPowerCalculations,rr_in_previous_study))

  return(c(expected_prob,powerAssociation))
}


# Note that these are not the true association tests as we are looking events in strict E1->E2 order here (for several reasons - first, as we do have the necessary available, second, to limit the analysis for directionality tests).
# Typically one would assume that the direction is not important.
# Therefore - if a pair comes out as not associated from here, it means that these are not associated in some sort of directionality aspect.
runPrefilteringTests<-function(connection,
                              trajectoryAnalysisArgs,
                              trajectoryLocalArgs,
                              forceRecalculation=F) {


  # Typically, we conduct association tests for all pairs that have non-zero count.
  # However, in validation mode, we skip tests for pairs that have power<80%


  # Find out how many tests need to be run in total
  RenderedSql <- Trajectories::loadRenderTranslateSql("GetPairsForAssociationTests.sql",
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                      forceRecalculation=1
  )
  pairs = DatabaseConnector::querySql(connection, RenderedSql)
  num.tests <- nrow(pairs)
  logger::log_info('Number of event pairs with non-zero event count and >80% statistical power for detecting relative risk as large as in previous study: {num.tests}')
  logger::log_info('Total number of pre-filtering tests to conduct: {num.tests}')

  # Determine p-value threshold of Bonferroni correction
  if(num.tests==0) {
    logger::log_info('Nothing to analyze, exit runPrefilteringTests() method.')
    return(1);
  }
  cutoff_pval = 0.05/num.tests
  logger::log_info(paste0('We use Bonferroni multiple test correction, therefore p-value threshold 0.05/',num.tests,'=',cutoff_pval,' is used in pre-filtering analysis.'))

  # if forceRecalculation=F, then some pairs might be tested already
  if(forceRecalculation==F) {
    RenderedSql <- Trajectories::loadRenderTranslateSql("GetPairsForAssociationTests.sql",
                                                        packageName=trajectoryAnalysisArgs$packageName,
                                                        dbms=connection@dbms,
                                                        resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                        prefix =  trajectoryLocalArgs$prefixForResultTableNames,
                                                        forceRecalculation= ifelse(forceRecalculation==T,1,0)
    )
    pairs = DatabaseConnector::querySql(connection, RenderedSql)
    precalculated.num.tests=num.tests-nrow(pairs)
    if(precalculated.num.tests>0) logger::log_info('For {precalculated.num.tests} pairs, pre-filtering tests are already performed. These will be skipped from re-testing.')
    num.tests <- nrow(pairs)
  } else {
    precalculated.num.tests=0
  }

  logger::log_info('Running {num.tests} statistical pre-filtering tests...')
  associated_count=0
  if(num.tests>0) {
    for(i in 1:num.tests){

      diagnosis1        <- pairs[i,'E1_CONCEPT_ID']
      diagnosis2        <- pairs[i,'E2_CONCEPT_ID']
      observed_matches  <- pairs[i,'E1_E2_EVENTPERIOD_COUNT']
      observation_count <- pairs[i,'E1_COUNT_AS_FIRST_EVENT'] #case group size
      expected_prob     <- pairs[i,'E2_PREVALENCE_IN_CONTROL_GROUP']

      logger::log_info('Running pre-filtering test {i}/{num.tests}: {diagnosis1}->{diagnosis2}...')

      event_pair_pvalue <- Trajectories:::getPValueForAccociation(expected_prob,observation_count,observed_matches)

      if(event_pair_pvalue < cutoff_pval) {
        logger::log_debug(paste0('Events in pair ',diagnosis1,' -> ',diagnosis2,' are significantly associated in pre-filtering test.'))
        associated_count <- associated_count+1
        significant_str="'*'"
      } else {
        logger::log_debug(paste0('Events in pair ',diagnosis1,' -> ',diagnosis2,' are not significantly associated in pre-filtering test.'))
        significant_str="''"
      }

      # Writing p-value to database
      RenderedSql <- Trajectories::loadRenderTranslateSql("PvalInserter.sql",
                                                          packageName=trajectoryAnalysisArgs$packageName,
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          pval = event_pair_pvalue,
                                                          pvalSignificant=significant_str,
                                                          diag1 = diagnosis1,
                                                          diag2 = diagnosis2,
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

    }
  }
  logger::log_info('...done.')
  logger::log_info('{associated_count} event pairs out of {num.tests} passed the pre-filtering tests.')

  return()

}

runDirectionTests<-function(connection,
                              trajectoryAnalysisArgs,
                              trajectoryLocalArgs) {

  # Direction tests are conducted for all pairs that are significantly associated
  RenderedSql <- Trajectories::loadRenderTranslateSql("GetPairsForDirectionTests.sql",
                                                      packageName=trajectoryAnalysisArgs$packageName,
                                                      dbms=connection@dbms,
                                                      resultsSchema =  trajectoryLocalArgs$resultsSchema,
                                                      prefix =  trajectoryLocalArgs$prefixForResultTableNames
  )
  pairs = DatabaseConnector::querySql(connection, RenderedSql)
  num.tests <- nrow(pairs)
  logger::log_info('Number of event pairs that are significantly associcated: {num.tests}')
  logger::log_info('Total number of direction tests to conduct: {num.tests}')

  # Determine p-value threshold of Bonferroni correction
  cutoff_pval = 0.05/num.tests
  if(num.tests==0) {
    logger::log_info('Nothing to analyze, exit runDirectionsTests() method.')
    return(1);
  }
  logger::log_info(paste0('We use Bonferroni multiple test correction, therefore p-value threshold 0.05/',num.tests,'=',cutoff_pval,' is used in direction tests.'))

  logger::log_info('Running {num.tests} statistical direction tests...')
  directional_count=0
  if(num.tests>0) {
    for(i in 1:num.tests){

      diagnosis1        <- pairs[i,'E1_CONCEPT_ID']
      diagnosis2        <- pairs[i,'E2_CONCEPT_ID']

      logger::log_info('Running direction test {i}/{num.tests}: {diagnosis1}->{diagnosis2}...')

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

      if (event_pair_pvalue < cutoff_pval){

        directional_count <- directional_count + 1
        significant_str="'*'"
        logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is significant.'))

      } else {

        logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is not significant.'))
        significant_str="''"

      }

      # Store the pvalue to database
      RenderedSql <- Trajectories::loadRenderTranslateSql("9PvalInserterDirection.sql",
                                                          packageName=trajectoryAnalysisArgs$packageName,
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          pval = event_pair_pvalue,
                                                          pvalSignificant=significant_str,
                                                          diag1 = diagnosis1,
                                                          diag2 = diagnosis2,
                                                          powerDirection=ifelse(is.na(powerDirection),'NULL',powerDirection),
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)


    }
  }
  logger::log_info('...done.')
  logger::log_info('Events in {directional_count} event pairs out of {num.tests} have a significant direction.')

  return()


}

