requireNamespace("SqlRender", quietly = TRUE)
requireNamespace("logger", quietly = TRUE)
requireNamespace("openxlsx", quietly = TRUE)


#' Runs the analysis that detects statistically significant directional event pairs and writes the results to file. Data is taken from database and it is expected that the tables are created by function createEventPairsTable()
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param forceRecalculation Set to TRUE if you wish to recalculate p-values for all pairs again. If it is set to FALSE, it avoids overcalculating p-values for pairs that have been analyzed already.
#' @return
#' @export
#'
#' @examples
runDiscoveryAnalysis<-function(connection,
                               trajectoryAnalysisArgs,
                               trajectoryLocalArgs,
                               forceRecalculation=F) {

  logger::log_info("Begin the analysis of detecting statistically significant directional event pairs...")


  #Delete the old results files if exist
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  allResultsFilenameTsv = file.path(outputFolder,'tables','event_pairs_tested.tsv')
  allResultsFilenameXls = file.path(outputFolder,'tables','event_pairs_tested.xlsx')
  directionalResultsFilenameTsv = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  directionalResultsFilenameXls = file.path(outputFolder,'tables','event_pairs_directional.xlsx')
  RRPvaluePlotFilename=file.path(outputFolder,'figures','RR-pvalue.pdf')
  processDiagramfilename=file.path(outputFolder,'figures','process.pdf')
  if(file.exists(allResultsFilenameTsv)) file.remove(allResultsFilenameTsv)
  if(file.exists(allResultsFilenameXls)) file.remove(allResultsFilenameXls)
  if(file.exists(directionalResultsFilenameTsv)) file.remove(directionalResultsFilenameTsv)
  if(file.exists(RRPvaluePlotFilename)) file.remove(RRPvaluePlotFilename)
  if(file.exists(processDiagramfilename)) file.remove(processDiagramfilename)

  #Set SQL role of the database session
  Trajectories::setRole(connection, trajectoryLocalArgs$sqlRole)

  #Get pairs
  pairs=Trajectories:::getAllPairs(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs)

  logger::log_info("Number of event pairs to analyze: {nrow(pairs)}")
  if(nrow(pairs)==0) {
    logger::log_info("Nothing to analyze. Exiting.")
    return()
  }

  logger::log_info("Matching case and control groups for calculating relative risk (RR), its p-value and power...")
  pairs<-Trajectories:::calcRRandPower(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs,
                                       pairs,
                                       #relativeRiskForPowerCalculations=relativeRiskForPowerCalculations, #get power of detecting RR=10
                                       #powerPvalCutoff=0.05, #no correction here
                                       forceRecalculation = forceRecalculation
  )

  #get Pairs that are having R outside of range RRrangeToSkip and sufficient power for detecting RR=10
  #pairs <- pairs %>% filter((RR < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR >= trajectoryAnalysisArgs$RRrangeToSkip[2]))
  #logger::log_info("Number of event pairs having RR outside of range [{trajectoryAnalysisArgs$RRrangeToSkip[1]},{trajectoryAnalysisArgs$RRrangeToSkip[2]}): {nrow(pairs)}")
  #pairs <- pairs %>% filter(RR_POWER>0.8)
  #logger::log_info("Number of event pairs having RR outside of range [{trajectoryAnalysisArgs$RRrangeToSkip[1]},{trajectoryAnalysisArgs$RRrangeToSkip[2]}) and sufficient power to detect RR=10: {nrow(pairs)}")
  #logger::log_info("Difference of RR from 1.0 of these pairs will be tested.")

  #RR tests
  ##logger::log_info("Running significance tests for RR values (to eliminate such event pairs from the directionality analysis where relative risk is too close to 1)...")
  #pairs<-Trajectories:::runRRTests(connection,
  #                                 trajectoryAnalysisArgs,
  #                                 trajectoryLocalArgs,
  #                                 pairs,
  #                                 forceRecalculation=forceRecalculation)

  #Multple testing correction for p-values of RR
  Trajectories:::adjustPValues(connection,trajectoryLocalArgs,dbcol.pvalue='RR_PVALUE',dbcol.pval.signficiant='RR_SIGNIFICANT')

  #Get pairs & Draw RR-PVALUE plot
  pairs=Trajectories:::getAllPairs(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs)
  Trajectories:::makeRRPvaluePlot(pairs,RRPvaluePlotFilename)

  #get Pairs that are having significant RR
  pairs <- pairs %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*')
  logger::log_info("Number of event pairs having significant RR: {nrow(pairs)}")

  #Pairs that have RR outside skipped range
  pairs <- pairs %>% filter(RR < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR >= trajectoryAnalysisArgs$RRrangeToSkip[2])
  logger::log_info("Out of these, number of event pairs having RR outside of range [{trajectoryAnalysisArgs$RRrangeToSkip[1]},{trajectoryAnalysisArgs$RRrangeToSkip[2]}): {nrow(pairs)}")

  #Run directionality test for pairs having significant RR and RR outside skip-range
  logger::log_info("Running direction tests for {nrow(pairs)} event pairs...")
  pairs<-Trajectories:::runDirectionTests(connection,
                                          trajectoryAnalysisArgs,
                                          trajectoryLocalArgs,
                                          pairs,
                                          forceRecalculation = forceRecalculation)

  #Add labels
  pairs<-Trajectories:::annotateDiscoveryResults(pairs,trajectoryAnalysisArgs=trajectoryAnalysisArgs,verbose=T)

  #Draw process diagram
  Trajectories:::drawProcessGraph(annotated.pairs=pairs,filename=processDiagramfilename,trajectoryAnalysisArgs=trajectoryAnalysisArgs,title='General process flow of the discovery of directional event pairs')

  #write results to file
  write.table(pairs, file=allResultsFilenameTsv, quote=FALSE, sep='\t', col.names = NA)
  openxlsx::write.xlsx(pairs, allResultsFilenameXls)
  logger::log_info('All tested pairs were written to {allResultsFilenameTsv} and {allResultsFilenameXls}.')

  p<-pairs %>% filter(!is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*')
  write.table(p, file=directionalResultsFilenameTsv, quote=FALSE, sep='\t', col.names = NA)
  openxlsx::write.xlsx(p, directionalResultsFilenameXls)
  logger::log_info('All directional pairs were written to {directionalResultsFilenameTsv} and {directionalResultsFilenameXls}')

  # Create validation setup for validating the results in another database
  Trajectories::createValidationSetup(trajectoryAnalysisArgs,
                                      trajectoryLocalArgs)


}


#' Runs the analysis that validates given event pairs and writes the results to file. Data is taken from database and it is expected that the tables are created by function createEventPairsTable()
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param forceRecalculation Set to TRUE if you wish to recalculate p-values for all pairs again. If it is set to FALSE, it avoids overcalculating p-values for pairs that have been analyzed already.
#' @param minRelativeRiskToValidate Relative risk that is used as a minimum threshold for filtering pairs from DISCOVERY STUDY that are being validated
#'
#' @return
#' @export
#'
#' @examples
runValidationAnalysis<-function(connection,
                               trajectoryAnalysisArgs,
                               trajectoryLocalArgs,
                               forceRecalculation=F) {

  logger::log_info("Begin the analysis of validation given directional event pairs...")


  #Delete the old results files if exist
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  allResultsFilenameTsv = file.path(outputFolder,'tables','event_pairs_tested.tsv')
  allResultsFilenameXls = file.path(outputFolder,'tables','event_pairs_tested.xlsx')
  directionalResultsFilenameTsv = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  directionalResultsFilenameXls = file.path(outputFolder,'tables','event_pairs_directional.xlsx')
  RRPvaluePlotFilename=file.path(outputFolder,'figures','RR-pvalue.pdf')
  processDiagramfilename=file.path(outputFolder,'figures','process.pdf')
  if(file.exists(allResultsFilenameTsv)) file.remove(allResultsFilenameTsv)
  if(file.exists(allResultsFilenameXls)) file.remove(allResultsFilenameXls)
  if(file.exists(directionalResultsFilenameTsv)) file.remove(directionalResultsFilenameTsv)
  if(file.exists(RRPvaluePlotFilename)) file.remove(RRPvaluePlotFilename)
  if(file.exists(processDiagramfilename)) file.remove(processDiagramfilename)


  #Set SQL role of the database session
  Trajectories::setRole(connection, trajectoryLocalArgs$sqlRole)

  #clear previous results
  if(forceRecalculation==T) Trajectories:::clearOldResultsFromDb(connection,
                                                                 trajectoryAnalysisArgs,
                                                                 trajectoryLocalArgs)

  #Get pairs
  pairs=Trajectories:::getAllPairs(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs)
  logger::log_info("Number of event pairs to validate: {nrow(pairs)}")
  if(nrow(pairs)==0) {
    logger::log_info("Nothing to analyze. Exiting.")
    return()
  }

  #get pairs that are having RR_IN_PREVIOUS_STUDY outside the range of trajectoryAnalysisArgs$RRrangeToSkip
  pairs <- pairs %>% filter(RR_IN_PREVIOUS_STUDY < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR_IN_PREVIOUS_STUDY >= trajectoryAnalysisArgs$RRrangeToSkip[2])
  logger::log_info("Number of event pairs having RR_IN_PREVIOUS_STUDY outside the range [{trajectoryAnalysisArgs$RRrangeToSkip[1]},{trajectoryAnalysisArgs$RRrangeToSkip[2]}): {nrow(pairs)}")

  #get pairs that have E1_COUNT_IN_EVENTS=0 or E2_COUNT_IN_EVENTS=0
  num.zerocount.events <- nrow(pairs %>% filter(E1_COUNT_IN_EVENTS == 0 | E2_COUNT_IN_EVENTS == 0))
  pairs <- pairs %>% filter(E1_COUNT_IN_EVENTS > 0 & E2_COUNT_IN_EVENTS > 0)
  logger::log_info("From those, the number of event pairs where at least one event never occurs in our data: {num.zerocount.events}")

  #get pairs that have E1_BEFORE_E2_COUNT_IN_EVENTS>0
  num.zerocount.pairs <- nrow(pairs %>% filter(E1_BEFORE_E2_COUNT_IN_EVENTS==0))
  pairs <- pairs %>% filter(E1_BEFORE_E2_COUNT_IN_EVENTS > 0)
  logger::log_info("From the remaining pairs, the number of event pairs that never happen in E1->E2 order in our data (even if such sequences exist, these do not satisfy the analysis requirements for the pairs): {num.zerocount.pairs}")


  logger::log_info("Matching case and control groups for calculating relative risk (RR) and p-value...")
  pairs<-Trajectories:::calcRRandPower(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs,
                                       pairs,
                                       #relativeRiskForPowerCalculations=NA, #the parameter is ignored as RR_IN_PREVIOUS_STUDY is given
                                       #powerPvalCutoff=0.05/nrow(pairs), #Bonferroni correction here
                                       forceRecalculation = forceRecalculation
  )

  #Adjust p-value for multiple testing correction
  Trajectories:::adjustPValues(connection,trajectoryLocalArgs,dbcol.pvalue='RR_PVALUE',dbcol.pval.signficiant='RR_SIGNIFICANT',method='bonferroni') #For validation, we use Bonferroni correction

  #Get pairs & Draw RR-PVALUE plot
  pairs=Trajectories:::getAllPairs(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs)
  Trajectories:::makeRRPvaluePlot(pairs,RRPvaluePlotFilename)

  #get Pairs that are having significant RR
  pairs <- pairs %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*')
  logger::log_info("Number of event pairs having significant RR: {nrow(pairs)}")

  #Pairs that have RR outside skipped range
  pairs <- pairs %>% filter(!is.na(RR) & (RR < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR >= trajectoryAnalysisArgs$RRrangeToSkip[2]))
  logger::log_info("Out of these, number of event pairs having RR outside of range [{trajectoryAnalysisArgs$RRrangeToSkip[1]},{trajectoryAnalysisArgs$RRrangeToSkip[2]}): {nrow(pairs)}")

  #get pairs having significant RR but having the opposite RR direction
  pairs.with.opposite.rr <- nrow(pairs %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*' & sign(1-RR_IN_PREVIOUS_STUDY)!=sign(1-RR)))
  pairs <- pairs %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*' & sign(1-RR_IN_PREVIOUS_STUDY)==sign(1-RR))
  logger::log_info("Number of event pairs having significant RR outside the skip range [{trajectoryAnalysisArgs$RRrangeToSkip[1]},{trajectoryAnalysisArgs$RRrangeToSkip[2]}) but the direction of RR is different from discovery study: {pairs.with.opposite.rr} (these will be skipped from the remaining of the analysis)")

  #Run directionality test for pairs having significant RR and RR outside skip-range
  logger::log_info("Running direction tests for {nrow(pairs)} event pairs...")
  pairs<-Trajectories:::runDirectionTests(connection,
                                          trajectoryAnalysisArgs,
                                          trajectoryLocalArgs,
                                          pairs,
                                          p.value.adjust.method = 'bonferroni',  #For validation, we use Bonferroni correction
                                          forceRecalculation = forceRecalculation)


  #Add labels
  pairs<-Trajectories:::annotateValidationResults(pairs,trajectoryAnalysisArgs=trajectoryAnalysisArgs,verbose=T)

  #Draw process diagram
  Trajectories:::drawProcessGraph(annotated.pairs=pairs,filename=processDiagramfilename,trajectoryAnalysisArgs=trajectoryAnalysisArgs,title='General process flow of the validation of directional event pairs')

  #write results to file
  write.table(pairs, file=allResultsFilenameTsv, quote=FALSE, sep='\t', col.names = NA)
  openxlsx::write.xlsx(pairs, allResultsFilenameXls)
  logger::log_info('All tested pairs were written to {allResultsFilenameTsv} and {allResultsFilenameXls}.')

  p<-pairs %>% filter(!is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*')
  write.table(p, file=directionalResultsFilenameTsv, quote=FALSE, sep='\t', col.names = NA)
  openxlsx::write.xlsx(p, directionalResultsFilenameXls)
  logger::log_info('All directional pairs were written to {directionalResultsFilenameTsv} and {directionalResultsFilenameXls}')


  # Create validation setup for validating the (validation) results in another database
  Trajectories::createValidationSetup(trajectoryAnalysisArgs,
                                      trajectoryLocalArgs)


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



#get p-value of getting that many (or that little) E2 counts in case group if (by null hypothesis) we assume that E2 prevalence in case group is the same as in matched control group
getPValueForAccociation<-function(expected_prob,observation_count,observed_matches) {

  if(observed_matches/observation_count > expected_prob) {

    #if relative risk > 1

    logger::log_debug('Actual prevalence of E2 in (adjusted) control group is {round(expected_prob*100)}% (and in case group {round(observed_matches*100/observation_count)}%). If the expected prevalence of E2 in case group is {round(expected_prob*100)}%, what is the probability that we observe E2 in case group more than {observed_matches-1} (we actually did {observed_matches})?')
    logger::log_debug('If the expected prevalence of event2_concept_id in case group is {round(expected_prob*100)}%, what is the probability that we observe event2_concept_id in case group more than {observed_matches-1}?')
    event_pair_pvalue <- pbinom(q = ifelse(observed_matches==0,0,observed_matches-1), size = observation_count, prob = expected_prob, lower.tail=FALSE)
    return(event_pair_pvalue)

  } else {

    #if relative risk <= 1

    logger::log_debug('Actual prevalence of E2 in (adjusted) control group is {round(expected_prob*100)}% (and in case group {round(observed_matches*100/observation_count)}%). If the expected prevalence of E2 in case group is {round(expected_prob*100)}%, what is the probability that we observe E2 in case group less than {observed_matches} (as we actually did)?')
    logger::log_debug('If the expected prevalence of event2_concept_id in case group is {round(expected_prob*100)}%, what is the probability that we observe event2_concept_id in case group more than {observed_matches}?')
    event_pair_pvalue <- pbinom(q = ifelse(observed_matches==0,0,observed_matches), size = observation_count, prob = expected_prob)
    return(event_pair_pvalue)

  }

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



#Get power: the probability that we detect the association between E1 and E2 in case E1 increases/decreases the risk of getting E2 by 20% (relative risk threshold) when compared to (age-sex matched) general population
# rr.threshold can also be <1 (decreased risk)
getPowerRR<-function(case_group_size,control_group_size,num_observations_in_cases,num_observations_in_controls,rr.threshold=1.2) {

  a=num_observations_in_cases
  b=case_group_size-num_observations_in_cases
  c=control_group_size-num_observations_in_controls
  d=num_observations_in_controls

  a=counts$num_observations_in_cases
  b=counts$case_group_size-num_observations_in_cases
  c=counts$control_group_size-num_observations_in_controls
  d=counts$num_observations_in_controls

  if(b==0 | c==0) {
    power=0
  } else {


    OR=a*d/(b*c)
    #RR=(a/(a+b)) / (c/(c+d))

    #when RR >1, OR is higher than RR
    #when RR <1, OR is lower than RR
    if(rr.threshold>=1) {
      odds_ratio_for_power_calculation=rr.threshold; #actually OR is higher, but for conservative power calculation, we can set them equal
    } else {
      odds_ratio_for_power_calculation=rr.threshold; #actually OR is lower, but as we do not have exact OR data, let it be like this
    }

    r<-epi.sscc(OR = odds_ratio_for_power_calculation,
                p0 = expected_prob, #probability of E2 in control group (is this correct? it should be the probability of EXPOSURE (E1)?)
                n = case_group_size+control_group_size,
                power = NA, #set to NA to calculate power
                r = control_group_size/case_group_size,
                rho.cc = 0,
                design = 1,
                sided.test = 2,
                conf.level = 0.95,
                method = "matched")

    #dat <- matrix(c(a,b,c,d), nrow = 2, byrow = TRUE)
    #rownames(dat) <- c("DF+", "DF-"); colnames(dat) <- c("FUS+", "FUS-"); dat
    #r<-epi.2by2(dat, method = "cohort.count", conf.level = 0.95, units = 100,
    #            interpret = TRUE, outcome = "as.columns")

    power<-r$power
  }


  logger::log_debug('If E1 increased the prevalence of E2 by {round(rr.threshold,3)}x, we would detect it with given case group size n={case_group_size} with probability {round(power*100)}% (=power).')
  return(power)

}

#Get power of direction: the probability that we detect the direction E1->E2 in case E1->E2 is overrepresented by 20%
getPowerDirection<-function(EVENTPERIOD_COUNT_E1_OCCURS_FIRST,EVENTPERIOD_COUNT_E2_OCCURS_FIRST,EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY,rr.threshold=1.2) {

  n=1000

  total_tests=EVENTPERIOD_COUNT_E1_OCCURS_FIRST+EVENTPERIOD_COUNT_E2_OCCURS_FIRST+EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY

  if(total_tests==1) return(0) #replicate() function does not work if total_tests==1 (does not provide matrix as output)

  elevated_signal_prob=rr.threshold/(1+rr.threshold) #if one direction has prevalence 55% (elevated signal prob) and the other 45%, then the signal strength is 20% (45% x 20%)
  if(elevated_signal_prob>1) elevated_signal_prob=1 #probability cant be higher than 1

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
RRandCIDeprecated<-function(cases_with_outcome,cases_total,expected_prob) {

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

RRandCI<-function(num_observations_in_cases,case_group_size,num_observations_in_controls,control_group_size) {
  r<-suppressWarnings(epiR::epi.mh(ev.trt=num_observations_in_cases,
         n.trt=case_group_size,
         ev.ctrl=num_observations_in_controls,
         n.ctrl=control_group_size,
         names=c('1'),
         method = "risk.ratio",
         alternative = "two.sided", conf.level = 0.95))
  return(c(r$RR,r$effect))
}


# Calculates all necessary input values for statistical tests
calcRRandPower<-function(connection,
                            trajectoryAnalysisArgs,
                            trajectoryLocalArgs,
                            pairs,
                            #relativeRiskForPowerCalculations=10, #ignored if RR_IN_PREVIOUS_STUDY is given in the data
                            #powerPvalCutoff=0.05,
                            forceRecalculation=T) {

  num.pairs=nrow(pairs)
  logger::log_info("Calculating relative risk for {num.pairs} event pairs...")

  #Set SQL role of the database session
  Trajectories::setRole(connection, trajectoryLocalArgs$sqlRole)

  if(forceRecalculation==F) {
    pairs <- pairs %>% filter(is.na(RR) | RR==0)
    num.already.calculated=num.pairs-nrow(pairs)
    if(num.already.calculated>0) logger::log_info("For {num.already.calculated} pairs, RR is already calculated. Skipping these from recalculating.")
  } else {
    num.already.calculated=0
  }

  starttime=Sys.time()
  # For each event pair, run the analysis
  if(nrow(pairs)>0) {
    for(i in 1:nrow(pairs))
    {
      diagnosis1        <- pairs[i,'E1_CONCEPT_ID']
      diagnosis2        <- pairs[i,'E2_CONCEPT_ID']
      #observation_count <- pairs[i,'E1_COUNT_AS_FIRST_EVENT_OF_PAIRS'] #case group size - eventperiod count where E1 is not the last event
      #observed_matches  <- pairs[i,'E1_BEFORE_E2_COUNT_IN_EVENTS'] #Number of cases where E1 is followed by E2

      rr_in_previous_study=pairs[i,'RR_IN_PREVIOUS_STUDY']

      logger::log_info(paste0('Matching control group for event pair ',i+num.already.calculated,'/',num.pairs,': ',diagnosis1,' -> ',diagnosis2,' (total progress ',
                              round(100*(i+num.already.calculated)/num.pairs),'%, ETA: ',Trajectories::estimatedTimeRemaining(progress_perc=(i-1)/nrow(pairs),starttime=starttime),
                              ')...'))

      # Do Case/Control matching, get the counts
      counts=Trajectories:::getMatchedCaseControlCounts(connection,trajectoryLocalArgs,diagnosis1,diagnosis2)

      #Get



      #if(counts$control_group_size<counts$case_group_size) {
      #  logger::log_info('Matching failed: not enough event periods without {diagnosis1} for matching all event periods containing {diagnosis1}')
      #  power=NA
      #  event_pair_rr=NA
      #  event_pair_rr_ci_lower=NA
      #  event_pair_rr_ci_upper=NA
      #} else {

        # Calculate power - typically power for detecting RR=... but in case RR_IN_PREVIOUS_STUDY is given, then the power to detect RR=RR_IN_PREVIOUS_STUDY
        #power <- Trajectories:::getPowerRR(case_group_size=counts$case_group_size,
        #                    control_group_size=counts$control_group_size,
        #                    num_observations_in_cases=counts$num_observations_in_cases,
        #                    num_observations_in_controls=counts$num_observations_in_controls,
        #                    rr.threshold=ifelse(is.na(rr_in_previous_study),relativeRiskForPowerCalculations,rr_in_previous_study)
        #)
        power=NA

        #What is the "relative risk" (effect) (how many times the event2_concept_id prevalence in case group is higher than in control group) and its CI
        rr_and_ci=Trajectories:::RRandCI(counts$num_observations_in_cases,
                                         counts$case_group_size,
                                         counts$num_observations_in_controls,
                                         counts$control_group_size)

        event_pair_rr=rr_and_ci$est
        event_pair_rr_ci_lower=rr_and_ci$lower
        event_pair_rr_ci_upper=rr_and_ci$upper
        event_pair_rr_pvalue=rr_and_ci$p.value
        if(!is.na(event_pair_rr)) logger::log_debug("Relative risk {round(event_pair_rr,2)} (95%CI {round(event_pair_rr_ci_lower,2)}..{round(event_pair_rr_ci_upper,2)}, p-value={event_pair_rr_pvalue})")

      #}


      # Writing the results back to database
      RenderedSql <- Trajectories::loadRenderTranslateSql("insertDataForPrefilter.sql",
                                                          packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          control_group_size=counts$control_group_size,
                                                          case_group_size=counts$case_group_size,
                                                          num_observations_in_controls = counts$num_observations_in_controls,
                                                          num_observations_in_cases = counts$num_observations_in_cases,
                                                          rr = ifelse(is.na(event_pair_rr),'NULL',event_pair_rr),
                                                          rr_ci_lower=ifelse(is.na(event_pair_rr_ci_lower),'NULL',event_pair_rr_ci_lower),
                                                          rr_ci_upper=ifelse(is.na(event_pair_rr_ci_upper),'NULL',event_pair_rr_ci_upper),
                                                          rr_pvalue=event_pair_rr_pvalue,
                                                          expected_prob=counts$expected_prob,
                                                          actual_prob=counts$actual_prob,
                                                          diag1 = diagnosis1,
                                                          diag2 = diagnosis2,
                                                          power=ifelse(is.na(power),'NULL',power),
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      #print(power)
      #print(RenderedSql)
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)


      event_pair_pvalue <- Trajectories:::getPValueForAccociation(counts$expected_prob,counts$case_group_size,counts$num_observations_in_cases)

      #if(event_pair_pvalue < cutoff_pval) {
      #  logger::log_debug(paste0('Events in pair ',diagnosis1,' -> ',diagnosis2,' are significantly associated in pre-filtering test.'))
      #  associated_count <- associated_count+1
      #  significant_str="'*'"
      #} else {
      #  logger::log_debug(paste0('Events in pair ',diagnosis1,' -> ',diagnosis2,' are not significantly associated in pre-filtering test.'))
      #  significant_str="''"
      #}

      # Writing p-value to database
      RenderedSql <- Trajectories::loadRenderTranslateSql("PvalInserter.sql",
                                                          packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          pval = event_pair_pvalue,
                                                          pvalSignificant="''",
                                                          diag1 = diagnosis1,
                                                          diag2 = diagnosis2,
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

    } # for
  } else {
    logger::log_info('Nothing to analyze, exit RR calculation function.')
  } #if

  logger::log_info('RR calculations done.')


  #Get updated pairs
  pairs<-Trajectories:::getAllPairs(connection,
                                    trajectoryAnalysisArgs,
                                    trajectoryLocalArgs)
  return(pairs)
}




# Testing whether RR is significantly different from 1 (either lower or higher)
runRRTests<-function(connection,
                              trajectoryAnalysisArgs,
                              trajectoryLocalArgs,
                              pairs,
                              forceRecalculation=F) {

  num.pairs=nrow(pairs)
  logger::log_info("Running RR tests for {num.pairs} event pairs to identify pairs having significant RR...")

  #Set SQL role of the database session
  Trajectories::setRole(connection, trajectoryLocalArgs$sqlRole)

  if(forceRecalculation==F) {
    associated_count <- nrow(pairs %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*'))

    pairs <- pairs %>% filter(is.na(RR_PVALUE))
    num.already.calculated=num.pairs-nrow(pairs)
    if(num.already.calculated>0) logger::log_info("For {num.already.calculated} pairs, RR test is already conducted (out of these, {associated_count} have significant RR). Skipping these from recalculating.")
  } else {
    num.already.calculated=0
    associated_count=0
  }



  starttime=Sys.time()
  if(nrow(pairs)>0) {
    for(i in 1:nrow(pairs)) {

      diagnosis1        <- pairs[i,'E1_CONCEPT_ID']
      diagnosis2        <- pairs[i,'E2_CONCEPT_ID']
      observed_matches  <- pairs[i,'E1_BEFORE_E2_COUNT_IN_EVENTS']
      observation_count <- pairs[i,'E1_COUNT_AS_FIRST_EVENT_OF_PAIRS'] #case group size
      expected_prob     <- pairs[i,'E2_PREVALENCE_IN_CONTROL_GROUP']

      logger::log_info(paste0('Running RR test ',i+num.already.calculated,'/',num.pairs,': ',diagnosis1,' -> ',diagnosis2,' (total progress ',
                              round(100*(i+num.already.calculated)/num.pairs),'%, ETA: ',Trajectories::estimatedTimeRemaining(progress_perc=(i-1)/nrow(pairs),starttime=starttime),
                              ')...'))

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
                                                          packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
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
  } else {
    logger::log_info('Nothing to analyze, exit analysis function.')
  } #if

  logger::log_info('...done.')

  logger::log_info('{associated_count} event pairs out of {num.pairs} passed the statistical tests of RR.')


  #Get updated pairs
  pairs<-Trajectories:::getAllPairs(connection,
                                    trajectoryAnalysisArgs,
                                    trajectoryLocalArgs)
  return(pairs)

}

runDirectionTests<-function(connection,
                              trajectoryAnalysisArgs,
                              trajectoryLocalArgs,
                            pairs,
                            p.value.adjust.method='fdr', #for validation, use 'bonferroni'
                            forceRecalculation=F) {

  num.pairs=nrow(pairs)
  logger::log_info("Running directionality tests for {num.pairs} event pairs to identify pairs where events have significant temporal order...")

  #Set SQL role of the database session
  Trajectories::setRole(connection, trajectoryLocalArgs$sqlRole)

  #cutoff_pval = 0.05/num.pairs
  #logger::log_info(paste0('We use Bonferroni multiple test correction, therefore p-value threshold 0.05/',num.pairs,'=',cutoff_pval,' is used in directionality tests.'))

  if(forceRecalculation==F) {
    directional_count <- nrow(pairs %>% filter(!is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*'))

    pairs <- pairs %>% filter(is.na(DIRECTIONAL_PVALUE))
    num.already.calculated=num.pairs-nrow(pairs)

    if(num.already.calculated>0) logger::log_info("For {num.already.calculated} pairs, RR test is already conducted (out of these, {directional_count} have significant direction). Skipping these from recalculating.")
  } else {
    num.already.calculated=0
    directional_count=0

  }



  starttime=Sys.time()
  if(nrow(pairs)>0) {

    for(i in 1:nrow(pairs)){

      diagnosis1        <- pairs[i,'E1_CONCEPT_ID']
      diagnosis2        <- pairs[i,'E2_CONCEPT_ID']

      logger::log_info(paste0('Running direction test ',i+num.already.calculated,'/',num.pairs,': ',diagnosis1,' -> ',diagnosis2,' (total progress ',
                              round(100*(i+num.already.calculated)/num.pairs),'%, ETA: ',Trajectories::estimatedTimeRemaining(progress_perc=(i-1)/nrow(pairs),starttime=starttime),
                              ')...'))


      #Calculate in database: among people that have event1_concept_id and event2_concept_id pair, how many have date1<date2, date1=date2, date1>date2
      RenderedSql <- Trajectories::loadRenderTranslateSql("7DirectionCounts.sql",
                                                          packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          diag1 = if(is.character(diagnosis1)) {paste0("'",diagnosis1,"'",sep="")} else {diagnosis1}, #if-then hocus-pocus is to handle character-based diagnosis codes when using some non-standard concept codes. Should never happen normally.
                                                          diag2 = if(is.character(diagnosis2)) {paste0("'",diagnosis2,"'",sep="")} else {diagnosis2},  #if-then hocus-pocus is to handle character-based diagnosis codes when using some non-standard concept codes. Should never happen normally.
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

      # Get calculation results from database
      RenderedSql <- Trajectories::loadRenderTranslateSql("8DpairReader.sql",
                                                          packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
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
      event_pair_pvalue = Trajectories:::getPValueForDirection(
        EVENTPERIOD_COUNT_E1_OCCURS_FIRST=direction_counts$E1_BEFORE_E2_COUNT_IN_EVENTS,
        EVENTPERIOD_COUNT_E2_OCCURS_FIRST=direction_counts$E1_AFTER_E2_COUNT_IN_EVENTS,
        EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY=direction_counts$E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS)

      # Calculate pvalue also for case when we "assume" that the events occurring on the same date are OK cases (this is just to understand why we get low pvalue because these events tend to on the same day)
      event_pair_pvalue_same_day_ok = Trajectories:::getPValueForDirection(
        EVENTPERIOD_COUNT_E1_OCCURS_FIRST=direction_counts$E1_BEFORE_E2_COUNT_IN_EVENTS+direction_counts$E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS,
        EVENTPERIOD_COUNT_E2_OCCURS_FIRST=direction_counts$E1_AFTER_E2_COUNT_IN_EVENTS,
        EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY=0)

      #Calculate direction power
      powerDirection = Trajectories:::getPowerDirection(direction_counts$E1_BEFORE_E2_COUNT_IN_EVENTS,
                                                        direction_counts$E1_AFTER_E2_COUNT_IN_EVENTS,
                                                        direction_counts$E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS,
                                                        rr.threshold=1.2)


    #  if (event_pair_pvalue < cutoff_pval){
#
 #       directional_count <- directional_count + 1
  ##     logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is significant.'))

    #  } else {

     #   logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is not significant.'))
      #  significant_str="''"

      #}

      #if (event_pair_pvalue_same_day_ok < cutoff_pval){
#
 #       significant_same_day_str="'*'"
  #      logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' would be significant if in ',direction_counts$E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS,' cases where both events occur on a same day actually had the order ',diagnosis1,' -> ',diagnosis2,' within the same day.'))

   #   } else {

    #    logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' would not be significant even if in ',direction_counts$E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS,' cases where both events occur on a same day actually had the order ',diagnosis1,' -> ',diagnosis2,' within the same day.'))
     #   significant_same_day_str="''"

    #  }


      # Store the pvalue to database
      RenderedSql <- Trajectories::loadRenderTranslateSql("9PvalInserterDirection.sql",
                                                          packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          pval = event_pair_pvalue,
                                                          pvalSignificant="''",
                                                          pvalIfSameDayOK = event_pair_pvalue_same_day_ok,
                                                          significantIfSameDayOK="''",
                                                          diag1 = diagnosis1,
                                                          diag2 = diagnosis2,
                                                          powerDirection=ifelse(is.na(powerDirection),'NULL',powerDirection),
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)


    }
  } else {
    logger::log_info('Nothing to analyze, exit directionality tests calculation function.')
  } #if
  #logger::log_info('Events in {directional_count} event pairs out of {num.pairs} have a significant direction.')

  #update significance p-value
  Trajectories:::adjustPValues(connection,trajectoryLocalArgs,dbcol.pvalue='DIRECTIONAL_PVALUE',dbcol.pval.signficiant='DIRECTIONAL_SIGNIFICANT', method=p.value.adjust.method)
  #Trajectories:::adjustPValues(connection,trajectoryLocalArgs,dbcol.pvalue='DIRECTIONAL_PVALUE_IF_SAME_DAY_EVENTS_ORDERED',dbcol.pval.signficiant='DIRECTIONAL_SIGNIFICANT_IF_SAME_DAY_EVENTS_ORDERED', method=p.value.adjust.method)



  #Get updated pairs
  pairs<-Trajectories:::getAllPairs(connection,
                                    trajectoryAnalysisArgs,
                                    trajectoryLocalArgs)
  return(pairs)


}


annotateDiscoveryResults<-function(pairs,trajectoryAnalysisArgs,verbose=F) {

  pairs$TEXTUAL_RESULT=NA
  pairs$FAILED_FILTER=NA


  pairs <- pairs %>%
    mutate(TEXTUAL_RESULT = case_when(
      is.na(RR_SIGNIFICANT) | RR_SIGNIFICANT==''                                                                              ~ 'RR not significantly different from 1.',
      !is.na(RR) & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2])                 ~ paste0('Not tested (RR in skipped range [',trajectoryAnalysisArgs$RRrangeToSkip[1],',',trajectoryAnalysisArgs$RRrangeToSkip[2],')).'),
      #!is.na(RR_POWER) & RR_POWER<=0.8                                                                                        ~ 'Not tested (low power for detecting RR=10).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' & DIRECTIONAL_SIGNIFICANT_IF_SAME_DAY_EVENTS_ORDERED=='*'  ~ 'Despite having significant RR and having enough power for directionality test, there is no significant E1->E2 order. However, if eventperiods where the events happened on the same day, were considered as directional, the pair would be directionally significant.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT==''                                                           ~ 'Despite having significant RR and having enough power for directionality test, there is no significant E1->E2 order.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*'                                                          ~ 'SUCCESS: Event pair has significant RR and direction.',
      TRUE                                                                                                                    ~  'Other (unkwown situation, not automatically labelled).'
    ))

  pairs <- pairs %>%
    mutate(FAILED_FILTER = case_when(
      is.na(RR_SIGNIFICANT) | RR_SIGNIFICANT==''                                                               ~ '1. Have RR significantly different from 1',
      !is.na(RR) & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2]) ~ paste0('2. Have RR<',trajectoryAnalysisArgs$RRrangeToSkip[1],' or RR>=',trajectoryAnalysisArgs$RRrangeToSkip[2]),
      #!is.na(RR_POWER) & RR_POWER<=0.8                                                                        ~ '2. Have power >80% for detecting RR=10',
      #RR_SIGNIFICANT==''                                                                                      ~ '3. Have RR significantly different from 1',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT==''                                          ~ '3. Have significant E1->E2 order',
      TRUE                                                                                                    ~ ''
    ))


  #show up nicely
  df<-data.frame(res=names(table(pairs$TEXTUAL_RESULT)),
                 count=table(pairs$TEXTUAL_RESULT),
                 perc=round(prop.table(table(pairs$TEXTUAL_RESULT))*100,1)) %>% select(res,count=count.Freq,freq=perc.Freq) %>% arrange(-freq)
  if(verbose) print(df)

#  df<-data.frame(res=names(table(pairs$FAILED_FILTER)),
#                 count=table(pairs$FAILED_FILTER),
#                 perc=round(prop.table(table(pairs$FAILED_FILTER))*100,1)) %>% select(res,count=count.Freq,freq=perc.Freq) %>% arrange(res)
#
#  df

  return(pairs)

}



annotateValidationResults<-function(pairs,trajectoryAnalysisArgs,verbose=F) {

  if(any(!is.na(pairs$RR_IN_PREVIOUS_STUDY) & pairs$RR_IN_PREVIOUS_STUDY>0)) {
    pairs$IS_PREVIOUS_RR_IN_OUR_RR_CI<-NA
    pairs[!is.na(pairs$RR_IN_PREVIOUS_STUDY),'IS_PREVIOUS_RR_IN_OUR_RR_CI']<-ifelse(
      !is.na(pairs$RR_IN_PREVIOUS_STUDY) & pairs$RR_IN_PREVIOUS_STUDY>=pairs$RR_CI_LOWER & pairs$RR_IN_PREVIOUS_STUDY<=pairs$RR_CI_UPPER,
      T,
      F)
  }

  pairs$TEXTUAL_RESULT=NA
  pairs$FAILED_FILTER=NA

  pairs <- pairs %>%
    mutate(TEXTUAL_RESULT = case_when(
      !is.na(RR_IN_PREVIOUS_STUDY)  & (RR_IN_PREVIOUS_STUDY>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR_IN_PREVIOUS_STUDY<trajectoryAnalysisArgs$RRrangeToSkip[2])  ~ paste0('Not tested (RR in previous study in skipped range [',trajectoryAnalysisArgs$RRrangeToSkip[1],',',trajectoryAnalysisArgs$RRrangeToSkip[2],')).'),
      E1_COUNT_IN_EVENTS==0 | E2_COUNT_IN_EVENTS==0                                                                                                                   ~ '1. Count of any of these events is 0.',
      E1_BEFORE_E2_COUNT_IN_EVENTS==0                                                                                                                                 ~ '2. Both events occur but never in given order.',
      RR_SIGNIFICANT==''                                                                                                                                              ~ '3. Validation failed: RR not significantly different from 1.',
      RR_SIGNIFICANT=='*' & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2])                                                ~ paste0('4. Validation failed: Despite having RR significantly different from 1, it\'s within the range [',trajectoryAnalysisArgs$RRrangeToSkip[1],',',trajectoryAnalysisArgs$RRrangeToSkip[2],').'),
      RR_SIGNIFICANT=='*' & sign(RR_IN_PREVIOUS_STUDY-1)!=sign(RR-1)                                                                                                  ~ '5. Validation failed: Despite having RR significantly different from 1, its direction is the opposite to discovery study).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' & DIRECTIONAL_SIGNIFICANT_IF_SAME_DAY_EVENTS_ORDERED=='*'  ~ '6a. Validation failed: Despite having RR significantly different from 1, there is no significant E1->E2 order. However, if eventperiods where the events happened on the same day, were considered as ordered, the pair would become directionally significant.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' & DIRECTIONAL_POWER<=0.8                                                         ~  '6c. Validation failed: Despite having RR significantly different from 1, there is no significant E1->E2 order (also low power for detecting less than 20% elevated order).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' ~ '6b. Validation failed: Despite having RR significantly different from 1, there is no significant E1->E2 order (despite having enough power for detecting 20% elevated E1->E2 order and even when consedered the same day events happening as ordered).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*'                                                          ~ '7. Validation successful: Event pair has RR significantly different from 1 and significant temporal order',
      TRUE                                                                                                                    ~  'Other (unkwown situation, not automatically labelled).'
    ))

  pairs <- pairs %>%
    mutate(FAILED_FILTER = case_when(
      !is.na(RR_IN_PREVIOUS_STUDY)  & (RR_IN_PREVIOUS_STUDY>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR_IN_PREVIOUS_STUDY<trajectoryAnalysisArgs$RRrangeToSkip[2])  ~ paste0('1. Have RR in previous study <',trajectoryAnalysisArgs$RRrangeToSkip[1],' or >=',trajectoryAnalysisArgs$RRrangeToSkip[2]),
      E1_COUNT_IN_EVENTS==0 | E2_COUNT_IN_EVENTS==0 | E1_BEFORE_E2_COUNT_IN_EVENTS==0 ~ '2. Occurs at least once',
      RR_SIGNIFICANT==''                                                                                      ~ '3. Have RR significantly different from 1',
      RR_SIGNIFICANT=='*' & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2]) ~ paste0('4. RR outside the range [',trajectoryAnalysisArgs$RRrangeToSkip[1],',',trajectoryAnalysisArgs$RRrangeToSkip[2],')'),
      RR_SIGNIFICANT=='*' & sign(RR_IN_PREVIOUS_STUDY-1)!=sign(RR-1)                                         ~ '5. Effect direction (increases/decreases risk) matches with discovery study',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT==''                                          ~ '6. Have significant E1->E2 order',
      TRUE                                                                                                    ~ ''
    ))



  #show up nicely
  df<-data.frame(res=names(table(pairs$TEXTUAL_RESULT)),
                 count=table(pairs$TEXTUAL_RESULT),
                 perc=round(prop.table(table(pairs$TEXTUAL_RESULT))*100,1)) %>% select(res,count=count.Freq,freq=perc.Freq) %>% arrange(res)
  if(verbose) {
    logger::log_info('Total number of event pairs validated: {nrow(pairs)}')
    print(df)
  }

  #df<-data.frame(res=names(table(pairs$FAILED_FILTER)),
  #                count=table(pairs$FAILED_FILTER),
  #                perc=round(prop.table(table(pairs$FAILED_FILTER))*100,1)) %>% select(res,count=count.Freq,freq=perc.Freq) %>% arrange(res)
  # df

  return(pairs)

}
getAllPairs<-function(connection,
                      trajectoryAnalysisArgs,
                      trajectoryLocalArgs) {
  RenderedSql <- Trajectories::loadRenderTranslateSql('d1d2_model_reader.sql',
                                                      packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                      dbms=connection@dbms,
                                                      resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                      prefiX =  trajectoryLocalArgs$prefixForResultTableNames
  )
  pairs = DatabaseConnector::querySql(connection, RenderedSql)
  return(pairs)
}

clearOldResultsFromDb<-function(connection,
                      trajectoryAnalysisArgs,
                      trajectoryLocalArgs) {
  RenderedSql <- Trajectories::loadRenderTranslateSql('clear_e1e2_model_results.sql',
                                                      packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                      dbms=connection@dbms,
                                                      resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                      prefiX =  trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection, RenderedSql)
}

getMatchedCaseControlCounts <- function(connection,trajectoryLocalArgs,diagnosis1,diagnosis2) {

  #create table for matching
  RenderedSql <- Trajectories::loadRenderTranslateSql('AssignIndexDatesForMatching.sql',
                                                      packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                      dbms=connection@dbms,
                                                      resultsSchema = trajectoryLocalArgs$resultsSchema,
                                                      prefiX =  trajectoryLocalArgs$prefixForResultTableNames,
                                                      diagnosis1 = diagnosis1
  )
  DatabaseConnector::executeSql(connection, RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

  #read data from the table
  RenderedSql <- SqlRender::render(sql='SELECT * FROM @resultsSchema.@prefiXmatching;',
                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                   prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
  d<-DatabaseConnector::querySql(connection=connection, sql=RenderedSql)

  d<-d %>% Trajectories:::calcPropensityScore()


  matches = d %>%
    Trajectories:::propensityScoreBasedMatch(nn = 1)

  #length(matches$Cases)
  #length(matches$Controls)

  #distribution of propensity scores after matching
  #d %>%
  #  filter(EVENTPERIOD_ID %in% c(matches$Cases, matches$Controls)) %>%
  #  ggplot(aes(x = PropScore, fill = as.factor(IS_CASE))) +
  #  geom_density(aes(y = ..count..), alpha = 0.5) +
  #  theme_bw()




  #get number of E2-s in both groups
  sql=paste0('SELECT
COUNT(DISTINCT m.EVENTPERIOD_ID) as CCC
FROM @resultsSchema.@prefiXpairs p
INNER JOIN @resultsSchema.@prefiXmatching m ON p.EVENTPERIOD_ID=m.EVENTPERIOD_ID
WHERE p.E1_CONCEPT_ID=',diagnosis1,'
  AND p.E2_CONCEPT_ID=',diagnosis2,'
  AND m.IS_CASE=1
  AND DATEDIFF(DAY,m.index_date, p.E2_DATE)>=0') #E2 occurs AFTER index date

  RenderedSql <- SqlRender::render(sql=sql,
                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                   prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
  E2_count_in_cases<-DatabaseConnector::querySql(connection=connection, sql=RenderedSql)$CCC


  #get number of E2-s in both groups
  sql=paste0('SELECT
COUNT(DISTINCT m.EVENTPERIOD_ID) as CCC
FROM @resultsSchema.@prefiXpairs p
INNER JOIN @resultsSchema.@prefiXmatching m ON p.EVENTPERIOD_ID=m.EVENTPERIOD_ID
WHERE p.E1_DATE=m.INDEX_DATE
  AND p.E2_CONCEPT_ID=',diagnosis2,'
  AND p.EVENTPERIOD_ID IN (',paste(matches$Controls,collapse=","),')
  AND DATEDIFF(DAY,m.index_date, p.E2_DATE)>=0') #E2 occurs AFTER index date

  RenderedSql <- SqlRender::render(sql=sql,
                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                   prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
  E2_count_in_controls<-DatabaseConnector::querySql(connection=connection, sql=RenderedSql)$CCC

  if(length(matches$Controls)==0) {
    expected_prob=0
  } else {
    expected_prob=E2_count_in_controls/length(matches$Controls)
  }

  if(length(matches$Cases)==0) {
    actual_prob=0
  } else {
    actual_prob=E2_count_in_cases/length(matches$Cases)
  }

  return(list(
    case_group_size=length(matches$Cases),
    control_group_size=length(matches$Controls),
    num_observations_in_cases=E2_count_in_cases,
    num_observations_in_controls=E2_count_in_controls,
    actual_prob=actual_prob,
    expected_prob=expected_prob
  ))

}

calcPropensityScore<-function(d) {
  #d is a data.frame of the following columns:
  #  eventperiod_id, gender, DateOfBirth, ObservationPeriodStart, ObservationPeriodEnd, e1_date


  #train model
  m = suppressWarnings( glm(IS_CASE ~ GENDER + scale(AGE) + scale(LEN_HISTORY_DAYS) + scale(LEN_FOLLOWUP_DAYS), family = binomial(), data = d) )

  #calculate propensity scores for all
  d = d %>%
    mutate(PropScore = predict(m, type = "response")) %>%
    select(EVENTPERIOD_ID, PropScore, everything()) %>%
    arrange(desc(PropScore))


  #distribution of propensity scores before matching
  #library(ggplot2)
  #d %>%
  #  ggplot(aes(x = PropScore, fill = as.factor(IS_CASE))) +
  #  geom_density(aes(y = ..count..), alpha = 0.5) +
  #  theme_bw()

  return(d)
}

propensityScoreBasedMatch = function(d, nn = 2){
  e = d %>%
    filter(IS_CASE == 1)

  u = d %>%
    filter(IS_CASE ==  0) %>%
    filter(!(EVENTPERIOD_ID %in% e$EVENTPERIOD_ID))

  controls = integer(0)
  for(i in 1:nrow(e)){
    if(nrow(u) >= nn){
      dist = abs(e[i, "PropScore", drop = T] - u$PropScore)
      r = rank(dist, ties.method = "random")

      controls = c(controls, u[r %in% 1:nn, "EVENTPERIOD_ID", drop = T])

      u = u %>%
        filter(!(r %in% 1:nn))
    }
  }

  return(list(Cases = e$EVENTPERIOD_ID, Controls = controls))
}

adjustPValues<-function(connection,trajectoryLocalArgs,dbcol.pvalue='RR_PVALUE',dbcol.pval.signficiant='RR_SIGNIFICANT',method='fdr') {

  logger::log_info('Adjusting p-values by using method={method}...')

  #get p-values
  sql="SELECT E1_CONCEPT_ID, E2_CONCEPT_ID, @dbcol FROM @resultsSchema.@prefiXE1E2_model WHERE @dbcol IS NOT NULL";
  RenderedSql <- SqlRender::render(sql=sql,
                                   dbcol=dbcol.pvalue,
                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                   prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
  d<-DatabaseConnector::querySql(connection=connection, sql=RenderedSql)
  num_values_to_adjust<-nrow(d)

  logger::log_info('  Number of p-values (tests) to adjust: {num_values_to_adjust}...')
  d$adjusted.pvalues=p.adjust(d[[dbcol.pvalue]], method = method)


  sql="UPDATE @resultsSchema.@prefiXE1E2_model SET @dbcol='';";
  RenderedSql <- SqlRender::render(sql=sql,
                                   dbcol=dbcol.pval.signficiant,
                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                   prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
  DatabaseConnector::executeSql(connection, RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

  d<-d %>% filter(adjusted.pvalues<0.05)
  num_sign_values<-nrow(d)

  if(nrow(d)>0) {

    tablename<-paste0(trajectoryLocalArgs$resultsSchema,'.',trajectoryLocalArgs$prefixForResultTableNames,'E1E2_MODEL')
    logger::log_info("  Filling {tablename} with adjusted p-values as 100-size-chunks)...")
    d$sql <- paste0("UPDATE ",
                    tablename,
                    " SET ",
                    dbcol.pval.signficiant,
                    "='*' WHERE E1_CONCEPT_ID=",
                    DBI::dbQuoteString(connection, d %>% mutate(E1_CONCEPT_ID=if_else(is.na(E1_CONCEPT_ID),'NULL',as.character(E1_CONCEPT_ID))) %>% pull(E1_CONCEPT_ID)  ),
                    " AND E2_CONCEPT_ID=",
                    DBI::dbQuoteString(connection, d %>% mutate(E2_CONCEPT_ID=if_else(is.na(E2_CONCEPT_ID),'NULL',as.character(E2_CONCEPT_ID))) %>% pull(E2_CONCEPT_ID)  ),
                    ";")
    #create chunks - in each chunk, 100 inserts
    chunks<-split(d$sql, ceiling(seq_along(d$sql)/100))
    for(i in 1:length(chunks)) {
      #print(paste('i=',i))
      chunk<-chunks[[i]]
      if(length(chunks)>5) logger::log_info(paste0('   ...chunk ',i,'/',length(chunks),'...'))
      insertSql=paste0(chunk,collapse="")
      RenderedSql <- SqlRender::translate(insertSql,targetDialect=attr(connection, "dbms"))
      #print(RenderedSql)
      DatabaseConnector::executeSql(connection, sql=RenderedSql, profile=F, progressBar = F, reportOverallTime = F)
    }
    logger::log_info("  ...done.")
  }

  logger::log_info('...done. Out of {num_sign_values} event pairs, {num_values_to_adjust} were significant after the multiple testing correction.')

}

makeRRPvaluePlot <- function(pairs,filename) {
  pairs_for_plot<-pairs %>% mutate(id = row_number()) %>% select(id, RR, RR_PVALUE, RR_SIGNIFICANT)
  p<-suppressWarnings(ggplot2::ggplot(pairs_for_plot, aes(x=RR,y=RR_PVALUE)) +
    geom_point() +
    geom_text(aes(label=id),hjust=0, vjust=0) +
    annotate("rect", xmin = trajectoryAnalysisArgs$RRrangeToSkip[1], xmax = trajectoryAnalysisArgs$RRrangeToSkip[2], ymin = 0, ymax = max(suppressWarnings(max(pairs_for_plot$RR_PVALUE)),1),
             alpha = .5) +
    annotate("rect", xmin = 0, xmax = max(suppressWarnings(max(pairs_for_plot$RR)),1), ymin = max(suppressWarnings(min(pairs_for_plot %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*') %>% pull(RR_PVALUE))),1), ymax = 1,
             alpha = .5) +
    scale_x_continuous(trans='log10') +
    scale_y_continuous(trans='log10') +
    #scale_y_reverse() +
    annotation_logticks(sides="trbl") +
    labs(title='RR/p-value plot of all tested event pairs. Shaded areas indicate non-significant p-values and RR values in skip range.') +
    theme_bw()
  )
  pdf(filename)
  suppressWarnings(print(p))
  dev.off()

}
