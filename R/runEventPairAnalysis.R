requireNamespace("SqlRender", quietly = TRUE)
requireNamespace("logger", quietly = TRUE)
requireNamespace("openxlsx", quietly = TRUE)


#' Runs the analysis that detects statistically significant directional event pairs and writes the results to file. Data is taken from database and it is expected that the tables are created by function createEventPairsTable()
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param forceRecalculation Set to TRUE if you wish to recalculate p-values for all pairs again. If it is set to FALSE, it avoids overcalculating p-values for pairs that have been analyzed already.
#' @param relativeRiskForPowerCalculations Relative risk that is used for power calculations (what is the power to detect that relative risk in these data).
#'
#' @return
#' @export
#'
#' @examples
runDiscoveryAnalysis<-function(connection,
                               trajectoryAnalysisArgs,
                               trajectoryLocalArgs,
                               forceRecalculation=F,
                               relativeRiskForPowerCalculations=10) {

  logger::log_info("Begin the analysis of detecting statistically significant directional event pairs...")


  #Delete the old results files if exist
  outputFolder<-Trajectories::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  allResultsFilenameTsv = file.path(outputFolder,'tables','event_pairs_tested.tsv')
  allResultsFilenameXsl = file.path(outputFolder,'tables','event_pairs_tested.xlsx')
  directionalResultsFilename = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  processDiagramfilename=file.path(outputFolder,'figures','process.pdf')
  if(file.exists(allResultsFilenameTsv)) file.remove(allResultsFilenameTsv)
  if(file.exists(allResultsFilenameXsl)) file.remove(allResultsFilenameXsl)
  if(file.exists(directionalResultsFilename)) file.remove(directionalResultsFilename)
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

  logger::log_info("Matching case and control groups for calculating relative risk (RR) and power...")
  pairs<-Trajectories:::calcRRandPower(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs,
                                       pairs,
                                       relativeRiskForPowerCalculations=relativeRiskForPowerCalculations, #get power of detecting RR=10
                                       powerPvalCutoff=0.05, #no correction here
                                       forceRecalculation = forceRecalculation
  )

  #get Pairs that are having R ourside of range RRrangeToSkip and sufficient power for detecting RR=10
  pairs <- pairs %>% filter((RR < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR >= trajectoryAnalysisArgs$RRrangeToSkip[2]))
  logger::log_info("Number of event pairs having RR outside of range [{trajectoryAnalysisArgs$RRrangeToSkip[1]},{trajectoryAnalysisArgs$RRrangeToSkip[2]}): {nrow(pairs)}")
  pairs <- pairs %>% filter(RR_POWER>0.8)
  logger::log_info("Number of event pairs having RR outside of range [{trajectoryAnalysisArgs$RRrangeToSkip[1]},{trajectoryAnalysisArgs$RRrangeToSkip[2]}) and sufficient power to detect RR=10: {nrow(pairs)}")
  logger::log_info("Difference of RR from 1.0 of these pairs will be tested.")

  #RR tests
  logger::log_info("Running significance tests for RR values (to eliminate such event pairs from the directionality analysis where relative risk is too close to 1)...")
  pairs<-Trajectories:::runRRTests(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs,
                                   pairs,
                                   forceRecalculation=forceRecalculation)

  #get Pairs that are having significant RR
  pairs <- pairs %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*')
  logger::log_info("Number of event pairs having significant RR: {nrow(pairs)}")

  #Run directionality test for pairs having significant RR
  logger::log_info("Running direction tests for pre-filtered event pairs...")
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
  openxlsx::write.xlsx(pairs, allResultsFilenameXsl)
  logger::log_info('All tested pairs were written to {allResultsFilenameTsv} and {allResultsFilenameXsl}.')

  write.table(pairs %>% filter(!is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*'), file=directionalResultsFilename, quote=FALSE, sep='\t', col.names = NA)
  logger::log_info('All directional pairs were written to {directionalResultsFilename}')

  # Create validation setup for validating the results in anohter database
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
  allResultsFilenameXsl = file.path(outputFolder,'tables','event_pairs_tested.xlsx')
  directionalResultsFilename = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  processDiagramfilename=file.path(outputFolder,'figures','process.pdf')
  if(file.exists(allResultsFilenameTsv)) file.remove(allResultsFilenameTsv)
  if(file.exists(allResultsFilenameXsl)) file.remove(allResultsFilenameXsl)
  if(file.exists(directionalResultsFilename)) file.remove(directionalResultsFilename)
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


  #Calculate Bonferroni-corrected power for these pairs
  logger::log_info("Calculate Bonferroni-corrected power for these pairs (also calculates RR)...")
  pairs<-Trajectories:::calcRRandPower(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs,
                                       pairs,
                                       relativeRiskForPowerCalculations=NA, #the parameter is ignored as RR_IN_PREVIOUS_STUDY is given
                                       powerPvalCutoff=0.05/nrow(pairs), #Bonferroni correction here
                                       forceRecalculation = forceRecalculation
  )

  #get pairs having RR_IN_PREVIOUS_STUDY > minRelativeRiskToValidate AND sufficient power
  pairs.with.power <- nrow(pairs %>% filter(RR_IN_PREVIOUS_STUDY < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR_IN_PREVIOUS_STUDY >= trajectoryAnalysisArgs$RRrangeToSkip[2]) %>% filter(E1_BEFORE_E2_COUNT_IN_EVENTS>0) %>% filter(RR_POWER>0.8))
  logger::log_info("Number of event pairs having RR outside of range [{trajectoryAnalysisArgs$RRrangeToSkip[1]},{trajectoryAnalysisArgs$RRrangeToSkip[2]}), nonzero count, and >80% power for detecting RR as large as in previous study: {pairs.with.power}")
  logger::log_info("(that was just for the information, lack of power does not throw them out from RR testing)")

  #RR tests
  pairs<-pairs %>% filter(RR_IN_PREVIOUS_STUDY < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR_IN_PREVIOUS_STUDY >= trajectoryAnalysisArgs$RRrangeToSkip[2])  %>% filter(E1_BEFORE_E2_COUNT_IN_EVENTS>0)
  logger::log_info("Running {nrow(pairs)} significance tests for RR values (to eliminate such event pairs from the directionality analysis where relative risk is too close to 1)...")
  pairs<-Trajectories:::runRRTests(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs,
                                   pairs,
                                   forceRecalculation=forceRecalculation)

  #get pairs having significant RR
  pairs <- pairs %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*')
  logger::log_info("Number of event pairs having significant RR: {nrow(pairs)}")

  #get pairs having significant RR but having the oppisite RR direction
  pairs.with.opposite.rr <- nrow(pairs %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*' & sign(1-RR_IN_PREVIOUS_STUDY)!=sign(1-RR)))
  pairs <- pairs %>% filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*' & sign(1-RR_IN_PREVIOUS_STUDY)==sign(1-RR))
  logger::log_info("Number of event pairs having significant RR but the direction of RR is different from discovery study: {pairs.with.opposite.rr} (these will be skipped from the remaining of the analysis)")


  #Run directionality test for pairs having significant RR (also calculates power)
  logger::log_info("Running direction tests for pre-filtered event pairs...")
  pairs<-Trajectories:::runDirectionTests(connection,
                                          trajectoryAnalysisArgs,
                                          trajectoryLocalArgs,
                                          pairs,
                                          forceRecalculation = forceRecalculation)



  #Add labels
  pairs<-Trajectories:::annotateValidationResults(pairs,trajectoryAnalysisArgs=trajectoryAnalysisArgs,verbose=T)

  #Draw process diagram
  Trajectories:::drawProcessGraph(annotated.pairs=pairs,filename=processDiagramfilename,trajectoryAnalysisArgs=trajectoryAnalysisArgs,title='General process flow of the validation of directional event pairs')

  #write results to file
  write.table(pairs, file=allResultsFilenameTsv, quote=FALSE, sep='\t', col.names = NA)
  openxlsx::write.xlsx(pairs, allResultsFilenameXsl)
  logger::log_info('All tested pairs were written to {allResultsFilenameTsv} and {allResultsFilenameXsl}.')

  write.table(pairs %>% filter(!is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*'), file=directionalResultsFilename, quote=FALSE, sep='\t', col.names = NA)
  logger::log_info('All directional pairs were written to {directionalResultsFilename}')


  # Create validation setup for validating the (validation) results in anohter database
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
getPowerRR<-function(case_control,expected_prob,rr.threshold=1.2,powerPvalCutoff=0.05) {


  #number of times to sample from general population. n=1000 seems reasonable to get quite accurate estimate
  n=1000
  control_group_size=sum(case_control$CONTROL_COUNT)
  case_group_size= sum(case_control$CASE_COUNT)
  elevated_E2_prevalence=expected_prob*rr.threshold
  if(elevated_E2_prevalence>1) elevated_E2_prevalence=1 #expected prevalence can't be larger than 1


  if(rr.threshold>1) {
    elevated_E2_count_in_control_group <- ceiling(control_group_size*elevated_E2_prevalence)
  } else {
    elevated_E2_count_in_control_group <- floor(control_group_size*elevated_E2_prevalence)
  }
  #check that it is not overfilled
  elevated_E2_count_in_control_group=ifelse(elevated_E2_count_in_control_group>control_group_size,control_group_size,elevated_E2_count_in_control_group)
  #or underfilled
  elevated_E2_count_in_control_group=ifelse(elevated_E2_count_in_control_group<0,0,elevated_E2_count_in_control_group)
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
  #power=sum(p.vals<powerPvalCutoff)/n
  power=sum(p.vals<powerPvalCutoff)/n

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
calcRRandPower<-function(connection,
                            trajectoryAnalysisArgs,
                            trajectoryLocalArgs,
                            pairs,
                            relativeRiskForPowerCalculations=10, #ignored if RR_IN_PREVIOUS_STUDY is given in the data
                            powerPvalCutoff=0.05,
                            forceRecalculation=T) {

  num.pairs=nrow(pairs)
  logger::log_info("Calculating relative risk and power for {num.pairs} event pairs...")

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
      observation_count <- pairs[i,'E1_COUNT_AS_FIRST_EVENT_OF_PAIRS'] #case group size - eventperiod count where E1 is not the last event
      observed_matches  <- pairs[i,'E1_BEFORE_E2_COUNT_IN_EVENTS'] #Number of cases where E2 is followed by E1

      rr_in_previous_study=pairs[i,'RR_IN_PREVIOUS_STUDY']

      logger::log_info(paste0('Matching control group for event pair ',i+num.already.calculated,'/',num.pairs,': ',diagnosis1,' -> ',diagnosis2,' (total progress ',
                              round(100*(i+num.already.calculated)/num.pairs),'%, ETA: ',Trajectories::estimatedTimeRemaining(progress_perc=(i-1)/nrow(pairs),starttime=starttime),
                              ')...'))

      # E2 prevalence in Case group
      actual_prob=observed_matches/observation_count
      if(observation_count==0) actual_prob=0 #should not happen, but just to be sure

      # Calculate
      # Expected E2 prevalence in matched control group
      # and
      # power - typically power for detecting RR=... but in case RR_IN_PREVIOUS_STUDY is given, then the power to detect RR=RR_IN_PREVIOUS_STUDY
      z<-Trajectories:::getExpectedPrevalenceAndPower(diagnosis1,
                                                      diagnosis2,
                                                      rr_in_previous_study,
                                                      relativeRiskForPowerCalculations,
                                                      connection,
                                                      trajectoryAnalysisArgs,
                                                      trajectoryLocalArgs,
                                                      powerPvalCutoff = powerPvalCutoff)
      expected_prob    <- z[1]
      power <- z[2]
      #powerRR_cutoff <- z[3]


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
                                                          packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          rr = event_pair_rr,
                                                          rr_ci_lower=event_pair_rr_ci_lower,
                                                          rr_ci_upper=event_pair_rr_ci_upper,
                                                          expected_prob=expected_prob,
                                                          actual_prob=actual_prob,
                                                          diag1 = diagnosis1,
                                                          diag2 = diagnosis2,
                                                          power=ifelse(is.na(power),'NULL',power),
                                                          prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      #print(power)
      #print(RenderedSql)
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



getExpectedPrevalenceAndPower<-function(diagnosis1,
                                diagnosis2,
                                rr_in_previous_study,
                                relativeRiskForPowerCalculations,
                                connection,
                                trajectoryAnalysisArgs,
                                trajectoryLocalArgs,
                                powerPvalCutoff=0.05) {
  # Extract necessary event1_concept_id->event2_concept_id data from table d1d2_analysistable
  RenderedSql <- Trajectories::loadRenderTranslateSql("5CaseControlStats.sql",
                                                      packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
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

  power=Trajectories:::getPowerRR(case_control,
                                  expected_prob,
                                  rr.threshold=ifelse(is.na(rr_in_previous_study),relativeRiskForPowerCalculations,rr_in_previous_study),
                                  powerPvalCutoff = powerPvalCutoff
                                  )
  return(c(expected_prob,power))
}


# Testing whether RR is significantly different from 1 (either lower or higher)
runRRTests<-function(connection,
                              trajectoryAnalysisArgs,
                              trajectoryLocalArgs,
                              pairs,
                              forceRecalculation=F) {

  num.pairs=nrow(pairs)
  logger::log_info("Running RR tests for {num.pairs} event pairs to identify pairs having significant RR...")

  cutoff_pval = 0.05/nrow(pairs)
  logger::log_info(paste0('We use Bonferroni multiple test correction, therefore p-value threshold 0.05/',nrow(pairs),'=',cutoff_pval,' is used in RR tests.'))

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
                            forceRecalculation=F) {

  num.pairs=nrow(pairs)
  logger::log_info("Running directionality tests for {num.pairs} event pairs to identifypairs that have RR significantly different from 1...")

  #Set SQL role of the database session
  Trajectories::setRole(connection, trajectoryLocalArgs$sqlRole)

  cutoff_pval = 0.05/num.pairs
  logger::log_info(paste0('We use Bonferroni multiple test correction, therefore p-value threshold 0.05/',num.pairs,'=',cutoff_pval,' is used in directionality tests.'))

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

      if (event_pair_pvalue < cutoff_pval){

        directional_count <- directional_count + 1
        significant_str="'*'"
        logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is significant.'))

      } else {

        logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is not significant.'))
        significant_str="''"

      }

      if (event_pair_pvalue_same_day_ok < cutoff_pval){

        significant_same_day_str="'*'"
        logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' would be significant if in ',direction_counts$E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS,' cases where both events occur on a same day actually had the order ',diagnosis1,' -> ',diagnosis2,' within the same day.'))

      } else {

        logger::log_debug(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' would not be significant even if in ',direction_counts$E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS,' cases where both events occur on a same day actually had the order ',diagnosis1,' -> ',diagnosis2,' within the same day.'))
        significant_same_day_str="''"

      }

      # Store the pvalue to database
      RenderedSql <- Trajectories::loadRenderTranslateSql("9PvalInserterDirection.sql",
                                                          packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                          dbms=connection@dbms,
                                                          resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                          pval = event_pair_pvalue,
                                                          pvalSignificant=significant_str,
                                                          significantIfSameDayOK=significant_same_day_str,
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
  logger::log_info('Events in {directional_count} event pairs out of {num.pairs} have a significant direction.')

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
      !is.na(RR) & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2])                 ~ paste0('Not tested (RR in skipped range [',trajectoryAnalysisArgs$RRrangeToSkip[1],',',trajectoryAnalysisArgs$RRrangeToSkip[2],')).'),
      !is.na(RR_POWER) & RR_POWER<=0.8                                                                                        ~ 'Not tested (low power for detecting RR=10).',
      RR_SIGNIFICANT==''                                                                                                      ~ 'RR not significantly different from 1.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' & DIRECTIONAL_SIGNIFICANT_IF_SAME_DAY_EVENTS_ORDERED=='*'  ~ 'Despite having significant RR and having enough power for directionality test, there is no significant E1->E2 order. However, if eventperiods where the events happened on the same day, were considered as directional, the pair would be directionally significant.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT==''                                                           ~ 'Despite having significant RR and having enough power for directionality test, there is no significant E1->E2 order.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*'                                                          ~ 'SUCCESS: Event pair has significant RR and direction.',
      TRUE                                                                                                                    ~  'Other (unkwown situation, not automatically labelled).'
    ))

  pairs <- pairs %>%
    mutate(FAILED_FILTER = case_when(
      !is.na(RR) & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2]) ~ paste0('1. Have RR<',trajectoryAnalysisArgs$RRrangeToSkip[1],' or RR>=',trajectoryAnalysisArgs$RRrangeToSkip[2]),
      !is.na(RR_POWER) & RR_POWER<=0.8                                                                        ~ '2. Have power >80% for detecting RR=10',
      RR_SIGNIFICANT==''                                                                                      ~ '3. Have RR significantly different from 1',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT==''                                          ~ '4. Have significant E1->E2 order',
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
      E1_COUNT_IN_EVENTS==0 | E2_COUNT_IN_EVENTS==0                                                                                                                   ~ '2. Low power (count of any of these events is 0).',
      E1_BEFORE_E2_COUNT_IN_EVENTS==0                                                                                                                                 ~ '3. Low power (both events occur but never in given order).',
      RR_POWER<=0.8 & RR_SIGNIFICANT==''                                                                                                                              ~ '4a. RR not significantly different from 1 (but low power also for detecting RR as close to 1 as in discovery study).',
      RR_SIGNIFICANT==''                                                                                                                                              ~ '4b. Validation failed: RR not significantly different from 1 (despite having enough power for detecting RR as close to 1 as in discovery study).',
      RR_SIGNIFICANT=='*' & sign(RR_IN_PREVIOUS_STUDY-1)!=sign(RR-1)                                                                                                  ~ '4c. Validation failed: Despite having significant RR, its direction is the opposite to discovery study).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' & DIRECTIONAL_SIGNIFICANT_IF_SAME_DAY_EVENTS_ORDERED=='*'  ~ '5a. Validation failed: Despite having significant RR, there is no significant E1->E2 order. However, if eventperiods where the events happened on the same day, were considered as ordered, the pair would become directionally significant.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' & DIRECTIONAL_POWER<=0.8                                                         ~  '5c. Validation failed: Despite having significant RR, there is no significant E1->E2 order (also low power for detecting less than 20% elevated order).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' ~ '5b. Validation failed: Despite having significant RR, there is no significant E1->E2 order (despite having enough power for detecting 20% elevated E1->E2 order and even when consedered the same day events happening as ordered).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*'                                                          ~ '7. Validation successful: Event pair has significant RR and direction.',
      TRUE                                                                                                                    ~  'Other (unkwown situation, not automatically labelled).'
    ))

  pairs <- pairs %>%
    mutate(FAILED_FILTER = case_when(
      !is.na(RR_IN_PREVIOUS_STUDY)  & (RR_IN_PREVIOUS_STUDY>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR_IN_PREVIOUS_STUDY<trajectoryAnalysisArgs$RRrangeToSkip[2])  ~ paste0('1. Have RR in previous study <',trajectoryAnalysisArgs$RRrangeToSkip[1],' or >=',trajectoryAnalysisArgs$RRrangeToSkip[2]),
      E1_COUNT_IN_EVENTS==0 | E2_COUNT_IN_EVENTS==0 | E1_BEFORE_E2_COUNT_IN_EVENTS==0 ~ '2.Occurs at least once',
      RR_SIGNIFICANT==''                                                                                      ~ '3. Have RR significantly different from 1',
      RR_SIGNIFICANT=='*' & sign(RR_IN_PREVIOUS_STUDY-1)!=sign(RR-1)                                         ~ '4. Effect direction (increases/decreases risk) matches with discovery study',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT==''                                          ~ '5. Have significant E1->E2 order',
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
