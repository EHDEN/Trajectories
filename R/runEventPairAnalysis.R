requireNamespace("SqlRender", quietly = TRUE)
requireNamespace("openxlsx", quietly = TRUE)
requireNamespace("dplyr", quietly = TRUE)


#' Runs the analysis that detects statistically significant directional event pairs and writes the results to file. Data is taken from database and it is expected that the tables are created by function createEventPairsTable()
#'
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param trajectoryAnalysisArgs TrajectoryAnalysisArgs object that must be created by createTrajectoryAnalysisArgs() method
#' @param trajectoryLocalArgs TrajectoryLocalArgs object that must be created by createTrajectoryLocalArgs() method
#' @param forceRecalculation Set to TRUE if you wish to recalculate p-values for all pairs again. If it is set to FALSE, it avoids overcalculating p-values for pairs that have been analyzed already.
#' @return
#'
#' @examples
runDiscoveryAnalysis<-function(connection,
                               trajectoryAnalysisArgs,
                               trajectoryLocalArgs,
                               forceRecalculation=F) {

  ParallelLogger::logInfo("Begin the analysis of detecting statistically significant directional event pairs...")


  #Delete the old results files if exist
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  allResultsFilenameTsv = file.path(outputFolder,'tables','event_pairs_tested.tsv')
  allResultsFilenameXls = file.path(outputFolder,'tables','event_pairs_tested.xlsx')
  directionalResultsFilenameTsv = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  directionalResultsFilenameXls = file.path(outputFolder,'tables','event_pairs_directional.xlsx')
  RRPvaluePlotFilename=file.path(outputFolder,'figures','RR-pvalue.pdf')
  processDiagramfilename=file.path(outputFolder,'figures','process.pdf')
  if(file.exists(allResultsFilenameTsv)) file.remove(allResultsFilenameTsv)
  if(file.exists(allResultsFilenameXls)) file.remove(allResultsFilenameXls)
  if(file.exists(directionalResultsFilenameTsv)) file.remove(directionalResultsFilenameTsv)
  if(file.exists(directionalResultsFilenameXls)) file.remove(directionalResultsFilenameXls)
  if(file.exists(RRPvaluePlotFilename)) file.remove(RRPvaluePlotFilename)
  if(file.exists(processDiagramfilename)) file.remove(processDiagramfilename)

  #Set SQL role of the database session
  Trajectories:::setRole(connection, trajectoryLocalArgs$sqlRole)

  #Get pairs
  pairs=Trajectories:::getAllPairs(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs)

  ParallelLogger::logInfo("Number of event pairs to analyze: ",nrow(pairs))
  if(nrow(pairs)==0) {
    ParallelLogger::logInfo("Nothing to analyze. Exiting.")
    return()
  }

  ParallelLogger::logInfo("Matching case and control groups for calculating relative risk (RR) and its p-value...")
  pairs<-Trajectories:::calcRRandPower(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs,
                                       pairs,
                                       #relativeRiskForPowerCalculations=relativeRiskForPowerCalculations, #get power of detecting RR=10
                                       #powerPvalCutoff=0.05, #no correction here
                                       outputFolder,
                                       forceRecalculation = forceRecalculation
  )

  #Multple testing correction for p-values of RR
  Trajectories:::adjustPValues(connection,trajectoryLocalArgs,dbcol.pvalue='RR_PVALUE',dbcol.pval.signficiant='RR_SIGNIFICANT')

  #Get pairs & Draw RR-PVALUE plot
  pairs=Trajectories:::getAllPairs(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs)
  #The following command for makeRRPvaluePlot is commented out as the function does not work properly
  #Trajectories:::makeRRPvaluePlot(pairs,RRPvaluePlotFilename,trajectoryAnalysisArgs)

  #get Pairs that are having significant RR
  pairs <- pairs %>% dplyr::filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*')
  ParallelLogger::logInfo("Number of event pairs having significant RR: ",nrow(pairs))

  #Pairs that have RR outside skipped range
  pairs <- pairs %>% dplyr::filter(RR_CI_UPPER < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR_CI_LOWER >= trajectoryAnalysisArgs$RRrangeToSkip[2])
  ParallelLogger::logInfo("Out of these, number of event pairs having CI of RR outside of range [",trajectoryAnalysisArgs$RRrangeToSkip[1],",",trajectoryAnalysisArgs$RRrangeToSkip[2],"): ",nrow(pairs))

  #Run directionality test for pairs having significant RR and RR outside skip-range
  ParallelLogger::logInfo("Running direction tests for ",nrow(pairs)," event pairs...")
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

  #convert 999-s back to Inf
  pairs$RR <- ifelse(pairs$RR==999,Inf,pairs$RR)
  pairs$RR_CI_LOWER <- ifelse(pairs$RR_CI_LOWER==999,Inf,pairs$RR_CI_LOWER)
  pairs$RR_CI_UPPER <- ifelse(pairs$RR_CI_UPPER==999,Inf,pairs$RR_CI_UPPER)

  write.table(pairs, file=allResultsFilenameTsv, quote=FALSE, sep='\t', col.names = NA)
  openxlsx::write.xlsx(pairs, allResultsFilenameXls, overwrite=T)
  ParallelLogger::logInfo('All tested pairs were written to ',allResultsFilenameTsv,' and ',allResultsFilenameXls,'.')

  p<-pairs %>% dplyr::filter(!is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*')
  write.table(p, file=directionalResultsFilenameTsv, quote=FALSE, sep='\t', col.names = NA)
  openxlsx::write.xlsx(p, directionalResultsFilenameXls, overwrite=T)
  ParallelLogger::logInfo('All directional pairs were written to ',directionalResultsFilenameTsv,' and ',directionalResultsFilenameXls)

  # Create validation setup for validating the results in another database
  Trajectories:::createValidationSetup(trajectoryAnalysisArgs,
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
#'
#' @examples
runValidationAnalysis<-function(connection,
                               trajectoryAnalysisArgs,
                               trajectoryLocalArgs,
                               forceRecalculation=F) {

  ParallelLogger::logInfo("Begin the analysis of validation given directional event pairs...")


  #Delete the old results files if exist
  outputFolder<-Trajectories:::GetOutputFolder(trajectoryLocalArgs,trajectoryAnalysisArgs)
  allResultsFilenameTsv = file.path(outputFolder,'tables','event_pairs_tested.tsv')
  allResultsFilenameXls = file.path(outputFolder,'tables','event_pairs_tested.xlsx')
  directionalResultsFilenameTsv = file.path(outputFolder,'tables','event_pairs_directional.tsv')
  directionalResultsFilenameXls = file.path(outputFolder,'tables','event_pairs_directional.xlsx')
  RRPvaluePlotFilename=file.path(outputFolder,'figures','RR-pvalue.pdf')
  processDiagramfilename=file.path(outputFolder,'figures','process.pdf')
  if(file.exists(allResultsFilenameTsv)) file.remove(allResultsFilenameTsv)
  if(file.exists(allResultsFilenameXls)) file.remove(allResultsFilenameXls)
  if(file.exists(directionalResultsFilenameTsv)) file.remove(directionalResultsFilenameTsv)
  if(file.exists(directionalResultsFilenameXls)) file.remove(directionalResultsFilenameXls)
  if(file.exists(RRPvaluePlotFilename)) file.remove(RRPvaluePlotFilename)
  if(file.exists(processDiagramfilename)) file.remove(processDiagramfilename)


  #Set SQL role of the database session
  Trajectories:::setRole(connection, trajectoryLocalArgs$sqlRole)

  #clear previous results
  if(forceRecalculation==T) Trajectories:::clearOldResultsFromDb(connection,
                                                                 trajectoryAnalysisArgs,
                                                                 trajectoryLocalArgs)

  #Get pairs
  pairs=Trajectories:::getAllPairs(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs)
  ParallelLogger::logInfo("Number of event pairs to validate: ",nrow(pairs))
  if(nrow(pairs)==0) {
    ParallelLogger::logInfo("Nothing to analyze. Exiting.")
    return()
  }

  #get pairs that are having RR_IN_PREVIOUS_STUDY outside the range of trajectoryAnalysisArgs$RRrangeToSkip
  pairs <- pairs %>% dplyr::filter(RR_IN_PREVIOUS_STUDY < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR_IN_PREVIOUS_STUDY >= trajectoryAnalysisArgs$RRrangeToSkip[2])
  ParallelLogger::logInfo("Number of event pairs having RR_IN_PREVIOUS_STUDY outside the range [",trajectoryAnalysisArgs$RRrangeToSkip[1],",",trajectoryAnalysisArgs$RRrangeToSkip[2],"): ",nrow(pairs))

  #get pairs that have E1_COUNT_IN_EVENTS=0 or E2_COUNT_IN_EVENTS=0
  num.zerocount.events <- nrow(pairs %>% dplyr::filter(E1_COUNT_IN_EVENTS == 0 | E2_COUNT_IN_EVENTS == 0))
  pairs <- pairs %>% dplyr::filter(E1_COUNT_IN_EVENTS > 0 & E2_COUNT_IN_EVENTS > 0)
  ParallelLogger::logInfo("From those, the number of event pairs where at least one event never occurs in our data: ",num.zerocount.events)
  forceRecalculation
  #get pairs that have E1_BEFORE_E2_COUNT_IN_EVENTS>0
  num.zerocount.pairs <- nrow(pairs %>% dplyr::filter(E1_BEFORE_E2_COUNT_IN_EVENTS==0))
  pairs <- pairs %>% dplyr::filter(E1_BEFORE_E2_COUNT_IN_EVENTS > 0)
  ParallelLogger::logInfo("From the remaining pairs, the number of event pairs that never happen in E1->E2 order in our data (even if such sequences exist, these do not satisfy the analysis requirements for the pairs): ",num.zerocount.pairs)


  ParallelLogger::logInfo("Matching case and control groups for calculating relative risk (RR) and p-value...")
  pairs<-Trajectories:::calcRRandPower(connection,
                                       trajectoryAnalysisArgs,
                                       trajectoryLocalArgs,
                                       pairs,
                                       #relativeRiskForPowerCalculations=NA, #the parameter is ignored as RR_IN_PREVIOUS_STUDY is given
                                       #powerPvalCutoff=0.05/nrow(pairs), #Bonferroni correction here
                                       outputFolder,
                                       forceRecalculation = forceRecalculation
  )

  #Adjust p-value for multiple testing correction
  Trajectories:::adjustPValues(connection,trajectoryLocalArgs,dbcol.pvalue='RR_PVALUE',dbcol.pval.signficiant='RR_SIGNIFICANT',method='bonferroni') #For validation, we use Bonferroni correction

  #Get pairs & Draw RR-PVALUE plot
  pairs=Trajectories:::getAllPairs(connection,
                                   trajectoryAnalysisArgs,
                                   trajectoryLocalArgs)
  #The following command for makeRRPvaluePlot is commented out as the function does not work properly
  #Trajectories:::makeRRPvaluePlot(pairs,RRPvaluePlotFilename,trajectoryAnalysisArgs)

  #get Pairs that are having significant RR
  pairs <- pairs %>% dplyr::filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*')
  ParallelLogger::logInfo("Number of event pairs having significant RR: ",nrow(pairs))

  #Pairs that have RR outside skipped range
  pairs <- pairs %>% dplyr::filter(!is.na(RR) & (RR_CI_UPPER < trajectoryAnalysisArgs$RRrangeToSkip[1] | RR_CI_LOWER >= trajectoryAnalysisArgs$RRrangeToSkip[2]))
  ParallelLogger::logInfo("Out of these, number of event pairs having RR outside of range [",trajectoryAnalysisArgs$RRrangeToSkip[1],",",trajectoryAnalysisArgs$RRrangeToSkip[2],"): ",nrow(pairs))

  #get pairs having significant RR but having the opposite RR direction
  pairs.with.opposite.rr <- nrow(pairs %>% dplyr::filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*' & sign(1-RR_IN_PREVIOUS_STUDY)!=sign(1-RR)))
  pairs <- pairs %>% dplyr::filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*' & sign(1-RR_IN_PREVIOUS_STUDY)==sign(1-RR))
  ParallelLogger::logInfo("Number of event pairs having significant RR outside the skip range [",trajectoryAnalysisArgs$RRrangeToSkip[1],",",trajectoryAnalysisArgs$RRrangeToSkip[2],") but the direction of RR is different from discovery study: ",pairs.with.opposite.rr," (these will be skipped from the remaining of the analysis)")

  #Run directionality test for pairs having significant RR and RR outside skip-range
  ParallelLogger::logInfo("Running direction tests for ",nrow(pairs)," event pairs...")
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

  #convert 999-s back to Inf
  pairs$RR <- ifelse(pairs$RR==999,Inf,pairs$RR)
  pairs$RR_CI_LOWER <- ifelse(pairs$RR_CI_LOWER==999,Inf,pairs$RR_CI_LOWER)
  pairs$RR_CI_UPPER <- ifelse(pairs$RR_CI_UPPER==999,Inf,pairs$RR_CI_UPPER)

  write.table(pairs, file=allResultsFilenameTsv, quote=FALSE, sep='\t', col.names = NA)
  openxlsx::write.xlsx(pairs, allResultsFilenameXls)
  ParallelLogger::logInfo('All tested pairs were written to ',allResultsFilenameTsv,' and ',allResultsFilenameXls)

  p<-pairs %>% dplyr::filter(!is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*')
  write.table(p, file=directionalResultsFilenameTsv, quote=FALSE, sep='\t', col.names = NA)
  openxlsx::write.xlsx(p, directionalResultsFilenameXls)
  ParallelLogger::logInfo('All directional pairs were written to ',directionalResultsFilenameTsv,' and ',directionalResultsFilenameXls)


  # Create validation setup for validating the (validation) results in another database
  Trajectories:::createValidationSetup(trajectoryAnalysisArgs,
                                      trajectoryLocalArgs)


}






# Functions for statistical power calculation

# Increases E2 counts in control group by 20% (alpha). Returns E2 counts in each age-sex bins
addSignalToBackground <- function(case_control, alpha=0.2){

  expected_prevalence=sum(case_control$group_prob * case_control$match_prob)
  if(expected_prevalence*(1+alpha)>1) {
    ParallelLogger::logInfo('Prevalence in control group is already ',round(expected_prevalence*100),'%, cannot increase it by {',round(alpha*100),'% to calculate power.')
    return(NA)
  }

  extra_total=ceiling(alpha*sum(case_control$CONTROL_D2))

  case_control<-case_control %>%
    dplyr::mutate(CONTROL_D2_ELEVATED=round((1+alpha)*CONTROL_D2)) %>% #note that some bins might be overfilled after this step - need to limit them (next command)
    dplyr::mutate(CONTROL_D2_ELEVATED=ifelse(CONTROL_D2_ELEVATED<=CONTROL_COUNT,CONTROL_D2_ELEVATED,CONTROL_COUNT))

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

  ParallelLogger::logDebug('Increased E2 counts in CONTROL group of case_control dataframe by 20%. Old count: ',sum(case_control$CONTROL_D2),'. New count: ',sum(case_control$CONTROL_D2_ELEVATED),'.')
  return(case_control$CONTROL_D2_ELEVATED)

}



#get p-value of getting that many (or that little) E2 counts in case group if (by null hypothesis) we assume that E2 prevalence in case group is the same as in matched control group
getPValueForAccociation<-function(expected_prob,observation_count,observed_matches) {

  if(observed_matches/observation_count > expected_prob) {

    #if relative risk > 1

    ParallelLogger::logDebug('Actual prevalence of E2 in (adjusted) control group is ',round(expected_prob*100),'% (and in case group ',round(observed_matches*100/observation_count),'%). If the expected prevalence of E2 in case group is ',round(expected_prob*100),'%, what is the probability that we observe E2 in case group more than ',(observed_matches-1),' (we actually did ',observed_matches,')?')
    ParallelLogger::logDebug('If the expected prevalence of event2_concept_id in case group is ',round(expected_prob*100),'%, what is the probability that we observe event2_concept_id in case group more than ',(observed_matches-1),'?')
    event_pair_pvalue <- pbinom(q = ifelse(observed_matches==0,0,observed_matches-1), size = observation_count, prob = expected_prob, lower.tail=FALSE)
    return(event_pair_pvalue)

  } else {

    #if relative risk <= 1

    ParallelLogger::logDebug('Actual prevalence of E2 in (adjusted) control group is ',round(expected_prob*100),'% (and in case group ',round(observed_matches*100/observation_count),'). If the expected prevalence of E2 in case group is ',round(expected_prob*100),'%, what is the probability that we observe E2 in case group less than ',observed_matches,' (as we actually did)?')
    ParallelLogger::logDebug('If the expected prevalence of event2_concept_id in case group is ',round(expected_prob*100),'%, what is the probability that we observe event2_concept_id in case group more than ',observed_matches,'?')
    event_pair_pvalue <- pbinom(q = ifelse(observed_matches==0,0,observed_matches), size = observation_count, prob = expected_prob)
    return(event_pair_pvalue)

  }

}

#pbinom(q = ifelse(eventperiod_count_event1_occurs_first_for_test==0,0,eventperiod_count_event1_occurs_first_for_test-1), size = total_tests, prob = 0.5, lower.tail=FALSE)
getPValueForDirection<-function(EVENTPERIOD_COUNT_E1_OCCURS_FIRST,EVENTPERIOD_COUNT_E2_OCCURS_FIRST,EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY) {
  total_tests = EVENTPERIOD_COUNT_E1_OCCURS_FIRST + EVENTPERIOD_COUNT_E2_OCCURS_FIRST + EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY #We also take into account the number of events on the same day to prevent problem, when 1000 events occur on same day, but 10 times E1 is before E2 and 1 times vice verca and this is significant.
  ParallelLogger::logDebug('In case group, event1 occurs ',EVENTPERIOD_COUNT_E1_OCCURS_FIRST,' times as the first event and ',EVENTPERIOD_COUNT_E2_OCCURS_FIRST,' as the second event.')
  ParallelLogger::logDebug('Both events occur on same day ',EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY,' times.')
  eventperiod_count_event1_occurs_first_for_test=EVENTPERIOD_COUNT_E1_OCCURS_FIRST + round(EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY/2)
  ParallelLogger::logDebug('If the expected probability of event1 being the first diagnosis is 0.5, what is the probability that we observe event1 as the first event more than ',EVENTPERIOD_COUNT_E1_OCCURS_FIRST,'+',EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY,'/2-1=',(eventperiod_count_event1_occurs_first_for_test-1),' times out of ',total_tests,' trials?')
  event_pair_pvalue <- pbinom(q = ifelse(eventperiod_count_event1_occurs_first_for_test==0,0,eventperiod_count_event1_occurs_first_for_test-1), size = total_tests, prob = 0.5, lower.tail=FALSE)
  ParallelLogger::logDebug('Answer: p-val=',event_pair_pvalue)
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


  ParallelLogger::logDebug('If E1 increased the prevalence of E2 by ',round(rr.threshold,3),'x, we would detect it with given case group size n=',case_group_size,' with probability ',round(power*100),'% (=power).')
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

  #Calculate power: how many p-values are below threshold?
  #Note that we do not use corrected threshold here as we select the true threshold AFTER the power analysis. Therefore, conservatively, we use 0.05 here to leave out event pairs that do not exceed the threshold even without correction.
  #power=sum(p.vals<cutoff_pval)/n
  power=sum(p.vals<0.05)/n

  ParallelLogger::logDebug('If direction E1->E2 were overrepresented ',round(rr.threshold,3),'x when compared to E2->E1, we would detect it with probability ',round(power*100),'% (=power)')
  return(power)

}



RRandCI<-function(num_observations_in_cases,case_group_size,num_observations_in_controls,control_group_size) {

  a=num_observations_in_cases
  b=case_group_size
  c=num_observations_in_controls
  d=control_group_size

  if(d==0) {
    res=list(est=NA, lower=0, upper=Inf, pvalue=1)
  } else {

    dat <- matrix(c(a,b-a,c,d-c), nrow = 2, byrow = TRUE)
    rownames(dat) <- c("E1+", "E1-"); colnames(dat) <- c("E2+", "E2-"); dat
    #print(dat)
    r<-suppressWarnings(epitools::riskratio(dat, rev="both")) #rev=both is required here as the riskratio() requires the cells to be in the right irder
    if(b==0) { #case group sizei 0
      RR=Inf
      RR.lower=0
      RR.upper=Inf
      p.val=1
    } else if (c==0) { #num observations in control group is 0
      RR=Inf
      RR.lower=Inf #in epitools, it is NaN, but we change it to Inf as otherwise it would cross 1 (and will be filtered out)
      RR.upper=Inf
      p.val=r$p.value[2,2]
    } else {
      RR=r$measure[2,1]
      RR.lower=r$measure[2,2]
      RR.upper=r$measure[2,3]
      p.val=r$p.value[2,2]
    }
    res=list(est=RR , lower=RR.lower, upper=RR.upper, pvalue=p.val)

  }

  return(res)
}


# Calculates all necessary input values for statistical tests
calcRRandPower<-function(connection,
                            trajectoryAnalysisArgs,
                            trajectoryLocalArgs,
                            pairs,
                            #relativeRiskForPowerCalculations=10, #ignored if RR_IN_PREVIOUS_STUDY is given in the data
                            #powerPvalCutoff=0.05
                            outputFolder,
                            forceRecalculation=T) {

  num.pairs=nrow(pairs)
  ParallelLogger::logInfo('Calculating relative risk for ',num.pairs,' event pairs...')

  #Set SQL role of the database session
  Trajectories:::setRole(connection, trajectoryLocalArgs$sqlRole)

  #Group pairs by E1_CONCEPT_ID (the same case-control group can be used for all of these pairs)
  E1s<-pairs %>%
    dplyr::group_by(E1_CONCEPT_ID) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::arrange(-n)
  num.E1s.total <- nrow(E1s)
  ParallelLogger::logInfo('As there are ',num.E1s.total,' different first events within these pairs, ',num.E1s.total,' case-countrol groups are built in total.')

  if(forceRecalculation==F) {
    #for how many pairs for each E1, the RR is not calculated (but should)
    pair_counts_starting_with_E1_and_having_rr_not_calculated<-pairs %>%
      dplyr::filter(is.na(CASE_GROUP_SIZE) | !is.na(CONTROL_GROUP_SIZE) ) %>% #means that composing a control group has not failed
      dplyr::filter(is.na(RR)) %>%
      dplyr::group_by(E1_CONCEPT_ID) %>%
      dplyr::summarise(n=dplyr::n()) %>%
      dplyr::arrange(-n)

    #for which pairs starting with E1 (and building matched control group has not failed) all RR-s are not FULLY calculated
    E1s_of_pairs_where_rr_not_fully_calculated <- E1s %>%
      dplyr::left_join(pair_counts_starting_with_E1_and_having_rr_not_calculated, by = c("E1_CONCEPT_ID")) %>%
      dplyr::filter(n.y!=0) %>%
      dplyr::select(E1_CONCEPT_ID)
    num.E1s.already.calculated=nrow(E1s)-nrow(E1s_of_pairs_where_rr_not_fully_calculated)
    if(num.E1s.already.calculated>0) ParallelLogger::logInfo('For the pairs of ',num.E1s.already.calculated,' first events, RR is already calculated. Skipping these from recalculation.')

    #Update pairs and E1s
    pairs <- pairs %>%
      dplyr::inner_join(E1s_of_pairs_where_rr_not_fully_calculated, by = c("E1_CONCEPT_ID"))
    E1s <- pairs %>%
      dplyr::group_by(E1_CONCEPT_ID) %>%
      dplyr::summarise(n=dplyr::n()) %>%
      dplyr::arrange(-n)
    num.already.calculated=num.pairs-nrow(pairs)
    if(num.already.calculated>0) ParallelLogger::logInfo('Therefore, for ',num.already.calculated,' pairs, RR is already calculated. Skipping these from recalculation.')
  } else {

    #Clear all previous results from database
    sql='UPDATE @resultsSchema.@prefiXE1E2_model
SET
  RR=NULL,
  RR_CI_LOWER=NULL,
  RR_CI_UPPER=NULL,
  E2_PREVALENCE_IN_CONTROL_GROUP=NULL,
  E2_PREVALENCE_IN_CASE_GROUP=NULL,
  RR_PVALUE=NULL,
  RR_SIGNIFICANT=NULL,
  CONTROL_GROUP_SIZE=NULL,
  CASE_GROUP_SIZE=NULL,
  E2_COUNT_IN_CASE_GROUP=NULL,
  E2_COUNT_IN_CONTROL_GROUP=NULL'

    #read data from the table
    RenderedSql <- SqlRender::render(sql=sql,
                                     resultsSchema = trajectoryLocalArgs$resultsSchema,
                                     prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
    RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection=connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)


    num.E1s.already.calculated=0
    num.already.calculated=0
  }


  starttime=Sys.time()
  counter=1
  # For each E1, create case-control groups
  if(nrow(E1s)>0) {
    for(j in 1:nrow(E1s)) {
      diagnosis1        <- as.data.frame(E1s)[j,'E1_CONCEPT_ID'] # as.data.frame() is used here to get single value instead of 1x1 tibble
      diagnosis1name    <- c(pairs %>% dplyr::filter(E1_CONCEPT_ID==diagnosis1) %>% pull(E1_NAME))[1]

      ParallelLogger::logInfo('Matching control group ',j+num.E1s.already.calculated,'/',num.E1s.total,' for event pairs starting with ',diagnosis1,' (total progress ',
                              round(100*(j+num.E1s.already.calculated)/num.E1s.total),'%, ETA: ',Trajectories:::estimatedTimeRemaining(progress_perc=(j-1)/nrow(E1s),starttime=starttime),
                              ')...')

      # build case-control groups (create some data to database table 'matching' also which will be used by getMatchedCaseControlCounts() function)
      matches <- Trajectories:::buildCaseControlGroups(connection,trajectoryLocalArgs,diagnosis1,diagnosis1name,outputFolder)

      if(length(matches$Controls)==0) {
        #Matching failed

        # Writing the results back to database
        RenderedSql <- Trajectories:::loadRenderTranslateSql("insertFailedMatchingResults.sql",
                                                             packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                             dbms=connection@dbms,
                                                             resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                             case_group_size=length(matches$Cases),
                                                             is_imbalanced=ifelse(matches$IsImbalanced==T,1,0),
                                                             imbalance_comment=ifelse(is.na(matches$ImbalanceComment),'NULL',matches$ImbalanceComment),
                                                             diag1 = diagnosis1,
                                                             prefix =  trajectoryLocalArgs$prefixForResultTableNames
        )
        #print(power)
        #print(RenderedSql)
        DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

      } else {

        # Conduct the test for each pair starting with that E1
        E1.pairs <- pairs %>%
          dplyr::filter(E1_CONCEPT_ID==diagnosis1)


        eventperiod.ids<-data.frame(EVENTPERIOD_ID=c(matches$Cases,matches$Controls))
        Trajectories:::insertTable(connection = connection,
                                       databaseSchema=trajectoryLocalArgs$resultsSchema,
                                       tableName = paste0(trajectoryLocalArgs$prefixForResultTableNames,'matching_ep_ids'),
                                       data = eventperiod.ids,
                                       dropTableIfExists=T,
                                       progressBar = F)





        sql=paste0('

      IF OBJECT_ID(\'@resultsSchema.@prefiXpairs_of_matching\', \'U\') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_of_matching;

      SELECT
        *
      INTO @resultsSchema.@prefiXpairs_of_matching
      FROM
      (SELECT
        m.IS_CASE,
        p.E2_CONCEPT_ID,
        p.EVENTPERIOD_ID
      FROM
      @resultsSchema.@prefiXpairs p
      INNER JOIN @resultsSchema.@prefiXmatching_ep_ids e ON p.EVENTPERIOD_ID=e.EVENTPERIOD_ID
      INNER JOIN @resultsSchema.@prefiXmatching m ON p.EVENTPERIOD_ID=m.EVENTPERIOD_ID
      WHERE
      p.E1_DATE=m.INDEX_DATE -- all pairs where the first event occurs on INDEX date
      AND p.E2_CONCEPT_ID IN (',paste(DBI::dbQuoteString(connection,as.character(E1.pairs %>% dplyr::pull(E2_CONCEPT_ID))),collapse=","),')
      AND DATEDIFF(DAY,m.index_date, p.E2_DATE)>=0 --E2 occurs AFTER index date
      ) a;')



        #read data from the table
        RenderedSql <- SqlRender::render(sql=sql,
                                         resultsSchema = trajectoryLocalArgs$resultsSchema,
                                         prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
        RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
        DatabaseConnector::executeSql(connection=connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)




        sql='SELECT
        E2_CONCEPT_ID,
        IS_CASE,
        COUNT(DISTINCT EVENTPERIOD_ID) AS COUNT
      FROM
        @resultsSchema.@prefiXpairs_of_matching
      GROUP BY
        E2_CONCEPT_ID,
        IS_CASE
      ORDER BY
        E2_CONCEPT_ID, IS_CASE;'

        #read data from the table
        RenderedSql <- SqlRender::render(sql=sql,
                                         resultsSchema = trajectoryLocalArgs$resultsSchema,
                                         prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
        RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
        E2counts.in.groups<-DatabaseConnector::querySql(connection=connection, sql=RenderedSql)


        # For each event pair, run the analysis
        if(nrow(E1.pairs)>0) {
          for(i in 1:nrow(E1.pairs))
          {
            diagnosis2        <- as.data.frame(E1.pairs)[i,'E2_CONCEPT_ID'] # as.data.frame() is used here to get single value instead of 1x1 tibble

            rr_in_previous_study=E1.pairs[i,'RR_IN_PREVIOUS_STUDY']

            ParallelLogger::logInfo('  Calculating RR for event pair ',counter+num.already.calculated,'/',num.pairs,': ',diagnosis1,' -> ',diagnosis2)

            # Do Case/Control matching, get the counts
            r=E2counts.in.groups %>% dplyr::filter(E2_CONCEPT_ID==get('diagnosis2') & IS_CASE==1) %>% dplyr::pull(COUNT)
            matches$E2_count_in_cases=ifelse(length(r)==0,0,r)
            r=E2counts.in.groups %>% dplyr::filter(E2_CONCEPT_ID==get('diagnosis2') & IS_CASE==0) %>% dplyr::pull(COUNT)
            matches$E2_count_in_controls=ifelse(length(r)==0,0,r)
            counts=Trajectories:::getMatchedCaseControlCounts(connection,trajectoryLocalArgs,matches,diagnosis1,diagnosis2)

            #What is the "relative risk" (effect) (how many times the event2_concept_id prevalence in case group is higher than in control group) and its CI
            rr_and_ci=Trajectories:::RRandCI(counts$num_observations_in_cases,
                                             counts$case_group_size,
                                             counts$num_observations_in_controls,
                                             counts$control_group_size)

            event_pair_rr=ifelse(rr_and_ci$est==Inf,999,ifelse(rr_and_ci$est==-Inf,0,rr_and_ci$est))
            event_pair_rr_ci_lower=NA
            event_pair_rr_ci_upper=NA
            event_pair_rr_ci_lower=ifelse(is.nan(rr_and_ci$lower),0,ifelse(rr_and_ci$lower==Inf,999,ifelse(rr_and_ci$lower==-Inf,0,rr_and_ci$lower)))
            event_pair_rr_ci_upper=ifelse(is.nan(rr_and_ci$upper),999,ifelse(rr_and_ci$upper==Inf,999,ifelse(rr_and_ci$upper==-Inf,0,rr_and_ci$upper)))
            event_pair_rr_pvalue=rr_and_ci$pvalue
            if(!is.na(event_pair_rr)) ParallelLogger::logDebug('Relative risk ',round(event_pair_rr,2),' (95%CI ',round(event_pair_rr_ci_lower,2),'..',round(event_pair_rr_ci_upper,2),', p-value=',event_pair_rr_pvalue,')')

            #}


            # Writing the results back to database
            RenderedSql <- Trajectories:::loadRenderTranslateSql("insertDataForPrefilter.sql",
                                                                 packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                                 dbms=connection@dbms,
                                                                 resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                                 control_group_size=counts$control_group_size,
                                                                 is_imbalanced=ifelse(matches$IsImbalanced==T,1,0),
                                                                 imbalance_comment=ifelse(is.na(matches$ImbalanceComment),'NULL',matches$ImbalanceComment),
                                                                 case_group_size=counts$case_group_size,
                                                                 num_observations_in_controls = counts$num_observations_in_controls,
                                                                 num_observations_in_cases = counts$num_observations_in_cases,
                                                                 rr = ifelse(is.na(event_pair_rr),'NULL',event_pair_rr),
                                                                 rr_ci_lower=ifelse(is.na(event_pair_rr_ci_lower),'NULL',event_pair_rr_ci_lower),
                                                                 rr_ci_upper=ifelse(is.na(event_pair_rr_ci_upper),'NULL',event_pair_rr_ci_upper),
                                                                 rr_pvalue=ifelse(is.na(event_pair_rr_pvalue),'NULL',event_pair_rr_pvalue),
                                                                 expected_prob=counts$expected_prob,
                                                                 actual_prob=counts$actual_prob,
                                                                 diag1 = diagnosis1,
                                                                 diag2 = diagnosis2,
                                                                 prefix =  trajectoryLocalArgs$prefixForResultTableNames
            )
            #print(power)
            #print(RenderedSql)
            DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)
            counter=counter+1
          } # for i
        } else {
          ParallelLogger::logInfo('Nothing to analyze.')
        } #if

      } #if length(matches$Controls)==0



    } #for j
  } else {
    ParallelLogger::logInfo('No events to analyze. Exiting RR calculations.')
  } #if


  ParallelLogger::logInfo('RR calculations done.')


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
  ParallelLogger::logInfo('Running directionality tests for ',num.pairs,' event pairs to identify pairs where events have significant temporal order...')

  #Set SQL role of the database session
  Trajectories:::setRole(connection, trajectoryLocalArgs$sqlRole)

  #Group pairs by E1_CONCEPT_ID
  E1s<-pairs %>%
    dplyr::group_by(E1_CONCEPT_ID) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::arrange(-n)
  num.E1s.total <- nrow(E1s)
  ParallelLogger::logInfo('Calculating counts for ',num.E1s.total,' different first events within these pairs.')

  if(forceRecalculation==F) {
    #for how many pairs for each E1, the directionality counts are not calculated (but should)
    pair_counts_starting_with_E1_and_having_counts_not_calculated<-pairs %>%
      dplyr::filter(is.na(E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS)) %>%
      dplyr::group_by(E1_CONCEPT_ID) %>%
      dplyr::summarise(n=dplyr::n()) %>%
      dplyr::arrange(-n)

    #for which pairs starting with E1 all counts are not FULLY calculated
    E1s_of_pairs_where_counts_not_fully_calculated <- E1s %>%
      dplyr::left_join(pair_counts_starting_with_E1_and_having_counts_not_calculated, by = c("E1_CONCEPT_ID")) %>%
      dplyr::filter(n.y!=0) %>%
      dplyr::select(E1_CONCEPT_ID)
    num.E1s.already.calculated=nrow(E1s)-nrow(E1s_of_pairs_where_counts_not_fully_calculated)
    if(num.E1s.already.calculated>0) ParallelLogger::logInfo('For the pairs of ',num.E1s.already.calculated,' first events, counts for directionality tests are already calculated. Skipping these from recalculation.')

    #Update pairs and E1s
    pairs <- pairs %>%
      dplyr::inner_join(E1s_of_pairs_where_counts_not_fully_calculated, by = c("E1_CONCEPT_ID"))
    E1s <- pairs %>%
      dplyr::group_by(E1_CONCEPT_ID) %>%
      dplyr::summarise(n=dplyr::n()) %>%
      dplyr::arrange(-n)
    num.already.calculated=num.pairs-nrow(pairs)
    if(num.already.calculated>0) ParallelLogger::logInfo('Therefore, for ',num.already.calculated,' pairs, counts are already calculated. Skipping these from recalculation.')
  } else {

    #Clear all previous results from database
    sql='UPDATE @resultsSchema.@prefiXE1E2_model
SET
  E1_BEFORE_E2_COUNT_IN_EVENTS=NULL,
  E1_AFTER_E2_COUNT_IN_EVENTS=NULL,
  E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS=NULL,
  DIRECTIONAL_PVALUE=NULL,
  DIRECTIONAL_SIGNIFICANT=NULL,
  DIRECTIONAL_PVALUE_IF_SAME_DAY_EVENTS_ORDERED=NULL,
  DIRECTIONAL_SIGNIFICANT_IF_SAME_DAY_EVENTS_ORDERED=NULL,
  DIRECTIONAL_POWER=NULL'

    #read data from the table
    RenderedSql <- SqlRender::render(sql=sql,
                                     resultsSchema = trajectoryLocalArgs$resultsSchema,
                                     prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
    RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection=connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)


    num.E1s.already.calculated=0
    num.already.calculated=0
  }


  starttime=Sys.time()
  counter=1
  # For each E1, create case-control groups
  if(nrow(E1s)>0) {
    for(j in 1:nrow(E1s)) {
      diagnosis1        <- as.data.frame(E1s)[j,'E1_CONCEPT_ID'] # as.data.frame() is used here to get single value instead of 1x1 tibble
      diagnosis1name    <- c(pairs %>% dplyr::filter(E1_CONCEPT_ID==diagnosis1) %>% pull(E1_NAME))[1]

      ParallelLogger::logInfo('Getting counts ',j+num.E1s.already.calculated,'/',num.E1s.total,' for event pairs starting with ',diagnosis1,' (total progress ',
                              round(100*(j+num.E1s.already.calculated)/num.E1s.total),'%, ETA: ',Trajectories:::estimatedTimeRemaining(progress_perc=(j-1)/nrow(E1s),starttime=starttime),
                              ')...')

      #Step 1. Get eventperiods that contain E1 (get also E1 dates) - put this to db as we use it for all pairs starting with the same E1.
      RenderedSql <- Trajectories:::loadRenderTranslateSql("7DirectionCounts-1.sql",
                                                           packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                           dbms=connection@dbms,
                                                           resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                           diag1 = if(is.character(diagnosis1)) {paste0("'",diagnosis1,"'",sep="")} else {diagnosis1}, #if-then hocus-pocus is to handle character-based diagnosis codes when using some non-standard concept codes. Should never happen normally.
                                                           prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

      #Step 2. Prepare a temporary table of all E2 events for that E1
      E1.pairs <- pairs %>% dplyr::filter(E1_CONCEPT_ID==get('diagnosis1')) %>% dplyr::arrange(E2_CONCEPT_ID)

      E2.concept.ids<-data.frame(CONCEPT_ID=c(E1.pairs %>% dplyr::pull(E2_CONCEPT_ID)))
      Trajectories:::insertTable(connection = connection,
                                 databaseSchema=trajectoryLocalArgs$resultsSchema,
                                 tableName = paste0(trajectoryLocalArgs$prefixForResultTableNames,'temp1_for_direction_counts'),
                                 data = E2.concept.ids,
                                 dropTableIfExists=T)
      RenderedSql <- Trajectories:::loadRenderTranslateSql("7DirectionCounts-2.sql",
                                                           packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                           dbms=connection@dbms,
                                                           resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                           prefix =  trajectoryLocalArgs$prefixForResultTableNames
      )
      DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

      #Step 3. For each event pair starting with E1, calculate the counts
      if(nrow(E1.pairs)>0) {
        for(i in 1:nrow(E1.pairs))
        {
          diagnosis2        <- as.data.frame(E1.pairs)[i,'E2_CONCEPT_ID'] # as.data.frame() is used here to get single value instead of 1x1 tibble

          ParallelLogger::logInfo('  Calculating direction counts ',counter+num.already.calculated,'/',num.pairs,': ',diagnosis1,' -> ',diagnosis2)

          #Calculate in database: among people that have event1_concept_id and event2_concept_id pair, how many have date1<date2, date1=date2, date1>date2
          RenderedSql <- Trajectories:::loadRenderTranslateSql("7DirectionCounts-3.sql",
                                                               packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                               dbms=connection@dbms,
                                                               resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                               diag1 = if(is.character(diagnosis1)) {paste0("'",diagnosis1,"'",sep="")} else {diagnosis1},  #if-then hocus-pocus is to handle character-based diagnosis codes when using some non-standard concept codes. Should never happen normally.
                                                               diag2 = if(is.character(diagnosis2)) {paste0("'",diagnosis2,"'",sep="")} else {diagnosis2},  #if-then hocus-pocus is to handle character-based diagnosis codes when using some non-standard concept codes. Should never happen normally.
                                                               prefix =  trajectoryLocalArgs$prefixForResultTableNames
          )
          DatabaseConnector::executeSql(connection, sql=RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

          counter=counter+1

        } #for i
      } #if nrow(E1.pairs)>0

    } #for j
  } #if (nrow(E1s)>0



  #By now, all counts are be calculated in Database and we can run the actual tests.

  # We load all pairs (and their counts) from database but we look at the pairs only that have the counts calculated
  pairs <- Trajectories:::getAllPairs(connection,
                                            trajectoryAnalysisArgs,
                                            trajectoryLocalArgs)
  pairs <- pairs %>% dplyr::filter(!is.na(E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS))
  num.pairs<-nrow(pairs)
  ParallelLogger::logInfo('Running statistical direction tests for ',num.pairs,' event pairs...')

  if(forceRecalculation==F) {
    #Update pairs (get pairs that have no DIRECTIONAL_PVALUE value)
    pairs <- pairs %>% dplyr::filter(is.na(DIRECTIONAL_PVALUE))
    num.already.calculated=num.pairs-nrow(pairs)
    if(num.already.calculated>0) ParallelLogger::logInfo('For ',num.already.calculated,' pairs, statistical directionality tests are already conducted. Skipping these from recalculation.')
  } else {
    num.already.calculated=0
  }

  starttime=Sys.time()
  if(nrow(pairs)>0) {

    for(i in 1:nrow(pairs)){

      diagnosis1        <- pairs[i,'E1_CONCEPT_ID']
      diagnosis2        <- pairs[i,'E2_CONCEPT_ID']

      ParallelLogger::logInfo('Running direction test ',i+num.already.calculated,'/',num.pairs,': ',diagnosis1,' -> ',diagnosis2,' (total progress ',
                              round(100*(i+num.already.calculated)/num.pairs),'%, ETA: ',Trajectories:::estimatedTimeRemaining(progress_perc=(i-1)/nrow(pairs),starttime=starttime),
                              ')...')

      direction_counts <- pairs[i,]

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


      # Store the pvalue to database
      RenderedSql <- Trajectories:::loadRenderTranslateSql("9PvalInserterDirection.sql",
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
    ParallelLogger::logInfo('Nothing to analyze, exit directionality tests calculation function.')
  } #if

  #update significance p-value
  Trajectories:::adjustPValues(connection,trajectoryLocalArgs,dbcol.pvalue='DIRECTIONAL_PVALUE',dbcol.pval.signficiant='DIRECTIONAL_SIGNIFICANT', method=p.value.adjust.method)


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
    dplyr::mutate(TEXTUAL_RESULT = dplyr::case_when(
      is.na(E2_COUNT_IN_CASE_GROUP)                                                                                           ~ 'Failed to build balanced matching case/control groups for RR calculation (groups too different)',
      is.na(RR_SIGNIFICANT) | RR_SIGNIFICANT==''                                                                              ~ 'RR not significantly different from 1.',
      !is.na(RR) & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2])                 ~ paste0('Not tested (RR in skipped range [',trajectoryAnalysisArgs$RRrangeToSkip[1],',',trajectoryAnalysisArgs$RRrangeToSkip[2],')).'),
      #!is.na(RR_POWER) & RR_POWER<=0.8                                                                                        ~ 'Not tested (low power for detecting RR=10).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' & DIRECTIONAL_SIGNIFICANT_IF_SAME_DAY_EVENTS_ORDERED=='*'  ~ 'Despite having significant RR and having enough power for directionality test, there is no significant E1->E2 order. However, if eventperiods where the events happened on the same day, were considered as directional, the pair would be directionally significant.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT==''                                                           ~ 'Despite having significant RR and having enough power for directionality test, there is no significant E1->E2 order.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*'                                                          ~ 'SUCCESS: Event pair has significant RR and direction.',
      TRUE                                                                                                                    ~  'Other (unkwown situation, not automatically labelled).'
    ))

  pairs <- pairs %>%
    dplyr::mutate(FAILED_FILTER = dplyr::case_when(
      is.na(E2_COUNT_IN_CASE_GROUP)                                                                            ~ '1. Balanced matched control group composed',
      is.na(RR_SIGNIFICANT) | RR_SIGNIFICANT==''                                                               ~ '2. Have RR significantly different from 1',
      !is.na(RR) & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2]) ~ paste0('3. Have RR<',trajectoryAnalysisArgs$RRrangeToSkip[1],' or RR>=',trajectoryAnalysisArgs$RRrangeToSkip[2]),
      #!is.na(RR_POWER) & RR_POWER<=0.8                                                                        ~ '3. Have power >80% for detecting RR=10',
      #RR_SIGNIFICANT==''                                                                                      ~ '4. Have RR significantly different from 1',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT==''                                          ~ '5. Have significant E1->E2 order',
      TRUE                                                                                                    ~ ''
    ))


  #show up nicely
  df<-data.frame(res=names(table(pairs$TEXTUAL_RESULT)),
                 count=table(pairs$TEXTUAL_RESULT),
                 perc=round(prop.table(table(pairs$TEXTUAL_RESULT))*100,1)) %>% dplyr::select(res,count=count.Freq,freq=perc.Freq) %>% dplyr::arrange(-freq)
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
    dplyr::mutate(TEXTUAL_RESULT = dplyr::case_when(
      !is.na(RR_IN_PREVIOUS_STUDY)  & (RR_IN_PREVIOUS_STUDY>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR_IN_PREVIOUS_STUDY<trajectoryAnalysisArgs$RRrangeToSkip[2])  ~ paste0('Not tested (RR in previous study in skipped range [',trajectoryAnalysisArgs$RRrangeToSkip[1],',',trajectoryAnalysisArgs$RRrangeToSkip[2],')).'),
      E1_COUNT_IN_EVENTS==0 | E2_COUNT_IN_EVENTS==0                                                                                                                   ~ '1. Count of any of these events is 0.',
      E1_BEFORE_E2_COUNT_IN_EVENTS==0                                                                                                                                 ~ '2. Both events occur but never in given order.',
      is.na(E2_COUNT_IN_CASE_GROUP)                                                                                           ~ '3.Failed to build balanced matching case/control groups for RR calculation (groups too different)',
      is.na(RR_SIGNIFICANT) | RR_SIGNIFICANT==''                                                                              ~ '4. Have RR significantly different from 1',
      RR_SIGNIFICANT==''                                                                                                                                              ~ '5. Validation failed: RR not significantly different from 1.',
      RR_SIGNIFICANT=='*' & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2])                                                ~ paste0('6. Validation failed: Despite having RR significantly different from 1, it\'s within the range [',trajectoryAnalysisArgs$RRrangeToSkip[1],',',trajectoryAnalysisArgs$RRrangeToSkip[2],').'),
      RR_SIGNIFICANT=='*' & sign(RR_IN_PREVIOUS_STUDY-1)!=sign(RR-1)                                                                                                  ~ '7. Validation failed: Despite having RR significantly different from 1, its direction is the opposite to discovery study).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' & DIRECTIONAL_SIGNIFICANT_IF_SAME_DAY_EVENTS_ORDERED=='*'  ~ '8a. Validation failed: Despite having RR significantly different from 1, there is no significant E1->E2 order. However, if eventperiods where the events happened on the same day, were considered as ordered, the pair would become directionally significant.',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' & DIRECTIONAL_POWER<=0.8                                                         ~  '8c. Validation failed: Despite having RR significantly different from 1, there is no significant E1->E2 order (also low power for detecting less than 20% elevated order).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='' ~ '8b. Validation failed: Despite having RR significantly different from 1, there is no significant E1->E2 order (despite having enough power for detecting 20% elevated E1->E2 order and even when considered the same day events happening as ordered).',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT=='*'                                                          ~ '9. Validation successful: Event pair has RR significantly different from 1 and significant temporal order',
      TRUE                                                                                                                    ~  'Other (unkwown situation, not automatically labelled).'
    ))

  pairs <- pairs %>%
    dplyr::mutate(FAILED_FILTER = dplyr::case_when(
      !is.na(RR_IN_PREVIOUS_STUDY)  & (RR_IN_PREVIOUS_STUDY>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR_IN_PREVIOUS_STUDY<trajectoryAnalysisArgs$RRrangeToSkip[2])  ~ paste0('1. Have RR in previous study <',trajectoryAnalysisArgs$RRrangeToSkip[1],' or >=',trajectoryAnalysisArgs$RRrangeToSkip[2]),
      E1_COUNT_IN_EVENTS==0 | E2_COUNT_IN_EVENTS==0 | E1_BEFORE_E2_COUNT_IN_EVENTS==0 ~ '2. Occurs at least once',
      is.na(E2_COUNT_IN_CASE_GROUP)                                                                            ~ '3. Balanced matched control group composed',
      RR_SIGNIFICANT==''                                                                                      ~ '4. Have RR significantly different from 1',
      RR_SIGNIFICANT=='*' & (RR>=trajectoryAnalysisArgs$RRrangeToSkip[1] & RR<trajectoryAnalysisArgs$RRrangeToSkip[2]) ~ paste0('5. RR outside the range [',trajectoryAnalysisArgs$RRrangeToSkip[1],',',trajectoryAnalysisArgs$RRrangeToSkip[2],')'),
      RR_SIGNIFICANT=='*' & sign(RR_IN_PREVIOUS_STUDY-1)!=sign(RR-1)                                         ~ '6. Effect direction (increases/decreases risk) matches with discovery study',
      !is.na(DIRECTIONAL_SIGNIFICANT) & DIRECTIONAL_SIGNIFICANT==''                                          ~ '7. Have significant E1->E2 order',
      TRUE                                                                                                    ~ ''
    ))



  #show up nicely
  df<-data.frame(res=names(table(pairs$TEXTUAL_RESULT)),
                 count=table(pairs$TEXTUAL_RESULT),
                 perc=round(prop.table(table(pairs$TEXTUAL_RESULT))*100,1)) %>% dplyr::select(res,count=count.Freq,freq=perc.Freq) %>% dplyr::arrange(res)
  if(verbose) {
    ParallelLogger::logInfo('Total number of event pairs validated: ',nrow(pairs))
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
  RenderedSql <- Trajectories:::loadRenderTranslateSql('d1d2_model_reader.sql',
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
  RenderedSql <- Trajectories:::loadRenderTranslateSql('clear_e1e2_model_results.sql',
                                                      packageName=get('TRAJECTORIES_PACKAGE_NAME', envir=TRAJECTORIES.CONSTANTS),
                                                      dbms=connection@dbms,
                                                      resultsSchema =   trajectoryLocalArgs$resultsSchema,
                                                      prefiX =  trajectoryLocalArgs$prefixForResultTableNames
  )
  DatabaseConnector::executeSql(connection, RenderedSql)
}

getMatchedCaseControlCounts <- function(connection,trajectoryLocalArgs,matches,diagnosis1,diagnosis2) {


  if(length(matches$Controls)==0) {
    expected_prob=0
  } else {
    expected_prob=matches$E2_count_in_controls/length(matches$Controls)
  }

  if(length(matches$Cases)==0) {
    actual_prob=0
  } else {
    actual_prob=matches$E2_count_in_cases/length(matches$Cases)
  }

  return(list(
    case_group_size=length(matches$Cases),
    control_group_size=length(matches$Controls),
    num_observations_in_cases=matches$E2_count_in_cases,
    num_observations_in_controls=matches$E2_count_in_controls,
    actual_prob=actual_prob,
    expected_prob=expected_prob
  ))

}

getMatchedCaseControlCountsDeprecated <- function(connection,trajectoryLocalArgs,matches,diagnosis1,diagnosis2) {




  #get number of E2-s in cases
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


  #get number of E2-s in controls
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

  #add some additional columns (need to do it in R as in SQLite these numbers are given as characters, not integers, and that breaks everything)
  d <- d %>%
    dplyr::mutate(YEAR_OF_INDEXDATE= lubridate::year(INDEX_DATE),
               MONTH_OF_INDEXDATE= lubridate::month(INDEX_DATE))

  #train model
  d$GENDER<-factor(d$GENDER, levels<-c('M','F'))
  #print(str(d))
  #print(d)

  if(all(d$IS_CASE==1)) {
    ParallelLogger::logWarn('All eventperiods are in case group (no controls), cannot build propensity score for the first event')
    d$PropScore=NA
  } else {
    m = suppressWarnings( glm(IS_CASE ~ GENDER + scale(AGE) + scale(YEAR_OF_INDEXDATE) + scale(MONTH_OF_INDEXDATE) + scale(LEN_HISTORY_DAYS) + scale(LEN_FOLLOWUP_DAYS), family = binomial(), data = d) )

    m1<-data.frame(n=names(m$coefficients)[2:length(m$coefficients)], b=signif(m$coefficients[2:length(m$coefficients)],2))
    ParallelLogger::logInfo('Betas of matching model: IS CASE ~ ',paste(m1$b, as.character(m1$n),  sep =" x ", collapse=" + "))

    #calculate propensity scores for all
    d = d %>%
      dplyr::mutate(PropScore = predict(m, type = "response")) %>%
      dplyr::select(EVENTPERIOD_ID, PropScore, everything()) %>%
      dplyr::arrange(desc(PropScore))
  }




  #distribution of propensity scores before matching
  #library(ggplot2)
  #d %>%
  #  ggplot(ggplot2::aes(x = PropScore, fill = as.factor(IS_CASE))) +
  #  geom_density(ggplot2::aes(y = ..count..), alpha = 0.5) +
  #  theme_bw()

  return(d)
}

propensityScoreBasedMatch = function(d, nn = 2){
  e = d %>%
    dplyr::filter(IS_CASE == 1)

  u = d %>%
    dplyr::filter(IS_CASE ==  0) %>%
    dplyr::filter(!(EVENTPERIOD_ID %in% e$EVENTPERIOD_ID))

  controls = integer(0)
  for(i in 1:nrow(e)){
    if(nrow(u) >= nn){
      dist = abs(e[i, "PropScore", drop = T] - u$PropScore)
      r = rank(dist, ties.method = "random")

      controls = c(controls, u[r %in% 1:nn, "EVENTPERIOD_ID", drop = T])

      u = u %>%
        dplyr::filter(!(r %in% 1:nn))
    }
  }

  return(list(Cases = e$EVENTPERIOD_ID, Controls = controls))
}

adjustPValues<-function(connection,trajectoryLocalArgs,dbcol.pvalue='RR_PVALUE',dbcol.pval.signficiant='RR_SIGNIFICANT',method='fdr') {

  ParallelLogger::logInfo('Adjusting p-values by using method=',method,'...')

  #get p-values
  sql="SELECT E1_CONCEPT_ID, E2_CONCEPT_ID, @dbcol FROM @resultsSchema.@prefiXE1E2_model WHERE @dbcol IS NOT NULL";
  RenderedSql <- SqlRender::render(sql=sql,
                                   dbcol=dbcol.pvalue,
                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                   prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
  d<-DatabaseConnector::querySql(connection=connection, sql=RenderedSql)
  num_values_to_adjust<-nrow(d)

  ParallelLogger::logInfo('  Number of p-values (tests) to adjust: ',num_values_to_adjust,'...')
  d$adjusted.pvalues=p.adjust(d[[dbcol.pvalue]], method = method)


  sql="UPDATE @resultsSchema.@prefiXE1E2_model SET @dbcol='';";
  RenderedSql <- SqlRender::render(sql=sql,
                                   dbcol=dbcol.pval.signficiant,
                                   resultsSchema = trajectoryLocalArgs$resultsSchema,
                                   prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
  RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
  DatabaseConnector::executeSql(connection, RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)

  #which is the largest RR_PVALUE which is significant after adjustment
  sign_rr_pvalues<-data.frame(d %>% dplyr::filter(adjusted.pvalues<0.05))[[dbcol.pvalue]]
  non_sign_rr_pvalues<-data.frame(d %>% dplyr::filter(adjusted.pvalues>=0.05))[[dbcol.pvalue]]
  num_sign_values<-length(sign_rr_pvalues)

  if(num_sign_values>0) {
    if(length(non_sign_rr_pvalues)>0) {
      MIN_NON_SIGN_RR_PVALUE=min(non_sign_rr_pvalues)
    } else {
      MIN_NON_SIGN_RR_PVALUE=1.0
    }


    sql=paste0("UPDATE @resultsSchema.@prefiXE1E2_model SET ",dbcol.pval.signficiant,"='*' WHERE @dbcol<@pval;")
    RenderedSql <- SqlRender::render(sql=sql,
                                     dbcol=dbcol.pvalue,
                                     pval=MIN_NON_SIGN_RR_PVALUE,
                                     resultsSchema = trajectoryLocalArgs$resultsSchema,
                                     prefiX =  trajectoryLocalArgs$prefixForResultTableNames)
    RenderedSql <- SqlRender::translate(sql=RenderedSql, targetDialect = connection@dbms)
    DatabaseConnector::executeSql(connection, RenderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  }

  ParallelLogger::logInfo('...done. Out of ',num_values_to_adjust,' event pairs, ',num_sign_values,' were significant after multiple testing correction.')

}

makeRRPvaluePlot <- function(pairs,filename,trajectoryAnalysisArgs) {
  pairs_for_plot<-pairs %>% dplyr::mutate(id = dplyr::row_number()) %>% dplyr::select(id, RR, RR_PVALUE, RR_SIGNIFICANT)
  pairs_for_plot <- pairs_for_plot %>% dplyr::arrange(-RR_PVALUE)
  pairs_for_plot <- pairs_for_plot %>% dplyr::filter(!is.na(RR) & !is.na(RR_PVALUE) & RR_PVALUE>0 & RR>0 & RR_PVALUE!=Inf & RR != Inf & RR != 999)

  #print(table(pairs_for_plot$RR))
  #print(table(pairs_for_plot$RR_PVALUE))

  MIN_RR_VALUE=dplyr::coalesce(suppressWarnings(min(pairs_for_plot$RR,na.rm=T)),0.001)
  MAX_RR_VALUE=dplyr::coalesce(suppressWarnings(max(pairs_for_plot$RR,na.rm=T)),999)
  MIN_PVALUE=min(pairs_for_plot$RR_PVALUE,na.rm=T)
  MAX_PVALUE=1
  pvals.of.significant.pairs<-pairs_for_plot %>% dplyr::filter(!is.na(RR_SIGNIFICANT) & RR_SIGNIFICANT=='*') %>% dplyr::pull(RR_PVALUE)
  if(length(pvals.of.significant.pairs[!is.na(pvals.of.significant.pairs)])==0) {
    MAX_SIGNIFICANT_PVALUE=0
  } else {
    MAX_SIGNIFICANT_PVALUE=max(
      pvals.of.significant.pairs,
      na.rm=T)
  }


  pairs_for_plot$LOG_RR=suppressWarnings(log10(pairs_for_plot$RR))
  pairs_for_plot$LOG_RR_PVAL=suppressWarnings(log10(pairs_for_plot$RR_PVALUE))

  #pairs_for_plot <- pairs_for_plot %>% dplyr::filter(LOG_RR>-Inf & LOG_RR_PVAL>-Inf)

  p<-suppressWarnings(ggplot2::ggplot(pairs_for_plot, ggplot2::aes(x=LOG_RR,y=LOG_RR_PVAL)) +
                        ggplot2::geom_point() +
                        #ggrepel::geom_text_repel(aes(label=model)) +
                        ggplot2::geom_text(ggplot2::aes(label=id),size=3) +
                        ggplot2::annotate("rect", xmin = log10(trajectoryAnalysisArgs$RRrangeToSkip[1]), xmax = log10(trajectoryAnalysisArgs$RRrangeToSkip[2]), ymin = log10(MIN_PVALUE), ymax = log(min(MAX_PVALUE,1)), alpha = .5) +
               ggplot2::annotate("rect", xmin = log10(MIN_RR_VALUE), xmax = log10(MAX_RR_VALUE), ymin = log10(MAX_SIGNIFICANT_PVALUE), ymax = 1, alpha = .5) +
               #ggplot2::scale_x_continuous(trans='log10') +
               #ggplot2::scale_y_continuous(trans='log10') +
      ggplot2::annotation_logticks(sides="trbl") +
      ggplot2::labs(title='RR/p-value plot of all tested event pairs. Shaded areas indicate insignificant p-values and RR values in skip range.') +
      ggplot2::theme_bw()
  )

  pdf(filename)
  suppressWarnings(print(p))
  dev.off()

}

makeRRPvalueQQPlot<-function(pairs) {
  observedPValues <- pairs %>% dplyr::mutate(id = dplyr::row_number()) %>% dplyr::pull(RR_PVALUE)
  plot(-log10(1:length(observedPValues)/length(observedPValues)),
       -log10(sort(observedPValues)))
  abline(0, 1, col = "red")
}



buildCaseControlGroups<-function(connection,trajectoryLocalArgs,diagnosis1,diagnosis1name,outputFolder) {
  #create table for matching
  RenderedSql <- Trajectories:::loadRenderTranslateSql('AssignIndexDatesForMatching.sql',
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

  num.cases.original<-sum(d$IS_CASE==1)
  num.noncases.original<-sum(d$IS_CASE==0)


  #Prepare data from propensity score based matching
  d <- d %>%
    dplyr::mutate(YEAR_OF_INDEXDATE= lubridate::year(INDEX_DATE),
                  MONTH_OF_INDEXDATE= lubridate::month(INDEX_DATE))

  d$GENDER<-factor(d$GENDER, levels<-c('M','F'))
  #remove rows with incorrect gender
  d <- d %>% filter(!is.na(GENDER))

  if(all(d$IS_CASE==1)) {
    ParallelLogger::logWarn('All eventperiods are in case group (no controls), cannot build case-control groups for the first event')
    return(list(Cases = sort(d$EVENTPERIOD_ID), Controls = c(), IsImbalanced=1, ImbalanceComment='All eventperiods are in case group (no controls), cannot build propensity score for the first event'))

  }

  # For age group stratification on propensity score matching, create age group bins
  d$AGEGROUP<-cut(d$AGE, breaks=c(0,1,2,5,10,20,30,40,50,60,70,80,150),right=F)
  #remove rows with incorrect agegroup (e.g the age is <0 or >150)
  d <- d %>% filter(!is.na(AGEGROUP))

  #Get season of INDEXDATE
  d$SEASON_OF_INDEXDATE=Trajectories:::getSeason(as.Date(d$INDEX_DATE))
  d <- d %>% filter(!is.na(SEASON_OF_INDEXDATE))

  #table(d$IS_CASE)
  #table(d[d$IS_CASE==1,]$GENDER)

  m.out1 <- Trajectories:::matchitWithTryCatch(d=d)
  #summary(m.out1)

  if(is.null(m.out1)) { #if matching failed (no matches were found)
    control.ids<-c()
    case.ids<-c()
  } else { #if matching process succeeded
    control.ids<-as.numeric(m.out1$match.matrix[,1][!is.na(m.out1$match.matrix[,1])])
    case.ids<-as.numeric(rownames(m.out1$match.matrix)[!is.na(m.out1$match.matrix[,1])])
  }


  if(length(control.ids)==0 | length(case.ids)==0) {
    ParallelLogger::logWarn('After matching, the number of cases or controls is 0 - cannot build case-control groups for the first event')
    return(list(Cases = case.ids, Controls = c(), IsImbalanced=1, ImbalanceComment='After matching, the number of cases or controls is 0 - cannot build case-control groups for the first event'))
  } else {
    ParallelLogger::logInfo('Out of ',num.cases.original,' cases and ',num.noncases.original,' non-cases, ',length(case.ids),'+',length(control.ids),' were selected for matched cases and controls.')
  }

  #propensity score covariate plot before and after matching
  #plot(summary(m.out1))

  #Assess the quality of matching (check that Std. Mean Diff. for all covariates is <0.15)
  MAX_ALLOWED_SDM=0.15
  if( max(c(abs(summary(m.out1)$sum.matched[,'Std. Mean Diff.']),-Inf),na.rm=T)>MAX_ALLOWED_SDM ) { #c(.., -Inf) here to prevent error when there are 1 case+1 control and only NA-s in "Std. Mean Diff." column
    ParallelLogger::logInfo('Propensity score based matching resulted in imbalanced groups: Std. Mean Diff. of the following covariates is >',MAX_ALLOWED_SDM,' after matching: ',paste(rownames(summary(m.out1)$sum.matched)[abs(summary(m.out1)$sum.matched[,'Std. Mean Diff.'])>MAX_ALLOWED_SDM], collapse=","))

    is_imbalanced=T
    imbalance_comment=paste0('SMD>',MAX_ALLOWED_SDM,':',paste(rownames(summary(m.out1)$sum.matched)[abs(summary(m.out1)$sum.matched[,'Std. Mean Diff.'])>MAX_ALLOWED_SDM],collapse=", "))

    filename=file.path(outputFolder,'figures',paste0('PropensityScoreSMDfor.',diagnosis1,'.pdf'))
    pdf(filename)
    plot(summary(m.out1), main=paste0('Imbalanced ctrl group matching for ',diagnosis1,' ',diagnosis1name))
    #plot(m.out1, type = "qq", interactive = FALSE)
    dev.off()

  } else {
    is_imbalanced=F
    imbalance_comment=NULL
  }



  return(list(Cases = sort(d[case.ids,'EVENTPERIOD_ID']), Controls = sort(d[control.ids,'EVENTPERIOD_ID']), IsImbalanced=is_imbalanced, ImbalanceComment=imbalance_comment))
}

getSeason <- function(input.date){
  numeric.date <- 100*lubridate::month(input.date)+lubridate::day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

matchitWithTryCatch <- function(d) {

  #in some rare cases, the number of different "SEASON_OF_INDEXDATE" values in case group is 1.
  #As we use it in formula, glm.fit produces error "contrasts can be applied only to factors with 2 or more levels"
  #To prevent that, take SEASON_OF_INDEXDATE out from the formula and require exact match
  if(length(unique(d[d$IS_CASE==1,]$SEASON_OF_INDEXDATE))==1) { #only 1 different SEASON_OF_INDEXDATE value in case group
    f=IS_CASE ~ scale(LEN_HISTORY_DAYS) + scale(LEN_FOLLOWUP_DAYS)
    exact=c("GENDER","AGEGROUP","YEAR_OF_INDEXDATE","SEASON_OF_INDEXDATE") #Gender, age group and year of index date must be match in case/control group
  } else {
    #normal use case
    f=IS_CASE ~ SEASON_OF_INDEXDATE + scale(LEN_HISTORY_DAYS) + scale(LEN_FOLLOWUP_DAYS)
    exact=c("GENDER","AGEGROUP","YEAR_OF_INDEXDATE") #Gender, age group and year of index date must be match in case/control group
  }

 tryCatch(
    expr = {
      m.out1<-suppressWarnings(MatchIt::matchit(formula=f, #formula for logistic regression
                       data=d,
                       method = "optimal", # Find a control patients so that the sum of the absolute pairwise distances in the matched sample is as small as possible
                       distance = "glm", #Use logistic regression based propensity score
                       exact=exact,
                       discard="both", # discard cases or controls where no good matching is found
                       reestimate=T) #After discarding some cases/controls, re-estimate the propensity scores
      )
      return(m.out1)
      #message("Successfully executed the log(x) call.")
    },
    error = function(e){
      ParallelLogger::logInfo('Caught an error in matchit() but catched it in try-catch: ',e)
      ParallelLogger::logInfo('Therefore, trying nearest neighbor matching instead of optimal...')
    }
    #warning = function(w){ #warning block is commented out to prevent stopping the execution of matchIt() if warning occurs
    #  ParallelLogger::logInfo('Caught a warning in matchit() but catched it in try-catch:',w)
    #},
    #finally = {
    #
    #}
  ) #tryCatch

  #should reach here only if an error occurred

  tryCatch(
    expr = {
      m.out1<-suppressWarnings(MatchIt::matchit(formula=f, #formula for logistic regression
                                                data=d,
                                                method = "nearest", # Find a control patients based on nearest neighbor method
                                                distance = "glm", #Use logistic regression based propensity score
                                                exact=exact,
                                                discard="both", # discard cases or controls where no good matching is found
                                                reestimate=T) #After discarding some cases/controls, re-estimate the propensity scores
      )
      return(m.out1)
    },
    error = function(e){
      ParallelLogger::logInfo('Still caught an error in matchit() but catched it in try-catch: ',e)
      ParallelLogger::logInfo('Giving up on trying matching case-control groups...')
      return(NULL)
    }
  ) #tryCatch

  #should never reach here
  return(NULL)
}
