library(SqlRender)
library(ff)

#' Runs the analysis that detects statistically significant directional event pairs and writes the results to file. Data is taken from database and it is expected that the tables are created by function createEventPairsTable()
#'
#' @param packageName Must always have value 'Trajectories'. The value is needed by SqlRender to find the SQL scripts from the right path.
#' @param connection DatabaseConnectorConnection object that is used to connect with database
#' @param dbms The target SQL dialect that is used by SqlRender
#' @param oracleTempSchema A schema that can be used to create temp tables in when using Oracle. Used by SqlRender
#' @param sqlRole Database role that is used when creating tables in 'resultsSchema'. Set to empty string ('') if setting a specific role is not needed.
#' @param resultsSchema Database schema where the temporary analysis tables are created. They are temporary in a sense that they are deleted in the end of the analysis (the tables are not created as CREATE TEMPORARY TABLE...)
#' @param prefixForResultTableNames This is the prefix that is used for all table names in analysis process. Table with that prefix should already exist in the database (created by function createEventPairsTable()). Default value is ''.
#' @param eventPairResultsFilename Filename where the results of the analysis (significant directional event pairs) are written to
#' @param eventPairResultsStatsFilename Filename where the some statics, used p-value threshold etc. are written for later use.
#'
#' @return
#' @export
#'
#' @examples
runEventPairAnalysis<-function(packageName,
                               connection,
                               dbms,
                               oracleTempSchema = NULL,
                               sqlRole='',
                               resultsSchema,
                               prefixForResultTableNames = "",
                               eventPairResultsFilename = 'event_pairs.tsv',
                               eventPairResultsStatsFilename = 'event_pairs_stats.txt') {
  print(paste0("Detect statistically significant directional event pairs and write the results to ",eventPairResultsFilename,"..."))

  # Make sure that no-one uses F as sqlRole
  if(sqlRole==F) sqlRole="";

  # Get all (frequent) event pairs from the database
  RenderedSql <- SqlRender::loadRenderTranslateSql("2GetPairs.sql",
                                                   packageName=packageName,
                                                   dbms=dbms,
                                                   resultsSchema = resultsSchema,
                                                   prefix = prefixForResultTableNames
  )
  dpairs = DatabaseConnector::querySql(connection, RenderedSql)

  # Determine p-value threshold of Bonferroni correction
  cutoff_pval = 0.05/nrow(dpairs)
  print(paste0('There are ',nrow(dpairs),' event pairs that are going to be analyzed.'))
  if(nrow(dpairs)==0) {
    print('Nothing to analyze, exit analysis function.')
    return(1);
  }
  print(paste0('We use Bonferroni multiple test correction, therefore p-value threshold 0.05/',nrow(dpairs),'=',cutoff_pval,' is used in this analysis.'))

  # For each event pair, run the analysis
  for(i in 1:nrow(dpairs))
    {
      diagnosis1 = dpairs[i,'EVENT1_CONCEPT_ID']
      diagnosis2 = dpairs[i,'EVENT2_CONCEPT_ID']
      print(paste0('Analyzing event pair ',diagnosis1,' -> ',diagnosis2,' (total progress ',round(100*i/nrow(dpairs)),'%)...'))

      # Extract necessary event1_concept_id->event2_concept_id data from table d1d2_analysistable
      RenderedSql <- SqlRender::loadRenderTranslateSql("5CaseControlStats.sql",
                                                       packageName=packageName,
                                                       dbms=dbms,
                                                       event1=diagnosis1,
                                                       event2=diagnosis2,
                                                       resultsSchema =  resultsSchema,
                                                       prefix = prefixForResultTableNames
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
      # We apply Laplace smoothing to solve the problem of (possible) zero probability.
      # That is: +1 observation for cases and +1 for controls (+2 for total)
      case_control$group_prob <- (case_control$CASE_COUNT+1) / (sum(case_control$CASE_COUNT)+2)

      # Add column "match_prob" which is event2_concept_id prevalence within this age, gender, discharge time group in general population
      case_control$match_prob <- (case_control$CONTROL_D2+1) / (case_control$CONTROL_COUNT + 2)

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
      #             what is the probability that we observe event2_concept_id in case group more than 'observed_matches'?
      # This question can be easily answered by pbinom(..., lower.tail=FALSE) which gives cumulative density function (cdf) as a result
      # Using lower.tail=FALSE is necessary, because otherwise pbinom would give the probability that we observe event2_concept_id in case group LESS than 'observed_matches'

      observed_matches <- sum(case_control$CASE_D2)
      observation_count <- sum(case_control$CASE_COUNT)
      event_pair_pvalue <- pbinom(q = observed_matches, size = observation_count, prob = expected_prob, lower.tail=FALSE)

      #In case event_pair_pvalue <= cutoff_pval, the null hypothesis can be rejected and the prevalence of event2_concept_id among event1_concept_id patients
      #is significantly larger than in general population.
      #This means that we have found a significant event1_concept_id-event2_concept_id event pair!

      #What is the "effect" (how many times the event2_concept_id prevalence in case group is higher than in control group)
      event_pair_effect <- observed_matches / (observation_count*expected_prob)

      # Writing the results back to database
      RenderedSql <- SqlRender::loadRenderTranslateSql("6PvalInserter.sql",
                                                       packageName=packageName,
                                                       dbms=dbms,
                                                       resultsSchema =  resultsSchema,
                                                       pval = event_pair_pvalue,
                                                       effect = event_pair_effect,
                                                       diag1 = diagnosis1,
                                                       diag2 = diagnosis2,
                                                       prefix = prefixForResultTableNames
      )
      DatabaseConnector::executeSql(connection, RenderedSql)


      # In case the event1_concept_id-event2_concept_id event pair is significant, let's investigate whether the order of the events is also important
      if (event_pair_pvalue > cutoff_pval){

        print(paste0('Event pair ',diagnosis1,' -> ',diagnosis2,' is not significant.'))

      } else {

        print(paste0('Event pair ',diagnosis1,' -> ',diagnosis2,' is significant. Testing its directionality...'))

        #Calculate in database: among people that have event1_concept_id and event2_concept_id pair, how many have date1<date2, date1=date2, date1>date2
        RenderedSql <- SqlRender::loadRenderTranslateSql("7DirectionCounts.sql",
                                                         packageName=packageName,
                                                         dbms=dbms,
                                                         sqlRole = sqlRole,
                                                         resultsSchema =  resultsSchema,
                                                         diag1 = diagnosis1,
                                                         diag2 = diagnosis2,
                                                         prefix = prefixForResultTableNames
        )
        DatabaseConnector::executeSql(connection, RenderedSql)

        # Get calculation results from database
        RenderedSql <- SqlRender::loadRenderTranslateSql("8DpairReader.sql",
                                                         packageName=packageName,
                                                         dbms=dbms,
                                                         resultsSchema =  resultsSchema,
                                                         diag1=diagnosis1,
                                                         diag2=diagnosis2,
                                                         prefix = prefixForResultTableNames
        )
        direction_counts = DatabaseConnector::querySql(connection, RenderedSql)


        # If there is no significant direction in event pair event1_concept_id and event2_concept_id, then we expect to see event1_concept_id->event2_concept_id and event2_concept_id->event1_concept_id sequences
        # with the same frequency. Say, we would expect that event1_concept_id is the first diagnosis in 50% cases (prob=0.5).
        # Therefore: If the expected probability of seeing event1_concept_id as the first diagnosis is 0.5,
        #            what is the probability that we observe event1_concept_id as the first diagnosis >='direction_counts$people_count_event1_occurs_first' times?
        # This question can be easily answered by pbinom(..., lower.tail=FALSE) which gives cumulative density function (cdf) as a result
        # Using lower.tail=FALSE is necessary, because otherwise pbinom would give the probability that we observe event1_concept_id as first diagnosis  LESS than 'direction_counts$people_count_event1_occurs_first'
        cohort_count_event1_occurs_first = direction_counts$COHORT_COUNT_EVENT1_OCCURS_FIRST
        total_tests = direction_counts$COHORT_COUNT_EVENT1_OCCURS_FIRST + direction_counts$COHORT_COUNT_EVENT1_EVENT2_OCCUR_ON_SAME_DAY + direction_counts$COHORT_COUNT_EVENT2_OCCURS_FIRST
        event_pair_pvalue <- pbinom(q = cohort_count_event1_occurs_first, size = total_tests, prob = 0.5, lower.tail=FALSE)

        # Store the pvalue to database
        RenderedSql <- SqlRender::loadRenderTranslateSql("9PvalInserterDirection.sql",
                                                         packageName=packageName,
                                                         dbms=dbms,
                                                         resultsSchema =  resultsSchema,
                                                         pval = event_pair_pvalue,
                                                         diag1 = diagnosis1,
                                                         diag2 = diagnosis2,
                                                         prefix = prefixForResultTableNames
        )
        DatabaseConnector::executeSql(connection, RenderedSql)

        if (event_pair_pvalue > cutoff_pval){

          print(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is not significant.'))

        } else {

          print(paste0('The direction of event pair ',diagnosis1,' -> ',diagnosis2,' is significant.'))

        }

      }

  } # for


  # Read in results
  RenderedSql <- SqlRender::loadRenderTranslateSql("11ResultsReader.sql",
                                                   packageName=packageName,
                                                   dbms=dbms,
                                                   #role_for_writing = sqlRole,
                                                   resultsSchema =  resultsSchema,
                                                   prefix = prefixForResultTableNames,
                                                   cutoff_val=cutoff_pval,
                                                   effectSize = 1.1
  )
  selected_data = DatabaseConnector::querySql(connection, RenderedSql)

  # Write result table into file
  write.table(selected_data, file=eventPairResultsFilename, quote=FALSE, sep='\t', col.names = NA)

  # Print some output message that all succeed so far
  print(paste0('Found ',nrow(selected_data),' event pairs that have significant direction and have effect size > 1.1.'))
  print(paste0('These event pairs were written to ',eventPairResultsFilename))


  # Show some event pair statistics:
  msg=c(paste('EVENT PAIR RESULTS STATS:'),
        paste('========================='),
        paste(format(Sys.time(), '%d %B %Y %H:%M')),
        paste(''),
        paste('Total number of event pairs analyzed:',nrow(dpairs)),
        paste('Bonferroni corrected p-value threshold:',cutoff_pval),
        paste('Number of significant directional event pairs having effect size > 1.1:',nrow(selected_data)),
        paste('These significant event pairs written to:',eventPairResultsFilename)

  )
  print(msg)

  # Print stats to file
  if(eventPairResultsStatsFilename!=F) {
    print(paste0('Writing event pair statistics to ',eventPairResultsStatsFilename,'...'))

    fileConn<-file(eventPairResultsStatsFilename)
    writeLines(msg, fileConn)
    close(fileConn)

    print('... done.')
  }

  print('TASK COMPLETED: Detecting statistically significant directional event pairs completed successfully.')
}
