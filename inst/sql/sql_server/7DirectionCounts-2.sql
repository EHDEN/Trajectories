IF OBJECT_ID('@resultsSchema.@prefixtemp2_for_direction_counts', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixtemp2_for_direction_counts;


        SELECT
          EVENTPERIOD_ID,
          COHORT_ID,
          date
        INTO @resultsSchema.@prefixtemp2_for_direction_counts
        FROM
             @resultsSchema.@prefixevents_in_eventperiods
        WHERE
            COHORT_ID IN (SELECT COHORT_ID FROM @resultsSchema.@prefixtemp1_for_direction_counts)
        ORDER BY COHORT_ID,EVENTPERIOD_ID
        ;
