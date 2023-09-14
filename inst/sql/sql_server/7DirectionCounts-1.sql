IF OBJECT_ID('@resultsSchema.@prefixeventperiods_with_E1', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixeventperiods_with_E1;

    SELECT
               EVENTPERIOD_ID,
               -- CONCEPT_ID,
               date

        INTO @resultsSchema.@prefixeventperiods_with_E1
        FROM
             @resultsSchema.@prefixevents_in_eventperiods
        WHERE
            COHORT_ID = (@diag1)
        ORDER BY EVENTPERIOD_ID,date;
