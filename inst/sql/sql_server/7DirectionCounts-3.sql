IF OBJECT_ID('@resultsSchema.@prefixeventperiods_with_E1E2', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixeventperiods_with_E1E2;

        SELECT
          CASE WHEN
            a.date<b.date THEN 1
            WHEN
            a.date=b.date THEN 0
            WHEN
            a.date>b.date THEN -1
          END AS E1E2_ORDER

        INTO @resultsSchema.@prefixeventperiods_with_E1E2
        FROM
             @resultsSchema.@prefixtemp2_for_direction_counts b
                 INNER JOIN @resultsSchema.@prefixeventperiods_with_E1 a
                ON a.EVENTPERIOD_ID = b.EVENTPERIOD_ID
        WHERE
            b.CONCEPT_ID = (@diag2)
        --ORDER BY EVENTPERIOD_ID
        ;


-- add counts to results
UPDATE @resultsSchema.@prefixE1E2_model SET E1_BEFORE_E2_COUNT_IN_EVENTS = (select count(*) from @resultsSchema.@prefixeventperiods_with_E1E2 where E1E2_ORDER=1),
                                E1_AFTER_E2_COUNT_IN_EVENTS = (select count(*) from @resultsSchema.@prefixeventperiods_with_E1E2 where E1E2_ORDER=-1),
                                E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS = (select count(*) from @resultsSchema.@prefixeventperiods_with_E1E2 where E1E2_ORDER=0)
WHERE E1_CONCEPT_ID = @diag1 and E2_CONCEPT_ID = @diag2;


-- drop calculation table
IF OBJECT_ID('@resultsSchema.@prefixeventperiods_with_E1E2', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixeventperiods_with_E1E2;



