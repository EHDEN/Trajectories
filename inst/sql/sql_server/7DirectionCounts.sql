-- crete temporary tables for calculation

IF OBJECT_ID('@resultsSchema.@prefixcohorts_with_d1', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixcohorts_with_d1;

CREATE TABLE @resultsSchema.@prefixcohorts_with_d1 as
    SELECT
               cohort_id as cohort_id,
               dgn,
               date,
               age
        FROM
             @resultsSchema.@prefixevents_cohort
        WHERE
            dgn = (@diag1)
        ORDER BY cohort_id,date;



IF OBJECT_ID('@resultsSchema.@prefixcohorts_with_d1d2', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixcohorts_with_d1d2;

CREATE TABLE @resultsSchema.@prefixcohorts_with_d1d2 as
    SELECT
               a.cohort_id as cohort_id,
               b.dgn as event1_concept_id,
               a.dgn as event2_concept_id,
               b.date as date1,
               a.date as date2,
               a.age as event1_age
        FROM
             @resultsSchema.@prefixevents_cohort a
                 JOIN @resultsSchema.@prefixcohorts_with_d1 b
                ON a.cohort_id = b.cohort_id
        WHERE
            a.dgn = (@diag2)
        ORDER BY cohort_id;

IF OBJECT_ID('@resultsSchema.@prefixcohorts_with_d1', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixcohorts_with_d1;





-- add counts to results
UPDATE @resultsSchema.@prefixd1d2_model SET COHORT_COUNT_EVENT1_OCCURS_FIRST = (select count(*) from @resultsSchema.@prefixcohorts_with_d1d2 where date1<date2),
                                AVG_AGE_OF_COHORT_EVENT1_OCCURS_FIRST = (select avg(event1_age) from @resultsSchema.@prefixcohorts_with_d1d2 where date1<date2),

                                COHORT_COUNT_EVENT2_OCCURS_FIRST = (select count(*) from @resultsSchema.@prefixcohorts_with_d1d2 where date2<date1),
                                COHORT_COUNT_EVENT1_EVENT2_OCCUR_ON_SAME_DAY = (select count(*) from @resultsSchema.@prefixcohorts_with_d1d2 where date1=date2)
WHERE event1_concept_id = @diag1 and event2_concept_id = @diag2;


-- add event1 age distribution for cases where event1 occurs before event2 also to results
UPDATE @resultsSchema.@prefixd1d2_model SET
                                AVG_AGE_OF_COHORT_EVENT1_OCCURS_FIRST = (select avg(event1_age) from @resultsSchema.@prefixcohorts_with_d1d2 where date1<date2)
WHERE event1_concept_id = @diag1 and event2_concept_id = @diag2;

-- drop calculation table
IF OBJECT_ID('@resultsSchema.@prefixcohorts_with_d1d2', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixcohorts_with_d1d2;



