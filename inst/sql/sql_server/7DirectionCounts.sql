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


UPDATE
    @resultsSchema.@prefixd1d2_model
SET
    MIN_AGE_OF_COHORT_EVENT1_OCCURS_FIRST = b.min_age,
    Q25_AGE_OF_COHORT_EVENT1_OCCURS_FIRST = b.q25_age,
    MEDIAN_AGE_OF_COHORT_EVENT1_OCCURS_FIRST = b.median_age,
    Q75_AGE_OF_COHORT_EVENT1_OCCURS_FIRST = b.q75_age,
    MAX_AGE_OF_COHORT_EVENT1_OCCURS_FIRST = b.max_age
FROM 
    (WITH ages AS
  (SELECT event1_age as age,
          ROW_NUMBER() OVER (ORDER BY event1_age) order_nr FROM @resultsSchema.@prefixcohorts_with_d1d2 where date1<date2
  )
SELECT
    MIN(age) AS min_age,
    MIN(CASE 
            WHEN order_nr < .25 * n
                THEN 9999
            ELSE age
            END) AS q25_age,
    MIN(CASE 
            WHEN order_nr < .50 * n
                THEN 9999
            ELSE age
            END) AS median_age,
    MIN(CASE 
            WHEN order_nr < .75 * n
                THEN 9999
            ELSE age
            END) AS q75_age,
    MAX(age) AS max_age
FROM ages
CROSS JOIN (
    SELECT COUNT(*) AS n
    FROM @resultsSchema.@prefixcohorts_with_d1d2 where date1<date2
    ) event1_population_size) b
WHERE event1_concept_id = @diag1 and event2_concept_id = @diag2;

-- drop calculation table
IF OBJECT_ID('@resultsSchema.@prefixcohorts_with_d1d2', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixcohorts_with_d1d2;



