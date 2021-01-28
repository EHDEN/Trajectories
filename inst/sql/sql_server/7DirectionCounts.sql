-- crete temporary tables for calculation

IF OBJECT_ID('@resultsSchema.@prefixeventperiods_with_E1', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixeventperiods_with_E1;

--CREATE TABLE @resultsSchema.@prefixcohorts_with_d1 as
    SELECT
               EVENTPERIOD_ID,
               CONCEPT_ID,
               date,
               age
        INTO @resultsSchema.@prefixeventperiods_with_E1
        FROM
             @resultsSchema.@prefixevents_in_eventperiods
        WHERE
            CONCEPT_ID = (@diag1)
        ORDER BY EVENTPERIOD_ID,date;



IF OBJECT_ID('@resultsSchema.@prefixeventperiods_with_E1E2', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixeventperiods_with_E1E2;

--CREATE TABLE @resultsSchema.@prefixeventperiods_with_E1E2 as
    SELECT
               a.eventperiod_id as EVENTPERIOD_ID,
               b.CONCEPT_ID as E1_CONCEPT_ID,
               a.CONCEPT_ID as E2_CONCEPT_ID,
               b.date as date1,
               a.date as date2,
               a.age as E1_age
        INTO @resultsSchema.@prefixeventperiods_with_E1E2
        FROM
             @resultsSchema.@prefixevents_in_eventperiods a
                 JOIN @resultsSchema.@prefixeventperiods_with_E1 b
                ON a.EVENTPERIOD_ID = b.EVENTPERIOD_ID
        WHERE
            a.CONCEPT_ID = (@diag2)
        ORDER BY EVENTPERIOD_ID;

IF OBJECT_ID('@resultsSchema.@prefixeventperiods_with_E1', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixeventperiods_with_E1;





-- add counts to results
UPDATE @resultsSchema.@prefixE1E2_model SET E1_BEFORE_E2_COUNT_IN_EVENTS = (select count(*) from @resultsSchema.@prefixeventperiods_with_E1E2 where date1<date2),
                                AVG_AGE_OF_E1_BEFORE_E2_IN_EVENTS = (select avg(E1_age) from @resultsSchema.@prefixeventperiods_with_E1E2 where date1<date2),
                                E1_AFTER_E2_COUNT_IN_EVENTS = (select count(*) from @resultsSchema.@prefixeventperiods_with_E1E2 where date2<date1),
                                E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS = (select count(*) from @resultsSchema.@prefixeventperiods_with_E1E2 where date1=date2)
WHERE E1_CONCEPT_ID = @diag1 and E2_CONCEPT_ID = @diag2;


-- add event1 age distribution for cases where event1 occurs before event2 also to results
UPDATE @resultsSchema.@prefixE1E2_model SET
                                AVG_AGE_OF_E1_BEFORE_E2_IN_EVENTS = (select avg(E1_age) from @resultsSchema.@prefixeventperiods_with_E1E2 where date1<date2)
WHERE E1_CONCEPT_ID = @diag1 and E2_CONCEPT_ID = @diag2;

-- drop calculation table
IF OBJECT_ID('@resultsSchema.@prefixeventperiods_with_E1E2', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefixeventperiods_with_E1E2;



