---------------------------------
-- This scripts creates 2 tables:
-- 1) @resultsSchema.@prefiXpairs - all event pairs for each person
-- 2) @resultsSchema.@prefiXE1E2_model - contains all event pair counts + empty columns for p-val calculations
-- 3) @resultsSchema.@prefiXevent_summary - gender, age, discharge_time, person_count based on events_person_pairs
---------------------------------


---------------------------------------------------------------------------------------
-- Create a temporary table for debugging
---------------------------------------------------------------------------------------
IF OBJECT_ID('@resultsSchema.@prefiXdebug', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXdebug;

CREATE TABLE @resultsSchema.@prefiXdebug (
    entry varchar(255) NOT NULL,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Debug table created');

-- First, add some data about the size of the database
INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('Database size: There are ',CAST((SELECT COUNT(*) FROM @cdmDatabaseSchema.person) AS VARCHAR),' rows in @cdmDatabaseSchema.person'));
INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('Database size: There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.condition_occurrence_brunak) AS VARCHAR),' rows in @resultsSchema.condition_occurrence_brunak'));



---------------------------------------------------------------------------------------
-- Create a temporary "mycohort" table - it is basically the same table & content as the cohort in OMOP cohorts table but
-- some additional fields are added (gender, year or birth)
-- ordinal numbers are added (id-s) for each event-period
-- and people with unknown gender and birthyear are removed
---------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXmycohort...');

IF OBJECT_ID('@resultsSchema.@prefiXmycohort', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXmycohort;

    SELECT
           ROW_NUMBER() OVER (
            ORDER BY cohort.subject_id, cohort.cohort_start_date
            ) AS eventperiod_id,
           p.person_id,
           CASE WHEN p.gender_concept_id = 8532 THEN 'F'
               WHEN p.gender_concept_id = 8507 THEN 'M'
               END AS gender,
           p.year_of_birth AS year_of_birth,
           cohort.cohort_start_date AS eventperiod_start_date,
           cohort.cohort_end_date AS eventperiod_end_date
    INTO @resultsSchema.@prefiXmycohort
    FROM
        @cohortTableSchema.@cohortTable AS cohort
        LEFT JOIN @cdmDatabaseSchema.person p ON cohort.subject_id=p.person_id
    WHERE
        cohort.cohort_definition_id=@cohortId
        AND p.gender_concept_id IS NOT NULL -- leave out persons with unknown gender
        AND p.gender_concept_id != 0 -- leave out persons with unknown gender
        AND p.year_of_birth IS NOT NULL -- leave out persons with unknown year of birth
        AND p.year_of_birth != 0 -- leave out persons with unknown year of birth
        -- for debugging: take only persons with breast cancer (C50) diagnosis
        -- AND person_id IN (SELECT DISTINCT person_id FROM @resultsSchema.condition_occurrence_brunak where condition_source_value like 'C50%');
;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXmycohort) AS VARCHAR),' rows in @resultsSchema.@prefiXmycohort'));

---------------------------------------------------------------------------------------------
-- Create an events table.
-- Keeping only the first occurrence of each event type for each eventperiod
---------------------------------------------------------------------------------------------
INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXevents...');


IF OBJECT_ID('@resultsSchema.@prefiXevents', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents;

    SELECT * INTO
    @resultsSchema.@prefiXevents
    FROM

    (
    -- conditions
    SELECT
      e.person_id                  AS person_id,
      c.eventperiod_id             AS eventperiod_id,
      e.condition_source_value       AS CONCEPT_ID,
      MIN(e.condition_start_date)  AS date -- This is min date per one event-period (for patients with multiple event-periods, there are several min dates)
     -- for CDM 6 use condition_start_datetime?
    FROM @resultsSchema.condition_occurrence_brunak e
    INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.condition_start_date)>=c.eventperiod_start_date } AND e.condition_start_date<=c.eventperiod_end_date
    WHERE
      1=@addConditions -- if addConditions is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      --AND
      --LENGTH(e.condition_concept_id!=0
    GROUP BY c.eventperiod_id,e.person_id,e.condition_source_value

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- observations
    SELECT
      e.person_id               AS person_id,
      c.eventperiod_id          AS eventperiod_id,
      e.observation_source_value  AS CONCEPT_ID,
      min(e.observation_date)   AS date
    FROM @resultsSchema.observation_brunak e
    INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.observation_date)>=c.eventperiod_start_date } AND e.observation_date>=c.eventperiod_start_date AND e.observation_date<=c.eventperiod_end_date
    WHERE
      1=@addObservations -- if addObservations is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      --AND
      --observation_concept_id!=0
      --AND
      --oc.concept_class_id != 'Procedure' -- do not include Observations having class "procedure" to prevent events like 'Hospital admission', 'Admission by GP' etc as if one is interested in them, he/she should look at the visits instead
    GROUP BY c.eventperiod_id,e.person_id,e.observation_source_value

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- procedures
    SELECT
      e.person_id             AS person_id,
      c.eventperiod_id        AS eventperiod_id,
      e.procedure_source_value AS CONCEPT_ID,
      min(e.procedure_date)   AS date
    FROM @resultsSchema.procedure_occurrence_brunak e
    -- note that the same event may belong to several event periods. It gets multiplied here while doing this INNER JOIN
    INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.procedure_date)>=c.eventperiod_start_date } AND e.procedure_date>=c.eventperiod_start_date AND e.procedure_date<=c.eventperiod_end_date
    WHERE
      1=@addProcedures -- if addProcedures is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
    GROUP BY c.eventperiod_id,e.procedure_source_value,e.person_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- drug eras
    SELECT
      e.person_id                 AS person_id,
      c.eventperiod_id            AS eventperiod_id,
      e.drug_concept_id           AS CONCEPT_ID,
      min(e.drug_era_start_date)  AS date
    FROM @cdmDatabaseSchema.drug_era e
    -- note that the same event may belong to several event periods. It gets multiplied here while doing this INNER JOIN
    INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.drug_era_start_date)>=c.eventperiod_start_date } AND e.drug_era_start_date>=c.eventperiod_start_date AND e.drug_era_start_date<=c.eventperiod_end_date
    WHERE
      1=@addDrugEras -- if addDrugEras is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      drug_concept_id!=0
    GROUP BY c.eventperiod_id,e.drug_concept_id,e.person_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)


    -- deaths
    SELECT
      e.person_id           AS person_id,
      c.eventperiod_id      AS eventperiod_id,
      'Y99'              AS CONCEPT_ID, -- this is a death event
      MIN(CAST(e.death_date AS DATE)) AS date --death.death_datetime not required in CDM 5.1, death.death_date used instead
    FROM @cdmDatabaseSchema.death e
    -- note that the same event may belong to several event periods. It gets multiplied here while doing this INNER JOIN
    INNER JOIN @resultsSchema.@prefiXmycohort c on e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,CAST(e.death_date AS DATE))>=c.eventperiod_start_date } AND CAST(e.death_date AS DATE)>=c.eventperiod_start_date AND CAST(e.death_date AS DATE)<=c.eventperiod_end_date
    WHERE
      1=@addDeaths -- if addDeaths is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      e.death_date IS NOT NULL
    GROUP BY c.eventperiod_id,e.person_id

    ) SSS

;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXevents) AS VARCHAR),' rows in @resultsSchema.@prefiXevents'));


---------------------------------------------------------------------------------------------
-- Mark events in event periods with only one event as "to_be_skipped_from_making_pairs" since we are looking for pairs of events
---------------------------------------------------------------------------------------------

ALTER TABLE @resultsSchema.@prefiXevents ADD SINGLE_EVENT_EVENTPERIOD INT; -- default: no skip
UPDATE @resultsSchema.@prefiXevents SET SINGLE_EVENT_EVENTPERIOD=0;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXsingle_event_eventperiods...');

IF OBJECT_ID('@resultsSchema.@prefiXsingle_event_eventperiods', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXsingle_event_eventperiods;

--CREATE TABLE  @resultsSchema.@prefiXsingle_event_eventperiods AS
    SELECT
        eventperiod_id,
        COUNT(CONCEPT_ID) AS no_events
    INTO @resultsSchema.@prefiXsingle_event_eventperiods
    FROM @resultsSchema.@prefiXevents
    GROUP BY eventperiod_id
    HAVING COUNT(CONCEPT_ID)=1;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXsingle_event_eventperiods) AS VARCHAR),' rows in @resultsSchema.@prefiXsingle_event_eventperiods (removing them from @resultsSchema.@prefiXevents and @resultsSchema.@prefiXmycohort)'));


--DELETE FROM @resultsSchema.@prefiXevents
UPDATE @resultsSchema.@prefiXevents
SET
  SINGLE_EVENT_EVENTPERIOD=1
WHERE eventperiod_id IN (SELECT eventperiod_id FROM @resultsSchema.@prefiXsingle_event_eventperiods);

--DELETE FROM @resultsSchema.@prefiXmycohort
--WHERE eventperiod_id IN (SELECT eventperiod_id FROM @resultsSchema.@prefiXsingle_event_eventperiods);


IF OBJECT_ID('@resultsSchema.@prefiXsingle_event_eventperiods', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXsingle_event_eventperiods;


---------------------------------------------------------------------------------------------
-- Calculate day-difference between the obs. period start and event date
---------------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXdaydiff_from_beginning_of_op...');

IF OBJECT_ID('@resultsSchema.@prefiXdaydiff_from_beginning_of_op', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXdaydiff_from_beginning_of_op;

SELECT e.person_id,
       e.concept_id,
       min(
          CASE WHEN
                DATEDIFF(DAY, OP.OBSERVATION_PERIOD_START_DATE, e.date) >=0
                THEN
                DATEDIFF(DAY, OP.OBSERVATION_PERIOD_START_DATE, e.date)
               ELSE
                99999
           END

       ) AS daydiff_from_beginning_of_op -- event is too close to the beginning of observation period (might not be the first event of that type)
    INTO @resultsSchema.@prefiXdaydiff_from_beginning_of_op
    FROM  @resultsSchema.@prefiXevents e
        -- join observation period to know how far the event is from the beginning of observation period
        -- By CDM rules the observation periods of the same patients should not overlap which means that no need to do a full join.
        -- However, to prevent any problems in this matter lets write the code in a way that it will not break (full join)
        JOIN @cdmDatabaseSchema.observation_period OP on e.person_id = OP.person_id
    GROUP BY e.person_id, e.concept_id
;


-- putting all together

IF OBJECT_ID('@resultsSchema.@prefiXevents_in_eventperiods', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents_in_eventperiods;

SELECT
  e.*,
  CASE WHEN p.gender_concept_id = 8532 THEN 'F'
               WHEN p.gender_concept_id = 8507 THEN 'M'
               END AS gender,
  p.year_of_birth,
  o.daydiff_from_beginning_of_op,
  YEAR(e.date)-p.year_of_birth AS age
INTO
  @resultsSchema.@prefiXevents_in_eventperiods
FROM @resultsSchema.@prefiXevents e
  LEFT JOIN @resultsSchema.@prefiXdaydiff_from_beginning_of_op o ON  o.person_id=e.person_id AND o.concept_id=e.concept_id
  LEFT JOIN @cdmDatabaseSchema.person p ON p.person_id=e.person_id
;




INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXevents_in_eventperiods) AS VARCHAR),' rows in @resultsSchema.@prefiXevents_in_eventperiods'));





------------------------------------------------------------------------------------
-- Adding information how many people have an event
------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXevent_counts_in_eventperiods...');

IF OBJECT_ID('@resultsSchema.@prefiXevent_counts_in_eventperiods', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevent_counts_in_eventperiods;

--CREATE TABLE  @resultsSchema.@prefiXevent_counts_in_eventperiods AS
SELECT CONCEPT_ID,
       COUNT(eventperiod_id)  AS event_counts_in_eventperiods
INTO @resultsSchema.@prefiXevent_counts_in_eventperiods
FROM @resultsSchema.@prefiXevents_in_eventperiods
GROUP BY CONCEPT_ID;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXevent_counts_in_eventperiods) AS VARCHAR),' rows in @resultsSchema.@prefiXevent_counts_in_eventperiods'));


------------------------------------------------------------------------------------
-- Event pair creation
-- Creates all event pairs for each eventperiod
-- but limits it to min...max window between them
------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXpairs...');

IF OBJECT_ID('@resultsSchema.@prefiXpairs', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs;

--CREATE TABLE  @resultsSchema.@prefiXpairs AS
    SELECT  a.eventperiod_id,
            a.gender,

            a.age,
            a.CONCEPT_ID AS E1_CONCEPT_ID,
            a.date as E1_DATE,
            b.CONCEPT_ID AS E2_CONCEPT_ID,
            b.date as E2_DATE,
            DATEDIFF(DAY,
                    a.date,
                    b.date) AS diff_days
    INTO @resultsSchema.@prefiXpairs
    FROM
           @resultsSchema.@prefiXevents_in_eventperiods a
           JOIN  @resultsSchema.@prefiXevents_in_eventperiods b -- full join!
              ON  a.eventperiod_id = b.eventperiod_id
                  AND a.CONCEPT_ID != b.CONCEPT_ID
    WHERE
      a.SINGLE_EVENT_EVENTPERIOD=0 AND b.SINGLE_EVENT_EVENTPERIOD=0 --skip eventperiods with single events
      --AND
      --(b.age<=1 OR b.daydiff_from_beginning_of_op>=365) --second event not happening within 1st year of observation period (unless close to birth) <- because it is possible that this is not actually the first occurrence of that event (the first occurrence was happening before observation period, especially valid for chronic diseases)
      AND
      DATEDIFF(DAY,
                    a.date,
                    b.date) >= @minimumDaysBetweenEvents -- this also requires that event b is always no later than event a. Seems that minimum allowed value for @minimumDaysBetweenEvents is 0.
      AND DATEDIFF(DAY,
                    a.date,
                    b.date) <= @maximumDaysBetweenEvents;

-- Seems that these indexes do not help much
-- CREATE INDEX @prefiXpairs_E1_idx ON @resultsSchema.@prefiXpairs(event1_concept_id);
-- CREATE INDEX @prefiXpairs_E2_idx ON @resultsSchema.@prefiXpairs(event2_concept_id);
-- CREATE INDEX @prefiXpairs_E1E2_idx ON o@resultsSchema.@prefiXpairs(event1_concept_id,event2_concept_id);

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXpairs) AS VARCHAR),' rows in @resultsSchema.@prefiXpairs'));

