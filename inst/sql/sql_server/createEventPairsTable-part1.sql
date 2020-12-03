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
INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('Database size: There are ',CAST((SELECT COUNT(*) FROM @cdmDatabaseSchema.condition_occurrence) AS VARCHAR),' rows in @cdmDatabaseSchema.condition_occurrence'));



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
        -- AND person_id IN (SELECT DISTINCT person_id FROM @cdmDatabaseSchema.condition_occurrence where condition_source_value like 'C50%');
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
      c.eventperiod_id             AS eventperiod_id,
      e.condition_concept_id       AS CONCEPT_ID,
      MIN(e.condition_start_date)  AS date -- This is min date per one event-period (for patients with multiple event-periods, there are several min dates)
     -- for CDM 6 use condition_start_datetime?
    FROM @cdmDatabaseSchema.condition_occurrence e
    INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.condition_start_date)>=c.eventperiod_start_date } AND e.condition_start_date<=c.eventperiod_end_date
    WHERE
      1=@addConditions -- if addConditions is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      e.condition_concept_id!=0
    GROUP BY c.eventperiod_id,e.condition_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- observations
    SELECT
      c.eventperiod_id          AS eventperiod_id,
      e.observation_concept_id  AS CONCEPT_ID,
      min(e.observation_date)   AS date
    FROM @cdmDatabaseSchema.observation e
    INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.observation_date)>=c.eventperiod_start_date } AND e.observation_date>=c.eventperiod_start_date AND e.observation_date<=c.eventperiod_end_date
    WHERE
      1=@addObservations -- if addObservations is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      observation_concept_id!=0
    GROUP BY c.eventperiod_id,observation_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- procedures
    SELECT
      c.eventperiod_id        AS eventperiod_id,
      e.procedure_concept_id  AS CONCEPT_ID,
      min(e.procedure_date)   AS date
    FROM @cdmDatabaseSchema.procedure_occurrence e
    INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.procedure_date)>=c.eventperiod_start_date } AND e.procedure_date>=c.eventperiod_start_date AND e.procedure_date<=c.eventperiod_end_date
    WHERE
      1=@addProcedures -- if addProcedures is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      procedure_concept_id!=0
    GROUP BY c.eventperiod_id,e.procedure_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- drugs
    SELECT
      c.eventperiod_id                  AS eventperiod_id,
      e.drug_concept_id                 AS CONCEPT_ID,
      min(e.drug_exposure_start_date)   AS date
    FROM @cdmDatabaseSchema.drug_exposure e
    INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.drug_exposure_start_date)>=c.eventperiod_start_date } AND e.drug_exposure_start_date>=c.eventperiod_start_date AND e.drug_exposure_start_date<=c.eventperiod_end_date
    WHERE
      1=@addDrugExposures -- if addDrugExposures is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      drug_concept_id!=0
    GROUP BY c.eventperiod_id,e.drug_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- drug eras
    SELECT
      c.eventperiod_id            AS eventperiod_id,
      e.drug_concept_id           AS CONCEPT_ID,
      min(e.drug_era_start_date)  AS date
    FROM @cdmDatabaseSchema.drug_era e
    INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.drug_era_start_date)>=c.eventperiod_start_date } AND e.drug_era_start_date>=c.eventperiod_start_date AND e.drug_era_start_date<=c.eventperiod_end_date
    WHERE
      1=@addDrugEras -- if addDrugEras is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      drug_concept_id!=0
    GROUP BY c.eventperiod_id,e.drug_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- births
     SELECT
      c.eventperiod_id          AS eventperiod_id,
      4216316                   AS CONCEPT_ID, -- this is a birth event
      MIN(CAST(e.birth_datetime AS DATE))   AS date
     FROM @cdmDatabaseSchema.person e
     INNER JOIN @resultsSchema.@prefiXmycohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,CAST(e.birth_datetime AS DATE))>=c.eventperiod_start_date } AND CAST(e.birth_datetime AS DATE)>=c.eventperiod_start_date AND CAST(e.birth_datetime AS DATE)<=c.eventperiod_end_date
     WHERE
      1=@addBirths -- if addBirths is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      e.birth_datetime IS NOT NULL
    GROUP BY c.eventperiod_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- deaths
    SELECT
      c.eventperiod_id      AS eventperiod_id,
      40566982              AS CONCEPT_ID, -- this is a death event
      MIN(CAST(e.death_date AS DATE)) AS date --death.death_datetime not required in CDM 5.1, death.death_date used instead
    FROM @cdmDatabaseSchema.death e
    INNER JOIN @resultsSchema.@prefiXmycohort c on e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,CAST(e.death_date AS DATE))>=c.eventperiod_start_date } AND CAST(e.death_date AS DATE)>=c.eventperiod_start_date AND CAST(e.death_date AS DATE)<=c.eventperiod_end_date
    WHERE
      1=@addDeaths -- if addDeaths is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      e.death_date IS NOT NULL
    GROUP BY c.eventperiod_id

    ) SSS

;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXevents) AS VARCHAR),' rows in @resultsSchema.@prefiXevents'));


---------------------------------------------------------------------------------------------
-- Remove event periods with only one event since we are looking for event pairs
---------------------------------------------------------------------------------------------

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

DELETE FROM @resultsSchema.@prefiXevents
WHERE eventperiod_id IN (SELECT eventperiod_id FROM @resultsSchema.@prefiXsingle_event_eventperiods);

DELETE FROM @resultsSchema.@prefiXmycohort
WHERE eventperiod_id IN (SELECT eventperiod_id FROM @resultsSchema.@prefiXsingle_event_eventperiods);


IF OBJECT_ID('@resultsSchema.@prefiXsingle_event_eventperiods', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXsingle_event_eventperiods;




---------------------------------------------------------------------------------------------
-- Adding gender and year of birth to eventperiod events (new table events_in_eventperiods)
---------------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXevents_in_eventperiods...');

IF OBJECT_ID('@resultsSchema.@prefiXevents_in_eventperiods', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents_in_eventperiods;
    SELECT e.*,
           p.gender,
           p.year_of_birth
    INTO @resultsSchema.@prefiXevents_in_eventperiods
    FROM  @resultsSchema.@prefiXevents e
        INNER JOIN  @resultsSchema.@prefiXmycohort p
            ON e.eventperiod_id = p.eventperiod_id;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXevents_in_eventperiods) AS VARCHAR),' rows in @resultsSchema.@prefiXevents_in_eventperiods'));


------------------------------------------------------------------------------------
-- Adding age during event
------------------------------------------------------------------------------------

ALTER TABLE  @resultsSchema.@prefiXevents_in_eventperiods
            ADD age int NULL;
UPDATE  @resultsSchema.@prefiXevents_in_eventperiods SET
    age = YEAR(date)-year_of_birth;

------------------------------------------------------------------------------------
-- Adding year and month of event
------------------------------------------------------------------------------------

ALTER TABLE  @resultsSchema.@prefiXevents_in_eventperiods
            ADD discharge_time VARCHAR(6) NULL;
UPDATE  @resultsSchema.@prefiXevents_in_eventperiods SET
    discharge_time = CONCAT(YEAR(date), MONTH(date));


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

           case when a.age <= 1 then '0-1'
                  when a.age <= 5 then '2-5'
                  when a.age <= 10 then '6-10'
                  when a.age <= 20 then '11-20'
                  when a.age <= 30 then '21-30'
                  when a.age <= 40 then '31-40'
                  when a.age <= 50 then '41-50'
                  when a.age <= 60 then '51-60'
                  when a.age <= 70 then '61-70'
                  when a.age <= 80 then '71-80'
                  when a.age > 80 then '80+'
                  end as age,

            a.discharge_time,
            a.CONCEPT_ID AS E1_CONCEPT_ID,
            -- a.date  as date1, -- commented out as this date is never used afterwards
            b.CONCEPT_ID AS E2_CONCEPT_ID,
            -- b.date as date2 -- commented out as this date is never used afterwards
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
