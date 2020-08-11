---------------------------------
-- This scripts creates 2 tables:
-- 1) @resultsSchema.@prefiXpairs - all event pairs for each person
-- 2) @resultsSchema.@prefiXD1D2_model - contains all event pair counts + empty columns for p-val calculations
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


---------------------------------------------------------------------------------------
-- Create a temporary "etcohort" table - it is basically the same table & content as the cohort in OMOP cohorts table but
-- some additional fields are added (gender, year or birth)
-- ordinal numbers are added (id-s) for each cohort
-- and people with unknown gender and birthyear are removed
---------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXetcohort...');

IF OBJECT_ID('@resultsSchema.@prefiXmycohort', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXetcohort;

CREATE TABLE @resultsSchema.@prefiXetcohort AS
    SELECT
           ROW_NUMBER() OVER (
            ORDER BY cohort.subject_id, cohort.cohort_start_date
            ) AS cohort_id,
           p.person_id,
           CASE WHEN p.gender_concept_id = 8532 THEN 'F'
               WHEN p.gender_concept_id = 8507 THEN 'M'
               END AS gender,
           YEAR(p.birth_datetime) AS year_of_birth,
           cohort.cohort_start_date AS cohort_start_date,
           cohort.cohort_end_date AS cohort_end_date
    FROM
        @cohortTableSchema.@cohortTable AS cohort
        LEFT JOIN @cdmDatabaseSchema.person p on cohort.subject_id=p.person_id
    WHERE
        cohort.cohort_definition_id=@cohortId
        AND p.gender_concept_id IS NOT NULL -- leave out persons with unknown gender
        AND p.gender_concept_id != 0 -- leave out persons with unknown gender
        AND p.year_of_birth IS NOT NULL -- leave out persons with unknown year of birth
        AND p.year_of_birth != 0 -- leave out persons with unknown year of birth
        -- for debugging: take only persons with breast cancer (C50) diagnosis
        -- AND person_id IN (SELECT DISTINCT person_id FROM @cdmDatabaseSchema.condition_occurrence where condition_source_value like 'C50%');
;

---------------------------------------------------------------------------------------------
-- Create an events table.
-- Keeping only the first occurrence of each event type for each cohort
---------------------------------------------------------------------------------------------
INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXevents...');


IF OBJECT_ID('@resultsSchema.@prefiXevents', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents;
CREATE TABLE @resultsSchema.@prefiXevents AS

    -- conditions
    SELECT
      c.cohort_id                  AS cohort_id,
      e.condition_concept_id       AS dgn,
      MIN(e.condition_start_date)  AS date -- This is min date per one cohort (for patients with multiple cohorts, there are several min dates)
    FROM @cdmDatabaseSchema.condition_occurrence e
    INNER JOIN @resultsSchema.@prefiXetcohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.condition_start_date)>=c.cohort_start_date } AND e.condition_start_date<=c.cohort_end_date
    WHERE
      TRUE=@addConditions -- if addConditions is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      e.condition_concept_id!=0
    GROUP BY c.cohort_id,e.condition_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- observations
    SELECT
      c.cohort_id              AS cohort_id,
      e.observation_concept_id  AS dgn,
      min(e.observation_date)   AS date
    FROM @cdmDatabaseSchema.observation e
    INNER JOIN @resultsSchema.@prefiXetcohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.condition_start_date)>=c.cohort_start_date } AND e.observation_date>=c.cohort_start_date AND e.observation_date<=c.cohort_end_date
    WHERE
      TRUE=@addObservations -- if addObservations is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      observation_concept_id!=0
    GROUP BY c.cohort_id,observation_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- procedures
    SELECT
      c.cohort_id          AS cohort_id,
      e.procedure_concept_id  AS dgn,
      min(e.procedure_date) AS date
    FROM @cdmDatabaseSchema.procedure_occurrence e
    INNER JOIN @resultsSchema.@prefiXetcohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.condition_start_date)>=c.cohort_start_date } AND e.procedure_date>=c.cohort_start_date AND e.procedure_date<=c.cohort_end_date
    WHERE
      TRUE=@addProcedures -- if addProcedures is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      procedure_concept_id!=0
    GROUP BY c.cohort_id,e.procedure_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- drugs
    SELECT
      c.cohort_id                      AS cohort_id,
      e.drug_concept_id                 AS dgn,
      min(e.drug_exposure_start_date)   AS date
    FROM @cdmDatabaseSchema.drug_exposure e
    INNER JOIN @resultsSchema.@prefiXetcohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.condition_start_date)>=c.cohort_start_date } AND e.drug_exposure_start_date>=c.cohort_start_date AND e.drug_exposure_start_date<=c.cohort_end_date
    WHERE
      TRUE=@addDrugExposures -- if addDrugExposures is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      drug_concept_id!=0
    GROUP BY c.cohort_id,e.drug_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- drug eras
    SELECT
      c.cohort_id                      AS cohort_id,
      e.drug_concept_id                 AS dgn,
      min(e.drug_era_start_date)   AS date
    FROM @cdmDatabaseSchema.drug_era e
    INNER JOIN @resultsSchema.@prefiXetcohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.condition_start_date)>=c.cohort_start_date } AND e.drug_era_start_date>=c.cohort_start_date AND e.drug_era_start_date<=c.cohort_end_date
    WHERE
      TRUE=@addDrugEras -- if addDrugEras is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      drug_concept_id!=0
    GROUP BY c.cohort_id,e.drug_concept_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- births
     SELECT
      c.cohort_id                   AS cohort_id,
      4216316                       AS dgn, -- this is a birth event
      MIN(e.birth_datetime::date)   AS date
     FROM @cdmDatabaseSchema.person e
     INNER JOIN @resultsSchema.@prefiXetcohort c ON e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.condition_start_date)>=c.cohort_start_date } AND e.birth_datetime::date>=c.cohort_start_date AND e.birth_datetime::date<=c.cohort_end_date
     WHERE
      TRUE=@addBirths -- if addBirths is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      e.birth_datetime IS NOT NULL
      --AND birth_datetime::date >= date '@earliestDate'
    GROUP BY c.cohort_id

    UNION ALL -- we use UNION ALL as it does not try to delete duplicates (faster) (although there cant be any anyways)

    -- deaths
    SELECT
      c.cohort_id                 AS cohort_id,
      40566982                    AS dgn, -- this is a death event
      MIN(e.death_datetime::date) AS date
    FROM @cdmDatabaseSchema.death e
    INNER JOIN @resultsSchema.@prefiXetcohort c on e.person_id=c.person_id {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.condition_start_date)>=c.cohort_start_date } AND e.death_datetime::date>=c.cohort_start_date AND e.death_datetime::date<=c.cohort_end_date
    WHERE
      TRUE=@addDeaths -- if addDeaths is TRUE, then this UNION is ADDED, otherwise this query give 0 rows as result
      AND
      e.death_datetime IS NOT NULL
    GROUP BY c.cohort_id

;


---------------------------------------------------------------------------------------------
-- Remove cohorts with only one event since we are looking for event pairs
---------------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXsingle_event_cohorts...');

IF OBJECT_ID('@resultsSchema.@prefiXsingle_event_cohorts', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXsingle_event_cohorts;

CREATE TABLE  @resultsSchema.@prefiXsingle_event_cohorts AS
    SELECT
        cohort_id,
        COUNT(dgn) AS no_events
    FROM @resultsSchema.@prefiXevents
    GROUP BY cohort_id
    HAVING COUNT(dgn)=1;

DELETE FROM @resultsSchema.@prefiXevents e
WHERE EXISTS
  (SELECT 1
    FROM @resultsSchema.@prefiXsingle_event_cohorts d
    WHERE e.cohort_id = d.cohort_id);

DELETE FROM @resultsSchema.@prefiXetcohort e
WHERE EXISTS
  (SELECT 1
    FROM @resultsSchema.@prefiXsingle_event_cohorts d
    WHERE e.cohort_id = d.cohort_id);


IF OBJECT_ID('@resultsSchema.@prefiXsingle_event_cohorts', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXsingle_event_cohorts;


---------------------------------------------------------------------------------------------
-- Adding gender and year of birth to cohort events (new table events_cohort)
---------------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXevents_cohort...');

IF OBJECT_ID('@resultsSchema.@prefiXevents_cohort', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents_cohort;
CREATE TABLE  @resultsSchema.@prefiXevents_cohort AS
    SELECT e.*,
           p.gender,
           p.year_of_birth
    FROM  @resultsSchema.@prefiXevents AS e
        INNER JOIN  @resultsSchema.@prefiXetcohort AS p
            ON e.cohort_id = p.cohort_id;


------------------------------------------------------------------------------------
-- Adding age during event
------------------------------------------------------------------------------------

ALTER TABLE  @resultsSchema.@prefiXevents_cohort ADD COLUMN age INTEGER;
UPDATE  @resultsSchema.@prefiXevents_cohort SET
    age= YEAR(date)- year_of_birth;

------------------------------------------------------------------------------------
-- Adding year and month of event
------------------------------------------------------------------------------------

ALTER TABLE  @resultsSchema.@prefiXevents_cohort ADD COLUMN discharge_time VARCHAR(6);
UPDATE  @resultsSchema.@prefiXevents_cohort SET
    discharge_time = CONCAT(YEAR(date), MONTH(date));


------------------------------------------------------------------------------------
-- Adding information how many people have an event
------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXevent_counts...');

IF OBJECT_ID('@resultsSchema.@prefiXevent_counts', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevent_counts;

CREATE TABLE  @resultsSchema.@prefiXevent_counts AS
SELECT dgn,
       COUNT(cohort_id)  AS event_counts
FROM @resultsSchema.@prefiXevents_cohort
GROUP BY dgn;

------------------------------------------------------------------------------------
-- Event pair creation
-- Creates all event pairs for each cohort
-- but limits it to min...max window between them
------------------------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXpairs...');

IF OBJECT_ID('@resultsSchema.@prefiXpairs', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs;

CREATE TABLE  @resultsSchema.@prefiXpairs AS
    SELECT  a.cohort_id,
            a.gender,

            -- vanemad iga 10a kaupa
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

            -- vanemad iga 5a kaupa
          /*case when a.age <= 1 then '0-1'
                  when a.age <= 5 then '2-5'
                  when a.age <= 10 then '6-10'
                  when a.age <= 15 then '11-15'
                  when a.age <= 20 then '16-20'
                  when a.age <= 25 then '21-25'
                  when a.age <= 30 then '26-30'
                  when a.age <= 35 then '31-35'
                  when a.age <= 40 then '36-40'
                  when a.age <= 45 then '41-45'
                  when a.age <= 50 then '46-50'
                  when a.age <= 55 then '51-55'
                  when a.age <= 60 then '56-60'
                  when a.age <= 65 then '61-65'
                  when a.age <= 70 then '66-70'
                  when a.age <= 75 then '71-75'
                  when a.age <= 80 then '76-80'
                  when a.age > 80 then '80+'
                  end as age,*/
                  --a.age,

            a.discharge_time,
            a.dgn as event1_concept_id,
            -- a.date  as date1, -- commented out as this date is never used afterwards
            b.dgn as event2_concept_id,
            -- b.date as date2 -- commented out as this date is never used afterwards
            DATEDIFF(DAY,
                    a.date,
                    b.date) AS diff_days
    FROM
           @resultsSchema.@prefiXevents_cohort a
           JOIN  @resultsSchema.@prefiXevents_cohort b -- full join!
              ON  a.cohort_id = b.cohort_id
                  AND a.dgn != b.dgn
    WHERE
      DATEDIFF(DAY,
                    a.date,
                    b.date) >= @minimumDaysBetweenEvents -- this also requires that event b is always no later than event a. Seems that minimum allowed value for @minimumDaysBetweenEvents is 0.
      AND DATEDIFF(DAY,
                    a.date,
                    b.date) <= @maximumDaysBetweenEvents;

-- Seems that these indexes do not help much
-- CREATE INDEX @prefiXpairs_d1_idx ON @resultsSchema.@prefiXpairs(event1_concept_id);
-- CREATE INDEX @prefiXpairs_d2_idx ON @resultsSchema.@prefiXpairs(event2_concept_id);
-- CREATE INDEX @prefiXpairs_d1d2_idx ON o@resultsSchema.@prefiXpairs(event1_concept_id,event2_concept_id);



---------------------------------------------------------------------
-- Create separate table for event pairs that are going to be analysed
-- and empty slots for p and effect values
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXD1D2_model...');

IF OBJECT_ID('@resultsSchema.@prefiXD1D2_model', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXD1D2_model;

CREATE TABLE @resultsSchema.@prefiXD1D2_model as
    SELECT
            event1_concept_id,
            event2_concept_id,
            ROUND(AVG(diff_days)) AS AVG_NUMBER_OF_DAYS_BETWEEN_EVENTS,
            count(*) AS EVENT1_EVENT2_COHORT_COUNT
    FROM
     @resultsSchema.@prefiXpairs
    GROUP BY
      event1_concept_id,
      event2_concept_id
    ORDER BY
      event1_concept_id,
      event2_concept_id;

DELETE FROM @resultsSchema.@prefiXD1D2_model where EVENT1_EVENT2_COHORT_COUNT< @minPatientsPerEventPair; -- Minimum limit
-- It is correct to not delete ther corresponding rows from @resultsSchema.@prefiXpairs as these rows can still be used for controls

ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN EVENT1_NAME VARCHAR(255);
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN EVENT1_DOMAIN VARCHAR(20);
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN EVENT2_NAME VARCHAR(255);
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN EVENT2_DOMAIN VARCHAR(20);
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN EVENT_PAIR_PVALUE FLOAT;
ALTER TABLE @resultsSchema.@prefiXd1d2_model ADD COLUMN EVENT_PAIR_EFFECT DECIMAL;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN COHORT_COUNT_EVENT1_OCCURS_FIRST INT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN AVG_AGE_OF_COHORT_EVENT1_OCCURS_FIRST FLOAT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN MIN_AGE_OF_COHORT_EVENT1_OCCURS_FIRST FLOAT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN Q25_AGE_OF_COHORT_EVENT1_OCCURS_FIRST FLOAT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN MEDIAN_AGE_OF_COHORT_EVENT1_OCCURS_FIRST FLOAT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN Q75_AGE_OF_COHORT_EVENT1_OCCURS_FIRST FLOAT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN MAX_AGE_OF_COHORT_EVENT1_OCCURS_FIRST FLOAT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN COHORT_COUNT_EVENT2_OCCURS_FIRST INT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN COHORT_COUNT_EVENT1_EVENT2_OCCUR_ON_SAME_DAY INT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN DIRECTIONAL_EVENT_PAIR_PVALUE FLOAT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN EVENT1_COUNT INT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN EVENT2_COUNT INT;
ALTER TABLE @resultsSchema.@prefiXD1D2_model ADD COLUMN COHORT_COUNT_HAVING_E2_RIGHT_AFTER_E1 INT; --this field is filled after the analysis when we consider only trajectories and events that are significant


CREATE INDEX @prefiXD1D2_model_d1_idx ON @resultsSchema.@prefiXD1D2_model(event1_concept_id);
CREATE INDEX @prefiXD1D2_model_d2_idx ON @resultsSchema.@prefiXD1D2_model(event2_concept_id);
CREATE INDEX @prefiXD1D2_model_d1d2_idx ON @resultsSchema.@prefiXD1D2_model(event1_concept_id,event2_concept_id);

--- Adding event counts to D1D2_model
UPDATE @resultsSchema.@prefiXD1D2_model
SET EVENT1_COUNT = (SELECT event_counts FROM @resultsSchema.@prefiXevent_counts WHERE dgn = event1_concept_id);

UPDATE @resultsSchema.@prefiXD1D2_model
SET EVENT2_COUNT = (SELECT event_counts FROM @resultsSchema.@prefiXevent_counts WHERE dgn = event2_concept_id);

--- Instead of using concept id-s, take concept names + concept domains to d1d2_model from concept table
UPDATE @resultsSchema.@prefiXd1d2_model
SET EVENT1_NAME = concept.concept_name, EVENT1_DOMAIN=concept.domain_id
FROM @vocabDatabaseSchema.concept
WHERE @prefiXd1d2_model.event1_concept_id=concept.concept_id;

UPDATE @resultsSchema.@prefiXd1d2_model
SET EVENT2_NAME = concept.concept_name, EVENT2_DOMAIN=concept.domain_id
FROM @vocabDatabaseSchema.concept
WHERE @prefiXd1d2_model.event2_concept_id=concept.concept_id;


---------------------------------------------------------------------
-- Create gender,age,discharge-time summary table for each event1_concept_id->event2_concept_id pairs
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXd1d2_summary...');

IF OBJECT_ID('@resultsSchema.@prefiXd1d2_summary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXd1d2_summary;

CREATE TABLE @resultsSchema.@prefiXd1d2_summary as
    SELECT
      a.event1_concept_id,
      a.event2_concept_id,
      gender,
      age,
      discharge_time,
      count(*) AS cohort_count -- as there can't be duplicate event1_concept_id->event2_concept_id event pairs per cohorts, count(*) in this table gives the cohort count
    FROM @resultsSchema.@prefiXpairs a
      INNER JOIN @resultsSchema.@prefiXD1D2_model b ON a.event1_concept_id=b.event1_concept_id AND a.event2_concept_id=b.event2_concept_id -- limit the table to event pairs only that are going to be analyzed
    GROUP BY a.event1_concept_id, a.event2_concept_id, gender, age, discharge_time
    ORDER BY event1_concept_id, event2_concept_id, gender, age, discharge_time;


---------------------------------------------------------------------
-- Create gender,age,discharge-time summary table for all event pairs
----------------------------------------------------------------------


INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXsummary...');

IF OBJECT_ID('@resultsSchema.@prefiXsummary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXsummary;

CREATE TABLE @resultsSchema.@prefiXsummary as
    SELECT
      gender,
      age,
      discharge_time,
      count(DISTINCT cohort_id) AS cohort_count
    FROM @resultsSchema.@prefiXpairs
    GROUP BY gender, age, discharge_time
    ORDER BY gender, age, discharge_time;

-------------------------------------------------------

---------------------------------------------------------------------
-- Create gender,age,discharge-time summary table for event1_concept_id
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXd1_summary...');

IF OBJECT_ID('@resultsSchema.@prefiXd1_summary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXd1_summary;

CREATE TABLE @resultsSchema.@prefiXd1_summary as
    SELECT
      a.event1_concept_id,
      gender,
      age,
      discharge_time,
      count(DISTINCT cohort_id) AS cohort_count
    FROM @resultsSchema.@prefiXpairs a
    INNER JOIN (SELECT DISTINCT event1_concept_id FROM @resultsSchema.@prefiXD1D2_model) b ON a.event1_concept_id=b.event1_concept_id -- limit the table to event pairs only that are going to be analyzed
    GROUP BY a.event1_concept_id, gender, age, discharge_time;


---------------------------------------------------------------------
-- Create gender,age,discharge-time summary table for event2_concept_id
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXd2_summary...');

IF OBJECT_ID('@resultsSchema.@prefiXd2_summary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXd2_summary;
CREATE TABLE @resultsSchema.@prefiXd2_summary as
    SELECT
      a.event2_concept_id,
      gender,
      age,
      discharge_time,
      count(DISTINCT cohort_id) AS cohort_count
    FROM @resultsSchema.@prefiXpairs a
    INNER JOIN (SELECT DISTINCT event2_concept_id FROM @resultsSchema.@prefiXD1D2_model) b ON a.event2_concept_id=b.event2_concept_id -- limit the table to event pairs only that are going to be analyzed
    GROUP BY a.event2_concept_id, gender, age, discharge_time;

-------------------------------------------------------

