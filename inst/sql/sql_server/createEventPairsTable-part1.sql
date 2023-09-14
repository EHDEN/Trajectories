---------------------------------
-- This scripts creates 2 tables:
-- 1) @resultsSchema.@prefiXpairs - all event pairs for each person
-- 2) @resultsSchema.@prefiXE1E2_model - contains all event pair counts + empty columns for p-val calculations
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
        COUNT(COHORT_ID) AS no_events
    INTO @resultsSchema.@prefiXsingle_event_eventperiods
    FROM @resultsSchema.@prefiXevents
    GROUP BY eventperiod_id
    HAVING COUNT(COHORT_ID)=1;

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
       e.COHORT_ID,
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
    GROUP BY e.person_id, e.COHORT_ID
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
  LEFT JOIN @resultsSchema.@prefiXdaydiff_from_beginning_of_op o ON  o.person_id=e.person_id AND o.COHORT_ID=e.COHORT_ID
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
SELECT COHORT_ID,
       COUNT(eventperiod_id)  AS event_counts_in_eventperiods
INTO @resultsSchema.@prefiXevent_counts_in_eventperiods
FROM @resultsSchema.@prefiXevents_in_eventperiods
GROUP BY COHORT_ID;

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
            a.COHORT_ID AS E1_COHORT_ID,
            a.date as E1_DATE,
            b.COHORT_ID AS E2_COHORT_ID,
            b.date as E2_DATE,
            DATEDIFF(DAY,
                    a.date,
                    b.date) AS diff_days
    INTO @resultsSchema.@prefiXpairs
    FROM
           @resultsSchema.@prefiXevents_in_eventperiods a
           JOIN  @resultsSchema.@prefiXevents_in_eventperiods b -- full join!
              ON  a.eventperiod_id = b.eventperiod_id
                  AND a.COHORT_ID != b.COHORT_ID
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
-- CREATE INDEX @prefiXpairs_E1_idx ON @resultsSchema.@prefiXpairs(event1_COHORT_ID);
-- CREATE INDEX @prefiXpairs_E2_idx ON @resultsSchema.@prefiXpairs(event2_COHORT_ID);
-- CREATE INDEX @prefiXpairs_E1E2_idx ON o@resultsSchema.@prefiXpairs(event1_COHORT_ID,event2_COHORT_ID);

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXpairs) AS VARCHAR),' rows in @resultsSchema.@prefiXpairs'));
