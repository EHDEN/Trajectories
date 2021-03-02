---------------------------------------------------------------------
-- Create separate table for event pairs that are going to be analysed
-- and empty slots for p and relative risk values
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXE1E2_model...');

IF OBJECT_ID('@resultsSchema.@prefiXE1E2_model_temp', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1E2_model_temp;

SELECT
        p.E1_CONCEPT_ID,
        p.E2_CONCEPT_ID,
        round(avg(p.diff_days),0) AS avg_number_of_days_between_events,
        count(*) AS E1_E2_EVENTPERIOD_COUNT
INTO @resultsSchema.@prefiXE1E2_model_temp
FROM
 @resultsSchema.@prefiXpairs p
INNER JOIN
  @resultsSchema.@prefiXE1E2_model_input i ON i.E1_CONCEPT_ID=p.E1_CONCEPT_ID AND i.E2_CONCEPT_ID=p.E2_CONCEPT_ID
GROUP BY
  p.E1_CONCEPT_ID,
  p.E2_CONCEPT_ID
ORDER BY
  p.E1_CONCEPT_ID,
  p.E2_CONCEPT_ID;


IF OBJECT_ID('@resultsSchema.@prefiXE1E2_model', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1E2_model;


-- In SQL Server, column order cannot be set later by ALTER TABLE, therefore we have to create the table with the right order from the beginning
CREATE TABLE @resultsSchema.@prefiXE1E2_model (
  E1_CONCEPT_ID INT NOT NULL,
  E2_CONCEPT_ID INT NOT NULL,
  E1_NAME VARCHAR(255) NULL,
  E2_NAME VARCHAR(255) NULL,
  E1_DOMAIN VARCHAR(20) NULL,
  E2_DOMAIN VARCHAR(20) NULL,

  E1_COUNT_IN_EVENTS INT NULL, -- this is the total number of eventperiods where E1 is present.
  E1_COUNT_IN_PAIRS INT NULL, -- this is the total number of eventperiods that contain E1 (either as E1->smht or smth->E1) The number is smaller than E1_COUNT_IN_EVENTS because there are additional restrictions for making pairs (e.g required day difference)
  E1_COUNT_AS_FIRST_EVENT_OF_PAIRS INT NULL, -- this is the total number of eventperiods containing E1->smth pair
  --E1_COUNT_AS_FIRST_EVENT_OF_EVENTPERIODS INT NULL, -- this is the total number of eventperiods having E1 as the very first event
  E1_COUNT_AS_LAST_EVENT_OF_EVENTPERIODS INT NULL, -- this is the total number of eventperiods having E1 as the very last event (these eventperiods do not produce any E1->smth pairs)

  E2_COUNT_IN_EVENTS INT NULL, -- this is the total number of eventperiods where E2 is present (does not equal to event pairs with E2 as single-event eventperiods do not form any pair).
  E2_COUNT_IN_PAIRS INT NULL,
  E2_COUNT_AS_SECOND_EVENT_OF_PAIRS INT NULL, -- this is the number of eventperiods containing event pairs where E2 is the second event
  E2_COUNT_AS_FIRST_EVENT_OF_EVENTPERIODS INT NULL, -- this is the total number of eventperiods having E2 as the very first event (these eventperiods do not produce any smth->E2 pairs)

  E2_PREVALENCE_IN_CASE_GROUP DECIMAL NULL,
  E2_PREVALENCE_IN_CONTROL_GROUP DECIMAL NULL,

  RR_IN_PREVIOUS_STUDY DECIMAL NULL,
  RR_POWER DECIMAL NULL,

  RR DECIMAL NULL,
  RR_CI_LOWER DECIMAL NULL,
  RR_CI_UPPER DECIMAL NULL,

  RR_PVALUE FLOAT NULL,
  RR_SIGNIFICANT VARCHAR(1) NULL,

  AVG_NUMBER_OF_DAYS_BETWEEN_E1_AND_E2 DECIMAL NULL,

  --EVENTPERIOD_COUNT_HAVING_E2_RIGHT_AFTER_E1 INT NULL, --this field is filled after the analysis when we consider only trajectories and events that are significant

  E1_AND_E2_TOGETHER_COUNT_IN_EVENTS INT NULL, -- this is the number of eventperiods where E1 and E2 are both present (in both directions).
  E1_BEFORE_E2_COUNT_IN_EVENTS INT NULL,
  E1_AND_E2_ON_SAME_DAY_COUNT_IN_EVENTS INT NULL,
  E1_AFTER_E2_COUNT_IN_EVENTS INT NULL,
  AVG_AGE_OF_E1_BEFORE_E2_IN_EVENTS DECIMAL NULL,

  DIRECTIONAL_POWER DECIMAL NULL,

  DIRECTIONAL_PVALUE FLOAT NULL,
  DIRECTIONAL_SIGNIFICANT VARCHAR(1) NULL,
  DIRECTIONAL_SIGNIFICANT_IF_SAME_DAY_EVENTS_ORDERED VARCHAR(1) NULL
);

INSERT INTO @resultsSchema.@prefiXE1E2_model
  (E1_CONCEPT_ID,
   E2_CONCEPT_ID,
   E1_NAME,
   E2_NAME,
   E1_DOMAIN,
   E2_DOMAIN,
   RR_IN_PREVIOUS_STUDY,
   AVG_NUMBER_OF_DAYS_BETWEEN_E1_AND_E2,
   E1_BEFORE_E2_COUNT_IN_EVENTS)
SELECT
        i.E1_CONCEPT_ID AS E1_CONCEPT_ID,
        i.E2_CONCEPT_ID AS E2_CONCEPT_ID,
        i.E1_NAME AS E1_NAME,
        i.E2_NAME AS E2_NAME,
        i.E1_DOMAIN AS E1_DOMAIN,
        i.E2_DOMAIN AS E2_DOMAIN,
        i.RR_IN_PREVIOUS_STUDY AS RR_IN_PREVIOUS_STUDY,
        CASE WHEN t.avg_number_of_days_between_events IS NULL THEN 0 ELSE t.avg_number_of_days_between_events END AS AVG_NUMBER_OF_DAYS_BETWEEN_E1_AND_E2,
        CASE WHEN t.E1_E2_EVENTPERIOD_COUNT IS NULL THEN 0 ELSE t.E1_E2_EVENTPERIOD_COUNT END AS E1_BEFORE_E2_COUNT_IN_EVENTS
FROM
 @resultsSchema.@prefiXE1E2_model_input i
 LEFT JOIN
 @resultsSchema.@prefiXE1E2_model_temp t ON i.E1_CONCEPT_ID=t.E1_CONCEPT_ID AND i.E2_CONCEPT_ID=t.E2_CONCEPT_ID
ORDER BY
  i.E1_CONCEPT_ID,
  i.E2_CONCEPT_ID;

-- DELETE FROM @resultsSchema.@prefiXE1E2_model where E1_E2_EVENTPERIOD_COUNT< @minPatientsPerEventPair; -- Minimum limit
-- It is correct to not delete ther corresponding rows from @resultsSchema.@prefiXpairs as these rows can still be used for controls

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXE1E2_model) AS VARCHAR),' rows in @resultsSchema.@prefiXE1E2_model'));


CREATE INDEX @prefiXE1E2_model_E1_idx ON @resultsSchema.@prefiXE1E2_model(E1_CONCEPT_ID);
CREATE INDEX @prefiXE1E2_model_E2_idx ON @resultsSchema.@prefiXE1E2_model(E2_CONCEPT_ID);
CREATE INDEX @prefiXE1E2_model_E1E2_idx ON @resultsSchema.@prefiXE1E2_model(E1_CONCEPT_ID,E2_CONCEPT_ID);

--- Calculating E1_COUNT_IN_EVENTS
UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT_IN_EVENTS = (SELECT event_counts_in_eventperiods FROM @resultsSchema.@prefiXevent_counts_in_eventperiods WHERE CONCEPT_ID = E1_CONCEPT_ID);
-- in validation mode E1 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT_IN_EVENTS = 0
WHERE E1_COUNT_IN_EVENTS IS NULL;

--- Calculating E2_COUNT_IN_EVENTS
UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT_IN_EVENTS = (SELECT event_counts_in_eventperiods FROM @resultsSchema.@prefiXevent_counts_in_eventperiods WHERE CONCEPT_ID = E2_CONCEPT_ID);
-- in validation mode E2 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT_IN_EVENTS = 0
WHERE E2_COUNT_IN_EVENTS IS NULL;


--- Create subset of table "pairs" to speed up statistics calculation of counts of E1 and E2 (table contains all pairs containing E1 or E2 as first or last event)
IF OBJECT_ID('@resultsSchema.@prefiXpairs_of_model', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_of_model;

SELECT eventperiod_id,
       e1_concept_id,
       e2_concept_id
INTO @resultsSchema.@prefiXpairs_of_model
FROM @resultsSchema.@prefiXpairs
         WHERE e1_concept_id in (SELECT DISTINCT E1_CONCEPT_ID as concept_id
                                 FROM @resultsSchema.@prefiXE1E2_model
                                 UNION
                                 --union removes duplicates
                                 SELECT DISTINCT E2_CONCEPT_ID as concept_id
                                 FROM @resultsSchema.@prefiXE1E2_model)
                OR
                e2_concept_id in (SELECT DISTINCT E1_CONCEPT_ID as concept_id
                                 FROM @resultsSchema.@prefiXE1E2_model
                                 UNION
                                 --union removes duplicates
                                 SELECT DISTINCT E2_CONCEPT_ID as concept_id
                                 FROM @resultsSchema.@prefiXE1E2_model)
;


--- Calculating E1_COUNT_IN_PAIRS and E2_COUNT_IN_PAIRS

IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;

SELECT concept_id,
       count(distinct eventperiod_id) as ccc
INTO @resultsSchema.@prefiXpairs_stat
FROM (
         SELECT e1_concept_id as concept_id,
                eventperiod_id
         FROM @resultsSchema.@prefiXpairs_of_model

         UNION

         SELECT e2_concept_id as concept_id,
                eventperiod_id
         FROM @resultsSchema.@prefiXpairs_of_model

     ) a
GROUP BY concept_id
;

UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT_IN_PAIRS = (SELECT ccc FROM @resultsSchema.@prefiXpairs_stat p WHERE p.CONCEPT_ID = @prefiXE1E2_model.E1_CONCEPT_ID);
-- in validation mode E1 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT_IN_PAIRS = 0
WHERE E1_COUNT_IN_PAIRS IS NULL;


UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT_IN_PAIRS = (SELECT ccc FROM @resultsSchema.@prefiXpairs_stat p WHERE p.CONCEPT_ID = @prefiXE1E2_model.E2_CONCEPT_ID);
-- in validation mode E1 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT_IN_PAIRS = 0
WHERE E2_COUNT_IN_PAIRS IS NULL;

IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;




--- Calculating E1_COUNT_AS_FIRST_EVENT_OF_PAIRS
--- to speed up, create statistics table  before calculating
IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;

SELECT
        e1_concept_id,
        COUNT(DISTINCT eventperiod_id) as ccc
INTO @resultsSchema.@prefiXpairs_stat
FROM @resultsSchema.@prefiXpairs_of_model
GROUP BY e1_concept_id;

UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT_AS_FIRST_EVENT_OF_PAIRS = (SELECT ccc FROM @resultsSchema.@prefiXpairs_stat p WHERE p.E1_CONCEPT_ID = @prefiXE1E2_model.E1_CONCEPT_ID);
-- in validation mode E1 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT_AS_FIRST_EVENT_OF_PAIRS = 0
WHERE E1_COUNT_AS_FIRST_EVENT_OF_PAIRS IS NULL;

IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;



--- Calculating E2_COUNT_AS_SECOND_EVENT_OF_PAIRS
--- to speed up, create statistics table  before calculating
IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;

SELECT
        e2_concept_id,
        COUNT(DISTINCT eventperiod_id) as ccc
INTO @resultsSchema.@prefiXpairs_stat
FROM @resultsSchema.@prefiXpairs_of_model
GROUP BY e2_concept_id;


UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT_AS_SECOND_EVENT_OF_PAIRS = (SELECT ccc FROM @resultsSchema.@prefiXpairs_stat p WHERE p.E2_CONCEPT_ID = @prefiXE1E2_model.E2_CONCEPT_ID);
-- in validation mode E2 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT_AS_SECOND_EVENT_OF_PAIRS = 0
WHERE E2_COUNT_AS_SECOND_EVENT_OF_PAIRS IS NULL;

IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;




--- Calculating E1_COUNT_AS_LAST_EVENT_OF_EVENTPERIODS

IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;

SELECT e2_concept_id                  as last_event_concept_id,
       count(distinct eventperiod_id) as ccc
INTO @resultsSchema.@prefiXpairs_stat
FROM (
         SELECT a.e2_concept_id,
                a.eventperiod_id
         FROM (
                  -- all non-first events (incl the last)
                  SELECT DISTINCT e2_concept_id,
                                eventperiod_id
                  FROM @resultsSchema.@prefiXpairs_of_model
              ) a
                  LEFT JOIN
              (
                  -- all non-last events
                  SELECT DISTINCT e1_concept_id,
                                  eventperiod_id
                  FROM @resultsSchema.@prefiXpairs_of_model
              ) b ON a.eventperiod_id = b.eventperiod_id AND a.e2_concept_id=b.e1_concept_id
         WHERE b.eventperiod_id IS NULL -- remains events that occur as the last elements of the eventperiods

     ) c
GROUP BY e2_concept_id
;

UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT_AS_LAST_EVENT_OF_EVENTPERIODS = (SELECT ccc FROM @resultsSchema.@prefiXpairs_stat p WHERE p.last_event_concept_id = @prefiXE1E2_model.E1_CONCEPT_ID);
-- in validation mode E1 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT_AS_LAST_EVENT_OF_EVENTPERIODS = 0
WHERE E1_COUNT_AS_LAST_EVENT_OF_EVENTPERIODS IS NULL;


IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;




--- Calculating E2_COUNT_AS_FIRST_EVENT_OF_EVENTPERIODS

IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;

SELECT e1_concept_id                  as first_event_concept_id,
       count(distinct eventperiod_id) as ccc
INTO @resultsSchema.@prefiXpairs_stat
FROM (
         SELECT a.e1_concept_id,
                a.eventperiod_id
         FROM (
                  -- all non-last events (incl the first)
                  SELECT DISTINCT e1_concept_id,
                                eventperiod_id
                  FROM @resultsSchema.@prefiXpairs_of_model
              ) a
                  LEFT JOIN
              (
                  -- all non-first events
                  SELECT DISTINCT e2_concept_id,
                                  eventperiod_id
                  FROM @resultsSchema.@prefiXpairs_of_model
              ) b ON a.eventperiod_id = b.eventperiod_id AND a.e1_concept_id=b.e2_concept_id
         WHERE b.eventperiod_id IS NULL -- remains events that occur as the first elements of the eventperiods

     ) c
GROUP BY e1_concept_id
;

UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT_AS_FIRST_EVENT_OF_EVENTPERIODS = (SELECT ccc FROM @resultsSchema.@prefiXpairs_stat p WHERE p.first_event_concept_id = @prefiXE1E2_model.E2_CONCEPT_ID);
-- in validation mode E1 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT_AS_FIRST_EVENT_OF_EVENTPERIODS = 0
WHERE E2_COUNT_AS_FIRST_EVENT_OF_EVENTPERIODS IS NULL;


IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;





--- Calculating E1_AND_E2_TOGETHER_COUNT_IN_EVENTS
IF OBJECT_ID('@resultsSchema.@prefiXevents_tmp', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents_tmp;

CREATE TABLE
    @resultsSchema.@prefiXevents_tmp AS
    SELECT eventperiod_id,concept_id FROM @resultsSchema.@prefiXevents
        WHERE concept_id in (SELECT DISTINCT E1_CONCEPT_ID as concept_id FROM @resultsSchema.@prefiXE1E2_model
            UNION --union removes duplicates
        SELECT DISTINCT E2_CONCEPT_ID as concept_id FROM @resultsSchema.@prefiXE1E2_model
            );

CREATE INDEX @prefiXevents_tmp_concept_idx ON @resultsSchema.@prefiXevents_tmp (concept_id);

UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_AND_E2_TOGETHER_COUNT_IN_EVENTS = (
    SELECT COUNT(*)
    FROM @resultsSchema.@prefiXevents_tmp
    where concept_id = @prefiXE1E2_model.e2_concept_id
      AND eventperiod_id IN
          (SELECT eventperiod_id
           FROM @resultsSchema.@prefiXevents_tmp
           where concept_id = @prefiXE1E2_model.e1_concept_id)
);

IF OBJECT_ID('@resultsSchema.@prefiXevents_tmp', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents_tmp;



---------------------------------------------------------------------
-- Create gender,age,discharge-time summary table for each E1_CONCEPT_ID->E2_CONCEPT_ID pairs
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXE1E2_summary...');

IF OBJECT_ID('@resultsSchema.@prefiXE1E2_summary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1E2_summary;

--CREATE TABLE @resultsSchema.@prefiXE1E2_summary as
    SELECT
      a.E1_CONCEPT_ID AS E1_CONCEPT_ID,
      a.E2_CONCEPT_ID AS E2_CONCEPT_ID,
      gender,
      age,
      discharge_time,
      count(*) AS eventperiod_count -- as there can't be duplicate event1_concept_id->event2_concept_id event pairs per eventperiod, count(*) in this table gives the eventperiod count
    INTO @resultsSchema.@prefiXE1E2_summary
    FROM @resultsSchema.@prefiXpairs a
      INNER JOIN @resultsSchema.@prefiXE1E2_model b ON a.E1_CONCEPT_ID=b.E1_CONCEPT_ID AND a.E2_CONCEPT_ID=b.E2_CONCEPT_ID -- limit the table to event pairs only that are going to be analyzed
    GROUP BY a.E1_CONCEPT_ID, a.E2_CONCEPT_ID, gender, age, discharge_time
    ORDER BY E1_CONCEPT_ID, E2_CONCEPT_ID, gender, age, discharge_time;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXE1E2_summary) AS VARCHAR),' rows in @resultsSchema.@prefiXE1E2_summary'));


---------------------------------------------------------------------
-- Create gender,age,discharge-time summary table for all event pairs
----------------------------------------------------------------------


INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXsummary...');

IF OBJECT_ID('@resultsSchema.@prefiXsummary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXsummary;

--CREATE TABLE @resultsSchema.@prefiXsummary as
    SELECT
      gender,
      age,
      discharge_time,
      count(DISTINCT eventperiod_id) AS eventperiod_count
    INTO @resultsSchema.@prefiXsummary
    FROM @resultsSchema.@prefiXpairs
    GROUP BY gender, age, discharge_time
    ORDER BY gender, age, discharge_time;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXsummary) AS VARCHAR),' rows in @resultsSchema.@prefiXsummary'));



-------------------------------------------------------

---------------------------------------------------------------------
-- Create gender,age,discharge-time summary table for E1_CONCEPT_ID
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXE1_summary...');

IF OBJECT_ID('@resultsSchema.@prefiXE1_summary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1_summary;

--CREATE TABLE @resultsSchema.@prefiXE1_summary as
    SELECT
      a.E1_CONCEPT_ID,
      gender,
      age,
      discharge_time,
      count(DISTINCT eventperiod_id) AS eventperiod_count
    INTO @resultsSchema.@prefiXE1_summary
    FROM @resultsSchema.@prefiXpairs a
    INNER JOIN (SELECT DISTINCT E1_CONCEPT_ID FROM @resultsSchema.@prefiXE1E2_model) b ON a.E1_CONCEPT_ID=b.E1_CONCEPT_ID -- limit the table to event pairs only that are going to be analyzed
    GROUP BY a.E1_CONCEPT_ID, gender, age, discharge_time;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXE1_summary) AS VARCHAR),' rows in @resultsSchema.@prefiXE1_summary'));


---------------------------------------------------------------------
-- Create gender,age,discharge-time summary table for E2_CONCEPT_ID
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXE2_summary...');

IF OBJECT_ID('@resultsSchema.@prefiXE2_summary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE2_summary;
--CREATE TABLE @resultsSchema.@prefiXE2_summary as
    SELECT
      a.E2_CONCEPT_ID,
      gender,
      age,
      discharge_time,
      count(DISTINCT eventperiod_id) AS eventperiod_count
    INTO @resultsSchema.@prefiXE2_summary
    FROM @resultsSchema.@prefiXpairs a
    INNER JOIN (SELECT DISTINCT E2_CONCEPT_ID FROM @resultsSchema.@prefiXE1E2_model) b ON a.E2_CONCEPT_ID=b.E2_CONCEPT_ID -- limit the table to event pairs only that are going to be analyzed
    GROUP BY a.E2_CONCEPT_ID, gender, age, discharge_time;

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES (CONCAT('..done. There are ',CAST((SELECT COUNT(*) FROM @resultsSchema.@prefiXE2_summary) AS VARCHAR),' rows in @resultsSchema.@prefiXE2_summary'));
