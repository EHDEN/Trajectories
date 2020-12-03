---------------------------------------------------------------------
-- Create separate table for event pairs that are going to be analysed
-- and empty slots for p and effect values
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

SELECT
        i.E1_CONCEPT_ID AS E1_CONCEPT_ID,
        i.E2_CONCEPT_ID AS E2_CONCEPT_ID,
        i.E1_NAME AS E1_NAME,
        i.E1_DOMAIN AS E1_DOMAIN,
        i.E2_NAME AS E2_NAME,
        i.E2_DOMAIN AS E2_DOMAIN,
        CASE WHEN t.avg_number_of_days_between_events IS NULL THEN 0 ELSE t.avg_number_of_days_between_events END AS avg_number_of_days_between_events,
        CASE WHEN t.E1_E2_EVENTPERIOD_COUNT IS NULL THEN 0 ELSE t.E1_E2_EVENTPERIOD_COUNT END AS E1_E2_EVENTPERIOD_COUNT
INTO @resultsSchema.@prefiXE1E2_model
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

ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD EVENT_PAIR_PVALUE FLOAT NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD EVENT_PAIR_PVALUE_SIGNIFICANT VARCHAR(1) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD EVENT_PAIR_EFFECT DECIMAL NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD EVENTPERIOD_COUNT_E1_OCCURS_FIRST INT NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD AVG_AGE_OF_EVENTPERIOD_E1_OCCURS_FIRST FLOAT NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD EVENTPERIOD_COUNT_E2_OCCURS_FIRST INT NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD EVENTPERIOD_COUNT_E1_E2_OCCUR_ON_SAME_DAY INT NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD DIRECTIONAL_EVENT_PAIR_PVALUE FLOAT NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD DIRECTIONAL_EVENT_PAIR_PVALUE_SIGNIFICANT VARCHAR(1) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD E1_COUNT INT NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD E2_COUNT INT NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD E2_COUNT_IN_CONTROL_GROUP INT NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model ADD EVENTPERIOD_COUNT_HAVING_E2_RIGHT_AFTER_E1 INT NULL; --this field is filled after the analysis when we consider only trajectories and events that are significant


CREATE INDEX @prefiXE1E2_model_E1_idx ON @resultsSchema.@prefiXE1E2_model(E1_CONCEPT_ID);
CREATE INDEX @prefiXE1E2_model_E2_idx ON @resultsSchema.@prefiXE1E2_model(E2_CONCEPT_ID);
CREATE INDEX @prefiXE1E2_model_E1E2_idx ON @resultsSchema.@prefiXE1E2_model(E1_CONCEPT_ID,E2_CONCEPT_ID);

--- Adding event counts to E1E2_model
UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT = (SELECT event_counts_in_eventperiods FROM @resultsSchema.@prefiXevent_counts_in_eventperiods WHERE CONCEPT_ID = E1_CONCEPT_ID);
-- in validation mode E1 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E1_COUNT = 0
WHERE E1_COUNT IS NULL;


UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT = (SELECT event_counts_in_eventperiods FROM @resultsSchema.@prefiXevent_counts_in_eventperiods WHERE CONCEPT_ID = E2_CONCEPT_ID);
-- in validation mode E2 might be not present in data, force these counts to 0
UPDATE @resultsSchema.@prefiXE1E2_model
SET E2_COUNT = 0
WHERE E2_COUNT IS NULL;

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
