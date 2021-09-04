-- check that cohort table exists to prevent error appearance in the end of the script (run random SELECT query)
SELECT * FROM @target_database_schema.@target_cohort_table WHERE cohort_definition_id = @target_cohort_id;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;

INSERT INTO @target_database_schema.@target_cohort_table (
  cohort_definition_id,
  subject_id,
  cohort_start_date,
  cohort_end_date)
SELECT
  @target_cohort_id as cohort_definition_id,
  person_id,
  MIN(observation_period_start_date) AS start_date,
  MAX(observation_period_end_date) AS end_date
FROM @cdm_database_schema.OBSERVATION_PERIOD
GROUP BY person_id
;

