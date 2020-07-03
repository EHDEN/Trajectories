-- check that cohort table exists to prevent error appearance in the end of the script (run random SELECT query)
SELECT * FROM @target_database_schema.@target_cohort_table WHERE cohort_definition_id = @target_cohort_id;

-- put all persons the the cohort with unlimited time window
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
SELECT
    @target_cohort_id,
    person_id,
    '1900-01-01'::date as cohort_start_date,
    '2100-01-01'::date as cohort_end_date
FROM @cdm_database_schema.person
GROUP BY person_id;

