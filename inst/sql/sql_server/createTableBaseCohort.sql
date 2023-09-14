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
       cohort.cohort_end_date AS eventperiod_end_date,
       0 AS is_validation_set
INTO @resultsSchema.@prefiXtraj_base_cohort
FROM
    @cohortDatabaseSchema.@cohortTableName AS cohort
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
