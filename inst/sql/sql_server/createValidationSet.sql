-- clear previous settings
UPDATE @resultsSchema.@prefiXtraj_base_cohort SET
    is_validation_set=0; --1=discovery data

-- new assignments to validation set
UPDATE @resultsSchema.@prefiXtraj_base_cohort SET
    is_validation_set=1
WHERE
    person_id IN (
        SELECT person_id
        FROM (SELECT person_id, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM @resultsSchema.@prefiXtraj_base_cohort) tmp
        WHERE rn <= ROUND(@size * (SELECT COUNT(*) FROM @resultsSchema.@prefiXtraj_base_cohort))
    );
