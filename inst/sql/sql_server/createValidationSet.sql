-- clear previous settings
UPDATE @resultsSchema.@prefiXcohort SET
    cohort_definition_id=1; --1=discovery data

-- new assignments to validation set
UPDATE @resultsSchema.@prefiXcohort SET
    cohort_definition_id=2 --2=validation data
WHERE
    subject_id IN (
        SELECT subject_id
        FROM (SELECT subject_id, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM @resultsSchema.@prefiXcohort) tmp
        WHERE rn <= ROUND(@size * (SELECT COUNT(*) FROM @resultsSchema.@prefiXcohort))
    );
