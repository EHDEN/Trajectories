SELECT  event1_concept_id,
        event2_concept_id,
        cohort_id
FROM
        @resultsSchema.@prefiXexact_sign_pairs
ORDER BY
        event1_concept_id,
        event2_concept_id
;
