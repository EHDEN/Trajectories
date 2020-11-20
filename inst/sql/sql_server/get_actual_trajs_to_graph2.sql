SELECT  *
FROM
        @resultsSchema.@prefiXgraph_event_pairs
WHERE eventperiod_id IN ( @eventperiodids )
ORDER BY
        eventperiod_id,
        e1_cohort_day,
        e2_cohort_day,
        e1_concept_id,
        e2_concept_id
;
