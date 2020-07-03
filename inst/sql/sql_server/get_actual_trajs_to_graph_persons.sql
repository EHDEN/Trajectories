SELECT  DISTINCT(cohort_id) AS cohort_id
FROM
        @resultsSchema.@prefiXgraph_events
ORDER BY cohort_id;