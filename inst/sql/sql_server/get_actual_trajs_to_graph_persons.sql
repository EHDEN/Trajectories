SELECT  DISTINCT(eventperiod_id) AS eventperiod_id
FROM
        @resultsSchema.@prefiXgraph_events
ORDER BY eventperiod_id;
