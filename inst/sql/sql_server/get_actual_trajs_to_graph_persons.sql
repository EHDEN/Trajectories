SELECT  DISTINCT(eventperiod_id) AS EVENTPERIOD_ID
FROM
        @resultsSchema.@prefiXgraph_event_pairs
ORDER BY eventperiod_id;
