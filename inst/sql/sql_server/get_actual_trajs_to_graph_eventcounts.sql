SELECT  event,
		COUNT(*) as count
FROM
        @resultsSchema.@prefiXgraph_events
GROUP BY 
		event
ORDER BY
        COUNT(*) DESC;
