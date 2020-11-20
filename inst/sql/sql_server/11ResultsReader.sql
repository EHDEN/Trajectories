SELECT
  *
FROM @resultsSchema.@prefixE1E2_model

WHERE
  directional_event_pair_pvalue <= @cutoff_val
  AND
  EVENT_PAIR_EFFECT >= @effectSize

ORDER BY EVENT_PAIR_EFFECT DESC;
