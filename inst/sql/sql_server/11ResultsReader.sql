SELECT
  *
FROM @resultsSchema.@prefixd1d2_model

WHERE
  directional_event_pair_pvalue <= @cutoff_val
  AND
  event_pair_effect >= @effectSize

ORDER BY event_pair_effect DESC;
