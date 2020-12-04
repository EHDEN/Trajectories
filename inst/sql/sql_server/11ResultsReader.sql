SELECT
  *
FROM @resultsSchema.@prefixE1E2_model

WHERE
  directional_event_pair_pvalue <= @cutoff_val
  AND
  EVENT_PAIR_EFFECT >= @effectSize

ORDER BY E1_CONCEPT_ID, E2_CONCEPT_ID; -- order by concept_ids as it provides consistent (similar) orderding both from exploratory mode and validation mode
