SELECT
  *
FROM @resultsSchema.@prefixE1E2_model

WHERE
  DIRECTIONAL_EVENT_PAIR_PVALUE_SIGNIFICANT='*'
  AND
  EVENT_PAIR_RR >= @rr

ORDER BY E1_CONCEPT_ID, E2_CONCEPT_ID; -- order by concept_ids as it provides consistent (similar) orderding both from exploratory mode and validation mode
