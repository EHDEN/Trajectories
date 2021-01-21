SELECT
  *
  FROM @resultsSchema.@prefixE1E2_model
WHERE
  EVENT_PAIR_PVALUE_SIGNIFICANT='*'
;
