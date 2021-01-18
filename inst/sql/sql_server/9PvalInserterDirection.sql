UPDATE @resultsSchema.@prefixE1E2_model
SET
  directional_event_pair_pvalue=@pval,
  POWER_DIRECTION=@powerDirection
WHERE
  E1_CONCEPT_ID = '@diag1'
  AND E2_CONCEPT_ID = '@diag2';
